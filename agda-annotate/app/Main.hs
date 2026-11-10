-- Type-check literate Agda modules and emit JSON annotations for the site
-- build.
--
-- Usage: agda-annotate -o OUTDIR FILE [FILE ...]
--
-- Type-checks every given file (plus everything they import) and, for each
-- visited literate-Typst module, emits token and type annotations:
--
--   OUTDIR/agda-annotations.json
--     { "Base.Family.Properties":
--         { "path": "subtrees/barb/source/Base/Family/Properties.lagda.typ",
--           "blocks": [ [ {"p": 135, "s": "module", "a": ["Keyword"]}, ... ] ] }
--       , ... }
--   OUTDIR/types/Base.Family.Properties.json
--     { "1200": {"name": "＝→reflexive", "type": "..."} }
--
-- agda-annotations.json is one input for the Typst build (one entry per module,
-- one block per ```agda fence, in document order); the types files are
-- per-module assets fetched by the hover client. "path" is relative to the
-- working directory the tool is invoked from. Builtin (non-Typst) modules
-- such as Agda.Primitive get a types file but no agda-annotations.json entry:
-- their fences render nowhere, but corpus tokens carry definition sites into
-- them and the hover client can still resolve those to a declared type.
--
-- Positions are 1-based character (not byte) offsets into the literate
-- source file, matching the anchors Agda's own HTML backend emits.
--
-- The type-extraction machinery (typeToText, removeImpls, usedInstances) and
-- the token stream construction are adapted from 1lab's support/shake
-- (itself derived from Agda's HTML backend; see the copyright note there).
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, BangPatterns #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL

import qualified Data.Map.Strict as Map

import Control.Monad (forM, forM_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import System.Directory
  (createDirectoryIfMissing, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)

import Agda.Compiler.Backend
import Agda.Compiler.Common (setInterface, curDefs, sortDefs)
import Agda.Interaction.FindFile (findFile)
import Agda.Interaction.Imports
  (Source(..), getNonMainInterface, importPrimitiveModules, parseSource)
import Agda.Interaction.Highlighting.Precise hiding (toList)
import Agda.Interaction.Options (defaultOptions)

import qualified Agda.Syntax.Common.Aspect as Asp
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C
import Agda.Interaction.Library.Base (AgdaLibFile(..), OptionsPragma(..))
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Internal.Generic as I

import Agda.Syntax.Abstract (TypedBinding(..))
import Agda.Syntax.Abstract.Views (mapExpr)
import Agda.Syntax.Info (exprNoRange)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty (render, pretty)
import Agda.Syntax.Position
import Agda.Syntax.TopLevelModuleName (TopLevelModuleName)
import Agda.Syntax.Translation.InternalToAbstract (Reify(reify))

import qualified Agda.TypeChecking.Monad.Base as TCB
import Agda.TypeChecking.Errors (prettyError)
import Agda.TypeChecking.Records (isRecord)
import Agda.TypeChecking.Reduce (normalise)

import Agda.Utils.FileName (AbsolutePath, absolute, filePath)

main :: IO ()
main = getArgs >>= \case
  ("-o" : outDir : files) | not (null files) -> run outDir files
  _ -> die "usage: agda-annotate -o OUTDIR FILE [FILE ...]"

-- | One emitted module: name, types JSON, and — for literate-Typst modules
-- only — the source path (relative to the invocation directory) and blocks
-- JSON. Builtin modules carry Nothing: they still get a types file.
data ModuleAnnotations = ModuleAnnotations
  { modName     :: String
  , modLiterate :: Maybe (FilePath, Aeson.Value)
  , modTypes    :: Aeson.Value
  }

run :: FilePath -> [FilePath] -> IO ()
run outDir files = do
  absPaths <- mapM absolute files
  tcState <- initStateIO
  (result, _) <- runTCM initEnv tcState $
    (Right <$> annotateAll absPaths) `catchError` \err ->
      Left <$> prettyError err
  case result of
    Left err -> hPutStrLn stderr (render err) >> die "type checking failed"
    Right mods -> writeOutputs outDir mods

-- | Find the Agda project root: the nearest ancestor of the given file
-- containing a .agda-lib file. Anchoring the options there (rather than at
-- the working directory) keeps the tool invocable from anywhere.
findProjectRoot :: FilePath -> IO FilePath
findProjectRoot = go . takeDirectory
  where
  go dir = do
    entries <- listDirectory dir
    if any ((== ".agda-lib") . takeExtension) entries
      then pure dir
      else let parent = takeDirectory dir in
        if parent == dir
          then die "no .agda-lib file found above the first source file"
          else go parent

-- | Type-check all the given files, then annotate every visited module:
-- token blocks + types for literate-Typst modules, types only for builtin
-- .agda modules.
annotateAll :: [AbsolutePath] -> TCM [ModuleAnnotations]
annotateAll absPaths = do
  projectRoot <- liftIO $ case absPaths of
    p : _ -> absolute =<< findProjectRoot (filePath p)
    [] -> die "no source files given"
  setCommandLineOptions' projectRoot defaultOptions
  -- Check each file as an import. typeCheckMain checks its module in place in
  -- the session's state, so calling it for several files makes the output
  -- depend on their order. The options are set as typeCheckMain sets them.
  forM_ absPaths \absPath -> do
    source <- parseSource =<< TCB.srcFromPath absPath
    mapM_ checkAndSetOptionsFromPragma $
      map _libPragmas (srcProjectLibs source) ++
      [ OptionsPragma options range
      | C.OptionsPragma range options <- C.modPragmas (srcModule source) ]
    importPrimitiveModules
    getNonMainInterface (srcModuleName source) (Just source)

  visited <- useTC stVisitedModules
  cwd <- liftIO getCurrentDirectory

  forM (Map.toList visited) \(name, moduleInfo) -> do
    let iface = miInterface moduleInfo
    literate <-
      if iFileType iface /= TypstFileType then pure Nothing else do
        let blocks = codeBlocks (tokenStream (iSource iface) (iHighlighting iface))
        sourcePath <- filePath <$> (fileFromId . srcFileId =<< findFile name)
        pure $ Just
          ( makeRelative cwd sourcePath
          , Aeson.toJSON (map (map tokenJson) blocks)
          )

    setInterface iface
    defs <- map snd . sortDefs <$> curDefs
    types <- withScope_ (iInsideScope iface) (mapM defEntry defs)

    pure ModuleAnnotations
      { modName     = render (pretty name)
      , modLiterate = literate
      , modTypes    = Aeson.object (mapMaybe id types)
      }

writeOutputs :: FilePath -> [ModuleAnnotations] -> IO ()
writeOutputs outDir mods = do
  createDirectoryIfMissing True (outDir </> "types")
  BSL.writeFile (outDir </> "agda-annotations.json") $ Aeson.encode $ Aeson.object
    [ Key.fromString (modName m) Aeson..= Aeson.object
        [ "path" Aeson..= path, "blocks" Aeson..= blocks ]
    | m <- mods
    , Just (path, blocks) <- [modLiterate m]
    ]
  forM_ mods \m ->
    BSL.writeFile (outDir </> "types" </> modName m <.> "json")
      (Aeson.encode (modTypes m))

------------------------------------------------------------------------
-- Token stream (adapted from the HTML backend)

type TokenInfo = (Int, String, Aspects)

tokenStream :: TL.Text -> HighlightingInfo -> [TokenInfo]
tokenStream contents info =
  map (\cs -> case cs of
          (mi, (pos, _)) : _ -> (pos, map (snd . snd) cs, fromMaybe mempty mi)
          [] -> error "impossible: empty token group") $
  List.groupBy ((==) `on` fst) $
  zipWith (\pos c -> (IntMap.lookup pos infoMap, (pos, c))) [1..] (TL.unpack contents)
  where
  infoMap = toMap info

-- | Split the literate token stream on Markup tokens (the fence delimiters)
-- and keep only the runs that contain actual Agda code, dropping the
-- Background (prose) runs.  One surviving run should correspond to one
-- ```agda fence.
codeBlocks :: [TokenInfo] -> [[TokenInfo]]
codeBlocks = filter containsCode . splitWhen isMarkup
  where
  isMarkup (_, _, mi) = aspect mi == Just Markup
  containsCode = any \(_, _, mi) -> aspect mi /= Just Background && aspect mi /= Nothing
  splitWhen p xs = case break p xs of
    (before, []) -> [before]
    (before, _ : rest) -> before : splitWhen p rest

tokenJson :: TokenInfo -> Aeson.Value
tokenJson (pos, s, mi) = Aeson.object $ concat
  [ [ "p" Aeson..= pos, "s" Aeson..= s ]
  , [ "a" Aeson..= classes | not (null classes) ]
  , case definitionSite mi of
      Just (Asp.DefinitionSite m defPos here _anchor) -> concat
        [ [ "d" Aeson..= (render (pretty m), defPos) ]
        , [ "def" Aeson..= True | here ]
        ]
      Nothing -> []
  ]
  where
  classes = concat
    [ map show (toList (otherAspects mi))
    , concatMap aspectClasses (toList (aspect mi))
    ]

aspectClasses :: Aspect -> [String]
aspectClasses (Asp.Name mKind op) = kindClass ++ opClass
  where
  kindClass = toList (fmap showKind mKind)
  showKind (Asp.Constructor Inductive)   = "InductiveConstructor"
  showKind (Asp.Constructor CoInductive) = "CoinductiveConstructor"
  showKind k = show k
  opClass = ["Operator" | op]
aspectClasses a = [show a]

------------------------------------------------------------------------
-- Definition types (adapted from 1lab's HTML.Backend)

defEntry :: Definition -> TCM (Maybe (Key.Key, Aeson.Value))
defEntry def = case definitionAnchor def of
  Nothing -> pure Nothing
  Just pos -> do
    ty <- typeToText def
    let name = render (pretty (qnameName (defName def)))
    pure $ Just
      ( Key.fromString (show pos)
      , Aeson.object [ "name" Aeson..= name, "type" Aeson..= ty ]
      )

definitionAnchor :: Definition -> Maybe Int
definitionAnchor def
  | defCopy def                        = Nothing
  | isExtendedLambdaName (defName def) = Nothing
  | isAbsurdLambdaName (defName def)   = Nothing
  | isWithFunction (theDef def)        = Nothing
  | otherwise =
      case rStart (nameBindingSite (qnameName (defName def))) of
        Just pn -> Just (fromIntegral (posPos pn))
        Nothing -> Nothing

typeToText :: Definition -> TCM Text.Text
typeToText d = do
  ui <- usedInstances (I.unEl (defType d))
  ty <- locallyReduceDefs (OnlyReduceDefs ui) $ normalise (defType d)
  expr <- removeImpls <$> reify ty
  Text.pack . render <$> prettyATop expr

usedInstances :: I.TermLike a => a -> TCM (Set QName)
usedInstances = I.foldTerm \case
  I.Def q _ -> do
    def <- getConstInfo q
    case defInstance def of
      Just c  -> Set.insert q <$> getClass (TCB.instanceClass c)
      Nothing -> pure mempty
  _ -> pure mempty

getClass :: QName -> TCM (Set QName)
getClass q = isRecord q >>= \case
  Just RecordData{ _recFields = f } -> pure $! Set.fromList (map I.unDom f)
  _ -> pure mempty

removeImpls :: A.Expr -> A.Expr
removeImpls (A.Pi _ (x :| xs) e) =
  let
    fixup :: TypedBinding -> TypedBinding
    fixup q@(TBind rng inf as _)
      | Hidden <- getHiding q = TBind rng inf as underscore
      | otherwise = q
    fixup q = q
  in makePi (map (mapExpr removeImpls . fixup) (x:xs)) (removeImpls e)
removeImpls (A.Fun span' arg ret) =
  A.Fun span' (removeImpls <$> arg) (removeImpls ret)
removeImpls e = e

makePi :: [TypedBinding] -> A.Expr -> A.Expr
makePi [] e = e
makePi (b:bs) (A.Pi _ bs' e') = makePi (b:bs ++ toList bs') e'
makePi (b:bs) e = A.Pi exprNoRange (b `mergeTBinds` bs) e

mergeTBinds :: TypedBinding -> [TypedBinding] -> NonEmpty TypedBinding
mergeTBinds (TBind r i as A.Underscore{}) (TBind _ _ as' A.Underscore{} : bs) =
  mergeTBinds (TBind r i (as <> as') underscore) bs
mergeTBinds x xs = x :| xs
