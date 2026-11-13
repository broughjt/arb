{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import Control.Monad (forM, forM_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), (<.>), takeDirectory)

import Agda.Compiler.Backend hiding ((.=))
import Agda.Compiler.Common (curDefs, setInterface, sortDefs)
import Agda.Interaction.Highlighting.Precise hiding (toList)
import Agda.Interaction.Imports
  (Source(..), getNonMainInterface, importPrimitiveModules, parseSource)
import Agda.Interaction.Library.Base (AgdaLibFile(..), OptionsPragma(..))
import Agda.Interaction.Options (defaultOptions, optPragmaOptions)
import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Abstract.Views (mapExpr)
import Agda.Syntax.Common
import qualified Agda.Syntax.Common.Aspect as Aspect
import Agda.Syntax.Common.Pretty (pretty, render)
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Info (exprNoRange)
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Internal.Generic as I
import Agda.Syntax.Position
import Agda.Syntax.Scope.Base (recomputeInverseScope')
import Agda.Syntax.Translation.InternalToAbstract (reify)
import qualified Agda.TypeChecking.Monad.Base as TCM
import Agda.TypeChecking.Errors (prettyError)
import Agda.TypeChecking.Records (isRecord)
import Agda.TypeChecking.Reduce (normalise)
import Agda.Utils.FileName (absolute)

main :: IO ()
main = getArgs >>= \case
  "-o" : directory : file : files -> do
    state <- initStateIO
    (result, _) <- runTCM initEnv state $
      (Right <$> annotate (file :| files))
        `catchError` (fmap Left . prettyError)
    case result of
      Left message -> die (render message)
      Right modules -> write directory modules
  _ -> die "usage: agda-annotate -o DIRECTORY FILE..."

-- A module Agda visited while checking the files. Only the modules of the
-- files themselves have a source: the path as it was given, and the tokens of
-- each code block.
data Annotations = Annotations
  { moduleName :: String
  , moduleTypes :: Aeson.Value
  , moduleSource :: Maybe (FilePath, [[Token]])
  }

-- Writes DIRECTORY/agda-annotations.json, with an entry for each module that
-- has a source,
--
--   { "Data.Bool.Core":
--       { "path": "source/Data/Bool/Core.lagda.typ"
--       , "blocks": [[{"s": "module", "a": ["Keyword"]}, ...], ...] } }
--
-- and DIRECTORY/types/MODULE.json for every module, the libraries' included,
-- keyed by the position of each definition's name:
--
--   { "1200": {"name": "not", "type": "Bool -> Bool"} }
--
-- Positions count characters of the source file from 1, as the anchors of
-- Agda's HTML backend do.
write :: FilePath -> [Annotations] -> IO ()
write directory modules = do
  createDirectoryIfMissing True (directory </> "types")
  Aeson.encodeFile (directory </> "agda-annotations.json") $ Aeson.object
    [ Key.fromString (moduleName m) .= Aeson.object
        [ "path" .= path, "blocks" .= map (map tokenJson) blocks ]
    | m <- modules
    , Just (path, blocks) <- [moduleSource m]
    ]
  forM_ modules \m ->
    ByteString.writeFile (directory </> "types" </> moduleName m <.> "json")
      (Aeson.encode (moduleTypes m))

annotate :: NonEmpty FilePath -> TCM [Annotations]
annotate files = do
  -- Agda resolves a session's include paths once, from the library that
  -- contains this directory, so the files must all belong to one library.
  root <- liftIO $ absolute (takeDirectory (NonEmpty.head files))
  setCommandLineOptions' root defaultOptions

  -- Check each file as an import. typeCheckMain checks its module in place in
  -- the session's state, so calling it for several files makes the output
  -- depend on their order.
  sources <- forM files \file -> do
    source <- parseSource =<< TCM.srcFromPath =<< liftIO (absolute file)
    -- An import is checked for compatibility with the options of the module
    -- importing it (--safe may not import unsafe code, say). Nothing imports
    -- this module, so give the session the module's own options: those of its
    -- library, then those of its OPTIONS pragmas.
    -- TODO: Use Agda's setOptionsFromSourcePragmas once the Agda this builds
    -- against exports it.
    setPragmaOptions (optPragmaOptions defaultOptions)
    mapM_ checkAndSetOptionsFromPragma $
      map _libPragmas (srcProjectLibs source) ++
      [ OptionsPragma strings range
      | C.OptionsPragma range strings <- C.modPragmas (srcModule source) ]
    importPrimitiveModules
    _ <- getNonMainInterface (srcModuleName source) (Just source)
    pure (srcModuleName source, file)

  visited <- useTC stVisitedModules
  forM (Map.elems visited) \moduleInfo -> do
    let interface = miInterface moduleInfo
        name = iTopLevelModuleName interface
    setInterface interface
    definitions <- map snd . sortDefs <$> curDefs
    -- A scope read from an interface has no inverse maps, without which every
    -- name is printed fully qualified.
    types <- evalWithScope (recomputeInverseScope' (iInsideScope interface))
                           (mapM typeEntry definitions)
    let blocks =
          codeBlocks (tokens (iSource interface) (iHighlighting interface))
    pure Annotations
      { moduleName = render (pretty name)
      , moduleTypes = Aeson.object (catMaybes types)
      , moduleSource = do
          file <- lookup name (toList sources)
          Just (file, blocks)
      }

------------------------------------------------------------------------
-- Tokens, after Agda's HTML backend

-- A maximal run of characters with the same highlighting.
type Token = (String, Aspects)

tokens :: LazyText.Text -> HighlightingInfo -> [Token]
tokens source highlighting =
  map (\run -> (toList (fmap fst run), snd (NonEmpty.head run))) $
  NonEmpty.groupBy ((==) `on` snd) $
  zipWith
    (\position character ->
      (character, IntMap.findWithDefault mempty position aspects))
    [1 ..]
    (LazyText.unpack source)
  where
  aspects = toMap highlighting

-- In a literate file the fence delimiters are Markup and the prose is
-- Background, so the code blocks are the runs between delimiters that hold
-- something else. A file that is not literate is one block.
codeBlocks :: [Token] -> [[Token]]
codeBlocks = filter (any isCode) . splitWhen ((== Just Markup) . aspect . snd)
  where
  isCode (_, aspects) = aspect aspects `notElem` [Nothing, Just Background]

-- "s" is the token's text, "a" its classes, named as the HTML backend names
-- them, "d" the module and position of the definition of the name it refers
-- to, and "def" is set where the token is that definition.
tokenJson :: Token -> Aeson.Value
tokenJson (text, aspects) = Aeson.object $ concat
  [ [ "s" .= text ]
  , [ "a" .= classes | not (null classes) ]
  , case definitionSite aspects of
      Just (Aspect.DefinitionSite name position here _) -> concat
        [ [ "d" .= (render (pretty name), position) ]
        , [ "def" .= True | here ]
        ]
      Nothing -> []
  ]
  where
  classes =
    map show (toList (otherAspects aspects)) ++
    concatMap aspectClasses (toList (aspect aspects))

aspectClasses :: Aspect -> [String]
aspectClasses = \case
  Aspect.Name kind operator ->
    map kindClass (toList kind) ++ [ "Operator" | operator ]
  other -> [show other]
  where
  kindClass = \case
    Aspect.Constructor Inductive -> "InductiveConstructor"
    Aspect.Constructor CoInductive -> "CoinductiveConstructor"
    kind -> show kind

------------------------------------------------------------------------
-- Types of definitions, after the 1lab's HTML backend

typeEntry :: Definition -> TCM (Maybe (Key.Key, Aeson.Value))
typeEntry definition = forM (definitionPosition definition) \position -> do
  text <- typeText definition
  let name = render (pretty (qnameName (defName definition)))
  pure
    ( Key.fromString (show position)
    , Aeson.object [ "name" .= name, "type" .= text ]
    )

-- Where the definition's name is bound, for the definitions written in the
-- source rather than generated by Agda.
definitionPosition :: Definition -> Maybe Int
definitionPosition definition
  | defCopy definition = Nothing
  | isExtendedLambdaName name = Nothing
  | isAbsurdLambdaName name = Nothing
  | isWithFunction (theDef definition) = Nothing
  | otherwise =
      fromIntegral . posPos <$> rStart (nameBindingSite (qnameName name))
  where
  name = defName definition

-- The type as it is shown to a reader: normalised, but unfolding only the
-- instances it mentions and the fields of their classes, so that overloaded
-- notation resolves to what it stands for and nothing else is unfolded.
typeText :: Definition -> TCM Text.Text
typeText definition = do
  instances <- usedInstances (I.unEl (defType definition))
  normalised <-
    locallyReduceDefs (OnlyReduceDefs instances) $
      normalise (defType definition)
  expression <- removeImplicitTypes <$> reify normalised
  Text.pack . render <$> prettyATop expression

usedInstances :: I.TermLike a => a -> TCM (Set QName)
usedInstances = I.foldTerm \case
  I.Def name _ -> do
    definition <- getConstInfo name
    case defInstance definition of
      Just info -> Set.insert name <$> classFields (TCM.instanceClass info)
      Nothing -> pure mempty
  _ -> pure mempty

classFields :: QName -> TCM (Set QName)
classFields name = isRecord name >>= \case
  Just RecordData{ _recFields = fields } ->
    pure (Set.fromList (map I.unDom fields))
  _ -> pure mempty

-- Replaces the type of every implicit binder with an underscore and merges
-- the neighbouring ones, so that {A : Type} {B : A -> Type} -> ... prints as
-- {A B : _} -> ...
removeImplicitTypes :: A.Expr -> A.Expr
removeImplicitTypes = \case
  A.Pi _ bindings body ->
    makePi
      (map (mapExpr removeImplicitTypes . removeType) (toList bindings))
      (removeImplicitTypes body)
  A.Fun info argument result ->
    A.Fun info (removeImplicitTypes <$> argument) (removeImplicitTypes result)
  expression -> expression
  where
  removeType binding = case binding of
    A.TBind range info names _
      | Hidden <- getHiding binding -> A.TBind range info names underscore
    _ -> binding

makePi :: [A.TypedBinding] -> A.Expr -> A.Expr
makePi [] body = body
makePi (b : bs) (A.Pi _ bs' body) = makePi (b : bs ++ toList bs') body
makePi (b : bs) body = A.Pi exprNoRange (mergeBindings b bs) body

mergeBindings :: A.TypedBinding -> [A.TypedBinding] -> NonEmpty A.TypedBinding
mergeBindings
  (A.TBind range info names A.Underscore{})
  (A.TBind _ _ names' A.Underscore{} : bindings) =
    mergeBindings (A.TBind range info (names <> names') underscore) bindings
mergeBindings binding bindings = binding :| bindings
