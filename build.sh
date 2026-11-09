# A stopgap build of the notes site, kept only until arb's build command
# replaces it. It runs from anywhere inside a notes repository and writes the
# site to `build/`. It deliberately leaves out the Agda annotations, the
# Pagefind index, the fonts and the bundled TypeScript.
#
# The flake prepends `styles=<the template's stylesheet partials>`.

# The project root is the nearest directory with a weibian.json.
root=$PWD
until [[ -e $root/weibian.json ]]; do
  if [[ $root == / ]]; then
    echo "arb-build: no weibian.json in this directory or above it" >&2
    exit 1
  fi
  root=$(dirname "$root")
done
cd "$root"

# Inputs and the staging directory live under the root, since Typst reads only
# files inside it.
scratch=.arb
rm -rf "$scratch"
mkdir -p "$scratch"

# The source list, one path per line relative to the root, from weibian.json's
# globs. `**` and the top-level `*` glob overlap, hence `sort -u`.
shopt -s globstar nullglob
jq -r '.sources[]' weibian.json | while read -r glob; do
  # shellcheck disable=SC2086 # the glob must expand
  printf '%s\n' $glob
done | sort -u > "$scratch/sources.txt"

typst compile \
  --root . \
  --features bundle,html \
  --format bundle \
  --input sources=/$scratch/sources.txt \
  site.typ "$scratch/staging"

# The partials in order, separated by blank lines, as the old wrapper joined
# them.
separator=
for partial in "$styles"/*.css; do
  printf '%b' "$separator"
  cat "$partial"
  separator='\n\n'
done > "$scratch/staging/style.css"

rm -rf build
mv "$scratch/staging" build
rm -rf "$scratch"
