#!/usr/bin/env bash
set -euo pipefail

# Build and serve the entire 7GUIs site locally, mimicking CI/GitHub Pages.
#
# Usage:
#   ./serve-local.sh              # build all + serve
#   ./serve-local.sh --port=9000  # custom port
#
# Auto-rebuild on source change (only watches source files, not build outputs):
#   find shared/7guis-theme.css elm/template.html \
#        $(find elm/*/src -name '*.elm') \
#        $(find purescript/halogen/*/src -name '*.purs') \
#        $(find purescript/halogen/*/dev -name '*.html' -o -name '*.css') \
#     | entr -r ./serve-local.sh

PORT=8765

for arg in "$@"; do
  case "$arg" in
    --port=*) PORT="${arg#*=}" ;;
  esac
done

ROOT="$(cd "$(dirname "$0")" && pwd)"
DOCS="$ROOT/docs"

# Kill any existing server on the port
pkill -f "python3 -m http.server $PORT" 2>/dev/null || true

echo "=== Cleaning docs/ ==="
rm -r "$DOCS"
mkdir -p "$DOCS/rescript/react" "$DOCS/elm" "$DOCS/purescript/halogen"

# ============ ReScript ============
echo ""
echo "=== Building ReScript ==="
cd "$ROOT/rescript-7guis"
[ -d node_modules ] || npm ci
npm run res:build 2>&1
npx vite build --base=/rescript/react/ 2>&1
cp -r dist/* "$DOCS/rescript/react/"
echo "  ReScript: done"

# ============ Elm ============
echo ""
echo "=== Building Elm ==="
if ! command -v elm &>/dev/null; then
  echo "  ERROR: elm not found in PATH. Install it first."
  exit 1
fi
for dir in counter temp_converter flight_booker timer circle_drawer cells; do
  if [ -d "$ROOT/elm/$dir" ] && [ -f "$ROOT/elm/$dir/src/Main.elm" ]; then
    echo "  Building elm/$dir..."
    cd "$ROOT/elm/$dir"
    rm -r elm-stuff
    mkdir -p "$DOCS/elm/$dir"
    elm make src/Main.elm --optimize --output="$DOCS/elm/$dir/elm.js" 2>&1 | tail -1
    cp ../template.html "$DOCS/elm/$dir/index.html"
    cp "$ROOT/shared/7guis-theme.css" "$DOCS/elm/$dir/"
  fi
done
# Copy Elm landing page
cp "$ROOT/elm/index.html" "$DOCS/elm/"
cp "$ROOT/shared/7guis-theme.css" "$DOCS/elm/"
echo "  Elm: done"

# ============ PureScript ============
echo ""
echo "=== Building PureScript ==="

# Ensure esbuild is available (needed by spago bundle-app)
if ! command -v esbuild &>/dev/null; then
  echo "  Installing esbuild..."
  npm install -g esbuild 2>&1 | tail -1
fi

for dir in counter temp_converter flight_booker timer crud circle_drawer; do
  if [ -d "$ROOT/purescript/halogen/$dir" ]; then
    echo "  Building purescript/halogen/$dir..."
    cd "$ROOT/purescript/halogen/$dir"
    [ -d node_modules ] || npm ci 2>&1 | tail -1
    npm run build-prod 2>&1
    mkdir -p "$DOCS/purescript/halogen/$dir"
    cp -r prod/* "$DOCS/purescript/halogen/$dir/"
    rm -r prod dist
  fi
done
cp "$ROOT/purescript/halogen/index.html" "$DOCS/purescript/halogen/"
cp "$ROOT/shared/7guis-theme.css" "$DOCS/purescript/halogen/"
echo "  PureScript: done"

# ============ Landing page ============
[ -f "$ROOT/landing/index.html" ] && cp "$ROOT/landing/index.html" "$DOCS/"

# ============ Ready ============
READY_FILE="$ROOT/.server-ready"
date '+%Y-%m-%d %H:%M:%S' > "$READY_FILE"

echo ""
echo "============================================="
echo "  Serving on http://localhost:$PORT"
echo ""
echo "  ReScript:    http://localhost:$PORT/rescript/react/#counter"
echo "  Elm:         http://localhost:$PORT/elm/counter/"
echo "  PureScript:  http://localhost:$PORT/purescript/halogen/counter/"
echo "============================================="
echo ""
echo "Press Ctrl+C to stop."
cd "$DOCS"
python3 -m http.server "$PORT"
