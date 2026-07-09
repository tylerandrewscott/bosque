#!/usr/bin/env bash
#
# setup_symlinks.sh
#
# Creates a symlink from this repo subdirectory to the Box-synced `bosque`
# folder, exposed here as `bosquebox`. Run once after cloning (or whenever the
# symlink needs to be recreated). Mirrors the pattern used in the tijuana and
# salinas repos: the symlink is gitignored and recreated locally per machine,
# so no absolute path is ever committed.
#
# Usage:
#   bash setup_symlinks.sh
#   bash setup_symlinks.sh /path/to/box/bosque   # override auto-detection
#
# After running, Box data is reachable as:
#   bosquebox/input/...          (raw ingestion inputs: twdd_records, tceq_*, etc.)
#   bosquebox/spatial_inputs/...  (shapefiles, geojson)
#   bosquebox/util_code/...       (scraping helpers)
#

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Detect or accept the Box bosque root ------------------------------------

if [[ -n "${1:-}" ]]; then
    BOX_BOSQUE="$1"
else
    BOX_BASE="$HOME/Library/CloudStorage"
    if [[ -d "$BOX_BASE/Box-Box/bosque" ]]; then
        BOX_BOSQUE="$BOX_BASE/Box-Box/bosque"
    elif [[ -d "$BOX_BASE/Box/bosque" ]]; then
        BOX_BOSQUE="$BOX_BASE/Box/bosque"
    elif [[ -d "$HOME/Box/bosque" ]]; then
        BOX_BOSQUE="$HOME/Box/bosque"
    else
        echo "ERROR: Could not find the bosque folder in Box."
        echo "Searched:"
        echo "  $BOX_BASE/Box-Box/bosque"
        echo "  $BOX_BASE/Box/bosque"
        echo "  $HOME/Box/bosque"
        echo ""
        echo "Re-run with an explicit path:"
        echo "  bash setup_symlinks.sh /path/to/box/bosque"
        exit 1
    fi
fi

echo "Using Box bosque at: $BOX_BOSQUE"

# --- Create symlink -----------------------------------------------------------

LINK_PATH="$REPO_DIR/bosquebox"

if [[ -L "$LINK_PATH" ]]; then
    rm "$LINK_PATH"
elif [[ -e "$LINK_PATH" ]]; then
    echo "WARNING: $LINK_PATH exists and is not a symlink — skipping."
    exit 1
fi

if [[ -d "$BOX_BOSQUE" ]]; then
    ln -s "$BOX_BOSQUE" "$LINK_PATH"
    echo "  OK  $LINK_PATH -> $BOX_BOSQUE"
else
    echo "  MISSING  $BOX_BOSQUE  (symlink not created)"
    exit 1
fi

echo ""
echo "Done. Symlink created. Data is now reachable under bosquebox/."
