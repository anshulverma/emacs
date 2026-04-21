#!/usr/bin/env bash
# Bootstrap external dependencies for this Emacs config.
#
# Only the binaries Emacs shells out to are installed here. All
# Emacs packages themselves are installed by package.el on first
# start (see src/av-packages.el).
#
# Supported: macOS with Homebrew.

set -euo pipefail

if [[ "$OSTYPE" != darwin* ]]; then
  echo "install.sh only supports macOS. Install the deps below manually:"
  echo "  ispell, pandoc, graphviz, plantuml, gnuplot"
  echo "and then link this directory into ~/.emacs.d."
  exit 1
fi

if ! command -v brew >/dev/null 2>&1; then
  echo "Homebrew not found. Install from https://brew.sh first."
  exit 1
fi

if ! command -v emacs >/dev/null 2>&1; then
  echo "Emacs not found. 'brew install --cask emacs' or install from"
  echo "https://emacsformacosx.com/, then re-run this script."
  exit 1
fi

packages=(
  ispell           # spell checking
  pandoc           # ox-pandoc export backend, markdown-command fallback
  graphviz         # dot for org-babel diagrams
  plantuml         # plantuml for org-babel diagrams
  gnuplot          # org-babel plots
)

for pkg in "${packages[@]}"; do
  if brew list --formula "$pkg" >/dev/null 2>&1; then
    echo "✓ $pkg already installed"
  else
    echo "→ installing $pkg"
    brew install "$pkg"
  fi
done

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
target="${HOME}/.emacs.d"

if [[ ! -e "$target" ]]; then
  echo "→ symlinking $basedir → $target"
  ln -s "$basedir" "$target"
elif [[ -L "$target" && "$(readlink "$target")" == "$basedir" ]]; then
  echo "✓ $target already points to this checkout"
else
  echo "⚠ $target exists and doesn't point here; leaving it alone"
fi

echo
echo "Done. Run 'emacs' — package.el will install Elisp packages on"
echo "first launch (may take a few minutes)."
