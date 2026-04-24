#!/usr/bin/env bash
# Bootstrap external dependencies for this Emacs config.
#
# Only the binaries Emacs shells out to are installed here. All
# Emacs packages themselves are installed by package.el on first
# start (see src/av-packages.el).
#
# Supported: macOS (Homebrew), Debian/Ubuntu (apt), GitHub Codespaces.

set -euo pipefail

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
target="${HOME}/.emacs.d"

detect_os() {
  case "${OSTYPE:-}" in
    darwin*) echo macos ;;
    linux*)
      if [[ -r /etc/os-release ]] && grep -qiE 'ubuntu|debian' /etc/os-release; then
        echo debian
      else
        echo linux-other
      fi ;;
    *) echo unknown ;;
  esac
}

install_macos() {
  if ! command -v brew >/dev/null 2>&1; then
    echo "Homebrew not found. Install from https://brew.sh first."
    exit 1
  fi

  if ! command -v emacs >/dev/null 2>&1; then
    echo "Emacs not found. 'brew install --cask emacs' or install from"
    echo "https://emacsformacosx.com/, then re-run this script."
    exit 1
  fi

  local packages=(ispell pandoc graphviz plantuml gnuplot)
  for pkg in "${packages[@]}"; do
    if brew list --formula "$pkg" >/dev/null 2>&1; then
      echo "✓ $pkg already installed"
    else
      echo "→ installing $pkg"
      brew install "$pkg"
    fi
  done
}

install_debian() {
  local sudo=""
  [[ $EUID -ne 0 ]] && sudo="sudo"

  # emacs-nox keeps the image small; Codespaces is terminal-only anyway.
  local packages=(emacs-nox ispell pandoc graphviz plantuml gnuplot)
  echo "→ apt-get install ${packages[*]}"
  $sudo apt-get update -y
  $sudo apt-get install -y "${packages[@]}"
}

link_emacs_d() {
  if [[ -L "$target" && "$(readlink "$target")" == "$basedir" ]]; then
    echo "✓ $target already points to this checkout"
    return
  fi

  if [[ -e "$target" || -L "$target" ]]; then
    local backup="${target}.backup-$(date +%Y%m%d%H%M%S)"
    echo "→ $target exists; renaming to $backup"
    mv "$target" "$backup"
  fi

  echo "→ symlinking $basedir → $target"
  ln -s "$basedir" "$target"
}

os="$(detect_os)"
echo "==> detected OS: $os"

case "$os" in
  macos)  install_macos ;;
  debian) install_debian ;;
  *)
    echo "install.sh does not auto-install on '$os'. Install these manually:"
    echo "  emacs, ispell, pandoc, graphviz, plantuml, gnuplot"
    echo "and then re-run with the deps satisfied."
    exit 1 ;;
esac

link_emacs_d

echo
echo "Done. Run 'emacs' — package.el will install Elisp packages on"
echo "first launch (may take a few minutes)."
