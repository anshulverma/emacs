#!/bin/bash

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

brew update

packages=("ispell"
          "bash-completion"
          "git"
          "python"
          "emacs"
          "auctex"
          "cask"
          "scala"
          "ditaa"
          "graphviz"
          "plantuml"
          "gnuplot")
for pkg in "${packages[@]}"; do
  brew install $pkg
done
