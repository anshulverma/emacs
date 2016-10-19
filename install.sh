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
          "gnuplot"
          "pyenv")
for pkg in "${packages[@]}"; do
  brew install $pkg
done

pip_packages=("jedi"
              "importmagic"
              "autopep8"
              "flake8"
              "epc"
              "deferred")
for pkg in "${pip_packages[@]}"; do
  pip install $pkg
done
