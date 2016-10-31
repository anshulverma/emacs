#!/bin/bash

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

brew update

packages=("ispell"
          "bash-completion"
          "git"
          "emacs"
          "auctex"
          "cask"
          "scala"
          "ditaa"
          "graphviz"
          "plantuml"
          "gnuplot"
          "python"
          "pyenv"
          "pig")
for pkg in "${packages[@]}"; do
  brew install $pkg
done

pip_packages=("jedi"
              "elpy"
              "rope"
              "importmagic"
              "autopep8"
              "yapf"
              "flake8"
              "epc"
              "deferred")
for pkg in "${pip_packages[@]}"; do
  pip install $pkg
done
