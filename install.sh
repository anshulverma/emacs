#!/bin/bash

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$OSTYPE" in
  darwin*)  ;;

  *)        echo "Only MacOSX supported currently."
            echo "Please consider adding support for your OS type."
            exit 1
            ;;
esac

# all the base packages are installed using homebrew

brew update

packages=("ispell"
          "bash-completion"
          "git"
          "cask"
          "scala"
          "ditaa"
          "graphviz"
          "plantuml"
          "gnuplot"
          "python"
          "pyenv"
          "pig"
          "scalariform"
          "pandoc")
for pkg in "${packages[@]}"; do
  brew install $pkg
done

brew install --HEAD paulp/extras/coursier
brew cask install pdftotext

# install python and required packages

python_version="2.7.10"
pyenv install $python_version
pyenv local $python_version

export PATH="$(pyenv root)/shims:$PATH"

if [ ! -f "$HOME/.python-version" ]; then
  echo "Setting base python version for pyenv to $python_version"
  echo "$python_version" >> $HOME/.python-version
fi

pip install --upgrade pip

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

# install mural
pushd $HOME
git clone git@github.com:anshulverma/mural.git .mural
cd .mural
make
popd
