#!/bin/bash

# basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# cp $basedir/fonts/*.ttf $HOME/Library/Fonts/

brew update
brew tap caskroom/fonts

brew cask install font-inconsolata
brew cask install font-symbola
brew cask install font-source-code-pro
