#!/bin/bash

basedir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cp $basedir/fonts/*.ttf $HOME/Library/Fonts/
