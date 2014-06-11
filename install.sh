#!/usr/bin/env  bash

# TODO check emacs version

# get platform type
platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
  platform='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
  platform='darwin'
fi

# check if ispell is installed
command_exists () {
  type "$1" &> /dev/null ;
}

# suggest how to install ispell
if command_exists ispell; then
  echo "ispell is already installed..."
else
  echo "Please install ispell:"
  if [[ $platform == 'linux' ]]; then
    echo "sudo apt-get install ispell"
  else
    echo "brew install ispell"
  fi
  exit 1
fi

# download all libraries
# TODO: use npi
URL='http://iweb.dl.sourceforge.net/project/jdee/jdee/2.4.1/jdee-bin-2.4.1.tar.bz2'
FILE=`mktemp`
wget "$URL" -O $FILE
mkdir -p "$PWD/lib-impl"
echo "extracting JDEE..."
tar -xvf $FILE -C "$PWD/lib-impl"
mkdir -p "$PWD/lib"
ln -s "$PWD/lib-impl/jdee-2.4.1" "$PWD/lib/jdee"

echo "generating init script from template.."
sed "s#{EMACS_HOME}#$PWD#" init.el.template > init.el

# setup the emacs init config and backup current config if present
emacs_file="$HOME/.emacs"
if [ -L $emacs_file ]; then
  echo "backing up $emacs_file to $emacs_file.old"
  mv $emacs_file "$emacs_file.old"
elif [ -f $emacs_file ]; then
  echo "backing up $emacs_file to $emacs_file.bak"
  mv $emacs_file "$emacs_file.bak"
else
  echo "you don't seem to have any emacs configuration; a new one will be created..."
fi
echo "creating new symlink for $emacs_file"
ln -s "$PWD/init.el" $emacs_file

echo "DONE"
