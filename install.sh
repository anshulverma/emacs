#!/usr/bin/env  bash

sed "s#{EMACS_HOME}#$PWD#" init.el.template > init.el

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

URL='https://dl.bintray.com/mitchellh/vagrant/vagrant_1.6.2_x86_64.deb'
FILE=`mktemp`
#wget "$URL" -qO $FILE
