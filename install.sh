#!/usr/bin/env  bash

echo "generating init script from template.."
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

URL='http://iweb.dl.sourceforge.net/project/jdee/jdee/2.4.1/jdee-bin-2.4.1.tar.bz2'
FILE=`mktemp`
wget "$URL" -O $FILE
mkdir -p "$PWD/lib-impl"
echo "extracting JDEE..."
tar -xvf $FILE -C "$PWD/lib-impl"
mkdir -p "$PWD/lib"
ln -s "$PWD/lib-impl/jdee-2.4.1" "$PWD/lib/jdee"
