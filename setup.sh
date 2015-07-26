#! /bin/sh

#
# setup.sh
#
# Replace dotfiles with rcs versions to ease system migration.
#
# NOTE: these are very osx centric.
#

pushd ~

# backup existing dotfiles
cp .emacs .emacs.$$.bak
cp .bashrc .bashrc.$$.bak
cp .profile .profile.$$.bak
cp .pythonrc .pythonrc.$$.bak
cp .gitconfig .gitconfig.$$.bak
cp .gitignore .gitignore.$$.bak
cp .screenrc .screenrc.$$.bak

# symbolic link to new source-controlled dotfiles
ln -s dot/.emacs .emacs
ln -s dot/.bashrc .bashrc
ln -s dot/.profile .profile
ln -s dot/.pythonrc .pythonrc
ln -s dot/.gitconfig .gitconfig
ln -s dot/.gitignore .gitignore
ln -s dot/.screenrc .screenrc

popd
