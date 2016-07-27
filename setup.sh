#! /bin/sh

#
# setup.sh
#
# Replace dotfiles with rcs versions to ease system migration.
#

pushd ~

# backup existing dotfiles
mv .emacs .emacs.$$.bak
mv emacs emacs.$$.bak
mv .bashrc .bashrc.$$.bak
mv .profile .profile.$$.bak
mv .pythonrc .pythonrc.$$.bak
mv .gitconfig .gitconfig.$$.bak
mv .gitignore .gitignore.$$.bak
mv .screenrc .screenrc.$$.bak
mv .Xresources .Xresources.$$.bak

# symbolic link to new source-controlled dotfiles
ln -s dot/emacs emacs
ln -s dot/.emacs .emacs
ln -s dot/.bashrc .bashrc
ln -s dot/.profile .profile
ln -s dot/.pythonrc .pythonrc
ln -s dot/.gitconfig .gitconfig
ln -s dot/.gitignore .gitignore
ln -s dot/.screenrc .screenrc
ln -s dot/.Xresouces .Xresources

popd

xrdb -merge ~/.Xresources