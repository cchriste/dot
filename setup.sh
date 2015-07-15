#! /bin/sh

#
# setup.sh
#
# Replace dotfiles with rcs versions to ease system migration.
#
# NOTE: these are very osx centric.
#

# backup existing dotfiles
cp ~/.emacs ~/.emacs.dot.bak
cp ~/.bashrc ~/.bashrc.dot.bak
cp ~/..profile ~/..profile.dot.bak
cp ~/..pythonrc ~/..pythonrc.dot.bak

# symbolic link to new source-controlled dotfiles
ln -s dot/.emacs ./.emacs
ln -s dot/.bashrc ./.bashrc
ln -s dot/.profile ./.profile
ln -s dot/.pythonrc ./.pythonrc
