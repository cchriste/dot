#
# .profile
# 
# Author: Cameron Christensen
# Created: August 9, 2007
#
# .profile for terminal OSX sessions.
#

# EDITOR (Emacs takes too long to startup to be used as EDITOR)
export EDITOR=vim

#source .bashrc if being run (interactively) from bash 
if [ "$BASH" ]; then
    . ~/.bashrc
fi
