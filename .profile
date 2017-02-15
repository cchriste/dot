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

#
# gunship-specific additions
#
if [ `hostname | cut -f1 -d "."` = gunship ]; then
  xinput set-prop 8 "Evdev Scrolling Distance" -1 1 1
fi

#source .bashrc if being run (interactively) from bash 
if [ "$BASH" ]; then
    . ~/.bashrc
fi
