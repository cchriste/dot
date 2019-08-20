#
# .profile
# 
# Author: Cameron Christensen
# Created: August 9, 2007
#
# .profile for terminal OSX sessions.
#

#set -x
#echo "************* .profile *************"

# EDITOR (Emacs takes too long to startup to be used as EDITOR)
export EDITOR=vim

if [ `hostname | cut -f1 -d "."` = gunship ]; then
  #echo ""
  xinput set-prop 8 "Evdev Scrolling Distance" -1 1 1
fi

# gunship-specific additions
if [ `hostname | cut -f1 -d "."` = gunship ]; then
  #start synergy keyboard/mouse sharing
  #synergys --enable-crypto --config /home/cam/synergy.conf
  synergys  --daemon --debug INFO --name gunship -c /home/cam/synergy.conf --address :24800 
  #(23.08.2017 - commented since it looks like synergy is actually run by window manager)
  #2018.10.19 - uncommented since synergy had some issues letting osx think it was asleep even while I was using it, so I downgraded and am using an older version of the command line as well, but still pretty much the same conf file.
fi

#source .bashrc if being run (interactively) from bash 
if [ "$BASH" ]; then
    . ~/.bashrc
fi


