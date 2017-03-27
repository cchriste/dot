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
if [ `hostname | cut -f1 -d "."` = mercury ]; then
    #CMAKE
    export PATH=/Applications/CMake.app/Contents/bin:$PATH

    #LATEX
    export PATH=/usr/local/texlive/2013/bin/x86_64-darwin:$PATH
fi

#VirtualGL
export PATH=/opt/VirtualGL/bin:$PATH

#GO
export GOPATH=$HOME/GO
export PATH=$GOPATH/bin:$PATH

# export PATH="/opt/local/bin:/opt/local/sbin:$PATH"  # removed macports in Mar 2017 (trying to avoid macports as well as homebrew for now)

# added by Anaconda2 4.3.0 installer
export PATH="/Users/cam/tools/anaconda2/bin:$PATH"

if [ `hostname | cut -f1 -d "."` = gunship ]; then
  xinput set-prop 8 "Evdev Scrolling Distance" -1 1 1
fi

#source .bashrc if being run (interactively) from bash 
if [ "$BASH" ]; then
    . ~/.bashrc
fi
