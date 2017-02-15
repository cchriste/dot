# .bashrc
# (called by .profile)

# Fink (disabled in favor of homebrew)
# test -r /sw/bin/init.sh && . /sw/bin/init.sh

# NOTE: Emacs paths seem duplicated because for some reason this build
#       of Emacs requires that it be executed from a symbolic link one
#       directory above the bin directory.  The first path finds the
#       Emacs/emacs symbolic link, while the second makes available
#       the correct versions of etags, etc.
export PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:$PATH

# Tab completion in python interpreter
export PYTHONSTARTUP=$HOME/.pythonrc


# Qt
#export QTDIR=$HOME/build/qt  #jupiter
#export PATH=$QTDIR/bin:$PATH
#export DYLD_LIBRARY_PATH=$QTDIR/lib:$DYLD_LIBRARY_PATH

# VTK
#export VTK_DIR=$HOME/code/VTK/build
#export VTK_DATA_ROOT=$HOME/data/VTKData
#export DYLD_LIBRARY_PATH=$VTK_DIR/bin
#export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$VTK_DIR/bin

# ITK
#export ITK_SOURCE_DIR=$HOME/code/InsightToolkit-3.16.0/
#export ITK_DIR=$HOME/code/InsightToolkit-3.16.0/build
#export ITK_BINARY_DIR=$ITK_DIR/bin

# CRCNS (ImageReconstruction project, path to ir-tools and Iris/Scripts)
#export CRCNS=$HOME/code/ir/trunk
#export PATH=$CRCNS/bin:$CRCNS/bin/ir-tools:$PATH
#export PATH=$CRCNS/bin:$CRCNS/bin/Scripts:$PATH

# BOOST
#export BOOST_ROOT=$HOME/tools/boost
#export BOOST_INCLUDEDIR=$BOOST_ROOT/include
#export BOOST_LIBRARYDIR=$BOOST_ROOT/lib
#export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$BOOST_LIBRARYDIR

# ViSUS
export VISUS_BIN=$HOME/tools/nvisus/bin
#export VISUSCONVERT=$VISUS/visusconvert.app/Contents/MacOS/visusconvert
#export TRUNK=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/trunk
#export BRANCH=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/branches/cam
export PATH=$VISUS_BIN:$VISUS_BIN/visusconvert.app/Contents/MacOS:$VISUS_BIN/visusselftest.app/Contents/MacOS:$VISUS_BIN/visusviewer.app/Contents/MacOS:$VISUS_BIN/visuswavelet.app/Contents/MacOS:$VISUS_BIN/visusguitest.app/Contents/MacOS:$PATH

#swarp (SDSS)
#export PATH=$PATH:$HOME/tools/swarp/bin

#hdf utils
#export PATH=$HOME/tools/h5utils/bin:$HOME/tools/hdf5/bin:$HOME/tools/hdf4/bin:$PATH

##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi

export PATH=/usr/local/mpi/bin:$PATH
export PATH=$HOME/Dropbox/Applications/MATLAB_R2014a.app/bin:$PATH

#MATLAB
export MATLAB_JAVA="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"
export PATH="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin":$PATH

#macports (disabled to compiled Uintah)
#export PATH=/opt/local/bin:/opt/local/sbin:$PATH
#export PATH=/usr/local/opt/gnu-sed/bin:$PATH

# doxygen
export PATH=/Applications/Doxygen.app/Contents/Resources:$PATH

# Setting PATH for Python 2.7
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH


#
# mercury-specific additions
#
if [ `hostname | cut -f1 -d "."` = mercury ]; then
    #CMAKE
    export PATH=/Applications/CMake_2.8-12.app/Contents/bin:$PATH

    #LATEX
    export PATH=/usr/local/texlive/2013/bin/x86_64-darwin:$PATH
fi

#Google CodeSearch (on Ubuntu installed with Go, see csearch github page) 
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

#nvidia nccl compositing
export LD_LIBRARY_PATH=$HOME/code/nccl/build/lib:$LD_LIBRARY_PATH
#export NCCL_LINK=NVLINK
#export NCCL_TOPOLOGY=CUBEMESH
export NCCL_DEBUG=INFO

export PATH=/usr/local/cuda/bin:$PATH

# Source global definitions
if [ -f ~/.openrc ]; then
        . ~/.openrc
fi

# User specific aliases and functions
if [ ! -f ~/.ssh/id_rsa ]; then   
    echo 'No public/private RSA keypair found.'
    ssh-keygen -t rsa -b 2048 -f ~/.ssh/id_rsa -N ""    
    cat ~/.ssh/id_rsa.pub > ~/.ssh/authorized_keys
    chmod 644 ~/.ssh/authorized_keys
fi

# Load saved modules
#module load null

# User specific aliases and functions

export PS1="\[\e[32;1m\]\h:\W \u\$ \[\e[0m\]"

#alias ls='ls -FhG'
alias ll='ls -alFhG'
alias df='df -H'
alias du='du -h'
alias hgstat='hg status | grep -Ev \(\\?\|\\!\)'

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
#alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export P4PORT="p4sw:2006"

# added by Anaconda2 4.3.0 installer
export PATH="/usr/local/anaconda2/bin:$PATH"


