#debug startup
#set -x

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# (called by .profile)
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;; #echo "non-interactive: aborting"; return;;    don't echo from .bashrc, breaks scp!!!
esac

. ~/dot/utils.sh

# Tab completion in python interpreter
export PYTHONSTARTUP=$HOME/.pythonrc


# VTK
#export VTK_DIR=$HOME/code/VTK/build
#export VTK_DATA_ROOT=$HOME/data/VTKData
#export DYLD_LIBRARY_PATH=$VTK_DIR/bin
#export QT_PLUGIN_PATH=$QT_PLUGIN_PATH:$VTK_DIR/bin

# ITK
#export ITK_SOURCE_DIR=$HOME/code/InsightToolkit-3.16.0/
#export ITK_DIR=$HOME/code/InsightToolkit-3.16.0/build
#export ITK_BINARY_DIR=$ITK_DIR/bin

# could add topology toolkit, ML, Jupyter, and more to this next!

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
#export VISUS_BIN=$HOME/tools/nvisus/bin
#export VISUSCONVERT=$VISUS/visusconvert.app/Contents/MacOS/visusconvert
#export TRUNK=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/trunk
#export BRANCH=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/branches/cam
#export PATH=$VISUS_BIN:$VISUS_BIN/visusconvert.app/Contents/MacOS:$VISUS_BIN/visusselftest.app/Contents/MacOS:$VISUS_BIN/visusviewer.app/Contents/MacOS:$VISUS_BIN/visuswavelet.app/Contents/MacOS:$VISUS_BIN/visusguitest.app/Contents/MacOS:$PATH

#swarp (SDSS)
#export PATH=$PATH:$HOME/tools/swarp/bin

#hdf utils
#export PATH=$HOME/tools/h5utils/bin:$HOME/tools/hdf5/bin:$HOME/tools/hdf4/bin:$PATH

#VirtualGL
add_to_path /opt/VirtualGL/bin

#CUDA
add_to_path /usr/local/cuda/bin

# OSX-specific additions
if [ `uname` = Darwin ]; then
  #echo "Including OSX-specific configuration..."

  # NOTE: Emacs paths seem duplicated because for some reason this build
  #       of Emacs requires that it be executed from a symbolic link one
  #       directory above the bin directory.  The first path finds the
  #       Emacs/emacs symbolic link, while the second makes available
  #       the correct versions of etags, etc.
  add_to_path /Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin

  #MPI
  add_to_path /usr/local/mpi/bin

  #ImageMagick
  add_to_path /usr/local/ImageMagick-6.9.3/bin
  export DYLD_LIBRARY_PATH=/usr/local/ImageMagick-6.9.3/lib:$DYLD_LIBRARY_PATH

  #doxygen
  add_to_path /Applications/Doxygen.app/Contents/Resources

  #MATLAB
  export MATLAB_JAVA="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"
  add_to_path "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin"

  #QT
  add_to_path $HOME/tools/Qt5.13.0/5.13.0/clang_64/bin

  #MATLAB
  add_to_path /Applications/MATLAB_R2015a.app/bin
  export MATLAB_JAVA="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"

  #JAVA
  add_to_path "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin"

  #Python (use conda instead)
  #add_to_path "/Library/Frameworks/Python.framework/Versions/2.7/bin"

  #implicit on suse linux, but maybe needed on osx (todo: verify)
  #TODO: test this
  add_to_path /usr/local/bin

  #LATEX
  if [ `hostname | cut -f1 -d "."` = mercury ]; then
    # 2013 full version
    add_to_path /usr/local/texlive/2013/bin/x86_64-darwin
  else
    # 2019basic from https://www.tug.org/mactex/morepackages.html since the conda version wouldn't work
    add_to_path /usr/local/texlive/2019basic/bin/x86_64-darwin

    # Also... How to install your own .sty packages:
    #  https://tex.stackexchange.com/questions/1137/where-do-i-place-my-own-sty-or-cls-files-to-make-them-available-to-all-my-te
  fi
fi

#linux-specific additions
if [ `uname` = Linux ]; then
  #echo "Including Linux-specific configuration..."

  #opencv (on gunship)
  add_to_path "/usr/local/opencv-3.2/bin"

  #openmpi (should be handled by mpi-selector, but isn't working; also note mvapich2 install in gcc/mvapich2)
  add_to_path "/usr/lib64/mpi/gcc/openmpi/bin"

  #QT (gunship as of 2019.10.17)
  add_to_path $HOME/tools/Qt5.13.1/5.13.1/gcc_64/bin
fi

#GO (for google codesearch)
export GOOS=linux
export GOROOT=/usr/lib64/go/1.9
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
add_to_path $GOBIN

#paraview and visit
add_to_path "/usr/local/paraview/bin"
add_to_path "/usr/local/visit/bin"

#swig
add_to_path "/Users/cam/tools/swig-3.0.12"

# see https://github.com/bgr/omdb-cli for more details
# note to get just rating: omdbtool -t Cars | sed -n '/^imdbrating/{n;p;}'
export OMDB_API_KEY=12dc5051
alias omdbtool="python $HOME/tools/omdb-cli/omdbtool.py"

# Source global definitions
if [ -f ~/.openrc ]; then
        . ~/.openrc
fi

if [ ! -f ~/.ssh/id_rsa ]; then   
    echo 'No public/private RSA keypair found.'
    ssh-keygen -t rsa -b 2048 -f ~/.ssh/id_rsa -N ""    
    cat ~/.ssh/id_rsa.pub > ~/.ssh/authorized_keys
    chmod 644 ~/.ssh/authorized_keys
fi

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=250000

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

#.bash_prompt
if [ -f ~/dot/.bash_prompt ]; then
    . ~/dot/.bash_prompt
fi

#GIT 
source ~/bin/git-completion.bash
#**disabled prompt since it doesn't update when you change directories/branches
#source ~/bin/git-prompt.sh 
#export PS1='[\u@\h \W$(__git_ps1 " (%s)")] '$PS1
#export PS1="$(__git_ps1 "[%s]") $PS1"


# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/dot/.bash_aliases ]; then
    . ~/dot/.bash_aliases
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

#3/2017: FINK, HOMEBREW, MACPORTS, ANACONDA removed to avoid all non-standard package mgrs
#FINK
#test -r /sw/bin/init.sh && . /sw/bin/init.sh # (disabled in favor of homebrew)
#MACPORTS
# add_to_path "/opt/local/bin:/opt/local/sbin"  
#HOMEBREW
#...

#ANACONDA (we like Anaconda, but it can sometimes interfere)
if [ -z $ANACONDA_ENABLE ]; then
  echo "Enabling Anaconda..."
  ANACONDA_ENABLE=1
fi
ANACONDA_PYTHON2=0  #python3 is default, set this to override
ANACONDA_MINI=1     #todo: find a way not to have to hardcode this
if [ -f ~/bin/start_anaconda.sh ]; then
  . ~/bin/start_anaconda.sh
fi

#DOCKER bash completion (but it still doesn't work...)
#. ~/bin/docker-compose.sh

#THE END: add my bin to the top of PATH
add_to_path $HOME/bin

