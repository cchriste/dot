#
# .profile
# 
# Author: Cameron Christensen
# Created: August 9, 2007
#
# .profile for terminal OSX sessions.
#

source ~/bin/git-completion.bash
source ~/bin/git-prompt.sh
export PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
#export PS1='[\e[32;1m\]\u@\h \W$(__git_ps1 " (%s)")]\$ \[\e[0m\]'
#export PS1="\[\e[32;1m\]\h:\W \u\$ \[\e[0m\]"

alias ls='ls -FhG'
alias ll='ls -alFhG'
alias df='df -H'
alias du='du -h'
alias hgstat='hg status | grep -Ev \(\\?\|\\!\)'

export PATH=/usr/local/bin:$PATH  # /usr/local/bin/should be first, but OSX in system default (controlled by /etc/paths) it's last!

# Fink (disabled in favor of homebrew)
# test -r /sw/bin/init.sh && . /sw/bin/init.sh

# NOTE: Emacs paths seem duplicated because for some reason this build
#       of Emacs requires that it be executed from a symbolic link one
#       directory above the bin directory.  The first path finds the
#       Emacs/emacs symbolic link, while the second makes available
#       the correct versions of etags, etc.
export PATH=$PATH:$HOME/bin:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin

# EDITOR (Emacs takes too long to startup to be used as EDITOR)
export EDITOR=vim

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
export VISUS_BIN=/usr/local/nvisusio/bin
export VISUS_ALT_BIN=/usr/local/visus/bin
#export VISUSCONVERT=$VISUS/visusconvert.app/Contents/MacOS/visusconvert
#export TRUNK=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/trunk
#export BRANCH=https://gforge.sci.utah.edu/svn/dar/ViSUS/src/nvisusio/branches/cam
export PATH=$VISUS_BIN:$VISUS_ALT_BIN:$PATH

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
    PATH=/usr/local/bin:$PATH
    export PATH
fi

export PATH=/usr/local/mpi/bin:$PATH
export PATH=$HOME/Dropbox/Applications/MATLAB_R2014a.app/bin:$PATH

#MATLAB
export MATLAB_JAVA="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"
export PATH="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin":$PATH

# doxygen
export PATH=/Applications/Doxygen.app/Contents/Resources:$PATH

# Setting PATH for Python 2.7
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

#ImageMagick
export PATH=/usr/local/ImageMagick-6.9.3/bin:$PATH
export DYLD_LIBRARY_PATH=/usr/local/ImageMagick-6.9.3/lib:$DYLD_LIBRARY_PATH

#
# mercury-specific additions
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

##
# Your previous /Users/cam/.profile file was backed up as /Users/cam/.profile.macports-saved_2016-08-27_at_20:50:53
##

# MacPorts Installer addition on 2016-08-27_at_20:50:53: adding an appropriate PATH variable for use with MacPorts.
# export PATH="/opt/local/bin:/opt/local/sbin:$PATH"  # removed macports in Mar 2017 (trying to avoid macports as well as homebrew for now)

# added by Miniconda2 4.2.12 installer
#export PATH="/Users/cam/tools/miniconda2/bin:$PATH"

# added by Anaconda2 4.3.0 installer
export PATH="/Users/cam/tools/anaconda2/bin:$PATH"

source ~/.bashrc
#source ~/.openrc

