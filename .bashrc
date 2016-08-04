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
module load null

# User specific aliases and functions

export PS1="\[\e[32;1m\]\h:\W \u\$ \[\e[0m\]"

alias ls='ls -FhG'
alias ll='ls -alFhG'
alias df='df -H'
alias du='du -h'
alias hgstat='hg status | grep -Ev \(\\?\|\\!\)'
