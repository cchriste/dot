#Custom aliases and functions
alias la='ls -A'
alias l='ls -CF'
alias ls='ls -FhG'
#alias ll='ls -alF'
alias ll='ls -alFhG'
alias df='df -H'
alias du='du -h'
alias hgstat='hg status | grep -Ev \(\\?\|\\!\)'
alias diff='diff -u'

# enable color support of ls and also add handy aliases
# 30.10.2018 - customized to first set defaults and then add customizations from ~/.dircolors
if [ -x /usr/bin/dircolors ]; then
    eval "$(dircolors -b)"
    DEFAULT_COLORS=$LS_COLORS
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)"
    export LS_COLORS=$DEFAULT_COLORS:$LS_COLORS
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
elif [ `uname` = Darwin ]; then
    # Here's a piece from the 'man ls' page on OSX:
    # LSCOLORS        The value of this variable describes what color to use for which attribute when colors are enabled with CLICOLOR.
    #                     This string is a concatenation of pairs of the format fb, where f is the foreground color and b is the background
    #                     color.
    #                 The default is "exfxcxdxbxegedabagacad", i.e. blue foreground and default background for regular directories,
    #                 black foreground and red background for setuid executables, etc.
    #    The order of the attributes are as follows:
    #   
    # The color designators are as follows:
    # a black
    # b red
    # c green
    # d brown
    # e blue
    # f magenta
    # g cyan
    # h light grey
    # A bold black, usually shows up as dark grey
    # B bold red
    # C bold green
    # D bold brown, usually shows up as yellow
    # E bold blue
    # F bold magenta
    # G bold cyan
    # H bold light grey; looks like bright white
    # x default foreground or background

    # 1. directory
    # 2. symbolic link
    # 3. socket
    # 4. pipe
    # 5. executable
    # 6. block special
    # 7. character special
    # 8. executable with setuid bit set
    # 9. executable with setgid bit set
    # 10. directory writable to others, with sticky bit
    # 11. directory writable to others, without sticky bit
    
    export CLICOLOR=1
    export LSCOLORS="dxhxcxdxbxegedabagacad"
fi


