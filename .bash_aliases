#Custom aliases and functions
alias la='ls -A'
alias l='ls -CF'
alias ls='ls -FhG'
#alias ll='ls -alF'
alias ll='ls -alFhG'
alias df='df -H'
alias du='du -h'
alias hgstat='hg status | grep -Ev \(\\?\|\\!\)'

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
fi

