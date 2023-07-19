# Antidote
# Source Antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# Source some envs
source $HOME/.env

#PATH
export PATH="$PATH:$HOME/.local/bin:$HOME/local/bin:$HOME/doom-emacs/bin:$HOME/.spicetify:$HOME/.cargo/bin"

# Enviorment Variables
export MONITOR="HDMI-2"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64" # Compilation Flag
export PAGER="most"
export ANDROID_ADB_SERVER_PORT=12345
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='emacsclient -c -a "emacs"'
fi
export BROWSER="firefox"

# Install sccache
export RUSTC_WRAPPER=sccache

# Function for extracting archives
ex ()
{
    if [ -f "$1" ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *.deb)       ar x $1      ;;
            *.tar.xz)    tar xf $1    ;;
            *.tar.zst)   unzstd $1    ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Function for disowning commands
detach ()
{
    ( "$@" &> /dev/null & disown )
}

# KEYBINDING TYPE
bindkey -e

# ALIASES
# Exa
alias l="exa -la"
alias ls="exa -a"
# lf
alias lf="$HOME/local/bin/lfub"
alias lfub="$HOME/local/bin/lfub"
# Vim
alias vim="nvim"
# Git
alias g="git"
alias gdot="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"
# Misc
alias c="clear"
alias xclass="xprop | grep WM_CLASS"
# Verbose
alias mv='mv -v'
alias cp='cp -vr'
alias rm='rm -vr'
alias cat='bat'
alias dirs='dirs -v'
# Emacs
alias e="emacsclient -c -n -u -a 'emacs'"
alias edr="killall emacs; emacs --daemon"
