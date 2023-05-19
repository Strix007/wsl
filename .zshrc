# Antidote
# Source Antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# Source some envs
source $HOME/.env
# ZSH Prompt Theme
ENABLE_CORRECTION="false"      # Autocorrection
CASE_SENSITIVE="true"          # Case Sensitive Autocomplete
HYPHEN_INSENSITIVE="false"     # Hyphen Insensitive a.k.a "-" and "_" Will Be Interchangeable. CASE_SENSITIVE Must Be Set To False
DISABLE_UPDATE_PROMPT="true"   # Enable Or Disable Update Prompt
COMPLETION_WAITING_DOTS="true" # Dot Buffer

#PATH
export PATH="$PATH:$HOME/.local/bin:$HOME/local/bin:$HOME/doom-emacs/bin:$HOME/.spicetify"

# Plugins Configuration
# Auto-notify
# export AUTO_NOTIFY_THRESHOLD=15
# export AUTO_NOTIFY_EXPIRE_TIME=20000
# AUTO_NOTIFY_IGNORE+=("docker" "emacs" "eww" "firefox" "mpv")
# Zsh-history-substring-search
# bindkey '^[[1;5A' history-substring-search-up
# bindkey '^[[1;5B' history-substring-search-down

# Emacs Mode
# bindkey -M emacs '^P' history-substring-search-up
# bindkey -M emacs '^N' history-substring-search-down
# Vi Mode
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

# Enviorment Variables
export MONITOR="HDMI-2"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64" # Compilation Flag
export PAGER="most"
export ANDROID_ADB_SERVER_PORT=12345
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='emacs'
fi
export BROWSER="firefox"

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

# KEYBINDING TYPE
bindkey -v
# ALIASES
# Exa
alias l="exa -la"
alias ls="exa -a"
# lf
alias lf="kitty $HOME/local/bin/lfub"
alias lfub="kitty $HOME/local/bin/lfub"
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
alias dirs='dirs -v'
# Emacs
alias e="emacsclient -c -n -u -a 'emacs'"
alias edr="killall emacs; emacs --daemon"
