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
# XDG ENVs
export ANDROID_HOME='"$XDG_DATA_HOME"/android'
export CARGO_HOME='"$XDG_DATA_HOME"/cargo'
export RUSTUP_HOME='"$XDG_DATA_HOME"/rustup'
export DOCKER_CONFIG='"$XDG_CONFIG_HOME"/docker'
export GRIPHOME='"$XDG_CONFIG_HOME/grip"'
export GTK2_RC_FILES='"$XDG_CONFIG_HOME"/gtk-2.0/gtkrc'
export ICEAUTHORITY='"$XDG_CACHE_HOME"/ICEauthority'
export _JAVA_OPTIONS='-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java'
export PYTHONSTARTUP="${XDG_CONFIG_HOME}/python/pythonrc"
export WINEPREFIX='"$XDG_DATA_HOME"/wine'
# User Defined
export MONITOR="HDMI-2"
export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64" # Compilation Flag
export PAGER="most"
export ANDROID_ADB_SERVER_PORT=12345
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST
export MOZ_ENABLE_WAYLAND=1 
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='nvim'
fi
export BROWSER="firefox"

# Install sccache
export RUSTC_WRAPPER=sccache

# Nord dir colors
eval $(dircolors ~/.dir_colors)

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

# KEYBINDINGS
bindkey '^ ' autosuggest-execute

# ALIASES
# LSD
alias l="lsd -lah"
alias ls="lsd -ah"
# lf
alias lf="$HOME/local/bin/lfub"
alias lfub="$HOME/local/bin/lfub"
# Vim
alias vim="nvim"
# Git
alias g="git"
# Misc
alias c="clear"
alias xclass="xprop | grep WM_CLASS"
alias cat='bat'
alias xmo="xmonad --recompile; startx /bin/xmonad; feh --bg-fill /home/arbab/.xmonad/wallpapers/wallpaper1.png --bg-fill /home/arbab/.xmonad/wallpapers/wallpaper2.png"
alias fzf="fzf --preview='fzf-preview {}' --bind alt-k:preview-page-up,alt-j:preview-page-down"
alias tmux="tmux attach"
alias wget='wget --hsts-file="$XDG_DATA_HOME/wget-hsts"'
# Verbose
alias mv='mv -vi'
alias cp='cp -vri'
alias rm='rm -vr'
alias dirs='dirs -v'
# Emacs
alias e="emacsclient -c -n -u -a 'emacs'"
alias edr="killall emacs; emacs --daemon"
