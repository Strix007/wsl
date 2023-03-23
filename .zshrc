# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
# Source some envs
source $HOME/.env
# ZSH Prompt Theme
ZSH_THEME="robbyrussell"
ENABLE_CORRECTION="false"      # Autocorrection
CASE_SENSITIVE="true"          # Case Sensitive Autocomple
HYPHEN_INSENSITIVE="true"      # Hyphen Insensitive a.k.a "-" and "_" Will Be Interchangeable. CASE_SENSITIVE Must Be Set To False
DISABLE_UPDATE_PROMPT="true"   # Enable Or Disable Update Prompt
COMPLETION_WAITING_DOTS="true" # Dot Buffer
# Plugins
plugins=(
	zsh-syntax-highlighting
	zsh-autosuggestions	
			)
source $ZSH/oh-my-zsh.sh
# Enviorment Variables
export MONITOR="HDMI-2"
export LANG=en_US.UTF-8 
export ARCHFLAGS="-arch x86_64" # Compilation Flag
export PAGER="most"
export ANDROID_ADB_SERVER_PORT=12345
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

export PATH="$PATH:$HOME/.local/bin:$HOME/local/bin:$HOME/doom-emacs/bin"
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST

