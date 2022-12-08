# Path to your oh-my-zsh installation.

export ZSH="/home/arbab/.oh-my-zsh"

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

export MONITOR="HDMI2"
export LANG=en_US.UTF-8 
export ARCHFLAGS="-arch x86_64" # Compilation Flag
export PAGER="most"
export ANDROID_ADB_SERVER_PORT=12345

# Editor For SSH And Local Separately

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# KEYBINDING TYPE

bindkey -v 

# ALIASES

# ZSH

alias zshconfig="nvim ~/.zshrc"
alias zshreload="source $HOME/.zshrc"

# Exa

alias l="exa -la"
alias ls="exa -a"

# Vim 

alias vim="nvim"
alias svim="sudo -e nvim"

# Git

alias g="git"
alias ga="git add ."
alias gpb="git push origin"
alias gm="ga && git commit -m"
alias gp="git push -u origin main"
alias gdot="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# Misc

alias c="clear"
alias vs="code"
alias pac="paru"
alias fetch="neofetch"
alias train="sl | lolcat"
alias speed="speedtest-cli"
alias xclass="xprop | grep WM_CLASS"

# Verbose 

alias mv='mv -v'
alias cp='cp -vr'
alias rm='rm -vr'
