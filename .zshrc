# Path To Your OhMyZsh Installation

export ZSH="/home/arbab/.oh-my-zsh"

# ZSH Prompt Theme

ZSH_THEME="robbyrussell"

# Autocompletion

ENABLE_CORRECTION="true"       # Autocorrection
CASE_SENSITIVE="true"          # Case Sensitive Autocomple
HYPHEN_INSENSITIVE="true"      # Hyphen Insensitive a.k.a "-" and "_" Will Be Interchangeable. CASE_SENSITIVE Must Be Set To False
DISABLE_UPDATE_PROMPT="true"   # Enable Or Disable Update Prompt
COMPLETION_WAITING_DOTS="true" # Dot Buffer

# Source OhMyZsh

source $ZSH/oh-my-zsh.sh


export UPDATE_ZSH_DAYS=1 # Check For OhMyZsh Updates Every n Days

# Plugins

plugins=(
	zsh-syntax-highlighting
	zsh-autosuggestions	
			)


# Enviorment Variables

export LANG=en_US.UTF-8 

if [[ -n $SSH_CONNECTION ]]; then # Editor For SSH And Local Separately
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

export ARCHFLAGS="-arch x86_64" # Compilation Flag
export DISPLAY="HDMI2"          # Display

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
alias clone="git clone"
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