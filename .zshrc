# Path to your oh-my-zsh installation.

export ZSH="/home/arbab/.oh-my-zsh"

# ZSH Prompt Theme

ZSH_THEME="robbyrussell"

# Autocompletion

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"

# Automatically Update

DISABLE_UPDATE_PROMPT="true"

# Check For Updates

export UPDATE_ZSH_DAYS=1

# Command Correction

ENABLE_CORRECTION="true"

# Wait For Completion

COMPLETION_WAITING_DOTS="true"

# Plugins

plugins=(
	zsh-syntax-highlighting
	zsh-autosuggestions	
			)

source $ZSH/oh-my-zsh.sh

# Enviorment Variables

export LANG=en_US.UTF-8 

if [[ -n $SSH_CONNECTION ]]; then # Editor For SSH And Local Separately
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

export ARCHFLAGS="-arch x86_64"   # Compilation Flag

export MONITOR="HDMI2"

# KEYBINDING TYPE

bindkey -v 

# ALIASES

alias vim="nvim"
alias svim="sudo -e nvim"
alias lmatrix="cmatrix | lolcat"
alias zshconfig="nvim ~/.zshrc"
alias zshreload="source $HOME/.zshrc"
alias cdPW="cd ~/Programming/WebDev/"
alias speed="speedtest-cli"
alias gp="git push -u origin main"
alias ga="git add ."
alias fetch="sudo neofetch"
alias rm-orphans="sudo pacman -Rns $(pacman -Qtdq)"
alias app-img-exec="chmod u+x"
alias train="sl | lolcat"
alias vs="code"
alias ls="exa -a"
alias la="exa -la"
alias clone="git clone"
alias gm="ga && git commit -m"
alias gpb="git push origin"
alias g="git"
alias pac="paru"
alias downgrade="sudo downgrade"
alias gdot="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"
alias c="clear"
alias spicetify-reapply="spicetify backup apply"
alias npm="sudo npm"
alias pipes="pipes.sh"
alias drive-set-perms="sudo chmod ugo+wx"
