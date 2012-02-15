
setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo '○'
}

function battery_charge {
    echo `~/bin/batt.py` 2>/dev/null
}

function box_name {
    [ -f ~/.box-name ] && cat ~/.box-name || hostname -s
}


zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# keys
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word
# ^^ my fucking god why isn't this set by default?

# aliases
alias ls='ls -F --color=auto' 
alias ll='ls -lh'
alias grep='grep --color=auto' 
alias ec='emacsclient -n -a emacs'

# Set up the prompt

# autoload -Uz promptinit
# promptinit
# prompt adam1

autoload -U colors && colors
PROMPT="%{$fg[green]%}%n%{$reset_color%}@%{$fg[cyan]%}%m?!%{$reset_color%} %{$fg[red]%}%~ %{$reset_color%}$ "

RPROMPT=$(battery_charge)

