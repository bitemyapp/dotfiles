autoload -U promptinit && promptinit
autoload -U compinit && compinit
autoload -U colors && colors

unsetopt correct_all
unsetopt correct

bindkey '^U' backward-kill-line
bindkey '^Y' yank

# base -> [%n@%m %~]$
prompt='%{%F{white}%}[%{%F{green}%}%n@%m %{%F{cyan}%}%~%{%F{white}%}]%{%F{reset}%}$ '
export LSCOLORS=GxFxCxDxBxegedabagaced

zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' '+l:|=*'

alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias qke='emacs -q -nw'
# alias ls='ls --color=auto'

alias gpo='git pull origin && git push origin'
alias gpom='git pull origin master && git push origin master'
alias gpgm='git pull gh master && git push gh master'
alias ph='gpom && gphm'
alias gpte='git checkout experimental && git merge master && git push origin experimental && git checkout master'
alias gpts='git checkout staging && git merge master && git push origin staging && git checkout master'
alias gptp='git checkout production && git merge master && git push origin production && git checkout master'
alias act='source `find . -name "activate" | grep "bin/activate"`'
alias kill-ghc-modi="ps aux | grep ghc-modi | grep -v grep | awk '{print $2}' | xargs kill"

# git branch --set-upstream new_frontpage origin/new_frontpage

function gh {
    open "https://github.com/$@"
}

alias hgdiff='hg diff | colordiff | less -R'
export EDITOR='mg'
export GIT_EDITOR='mg'

# I am baffled that I even have to do this.
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word

export MARKPATH=$HOME/.marks
function jump { 
    cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p $MARKPATH; ln -s $(pwd) $MARKPATH/$1
}
function unmark { 
    rm -i $MARKPATH/$1 
}
# function marks {
#     ls -l $MARKPATH | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
# }
function marks {
        \ls -l $MARKPATH | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
}

export TERM=xterm-256color

export PATH=~/bin:~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:/usr/local/bin:$HOME/code/ghc/bin:$HOME/Library/Haskell/bin:$HOME/.cabal/bin:~/bin:$PATH:$HOME/.carton/bin:~/bin:/Applications/Postgres.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin/FDK/Tools/osx:/usr/local/share/npm/bin

export PATH=~/.cask/bin:$PATH

export PATH=~/.screenlayout:$PATH

export FPATH=$HOME/.zsh_completion:$FPATH

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

export PRE_NIX_PATH=$PATH

export PATH=/opt/ghc/7.8.3/bin:$PATH

export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH=/Applications/ghc-7.8.3.app/Contents/bin:$PATH
