autoload -U promptinit && promptinit
autoload -U compinit && compinit
autoload -U colors && colors

unsetopt correct_all
unsetopt correct

bindkey '^U' backward-kill-line
bindkey '^Y' yank

# base -> [%n@%m %~]$
export LSCOLORS=GxFxCxDxBxegedabagaced

zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' '+l:|=*'

alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias qke='emacs -q -nw'

alias gpo='git pull origin && git push origin'
alias gpoc='git pull origin `git name-rev --name-only HEAD` && git push origin `git name-rev --name-only HEAD`'
alias gpom='git pull origin master'
alias gpgm='git pull gh master'
alias gl='git log --oneline --graph --decorate --all'
alias ph='gpom && gphm'
alias act='source `find . -name "activate" | grep "bin/activate"`'
alias kill-ghc-modi="ps aux | grep ghc-modi | grep -v grep | awk '{print $2}' | xargs kill"
alias repl='cabal repl --with-ghc=ghci-ng'

alias hgdiff='hg diff | colordiff | less -R'
export EDITOR="emacs -q -nw"
export GIT_EDITOR="emacs -q -nw"

# I am baffled that I even have to do this.
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word
bindkey -e
# bindkey '^[[1;5D' forward-word
# bindkey '^[[1;5C' backward-word
# bindkey '^[[1;9C' forward-word
# bindkey '^[[1;9D' backward-word

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

export PATH=.cabal-sandbox/bin:~/bin:~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.1/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:/usr/local/bin:$HOME/code/ghc/bin:$HOME/Library/Haskell/bin:$HOME/.cabal/bin:~/bin:$PATH:$HOME/.carton/bin:~/bin:/Applications/Postgres.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin/FDK/Tools/osx:/usr/local/share/npm/bin

export PATH=~/.cask/bin:$PATH

export PATH=~/.screenlayout:$PATH

export FPATH=$HOME/.zsh_completion:$FPATH

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

export PRE_NIX_PATH=$PATH

export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

export PATH=/usr/local/texlive/2015/bin/x86_64-darwin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.stack/programs/x86_64-osx/ghc-7.10.2/bin:$PATH
export PATH=/opt/ghc/8.0.1/bin:$PATH
export PATH=$HOME/work/node-v4.2.0-linux-x64/bin:$PATH

source ~/.secrets

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

setopt prompt_subst
PROMPT='%{%F{white}%}[ %{%F{blue}%}%n@%m %{%F{cyan}%}%~%{%F{white}%} $(git_prompt_info) $(parse_git_dirty)]
%{%F{reset}%}$ '

parse_git_branch() {
  (command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
}

parse_git_dirty() {
  if command git diff-index --quiet HEAD 2> /dev/null; then
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  else
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  fi
}

ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✔ %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%}✗ %{$reset_color%}"

export NVM_DIR="/home/callen/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
