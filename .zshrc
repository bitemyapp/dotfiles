# Getting pure in the path
# fpath+=$HOME/.zsh/pure
source $HOME/.aliases

bindkey '^U' backward-kill-line
bindkey '^Y' yank

# Completion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' '+l:|=*'

export EDITOR="emacs -q -nw"
export GIT_EDITOR="emacs -q -nw"

# I am baffled that I even have to do this.
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word
bindkey -e

export TERM=xterm-256color

export PATH=~/.cask/bin:$PATH

export FPATH=$HOME/.zsh_completion:$FPATH

GPG_TTY=$(tty)
export GPG_TTY

export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.bin:$PATH
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$HOME/work/rustc/src"
export CARGO_HOME="$HOME/.cargo"
export CARGO_TARGET_DIR="$HOME/.cargo/cache"
export GOPATH="$HOME/.local/goworkspace"
export PATH="$PATH:node_modules/.bin"
export PATH="$PATH:/opt/homebrew/bin"
export ANDROID_HOME="$HOME/Android/Sdk"
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=/usr/local/cuda/bin${PATH:+:${PATH}}

if [[ `uname` == 'Linux' ]]
then
        export LINUX=1
        export GNU_USERLAND=1
else
        export LINUX=
fi

if [[ `uname` == 'Darwin' ]]
then
        export OSX=1
else
        export OSX=
fi
if [[ "$OSX" == "1" ]]
then
    export BREW_PREFIX=`brew --prefix`
    export OPENSSL_INCLUDE_DIR=`brew --prefix openssl`/include
    export OPENSSL_LIB_DIR=`brew --prefix openssl`/lib
    export DEP_OPENSSL_INCLUDE=`brew --prefix openssl`/include
    # export LDFLAGS="-L$BREW_PREFIX/lib"
    # export CPPFLAGS="-I$BREW_PREFIX/include"
    export PQ_LIB_DIR="$BREW_PREFIX/lib"
    export PATH="/usr/local/opt/llvm/bin:$PATH"
fi

if test -f $HOME/.secrets; then
    source ~/.secrets
fi

# Load pure
autoload -U promptinit; promptinit
# prompt pure
eval "$(starship init zsh)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/callen/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/callen/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/callen/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/callen/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
