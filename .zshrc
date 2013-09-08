autoload -U promptinit && promptinit
autoload -U compinit && compinit
autoload -U colors && colors

unsetopt correct_all
unsetopt correct

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
alias ls='ls -G'

alias gpo='git pull origin && git push origin'
alias gpom='git pull origin master && git push origin master'
alias gphm='git push heroku master'
alias ph='gpom && gphm'
alias gpte='git checkout experimental && git merge master && git push origin experimental && git checkout master'
alias gpts='git checkout staging && git merge master && git push origin staging && git checkout master'
alias gptp='git checkout production && git merge master && git push origin production && git checkout master'
alias act='source `find . -name "activate" | grep "bin/activate"`'
# git branch --set-upstream new_frontpage origin/new_frontpage
# Thanks TimMc :)
alias learn="java -jar ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar -e '(->> clojure.core quote the-ns ns-publics seq rand-nth val meta ((juxt :name :doc)) (map println) dorun)'"
alias cljhere="java -jar ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar -e"
# '(java.util.UUID/randomUUID)'

function gh {
    open "https://github.com/$@"
}

alias hgdiff='hg diff | colordiff | less -R'
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

export PATH=/usr/local/bin:~/bin:$PATH:~/callen/.carton/bin:~/Library/Haskell/bin:~/callen/.cabal/bin:~/callen/bin:/Applications/Postgres.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:~/callen/bin/FDK/Tools/osx:/usr/local/share/npm/bin/:~/.cabal/bin

# OPAM configuration
. /Users/callen/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
