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
alias pte='gpom && gpte && fab push_experimental_scar'
alias pts='gpom && gpts && fab push_staging_scar'
alias ptp='gpom && gptp && fab push_prod_scar'
# git branch --set-upstream new_frontpage origin/new_frontpage

alias redisstart='sudo launchctl start io.redis.redis-server'
alias redisstop='sudo launchctl stop io.redis.redis-server'
alias love='/Applications/love.app/Contents/MacOS/love'
alias pg='sudo -u postgres psql'
alias cdb='sudo -u postgres createdb'
alias hgdiff='hg diff | colordiff | less -R'

# I am baffled that I even have to do this.
bindkey ';5D' emacs-backward-word
bindkey ';5C' emacs-forward-word

export PATH=$PATH:/Users/callen/.carton/bin:/usr/local/bin:/Users/callen/bin:/Users/callen/Library/Haskell/bin:/Users/callen/.cabal/bin:/Users/callen/bin:/Applications/Postgres.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/callen/bin/FDK/Tools/osx:/usr/local/share/npm/bin/
