export EDITOR="emacs -q -nw"

alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias qke='emacs -q -nw'

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

export LANG=en_US.UTF-8
