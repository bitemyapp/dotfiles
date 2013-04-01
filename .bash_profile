function parse_git_branch {
        git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \[\1\]/'
}

function proml {

  local        BLUE="\[\033[0;34m\]"

  local        LIGHT_BLUE="\[\033[1;34m\]"

  local         RED="\[\033[0;31m\]"

  local   LIGHT_RED="\[\033[1;31m\]"

  local       GREEN="\[\033[0;32m\]"

  local LIGHT_GREEN="\[\033[1;32m\]"

  local       WHITE="\[\033[1;37m\]"

  local  LIGHT_GRAY="\[\033[0;37m\]"

  local     DEFAULT="\[\033[0m\]"

  PS1="\W$LIGHT_GREEN\$(parse_git_branch)$DEFAULT\$ "

}

proml

# PS1='[\u@\h \W]\$ '

export EDITOR="emacs -q -nw"

alias ls='ls -G'
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

function replace() {
    find . -type f | xargs sed -i "s/$1/$2/g"
}

export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

export LANG=en_US.UTF-8
# Initialization for FDK command line tools.Mon Oct  8 10:18:27 2012
FDK_EXE="/Users/callen/bin/FDK/Tools/osx"

PATH="/Applications/Postgres.app/Contents/MacOS/bin:$PATH"
export PATH=$HOME/Library/Haskell/bin:$HOME/.cabal/bin:$HOME/bin:${PATH}:"/Users/callen/bin/FDK/Tools/osx"
export PATH=${PATH}:/usr/local/share/npm/bin/
export PATH=$HOME/bin:${PATH}
export FDK_EXE
export TARGET_ENV=local
export JPDA_TRANSPORT=dt_socket
export JPDA_ADDRESS=9000
export TOMCAT_HOME=/usr/local/Cellar/tomcat/7.0.32/libexec

[[ -s /Users/callen/.nvm/nvm.sh ]] && . /Users/callen/.nvm/nvm.sh

source ~/.git-flow-completion.bash
source `which virtualenvwrapper.sh`
