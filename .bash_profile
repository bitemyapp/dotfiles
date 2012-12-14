function exitstatus {

    EXITSTATUS="$?"
    BOLD="\[\033[1m\]"
    RED="\[\033[0;31m\]"
    GREEN="\[\e[0;32m\]"
    BLUE="\[\e[34m\]"
    OFF="\[\033[m\]"

    HOST="\h"
    USER="\u"
    DIR="\w"
    NEWLINE="\n"
    DATE="\d"
    TIME="\t"

    PROMPT="\[\033]0;${USER}@${HOST}: \w\007\n${GREEN}${TIME} ${DATE} [${USER}@${HOST}:[${BLUE}\w${GREEN}]"

    if [ "${EXITSTATUS}" -eq 0 ]
    then
        PS1="${PROMPT} [${GREEN}${EXITSTATUS}${GREEN}]${OFF}\n$ "
    else
        PS1="${PROMPT} [${BOLD}${EXITSTATUS}${GREEN}]${OFF}\n$ "
    fi

    PS2="${BOLD}>${OFF} "
}

PROMPT_COMMAND=exitstatus

export EDITOR="emacs -q -nw"

alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias qke='emacs -q -nw'

alias gpom='git pull origin master && git push origin master'
alias gpte='git checkout experimental && git merge master && git push origin experimental && git checkout master'
alias gpts='git checkout staging && git merge master && git push origin staging && git checkout master'
alias gptp='git checkout production && git merge master && git push origin production && git checkout master'
alias pte='gpom && gpte && fab push_experimental_scar'
alias pts='gpom && gpts && fab push_staging_scar'

alias redisstart='sudo launchctl start io.redis.redis-server'
alias redisstop='sudo launchctl stop io.redis.redis-server'
alias love='/Applications/love.app/Contents/MacOS/love'

function replace() {
    find . -type f | xargs sed -i "s/$1/$2/g"
}

export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules
source /usr/local/bin/virtualenvwrapper.sh
export LANG=en_US.UTF-8
# Initialization for FDK command line tools.Mon Oct  8 10:18:27 2012
FDK_EXE="/Users/callen/bin/FDK/Tools/osx"
PATH="/Applications/Postgres.app/Contents/MacOS/bin:$PATH"
export PATH=$HOME/Library/Haskell/bin:$HOME/.cabal/bin:$HOME/bin:${PATH}:"/Users/callen/bin/FDK/Tools/osx"
export FDK_EXE
export TARGET_ENV=local
export JPDA_TRANSPORT=dt_socket
export JPDA_ADDRESS=9000
export TOMCAT_HOME=/usr/local/Cellar/tomcat/7.0.32/libexec
