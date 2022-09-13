# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

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

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# hvr PPA
export PATH=/opt/cabal/1.22/bin:$PATH
export PATH=/opt/ghc/7.10.2/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

# Add GHC 7.8.4 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.8.4.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

if [ -e /home/callen/.nix-profile/etc/profile.d/nix.sh ]; then . /home/callen/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYTHONPATH=$PYTHONPATH:~/work/smb-measurements/smb-measurements-aws

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/callen/.sdkman"
[[ -s "/Users/callen/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/callen/.sdkman/bin/sdkman-init.sh"
. "$HOME/.cargo/env"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
. "$HOME/.cargo/env"


# Added by Toolbox App
export PATH="$PATH:/home/callen/.local/share/JetBrains/Toolbox/scripts"