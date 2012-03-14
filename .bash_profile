export PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

alias ls='ls -G'
alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias gpom='git pull origin master && git push origin master'
alias qke='emacs -q -nw'

export PATH=/usr/local/Cellar/ruby/1.9.2-p290/bin:/usr/local/bin:~/bin:$PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
