for file in ~/.{extra,bash_prompt,exports,aliases,functions}; do
	[ -r "$file" ] && source "$file"
done
unset file

export PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

alias ls='ls -G'
alias grep='grep --colour=auto'
alias less='less -R'
alias mg='mg -n'
alias gpom='git pull origin master && git push origin master'
alias qke='emacs -q -nw'

scrib(){ sed -n ' s,http://html.scribd.com,\ &,g; s,http://html[1-9].scribdassets.com,\ &,g; s,assets,,; s,pages,images,; s,jsonp,jpg, ; s,html[1-9].scribd.com,html.scribd.com,; s,pageParams.contentUrl = \",,; s,\";,,; s,<img class=\".orig=\",,; s,\"/>,,; /^$/d;

/html.scribd.com.images.*jpg/p; ' \ |sed '/http/!d'; }

scrib_(){ scrib \ |while read a;do feh $a ; done; }

# export BYOBU_PREFIX=`brew --prefix`

function replace() {
    find . -type f | xargs sed -i "s/$1/$2/g"
}

export PATH=/Users/callen/code/valgrind/Inst/bin:~/Library/Haskell/bin:/usr/local/Cellar/ruby/1.9.2-p290/bin:/usr/local/bin:~/bin:/usr/local/sbin:$PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
