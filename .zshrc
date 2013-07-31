autoload -U promptinit && promptinit
autoload -U compinit && compinit
autoload -U colors && colors

unsetopt correct_all
unsetopt correct

prompt redhat
# zstyle ':completion:*' completer _complete _match _approximate
# zstyle ':completion:*:match:*' original only
# zstyle ':completion:*:approximate:*' max-errors 1 numeric
# zstyle -e ':completion:*:approximate:*' \
#         max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'\
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' '+l:|=*'

export PATH=$PATH:/Users/callen/.carton/bin:/usr/local/bin:/Users/callen/bin:/Users/callen/Library/Haskell/bin:/Users/callen/.cabal/bin:/Users/callen/bin:/Applications/Postgres.app/Contents/MacOS/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/callen/bin/FDK/Tools/osx:/usr/local/share/npm/bin/
