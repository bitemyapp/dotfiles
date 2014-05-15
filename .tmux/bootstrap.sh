#!/bin/bash

function die()
{
    echo "${@}"
    exit 1
}

for i in $HOME/.tmux $HOME/.tmux.conf; do
  if [[ ( -e $i ) || ( -h $i ) ]]; then
    echo "${i} has been renamed to ${i}.old"
    mv "${i}" "${i}.old" || die "Could not move ${i} to ${i}.old"
  fi
done

git clone --recursive git://github.com/yesmeck/tmuxrc.git $HOME/.tmux \
    || die "Could not clone the repository to ${HOME}/.tmux"

ln -s $HOME/.tmux/tmux.conf $HOME/.tmux.conf

echo "Install successfully."
