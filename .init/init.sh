#!/bin/bash

set -e

sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 0xd66b746e

sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update && sudo apt-get upgrade
sudo apt-get install emacs24 git xmonad xmobar suckless-tools stterm vim wget curl mosh zsh texlive-full build-essential stalonetray silversearcher-ag python-pygments coq coqide skype pianobar pasystray libtinfo-dev vlc youtube-dl ddd inotify-tools mg

sudo apt-get install ghc-7.8.4 cabal-install-1.22 happy-1.19.4 alex-3.1.3

wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'

sudo apt-get update
sudo apt-get install google-chrome-stable

mkdir -p ~/Work
git clone git@github.com:bitemyapp/dotfiles.git ~/Work/dotfiles

mv ~/Work/dotfiles/.git ~/
mv ~/Work/dotfiles/.* ~/

./agda.sh
