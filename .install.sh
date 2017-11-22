#!/bin/bash

set -e

sudo add-apt-repository -y ppa:git-core/ppa
sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
sudo add-apt-repository -y ppa:hvr/ghc

sudo apt-get update

sudo apt-get install -y xmonad xmobar texlive-full texlive-xetex biber git suckless-tools zsh emacs-snapshot emacs-snapshot-el xtrlock xbacklight stalonetray fdpowermon pasystray python-pygments mosh tmux screen htop silversearcher-ag mumble darcs libtinfo-dev inotify-tools colordiff fonts-roboto postgresql-9.5 postgresql-server-dev-9.5 libpq-dev libmysqlclient-dev libpcre3-dev cmake gimp audacity libav-tools normalize-audio libavcodec-extra curl libcurl4-openssl-dev libavcodec-dev libavdevice-dev libvulkan1 libgraphite2-dev libharfbuzz-dev libharfbuzz-gobject0

# SDL deps
sudo apt-get install -y libsdl2-dev libsdl2-image-dev libsdl2-gfx-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev

wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main' | sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y
stack setup
stack install ghc-mod hlint stylish-haskell

sudo apt-get update
sudo apt-get install -y ghc-7.10.3
sudo apt-get install -y cabal-install-1.22

sudo ln -sf /opt/ghc/7.10.3/bin/ghc /usr/bin/ghc
sudo ln -sf /opt/ghc/7.10.3/bin/ghci /usr/bin/ghci
sudo ln -sf /opt/ghc/7.10.3/bin/ghc-pkg /usr/bin/ghc-pkg
sudo ln -sf /opt/ghc/7.10.3/bin/runhaskell /usr/bin/runhaskell

sudo ln -sf /opt/cabal/1.22/bin/cabal /usr/bin/cabal

sudo apt-get install -y libghc-x11-xft-dev libasound2-dev libiw-dev libghc-libxml-sax-dev c2hs
cabal update
cabal install xmonad xmonad-contrib xmonad-extras

curl https://sh.rustup.rs -sSf | sh

curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt-get install -y nodejs

touch .secrets
