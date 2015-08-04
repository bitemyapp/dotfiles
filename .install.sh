#!/bin/bash

sudo add-apt-repository ppa:git-core/ppa
sudo add-apt-repository ppa:ubuntu-elisp/pp
sudo add-apt-repository ppa:hvr/ghc

sudo apt-get install xmonad xmobar texlive-full texlive-xetex git suckless-tools zsh xorg-dev emacs-snapshot emacs-snapshot-el xtrlock xbacklight stalonetray fdpowermon pasystray python-pygments mosh

wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y


sudo apt-get update
sudo apt-get install ghc-7.10.2
sudo apt-get install cabal-install-1.22

sudo ln -s /opt/ghc/7.10.2/bin/ghc /usr/bin/ghc
sudo ln -s /opt/ghc/7.10.2/bin/ghci /usr/bin/ghci
sudo ln -s /opt/ghc/7.10.2/bin/ghc-pkg /usr/bin/ghc-pkg
sudo ln -s /opt/ghc/7.10.2/bin/runhaskell /usr/bin/runhaskell

sudo ln -s /opt/cabal/1.22/bin/cabal /usr/bin/cabal

cabal update
cabal install xmonad xmonad-contrib xmonad-extras
