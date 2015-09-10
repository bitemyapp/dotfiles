#!/bin/bash
set -e

sudo add-apt-repository -y ppa:git-core/ppa
sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
sudo add-apt-repository -y ppa:hvr/ghc

sudo apt-get update

sudo apt-get install -y xmonad xmobar texlive-full texlive-xetex git suckless-tools zsh xorg-dev emacs-snapshot emacs-snapshot-el xtrlock xbacklight stalonetray fdpowermon pasystray python-pygments mosh xorg-dev tmux xorg-dev screen htop silversearcher-ag mumble darcs


wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main' | sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y


sudo apt-get update
sudo apt-get install -y ghc-7.10.2
sudo apt-get install -y cabal-install-1.22

sudo ln -sf /opt/ghc/7.10.2/bin/ghc /usr/bin/ghc
sudo ln -sf /opt/ghc/7.10.2/bin/ghci /usr/bin/ghci
sudo ln -sf /opt/ghc/7.10.2/bin/ghc-pkg /usr/bin/ghc-pkg
sudo ln -sf /opt/ghc/7.10.2/bin/runhaskell /usr/bin/runhaskell

sudo ln -sf /opt/cabal/1.22/bin/cabal /usr/bin/cabal

cabal update
cabal install xmonad xmonad-contrib xmonad-extras

touch .secrets
