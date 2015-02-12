#!/bin/bash

set -e

mkdir -p ~/Work/agda
cd ~/Work/agda
cabal sandbox init && cabal update && cabal install Agda
sudo ln -s `pwd`/.cabal-sandbox/bin/agda /usr/bin/agda
sudo ln -s `pwd`/.cabal-sandbox/bin/agda-mode /usr/bin/agda-mode

# I keep the agda stuff cached in my git repo
# /usr/bin/agda-mode setup
# /usr/bin/agda-mode compile
