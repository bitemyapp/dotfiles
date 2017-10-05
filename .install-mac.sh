#!/bin/bash

# osascript .disable-natural-scrolling.scpt

mkdir ~/Screenshots

defaults write com.apple.screencapture ~/Screenshots

killall SystemUIServer

~/.install-xcode.sh

sudo xcodebuild -license accept

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew doctor

brew update

brew bundle --file=.brewfile

curl -sSL https://get.haskellstack.org/ | sh

stack setup

stack build yesod lens servant persistent esqueleto

curl https://sh.rustup.rs -sSf | sh -s -- -y

rustup default nightly

cargo install ripgrep loc
