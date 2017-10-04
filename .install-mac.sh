#!/bin/bash

osascript .disable-natural-scrolling.scpt

mkdir ~/Screenshots

defaults write com.apple.screencapture ~/Screenshots

killall SystemUIServer

~/.install-xcode.sh

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew doctor

brew update

brew bundle --file=.brewfile
