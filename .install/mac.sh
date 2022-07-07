#!/bin/bash

# osascript .disable-natural-scrolling.scpt
touch .profile
touch .secrets
touch .env

mkdir ~/Screenshots

defaults write com.apple.screencapture ~/Screenshots

killall SystemUIServer

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew bundle --file=.brewfile

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

cargo install cargo-trim
