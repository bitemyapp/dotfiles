#!/usr/bin/env bash

set -ex

sudo apt-get update

./google-chrome.sh
./apt-packages.sh
./vscodium.sh
./node.sh
./rust.sh
./zsh-pure.sh
./docker.sh
./spotify.sh

source $HOME/.cargo/env

git config --global user.email "cma@bitemyapp.com"
git config --global user.name "Chris Allen"

# Utilities written in Rust
cargo install tokei xsv bingrep ripgrep cargo-edit

mkdir ~/Screenshots

gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

touch ~/.secrets

sudo cp -r ~/.fonts/*.ttf /usr/local/share/fonts/

fc-cache -f -v

cd ~

