#!/usr/bin/env bash

set -ex

sudo apt-get update

sudo apt install gnome-screenshot xclip

script_dir=$(dirname "$0")

$script_dir/google-chrome.sh
$script_dir/apt-packages.sh
$script_dir/rust.sh
$script_dir/fonts.sh
$script_dir/vscode.sh
$script_dir/docker.sh
$script_dir/spotify.sh
$script_dir/telegram.sh
$script_dir/signal.sh

source $HOME/.cargo/env

git config --global user.email "cma@bitemyapp.com"
git config --global user.name "Chris Allen"

# Utilities written in Rust
cargo install --locked tokei ripgrep just rink

mkdir ~/Screenshots

gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

touch ~/.secrets

sudo cp -r ~/.fonts/*.ttf /usr/local/share/fonts/

fc-cache -f -v

chsh -s /bin/zsh

cd ~
