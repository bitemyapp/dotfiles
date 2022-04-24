#!/usr/bin/env bash

set -ex

sudo apt-get update

script_dir=$(dirname "$0")

$script_dir/google-chrome.sh
$script_dir/apt-packages.sh
$script_dir/vscodium.sh
# $script_dir/node.sh # doesn't work on jammy presently.
$script_dir/rust.sh
$script_dir/zsh-pure.sh
$script_dir/docker.sh
# $script_dir/spotify.sh # doesn't work in jammy either.

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

chsh -s /bin/zsh

cd ~

