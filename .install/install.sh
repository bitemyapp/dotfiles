#!/usr/bin/env bash

set -ex

script_dir=$(cd -- "$(dirname -- "$0")" && pwd)

# Repair an existing Spotify source before it can block the initial APT update.
if [[ -f /etc/apt/sources.list.d/spotify.list || -f /etc/apt/sources.list.d/spotify.sources ]]; then
  "$script_dir/spotify.sh" --configure-only
fi

sudo apt-get update

sudo apt install -y gnome-screenshot xclip

# $script_dir/google-chrome.sh
$script_dir/apt-packages.sh
$script_dir/rust.sh
$script_dir/fonts.sh
# $script_dir/vscode.sh
$script_dir/docker.sh
$script_dir/spotify.sh
$script_dir/telegram.sh
$script_dir/signal.sh
$script_dir/cursor.sh
$script_dir/claude.sh
$script_dir/codex.sh
$script_dir/ghostty.sh

source $HOME/.cargo/env

git config --global user.email "cma@bitemyapp.com"
git config --global user.name "Chris Allen"

# Utilities written in Rust
cargo install --locked tokei ripgrep just rink fd-find starship difftastic mergiraf

mkdir -p "$HOME/Screenshots"

gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

touch ~/.secrets

sudo cp -r ~/.fonts/*.ttf /usr/local/share/fonts/

fc-cache -f -v

if [[ $(getent passwd "$USER" | cut -d: -f7) != /bin/zsh ]]; then
  chsh -s /bin/zsh
fi

cd ~
