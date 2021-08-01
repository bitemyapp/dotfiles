#!/bin/bash

set -e

sudo add-apt-repository -y ppa:git-core/ppa
sudo add-apt-repository -y ppa:ubuntu-elisp/ppa

sudo apt-get update

# So Emacs doesn't crash due to the a11y dbus thingy
sudo apt install at-spi2-core

# Dev tools
sudo apt install -y git zsh xbacklight mosh tmux screen htop silversearcher-ag darcs colordiff cmake curl gnome-screenshot

# xtrlock isn't secure, so I don't use it anymore. See `xsecurelock` below.

# Emacs
sudo apt-get install -y emacs-snapshot emacs-snapshot-el

# Window management
sudo apt-get install -y xmonad xmobar suckless-tools stalonetray fdpowermon pasystray xcompmgr pavucontrol

# TeX
sudo apt-get install -y texlive-full texlive-xetex biber python3-pygments texlive-fonts-extra

# Fonts
sudo apt install -y fonts-roboto

# Databases
sudo apt install -y libpq-dev libmysqlclient-dev

# SDL deps
sudo apt-get install -y libsdl2-dev libsdl2-image-dev libsdl2-gfx-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev

# Misc. dev dependencies
sudo apt install -y libcurl4-openssl-dev libavcodec-dev libavdevice-dev libvulkan1 libgraphite2-dev libharfbuzz-dev libharfbuzz-gobject0 libpam0g-dev libreadline-dev libxmu-dev libtinfo-dev inotify-tools libpcre3-dev

# A/V
sudo apt install -y gimp audacity normalize-audio libavcodec-extra

# Communication
sudo apt install -y mumble

curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs

# ./.install-xsecurelock.sh

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

source $HOME/.cargo/env

# utilities
cargo install tokei xsv diffr lsd starship mdcat strace-analyzer procs bingrep ripgrep sd cargo-watch cargo-edit

mkdir ~/Screenshots

gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

touch ~/.secrets

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k

sudo cp -r ~/.fonts/*.ttf /usr/local/share/fonts/

fc-cache -f -v

cd ~
