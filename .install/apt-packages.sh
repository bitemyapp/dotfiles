#!/usr/bin/env bash

set -ex

# So Emacs doesn't crash due to the a11y dbus thingy
sudo apt install -y at-spi2-core

# Dev tools
sudo apt install -y git zsh xbacklight mosh tmux screen htop colordiff cmake curl gnome-screenshot apt-transport-https ca-certificates software-properties-common gnupg-agent btop procps

# Emacs
sudo apt install -y emacs-gtk

# Fonts
sudo apt install -y fonts-roboto fonts-anonymous-pro

# build-essential, compiler, etc.
sudo apt install -y build-essential clang

