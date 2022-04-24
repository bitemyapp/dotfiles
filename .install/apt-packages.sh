#!/usr/bin/env bash

set -ex

# So Emacs doesn't crash due to the a11y dbus thingy
sudo apt install -y at-spi2-core

# Dev tools
sudo apt install -y git zsh xbacklight mosh tmux screen htop silversearcher-ag darcs colordiff cmake curl gnome-screenshot apt-transport-https ca-certificates software-properties-common gnupg-agent

# Emacs
sudo apt install -y emacs

# Window management
# sudo apt-get install -y xmonad xmobar suckless-tools stalonetray fdpowermon pasystray xcompmgr pavucontrol

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
