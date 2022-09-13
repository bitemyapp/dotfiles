#!/usr/bin/env bash

sudo apt-get install -y build-essential autoconf apache2-utils automake binutils gcc libc6-dev libpam-dev libx11-dev libxcomposite-dev libxss-dev mplayer mpv imagemagick pamtester x11-xserver-utils xscreensaver

mkdir -p ~/work
cd ~/work
git clone git@github.com:google/xsecurelock.git
cd xsecurelock
sh autogen.sh
./configure --with-pam-service-name=common-auth
make
sudo make install
