#!/bin/bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component google-chrome "$@"

if dpkg -s google-chrome; then
    exit 0;
fi

if ! test -f https://dl-ssl.google.com/linux/linux_signing_key.pub; then
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
fi

if ! test -f /etc/apt/sources.list.d/google-chrome.list; then
    sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
fi

sudo apt update && sudo apt install -y google-chrome-stable

