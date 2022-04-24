#!/bin/bash

if dpkg -s google-chrome-beta; then
    exit 0;
fi

if ! test -f https://dl-ssl.google.com/linux/linux_signing_key.pub; then
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
fi

if ! test -f /etc/apt/sources.list.d/google-chrome.list; then
    sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
fi

sudo apt update && sudo apt install -y google-chrome-beta

