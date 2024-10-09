#!/usr/bin/env bash

if dpkg -s telegram-desktop; then
    exit 0;
fi

sudo add-apt-repository ppa:atareao/telegram

sudo apt update && sudo apt install -y telegram
