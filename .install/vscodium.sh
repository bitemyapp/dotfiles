#!/usr/bin/env bash

set -ex

if dpkg -s codium; then
    exit 0;
fi

CODIUM_KEYRING="/usr/share/keyrings/vscodium-archive-keyring.gpg"

if ! test -f $CODIUM_KEYRING; then
    wget -qO - https://gitlab.com/paulcarroty/vscodium-deb-rpm-repo/raw/master/pub.gpg \
        | gpg --dearmor \
        | sudo dd of=$CODIUM_KEYRING
fi

CODIUM_SOURCE="/etc/apt/sources.list.d/vscodium.list"

if ! test -f $CODIUM_SOURCE; then
    echo 'deb [ signed-by=/usr/share/keyrings/vscodium-archive-keyring.gpg ] https://download.vscodium.com/debs vscodium main' \
        | sudo tee $CODIUM_SOURCE
fi

sudo apt update && sudo apt install -y codium

