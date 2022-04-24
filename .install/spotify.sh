#!/bin/bash

set -ex

if dpkg -s spotify-client; then
    exit 0;
fi

curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -

sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"

sudo apt install spotify-client
