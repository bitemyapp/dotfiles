#!/bin/bash

set -x

if dpkg -s spotify-client; then
    exit 0;
fi

# sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90

# curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -
curl -sS https://download.spotify.com/debian/pubkey_5E3C45D7B312C643.gpg | sudo apt-key add - 

sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"

sudo apt install spotify-client
