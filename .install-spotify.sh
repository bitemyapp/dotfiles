#!/bin/bash

set -ex

curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -

#  &&
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D2C19886 &&
# sudo apt-get update -qq &&
sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"

sudo apt install spotify-client
