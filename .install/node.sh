#!/usr/bin/env bash

set -ex

if dpkg -s nodejs; then
    exit 0;
fi

curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs

