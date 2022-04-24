#!/usr/bin/env bash

set -ex

if test -d $HOME/.cargo; then
    exit 0
fi

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
