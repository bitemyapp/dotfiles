#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component rust "$@"

set -ex

if test -d $HOME/.cargo; then
    exit 0
fi

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
