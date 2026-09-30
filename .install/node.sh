#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component node "$@"

set -ex

if dpkg -s nodejs; then
    exit 0;
fi

curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs

