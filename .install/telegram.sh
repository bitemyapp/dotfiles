#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component telegram "$@"

set -euo pipefail

for package in telegram telegram-desktop; do
    if [[ $(dpkg-query -W -f='${Status}' "$package" 2>/dev/null) == 'install ok installed' ]]; then
        exit 0
    fi
done

sudo add-apt-repository -y ppa:atareao/telegram

sudo apt update && sudo apt install -y telegram
