#!/usr/bin/env bash

set -euo pipefail

for package in telegram telegram-desktop; do
    if [[ $(dpkg-query -W -f='${Status}' "$package" 2>/dev/null) == 'install ok installed' ]]; then
        exit 0
    fi
done

sudo add-apt-repository -y ppa:atareao/telegram

sudo apt update && sudo apt install -y telegram
