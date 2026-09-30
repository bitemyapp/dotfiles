#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component fonts "$@"

set -euo pipefail

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

font_url='https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip'
curl -fsSL "$font_url" -o "$tmp_dir/FiraCode.zip"
mkdir -p "$HOME/.fonts"
unzip -o "$tmp_dir/FiraCode.zip" -d "$HOME/.fonts"
fc-cache -fv
