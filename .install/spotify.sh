#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component spotify "$@"

set -euo pipefail

if [[ $# -gt 1 || ( $# -eq 1 && $1 != --configure-only ) ]]; then
  echo "Usage: $0 [--configure-only]" >&2
  exit 2
fi

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

# Current key from https://www.spotify.com/us/download/linux/.
# Finish downloading and decoding before replacing the installed key.
curl -fsSL https://download.spotify.com/debian/pubkey_5384CE82BA52C83A.asc \
  -o "$tmp_dir/spotify.asc"
gpg --batch --yes --dearmor -o "$tmp_dir/spotify.gpg" "$tmp_dir/spotify.asc"

cat > "$tmp_dir/spotify.sources" <<EOF
Types: deb
URIs: https://repository.spotify.com/
Suites: stable
Components: non-free
Architectures: $(dpkg --print-architecture)
Signed-By: /etc/apt/keyrings/spotify.gpg
EOF

sudo install -d -m 0755 /etc/apt/keyrings
sudo install -m 0644 "$tmp_dir/spotify.gpg" /etc/apt/keyrings/spotify.gpg
sudo install -m 0644 "$tmp_dir/spotify.sources" /etc/apt/sources.list.d/spotify.sources
# Replace the legacy source as well as files created by apt modernize-sources.
sudo rm -f /etc/apt/sources.list.d/spotify.list /etc/apt/trusted.gpg.d/spotify.gpg

if [[ ${1:-} != --configure-only ]]; then
  sudo apt-get update
  sudo apt-get install -y spotify-client
fi
