#!/bin/bash
set -e

# Install OpenAI Codex CLI via GitHub releases (idempotent, no Node/npm required)

INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

# Get latest stable version (rust-vX.Y.Z, not alpha)
# Use the GitHub API instead of scraping the HTML releases page; GitHub may
# fill the first page with prereleases, which makes the old scraper fail.
LATEST=$(curl -fsSL \
    -H "Accept: application/vnd.github+json" \
    "https://api.github.com/repos/openai/codex/releases/latest" 2>/dev/null \
    | sed -n 's/.*"tag_name": "\(rust-v[0-9][^"]*\)".*/\1/p' \
    | head -1)

if [[ -z "$LATEST" || "$LATEST" == *alpha* ]]; then
    echo "Failed to determine latest version"
    exit 1
fi

echo "Latest Codex version: $LATEST"

# Check current version if installed
if command -v codex &> /dev/null; then
    CURRENT=$(codex --version 2>/dev/null | head -1 || echo "unknown")
    echo "Currently installed: $CURRENT"
fi

# Download and install
ARCHIVE="codex-x86_64-unknown-linux-musl.tar.gz"
URL="https://github.com/openai/codex/releases/download/${LATEST}/${ARCHIVE}"

echo "Downloading $URL..."
curl -fsSL "$URL" -o "/tmp/${ARCHIVE}"

# Extract (binary name inside has platform suffix)
tar -xzf "/tmp/${ARCHIVE}" -C /tmp
mv /tmp/codex-x86_64-unknown-linux-musl "$INSTALL_DIR/codex"
chmod +x "$INSTALL_DIR/codex"
rm "/tmp/${ARCHIVE}"

# Ensure ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc 2>/dev/null || true
    export PATH="$INSTALL_DIR:$PATH"
fi

echo "Codex installed: $($INSTALL_DIR/codex --version 2>/dev/null || echo 'restart shell and run: codex --version')"
