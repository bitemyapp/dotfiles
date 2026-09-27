#!/usr/bin/env bash
set -euo pipefail

# Install OpenAI Codex CLI via GitHub releases (idempotent, no Node/npm required)

INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

# Check current version if installed
if [[ -x "$INSTALL_DIR/codex" ]]; then
    CURRENT=$("$INSTALL_DIR/codex" --version 2>/dev/null || echo "unknown")
    echo "Currently installed: $CURRENT"
fi

# GitHub resolves the latest stable release directly; no API lookup or JSON
# scraping is needed, and curl errors remain visible if the download fails.
BINARY="codex-x86_64-unknown-linux-musl"
ARCHIVE="${BINARY}.tar.gz"
URL="https://github.com/openai/codex/releases/latest/download/${ARCHIVE}"

# Stage on the same filesystem so replacement is atomic, including when the
# installed executable is running. Failed downloads leave that copy intact.
TMP_DIR=$(mktemp -d "$INSTALL_DIR/.codex-install.XXXXXX")
trap 'rm -rf "$TMP_DIR"' EXIT

echo "Downloading $URL..."
curl -fsSL --retry 3 --connect-timeout 30 "$URL" -o "$TMP_DIR/$ARCHIVE"

# Extract (binary name inside has platform suffix)
tar -xzf "$TMP_DIR/$ARCHIVE" -C "$TMP_DIR" "$BINARY"
chmod +x "$TMP_DIR/$BINARY"
VERSION=$("$TMP_DIR/$BINARY" --version)
mv -f "$TMP_DIR/$BINARY" "$INSTALL_DIR/codex"

# Ensure ~/.local/bin is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    for rc_file in "$HOME/.bashrc" "$HOME/.zshrc"; do
        if ! grep -qxF 'export PATH="$HOME/.local/bin:$PATH"' "$rc_file" 2>/dev/null; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$rc_file"
        fi
    done
    export PATH="$INSTALL_DIR:$PATH"
fi

echo "Codex installed: $VERSION"
