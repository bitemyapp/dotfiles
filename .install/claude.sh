#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component claude "$@"
set -e

# Install Claude Code via native binary (idempotent, no Node/npm required)

# Check if already installed and get version
if command -v claude &> /dev/null; then
    CURRENT_VERSION=$(claude --version 2>/dev/null || echo "unknown")
    echo "Claude Code currently installed: $CURRENT_VERSION"
    echo "Checking for updates..."
fi

# Install or update via official installer
curl -fsSL https://claude.ai/install.sh | bash

# Ensure ~/.local/bin is in PATH (the installer puts it there)
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    for rc_file in "$HOME/.bashrc" "$HOME/.zshrc"; do
        if ! grep -qxF 'export PATH="$HOME/.local/bin:$PATH"' "$rc_file" 2>/dev/null; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$rc_file"
        fi
    done
    export PATH="$HOME/.local/bin:$PATH"
fi

echo "Claude Code installed: $(claude --version 2>/dev/null || echo 'restart shell and run: claude --version')"
