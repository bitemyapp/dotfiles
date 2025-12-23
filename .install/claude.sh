#!/usr/bin/env bash
set -e

# Install Claude Code via native binary (idempotent, no Node/npm required)

# Check if already installed and get version
if command -v claude &> /dev/null; then
    CURRENT_VERSION=$(claude --version 2>/dev/null || echo "unknown")
    echo "Claude Code already installed: $CURRENT_VERSION"
    echo "Checking for updates..."
fi

# Install or update via official installer
curl -fsSL https://claude.ai/install.sh | bash

# Ensure ~/.local/bin is in PATH (the installer puts it there)
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc 2>/dev/null || true
    export PATH="$HOME/.local/bin:$PATH"
fi

echo "Claude Code installed: $(claude --version 2>/dev/null || echo 'restart shell and run: claude --version')"
