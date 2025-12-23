#!/usr/bin/env bash

set -e

# Install Slack via official apt repository (idempotent)

# Add Slack GPG key if not present
if [[ ! -f /usr/share/keyrings/slack.gpg ]]; then
    echo "Adding Slack GPG key..."
    wget -qO- https://packagecloud.io/slacktechnologies/slack/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/slack.gpg
fi

# Add Slack repository if not present
if [[ ! -f /etc/apt/sources.list.d/slack.list ]]; then
    echo "Adding Slack repository..."
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/slack.gpg] https://packagecloud.io/slacktechnologies/slack/debian/ jessie main" | sudo tee /etc/apt/sources.list.d/slack.list
fi

# Install or upgrade
sudo apt update
sudo apt install -y slack-desktop

echo "Slack installed: $(slack --version 2>/dev/null || echo 'run from applications menu')"
