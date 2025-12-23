#!/usr/bin/env bash

mkdir -p ~/Applications

# Get the actual download URL from the API
CURSOR_URL=$(curl -s "https://cursor.com/api/download?platform=linux-x64&releaseTrack=stable" | grep -o 'https://[^"]*\.AppImage')
wget -O ~/Applications/Cursor.AppImage "$CURSOR_URL"
chmod +x ~/Applications/Cursor.AppImage


# Create desktop entry
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/cursor.desktop << 'EOF'
[Desktop Entry]
Name=Cursor
Exec=/home/callen/Applications/Cursor.AppImage --no-sandbox %U
Icon=cursor
Type=Application
Categories=Development;IDE;
Comment=AI-powered code editor
MimeType=text/plain;
StartupWMClass=Cursor
EOF

# Download an icon
mkdir -p ~/.local/share/icons
wget -O ~/.local/share/icons/cursor.png "https://www.cursor.com/brand/icon.svg" 2>/dev/null || \
wget -O ~/.local/share/icons/cursor.png "https://raw.githubusercontent.com/getcursor/cursor/main/resources/app/resources/linux/code.png" 2>/dev/null

# Refresh desktop database
update-desktop-database ~/.local/share/applications/

cat << 'EOF' | sudo tee /usr/local/bin/cursor
#!/bin/bash
nohup ~/Applications/Cursor.AppImage --no-sandbox "$@" > /dev/null 2>&1 &
disown
EOF

sudo chmod +x /usr/local/bin/cursor
