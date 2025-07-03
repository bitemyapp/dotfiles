#!/usr/bin/env bash

sudo apt install -y gcc clang libudev-dev libgbm-dev libxkbcommon-dev libegl1-mesa-dev libwayland-dev libinput-dev libdbus-1-dev libsystemd-dev libseat-dev libpipewire-0.3-dev libpango1.0-dev libdisplay-info-dev
sudo apt install -y fuzzel alacritty ptyxis swaylock

WORK_DIR=$HOME/work/third-party
mkdir -p $WORK_DIR
# Clone only if it doesn't already exist
if [ -d "$WORK_DIR/niri" ]; then
    echo "Niri repository already exists. Skipping clone."
else
    echo "Cloning Niri repository..."
    cd $WORK_DIR && git clone git@github.com:YaLTeR/niri.git
fi

cd $WORK_DIR/niri && cargo build --release

cd $WORK_DIR/niri && sudo cp target/release/niri /usr/bin/
cd $WORK_DIR/niri && sudo cp resources/niri-session /usr/bin/
cd $WORK_DIR/niri && sudo cp resources/niri.desktop /usr/share/wayland-sessions/
cd $WORK_DIR/niri && sudo cp resources/niri-portals.conf /usr/share/xdg-desktop-portal/
cd $WORK_DIR/niri && sudo cp resources/niri.service /etc/systemd/user/
cd $WORK_DIR/niri && sudo cp resources/niri-shutdown.target /etc/systemd/user/
# Add --ozone-platform=wayland to your Chrome's command line options
# /usr/share/applications/google-chrome.desktop
# Before: Exec=/usr/bin/google-chrome-stable %U
# After: Exec=/usr/bin/google-chrome-stable --ozone-platform=wayland %U
# Check if the file has already been modified
# if grep -q 'ozone-platform=wayland' /usr/share/applications/google-chrome.desktop; then
#     echo "Google Chrome desktop entry already modified."
# else
#     echo "Modifying Google Chrome desktop entry..."
#     sudo sed -i 's|Exec=/usr/bin/google-chrome-stable %U|Exec=/usr/bin/google-chrome-stable --ozone-platform=wayland %U|' /usr/share/applications/google-chrome.desktop
# fi
