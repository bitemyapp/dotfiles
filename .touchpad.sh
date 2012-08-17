#!/bin/bash
synclient FingerHigh=50
synclient TapButton1=1
synclient TapButton2=2
synclient TapButton3=3
synclient TapAndDragGesture=0
synclient PalmDetect=1

# /etc/X11/xorg.conf.d
# Drop a file with your configs in there called “60-synaptics.conf”. (The xorg driver has some defaults stored in a file beginning with “50″, which is why I used “60″ to override them.)

# Section "InputClass"
#   Identifier "touchpad"
#   Driver "synaptics"
#   MatchIsTouchpad "on"
#   MatchDevicePath "/dev/input/event*"
#   Option "FingerHigh" "50"
#   Option "RTCornerButton" "0"
#   Option "RBCornerButton" "0"
#   Option "MinSpeed" "0.7"
#   Option "MaxSpeed" "1.7"
#   Option "SHMConfig" "on"
#   Option "TapAndDragGesture" "off"
#   Option "PalmDetect" "on"
# EndSection
