#!/bin/bash

mkdir ~/Screenshots

defaults write com.apple.screencapture ~/Screenshots

killall SystemUIServer
