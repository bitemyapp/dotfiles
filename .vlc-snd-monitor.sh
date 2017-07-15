#!/bin/bash

vlc \
    --no-video-deco \
    --no-embedded-video \
    --screen-fps=20 \
    --screen-top=24 \
    --screen-left=1920 \
    --screen-width=1920 \
    --screen-height=1080 \
    screen://

# Include top/bottom
# vlc \
#     --no-video-deco \
#     --no-embedded-video \
#     --screen-fps=20 \
#     --screen-top=0 \
#     --screen-left=1920 \
#     --screen-width=1920 \
#     --screen-height=1080 \
#     screen://
