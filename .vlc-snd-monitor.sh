#!/bin/bash

# vlc \
#     --no-video-deco \
#     --no-embedded-video \
#     --screen-fps=30 \
#     --screen-top=380 \
#     --screen-left=1920 \
#     --screen-width=1920 \
#     --screen-height=1080 \
#     screen://

# ffmpeg -video_size 3840x2160 -framerate 25 -f x11grab -i :1.0+3840,0 -v 0 -vcodec mpeg4 -f mpegts udp://127.0.0.1:23000
# ffplay -probesize 32 udp://127.0.0.1:23000

cvlc --no-video-deco \
     --no-embedded-video screen:// \
     --no-video-deco \
     --no-embedded-video \
     --fps=30 \
     --width=3840 \
     --height=2160 \
     --video-x=3840 \
     --video-y=0
     # --screen-top=0 \
     # --screen-left=3840 \
     # --screen-width=3840 \
     # --screen-height=2160 \

# cvlc --no-video-deco --no-embedded-video screen://
#     --no-video-deco \
#     --no-embedded-video \
#     --screen-fps=30 \
#     --screen-top=0 \
#     --screen-left=1920 \
#     --screen-width=1920 \
#     --screen-height=1080 \

# Include top/bottom
# vlc \
#     --no-video-deco \
#     --no-embedded-video \
#     --screen-fps=20 \
#     --screen-top=24 \
#     --screen-left=1920 \
#     --screen-width=1920 \
#     --screen-height=1080 \
#     screen://
