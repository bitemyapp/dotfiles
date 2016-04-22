# This is a sample config for Kwm

# Commands prefixed with 'kwmc' will call
# local functions corresponding to the
# kwmc syntax.

# To run a system command, use the prefix
# 'sys' and then the requested command
# e.g 'sys mvim' would open macvim

# Read file and execute commands as
# if they were written inside kwmrc.

# include filename

# e.g: bind-commands in a separate file called 'binds'
# include binds

# Create variables that can be used in the below commands.
# Variables are in file-scope and must be defined before
# they can be used. Re-definition is also possible.
# Variable names can not contain whitespace, where as
# the value can.

# define variable value

# e.g: create a variable for the hyper-key
# define hyper cmd+ctrl+alt+shift
# kwmc bind hyper-h window -f west

# Set default values for screen padding
kwmc config padding 40 20 20 20

# Set default values for container gaps
kwmc config gap 15 15

# Default tiling mode for Kwm (bsp | monocle | float | off)
kwmc config tiling bsp

# Let Kwm listen for hotkeys
kwmc config hotkeys on

# Set prefix for Kwms hotkeys
kwmc config prefix-key ctrl-s

# Prefix is not applied globally
kwmc config prefix-global off

# Time in seconds, before prefix must be re-activated
kwmc config prefix-timeout 0.75

# Automatically float windows that fail to resize
kwmc config float-non-resizable on

# Automatically resize the window to its container
kwmc config lock-to-container on

# Set focus-follows-mouse-mode to autoraise
kwmc config focus-follows-mouse autoraise

# Focus-follows-mouse is temporarily disabled when
# a floating window has focus
kwmc config standby-on-float on

# The mouse will automatically move to the center
# of the focused window
kwmc config mouse-follows-focus on

# Allow window focus to wrap-around
kwmc config cycle-focus screen

# Override default settings for space 1 on screen 0
# kwmc config space 0 1 mode monocle
# kwmc config space 0 1 padding 100 100 100 100
# kwmc config space 0 1 gap 40 40

# Override default settings for screen 1
# kwmc config display 1 mode bsp
# kwmc config display 1 padding 100 100 100 100
# kwmc config display 1 gap 40 40

# Set default container split-ratio
kwmc config split-ratio 0.5

# New splits become the left leaf-node
kwmc config spawn left

# Add custom tiling rules for applications that
# does not get tiled by Kwm by default.
# This is because some applications do not have the
# AXWindowRole and AXStandardWindowSubRole
kwmc config add-role AXDialog iTerm2

# The following commands create rules that
# blacklists an application from Kwms tiling
#
# kwmc rule owner="Steam" properties={float="true"}
# kwmc rule owner="Photoshop" properties={float="true"}

# The following command creates a rule that
# captures an application to the given screen,
# if the screen exists.
#
# kwmc rule owner="iTunes" properties={display="1"}

# Enable border for focused window
kwmc config focused-border on
kwmc config focused-border size 2
kwmc config focused-border color FFBDD322
kwmc config focused-border radius 6

# Enable border for marked window
kwmc config marked-border on
kwmc config marked-border size 2
kwmc config marked-border color FFCC5577
kwmc config marked-border radius 6

# Change border when prefix-key is active
kwmc config prefix-border on
kwmc config prefix-border size 2
kwmc config prefix-border color FF458588
kwmc config prefix-border radius 6

########  Default Keybinds ########

# Quit Kwm
kwmc bind cmd+alt+ctrl-q quit

# Launch iTerm2
kwmc bind cmd-return sys open -na /Applications/iTerm2.app

# Set Space Tiling Mode To BSP
kwmc bind cmd+ctrl-a space -t bsp

# Set Space Tiling Mode To Monocle
kwmc bind cmd+ctrl-s space -t monocle

# Set Space Tiling Mode To Floating
kwmc bind cmd+ctrl-d space -t float

# Rotate Window-Tree By 90* (Clockwise)
kwmc bind cmd+ctrl-r tree rotate 90

# Modify Container
kwmc bind prefix-s window -c split-mode toggle
kwmc bind prefix-h window -c reduce 0.05
kwmc bind prefix-l window -c expand 0.05
kwmc bind prefix-< window -c type toggle

# Set Temporary Window Container
kwmc bind prefix-f window -z fullscreen
kwmc bind prefix-d window -z parent
kwmc bind prefix-w window -t focused

# Mark Window
kwmc bind cmd+alt+ctrl-m window -mk focused
kwmc bind cmd+alt+ctrl-h window -mk west wrap
kwmc bind cmd+alt+ctrl-l window -mk east wrap
kwmc bind cmd+alt+ctrl-j window -mk south wrap
kwmc bind cmd+alt+ctrl-k window -mk north wrap

# Give Focus To Window
kwmc bind cmd+alt-h window -f west
kwmc bind cmd+alt-l window -f east
kwmc bind cmd+alt-j window -f south
kwmc bind cmd+alt-k window -f north

kwmc bind cmd+alt-n window -fm prev
kwmc bind cmd+alt-m window -fm next

# Give Focus To Screen
kwmc bind cmd+alt-1 display -f 0
kwmc bind cmd+alt-2 display -f 1
kwmc bind cmd+alt-3 display -f 2

# Swap Focused Window
kwmc bind ctrl+alt-h window -s west
kwmc bind ctrl+alt-j window -s south
kwmc bind ctrl+alt-k window -s north
kwmc bind ctrl+alt-l window -s east
kwmc bind ctrl+alt-m window -s mark

# Pseudo containers
kwmc bind cmd+ctrl+alt-p tree -pseudo create
kwmc bind cmd+ctrl+alt-o tree -pseudo destroy
kwmc bind ctrl+alt-p window -s prev
kwmc bind ctrl+alt-n window -s next

# Detach Focused Window And Reinsert In Direction
kwmc bind ctrl+shift-k window -m north
kwmc bind ctrl+shift-l window -m east
kwmc bind ctrl+shift-j window -m south
kwmc bind ctrl+shift-h window -m west

# Detach Marked Window And Reinsert At Focused Window
kwmc bind ctrl+shift-x window -m mark

# Move Focused Window To Space
kwmc bind ctrl+alt-left window -m space left
kwmc bind ctrl+alt-right window -m space right

# Move Focused Window To Screen
kwmc bind ctrl+alt-1 window -m display 0
kwmc bind ctrl+alt-2 window -m display 1
kwmc bind ctrl+alt-3 window -m display 2

# Increase Container Gaps
kwmc bind prefix-x space -g increase horizontal
kwmc bind prefix-y space -g increase vertical

# Decrease Container Gaps
kwmc bind prefix+shift-x space -g decrease horizontal
kwmc bind prefix+shift-y space -g decrease vertical

# Increase Screen Padding
kwmc bind prefix-left space -p increase left
kwmc bind prefix-right space -p increase right
kwmc bind prefix-up space -p increase top
kwmc bind prefix-down space -p increase bottom
kwmc bind prefix-p space -p increase all

# Decrease Screen Padding
kwmc bind prefix+shift-left space -p decrease left
kwmc bind prefix+shift-right space -p decrease right
kwmc bind prefix+shift-up space -p decrease top
kwmc bind prefix+shift-down space -p decrease bottom
kwmc bind prefix+shift-p space -p decrease all

# Autowrite
kwmc bind cmd-7 write \
kwmc bind cmd-8 write {
kwmc bind cmd-9 write }
