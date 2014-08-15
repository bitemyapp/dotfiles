#!/bin/bash

sudo chmod 777 /sys/class/leds/smc::kbd_backlight/brightness
echo 0 | tee -a /sys/class/leds/smc::kbd_backlight/brightness
