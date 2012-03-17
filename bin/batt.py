#!/usr/bin/env python
# coding=UTF-8

import math, subprocess, sys

if sys.version.find("Apple") != 0:
    p = subprocess.Popen(["ioreg", "-rc", "AppleSmartBattery"], stdout=subprocess.PIPE)
else:
    p = subprocess.Popen(["acpi",], stdout=subprocess.PIPE)

output = p.communicate()[0]

# import ipdb; ipdb.set_trace()

charge_threshold = int(math.ceil(float(output[output.index("%") - 3:output.index("%")].strip(',').strip()) / 10))

# Output

total_slots, slots = 10, []
filled = int(math.ceil(charge_threshold * (total_slots / 10.0))) * u'▸'
empty = (total_slots - len(filled)) * u'▹'

out = (filled + empty).encode('utf-8')
import sys

color_green = '%{[32m%}'
color_yellow = '%{[1;33m%}'
color_red = '%{[31m%}'
color_reset = '%{[00m%}'
color_out = (
    color_green if len(filled) > 6
    else color_yellow if len(filled) > 4
    else color_red
)

out = color_out + out + color_reset
sys.stdout.write(out)
