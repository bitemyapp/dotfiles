#!/bin/bash

rsync -avz  ~/code/hippocampus prod@wiki.hungrytribe.com:/home/prod/
ssh prod@wiki.hungrytribe.com "cd ~/hippocampus; pkill -f gollum; sleep 5; nohup ~/hippocampus/bin/gollum --page-file-dir pages 2>&1 > /dev/null &"
