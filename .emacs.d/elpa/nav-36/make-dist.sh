#!/bin/bash

# basenames of distribution files
dist_files="nav ack"

dist_dir=emacs-nav-$(grep 'Version:' nav.el | awk '{print $3}')
rm -rf $dist_dir
mkdir -p $dist_dir
for file in $dist_files; do
    emacs -batch -f batch-byte-compile $file.el
    cp $file.el $dist_dir
    mv $file.elc $dist_dir
done
cp ack $dist_dir

tar czvf $dist_dir.tar.gz $dist_dir
rm -rf $dist_dir
