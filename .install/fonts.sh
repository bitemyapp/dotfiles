#!/usr/bin/env bash

font_url='https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip'; font_name=${font_url##*/}; wget ${font_url} -O ${font_name} && unzip ${font_name} -d ~/.fonts && fc-cache -fv && cd ~
