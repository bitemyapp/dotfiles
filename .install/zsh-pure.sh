#!/usr/bin/env bash

mkdir -p "$HOME/.zsh"

if test -d $HOME/.zsh/pure; then
    exit 0
fi

git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"
