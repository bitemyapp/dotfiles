#!/usr/bin/env bash

# Use native packages and preserve existing installs on Arch derivatives.
source "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/platform.sh"
dotfiles_dispatch_arch --component cuda "$@"

sudo apt install nvidia-cuda-dev nvidia-cuda-toolkit nvidia-cudnn libnccl-dev libnccl2
