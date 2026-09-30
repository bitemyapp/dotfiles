#!/usr/bin/env bash

# Shared by the legacy entry points. Keep Debian/macOS behavior in those files.
dotfiles_is_arch() {
  local ID= ID_LIKE=
  [[ -r ${DOTFILES_OS_RELEASE:-/etc/os-release} ]] || return 1
  # shellcheck disable=SC1090
  source "${DOTFILES_OS_RELEASE:-/etc/os-release}"
  [[ $ID == arch || " $ID_LIKE " == *" arch "* ]]
}

dotfiles_dispatch_arch() {
  if dotfiles_is_arch; then
    local install_dir
    install_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
    exec bash "$install_dir/arch.sh" "$@"
  fi
}
