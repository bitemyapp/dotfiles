#!/usr/bin/env bash
set -euo pipefail

tmp_file="$(mktemp /tmp/gnome-screenshot-XXXXXX.png)"

cleanup() {
  rm -f "$tmp_file"
}
trap cleanup EXIT

session_type="${XDG_SESSION_TYPE:-}"
is_wayland=false
if [ "$session_type" = "wayland" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
  is_wayland=true
fi

if $is_wayland; then
  if command -v slurp >/dev/null 2>&1 && command -v grim >/dev/null 2>&1; then
    geometry="$(slurp)" || exit 1
    if [ -z "$geometry" ]; then
      exit 1
    fi
    grim -g "$geometry" "$tmp_file"
  elif command -v gnome-screenshot >/dev/null 2>&1; then
    gnome-screenshot -a -f "$tmp_file"
  else
    echo "Wayland session detected, but grim/slurp or gnome-screenshot not available." >&2
    exit 1
  fi
else
  if command -v gnome-screenshot >/dev/null 2>&1; then
    gnome-screenshot -a -f "$tmp_file"
  else
    echo "gnome-screenshot not found in PATH." >&2
    exit 1
  fi
fi

for _ in {1..20}; do
  if [ -s "$tmp_file" ]; then
    break
  fi
  sleep 0.05
done

if [ ! -s "$tmp_file" ]; then
  exit 1
fi

if $is_wayland; then
  if command -v wl-copy >/dev/null 2>&1; then
    wl-copy --type image/png < "$tmp_file"
    exit 0
  fi
fi

if command -v xclip >/dev/null 2>&1; then
  xclip -selection clipboard -t image/png -i < "$tmp_file"
elif command -v xsel >/dev/null 2>&1; then
  xsel --clipboard --input --mime-type image/png < "$tmp_file"
elif command -v wl-copy >/dev/null 2>&1; then
  wl-copy --type image/png < "$tmp_file"
else
  echo "No clipboard tool found (wl-copy, xclip, or xsel)." >&2
  exit 1
fi
