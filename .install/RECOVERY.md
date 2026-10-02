# Restore the Arch / CachyOS desktop

These instructions restore the changes made to an existing CachyOS Plasma
desktop. Start from a working Plasma Wayland installation and log in as your
normal desktop user. Package installation uses the enabled distribution
repositories and the existing AUR helper; existing applications are preserved.

## Get this branch

```sh
git clone --branch arch-idempotent-shells git@github.com:bitemyapp/dotfiles.git ~/work/dotfiles
cd ~/work/dotfiles
```

If a new installation has no SSH key registered with GitHub yet, use
`https://github.com/bitemyapp/dotfiles.git` instead. SSH private keys and
account credentials are not in these repositories. Restore those from your
own secure backup or generate and register new keys.

## Applications and shells

Update the system with the usual full `sudo pacman -Syu` before installing
missing packages. Preview the exact selected components:

```sh
bash .install/install.sh --dry-run --aur --shell fish \
  --component core --component rust --component fonts --component shells \
  --component zed --component chatgpt-desktop --component claude-desktop \
  --component ghostty --component google-chrome --component telegram \
  --component plasma
```

Remove `--dry-run` to apply. Use `--shell zsh` for zsh, or `--shell keep`
to preserve your login shell; both shells receive Starship configuration.
Install yay or paru separately if missing applications require AUR packages.
Telegram uses the enabled CachyOS repository on CachyOS, rather than a new
third-party download. Package versions follow the current repositories.

The installer preserves installed applications, including existing AUR copies,
and app settings. Existing shell configuration is retained and receives a
guarded hook. Existing Starship configuration is also preserved. Read the
main README for conflict behavior and individual component entry points.

## Panel, clipboard, SSH alias and wallpapers

```sh
python3 .install/restore-desktop.py --all --dry-run
python3 .install/restore-desktop.py --all
```

Use `--component ssh`, `--component plasma` or `--component wallpapers` to
apply only that portion. No application packages are installed by this tool.

The Plasma restore uses the [desktop scripting API](https://develop.kde.org/docs/plasma/scripting/api/)
to reuse a bottom panel, retain existing widgets and launchers, and add missing
ones. It restores a 36px floating panel with adaptive opacity, a battery
percentage, Bluetooth control, notifications and the clipboard widget on the
right. App launchers are added only when their desktop entries exist. If
multiple bottom panels exist, choose one with `--panel-id NUMBER`.

Clipboard history preferences retain images, retain clipboard contents,
ignore primary-selection changes and allow 100 entries. Only those four
preferences are merged into `klipperrc`; no clipboard contents are imported,
read or exported. Those preferences take effect on the next login. The tool
does not restart Plasma or KWin.

Before changing configuration, the tool stores private, local backups under
`${XDG_STATE_HOME:-~/.local/state}/dotfiles/backups`. It refuses file symlinks,
conflicting SSH fragments and conflicting wallpaper packages rather than
overwriting them. The SSH alias routes `gitlab.com-bitemyapp` to `gitlab.com`
using your existing `~/.ssh/id_ed25519`; it neither generates nor uploads a key.
After registering that key with GitLab, recover the submodules with:

```sh
git clone --recursive git@gitlab.com:bitemyapp/omperor.git ~/work/omperor
# Or, inside an existing partial clone:
git submodule update --init --recursive
```

The committed wallpaper manifest records all 92 original Omarchy v4.0.4
images, dimensions, byte sizes, Git blob hashes and SHA-256 hashes. The restore
downloads the originals from their public source, verifies both hashes, and
creates KDE wallpaper packages under your XDG data directory. They become
available when you reopen KDE's wallpaper selector. Images remain full
resolution; your current wallpaper is not changed. Repeated runs verify and
keep installed images. The downloads total approximately 107 MiB. Image
rights remain with their original creators; this repository contains the
manifest and installer, not copies of the images.

## OCR-free Spectacle and Alt+Shift+4

The [Spectacle fork](https://github.com/bitemyapp/spectacle/tree/fast-region)
contains both Arch package recipes, the service and safe native shortcut setup:

```sh
git clone --branch fast-region git@github.com:bitemyapp/spectacle.git ~/work/spectacle
cd ~/work/spectacle
makepkg -si
spectacle-fast-setup
```

The root PKGBUILD installs `spectacle-snappy-git`, replacing stock Spectacle
and including the resident selector. Press physical Alt+Shift+4, select a
region and release to copy it to the clipboard. KDE represents this key as
Alt+$ on a US layout. The regular Spectacle GUI remains available with OCR
removed. The alternative recipe under `packaging/fast/` installs only the
selector and leaves stock Spectacle installed. Choose one recipe.

The last tested source revision before recovery documentation was added is
`dbebba6a662fa487a6f7171ddbbac83fabb9fb24`, based on Plasma 6.7.5.
`makepkg` normally fetches the current `fast-region` branch even when the
checkout is older. For an exact source rebuild, edit the chosen PKGBUILD's
source fragment from `#branch=fast-region` to
`#commit=dbebba6a662fa487a6f7171ddbbac83fabb9fb24` before building. That
revision still needs compatible current build dependencies; a pinned source
revision is not a snapshot of the entire distribution.

These are VCS packages: `pacman -Syu` does not fetch GitHub changes or rebuild
them. Rebuild and test when KDE/Qt dependency changes require it; avoid
indefinitely holding KDE libraries back with `IgnorePkg`. The differently
named full fork provides/conflicts with `spectacle`, so an ordinary update
of stock Spectacle does not silently replace the installed fork.

These commands target a fresh installation. On this original workstation,
the experimental build remains under `~/.local`. Before migrating that
machine to package ownership, back up/remove its old binaries, desktop/D-Bus
entries and user service files, and update its service paths; otherwise the
old local installation can keep taking precedence. Do not blindly delete
all of `~/.local` or replace the complete shortcut configuration.

## Upstream shortcut crash fix

The current-master KGlobalAccelD fix, including upstream regression tests,
is preserved as a patch under
[the Spectacle fork's upstream archive](https://github.com/bitemyapp/spectacle/tree/fast-region/tools/upstream).
The branch is also published on
[KDE Invent](https://invent.kde.org/theodorvaryag/kglobalacceld/-/tree/fix/desktop-action-registration).
Neither the restore tool nor the Spectacle package patches the system's
KWin/KGlobalAccelD. See the archive's instructions to recreate the upstream
branch for development or submission.

Power-consumption and swap/zram investigations were diagnostics, not custom
kernel or power-management packages to reinstall. Full OS diagnostics,
crash dumps, application sessions, home-directory snapshots, SSH keys and
clipboard history are intentionally absent from this recovery profile.
