# Dotfiles

## Arch Linux and derivatives (including CachyOS)

Clone using SSH and preview the installer:

```sh
git clone git@github.com:bitemyapp/dotfiles.git ~/work/dotfiles
cd ~/work/dotfiles
bash .install/install.sh --dry-run --aur
```

Run the installer as your normal user. It uses sudo only for native package
installation. With no options it installs missing development tools, Rust,
fonts, fish, zsh, Starship, Zed, ChatGPT Desktop, and Claude Desktop, then adds
shell configuration hooks. It keeps your current login shell. If a missing
desktop app is outside your enabled repositories, use `--aur` to install it
through yay or paru. To select fish or zsh explicitly:

```sh
bash .install/install.sh --shell fish --aur
# Or:
bash .install/install.sh --shell zsh --aur
```

Packages already installed, including AUR packages and installed providers,
are skipped even if a newer version is available. Existing CLI executables
are also kept, including tools under `~/.local/bin` and `~/.cargo/bin`.
Rustup installations satisfy the Rust dependency; a new installation uses
Arch's packaged Rust instead of downloading another toolchain.

The installer uses `pacman -S --needed` for missing packages. It does not
refresh package databases or run a system upgrade. Maintain a fully updated
Arch system before installation; if your mirrors/database are stale, run
your usual full `sudo pacman -Syu` separately. Package dependencies are still
resolved by pacman; review its transaction before accepting it. See the
[pacman manual](https://man.archlinux.org/man/pacman.8.en).

Zed, ChatGPT Desktop, and Claude Desktop are included by default; existing
installations are kept. Other desktop applications such as Ghostty and Chrome
are opt-in. Application configuration, desktop entries, Docker services/groups,
and compositor settings are preserved. The `claude` component refers to
**Claude Code**, the CLI; `claude-desktop` refers to the desktop application.

Install an individual component using the main entry point or its existing
script. Missing packages in enabled repositories use pacman; missing packages
outside those repositories require `--aur` and an existing yay or paru.
The AUR helper runs as your normal user with its normal review prompts.
Existing applications are skipped regardless of `--aur`.

```sh
bash .install/install.sh --component docker --component github
bash .install/ghostty.sh --dry-run
bash .install/google-chrome.sh --aur
bash .install/install.sh --component spotify --aur
bash .install/zed.sh --dry-run
bash .install/chatgpt-desktop.sh --aur
bash .install/claude-desktop.sh --aur
```

Use `bash .install/install.sh --help` for all components and options. Linux
component scripts automatically dispatch to the Arch installer; they do not
run their Debian downloads or repositories on Arch. The historical Selenium
script stops with an explanation on Arch instead of overwriting ChromeDriver.
The older Debian and macOS paths remain available on their respective systems.

## Shell configuration

Both shells use the same `starship.toml`. Starship is initialized interactively
using its [documented shell integrations](https://github.com/starship/starship#-installation).
The installer links only `starship.toml` and the two small files under
`.config/dotfiles/`, keeping existing files and symlinks at those destinations.
It adds one guarded source hook to the **end** of existing `.zshrc` and fish's
`config.fish`, keeping distro setup and user settings. That placement lets
Starship replace CachyOS's existing prompt. Powerlevel10k's prompt hooks are
disabled when Starship is available. Existing `STARSHIP_CONFIG` settings take
precedence.

`XDG_CONFIG_HOME` and `ZDOTDIR` are respected. The installer never edits the
target of an existing startup-file symlink: it prints the hook to add manually
unless the hook is already present. Configurations managed by another tool
can therefore need that manual step. New links follow edits in this checkout;
keep the checkout at its installed location.

PATH setup avoids duplicate entries and includes existing user tool directories.
Startup works without Rust or Starship installed. Fish reads optional
`~/.secrets.fish`; zsh reads optional `~/.secrets`. Fish does not source the
repository's Bash/zsh `.profile` or Cargo's POSIX `env` file. The installer
does not overwrite Ghostty configuration or force a terminal type. Open a new
terminal after installing. Login-shell changes take effect on your next login;
launch `fish` or `zsh` directly to try either shell immediately.

## Validation

```sh
python3 -m unittest discover -s .install/tests -v
```

The tests use temporary homes and mocked package operations. They check repeat
runs, shell hooks, custom config directories, preserved applications/configs,
dry runs, package failures, and opt-in AUR installation without touching the
host package database or login shell.
