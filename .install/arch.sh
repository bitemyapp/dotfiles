#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_dir=$(dirname -- "$script_dir")
source "$script_dir/platform.sh"

usage() {
  cat <<'EOF'
Usage: .install/install.sh [--dry-run] [--shell keep|fish|zsh] [--aur]
                           [--component NAME ...]

With no components, install core tools, Rust, fonts, shell configuration,
Zed, ChatGPT Desktop, and Claude Desktop.
Both fish and zsh are supported; keep the current login shell by default.
Existing packages, executables, and desktop app configuration are preserved.

Components: core, rust, fonts, shells, docker, ghostty, google-chrome,
            spotify, telegram, signal, cursor, claude, codex, github,
            node, opencode, niri, vscode, vscodium, slack, cuda, xsecurelock,
            zed, chatgpt-desktop, claude-desktop, plasma
--aur permits yay/paru to install missing packages absent from the repositories.
--dry-run prints changes without installing packages, editing files, or chsh.
EOF
}

dry_run=false
allow_aur=false
configure_only=false
target_shell=keep
components=()
while (( $# )); do
  case $1 in
    --dry-run) dry_run=true; shift ;;
    --aur) allow_aur=true; shift ;;
    --configure-only) configure_only=true; shift ;;
    --shell|--component)
      if (( $# < 2 )); then echo "Missing value for $1" >&2; exit 2; fi
      if [[ $1 == --shell ]]; then target_shell=$2; else components+=("$2"); fi
      shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done
case $target_shell in keep|fish|zsh) ;; *) echo "Invalid shell: $target_shell" >&2; exit 2 ;; esac
if (( ${#components[@]} == 0 )); then
  components=(core rust fonts shells zed chatgpt-desktop claude-desktop)
fi
if $configure_only; then
  if [[ ${components[*]} != spotify || $target_shell != keep ]]; then
    echo "--configure-only is supported only by the Spotify entry point." >&2
    exit 2
  fi
  echo "Arch uses package-managed Spotify; no APT source configuration is needed."
  exit 0
fi

dotfiles_is_arch || { echo "This installer requires Arch Linux or an Arch derivative." >&2; exit 1; }
command -v pacman >/dev/null || { echo "pacman is required." >&2; exit 1; }
if (( EUID == 0 )); then echo "Run as your normal user; package installation uses sudo." >&2; exit 1; fi

# Find user-installed CLI tools too, without installing a competing copy.
export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
config_dir=${XDG_CONFIG_HOME:-$HOME/.config}
zsh_dir=${ZDOTDIR:-$HOME}
specs=()
configure_shells=false
for component in "${components[@]}"; do
  case $component in
    core)
      specs+=(git:git fish:fish zsh:zsh starship:starship base-devel
        clang:clang cmake:cmake curl:curl unzip:unzip ca-certificates
        openssl pkgconf:pkg-config gnupg:gpg procps-ng:ps
        mosh:mosh tmux:tmux screen:screen htop:htop btop:btop
        colordiff:colordiff emacs:emacs ripgrep:rg fd:fd tokei:tokei
        just:just rink:rink difftastic:difft mergiraf:mergiraf) ;;
    rust) specs+=(rust:rustc) ;;
    fonts) specs+=(fontconfig:fc-cache ttf-roboto ttf-anonymous-pro ttf-firacode-nerd) ;;
    shells) specs+=(fish:fish zsh:zsh starship:starship); configure_shells=true ;;
    docker) specs+=(docker:docker docker-buildx docker-compose:docker-compose) ;;
    ghostty) specs+=(ghostty:ghostty) ;;
    google-chrome) specs+=(google-chrome:google-chrome) ;;
    spotify) specs+=(spotify:spotify) ;;
    telegram) specs+=(telegram-desktop:telegram-desktop) ;;
    signal) specs+=(signal-desktop:signal-desktop) ;;
    cursor) specs+=(cursor-bin:cursor) ;;
    claude) specs+=(claude-code:claude) ;;
    claude-desktop) specs+=(claude-desktop:claude-desktop) ;;
    chatgpt-desktop) specs+=(chatgpt-desktop:chatgpt) ;;
    zed) specs+=(zed:zeditor,zed) ;;
    plasma) specs+=(plasma-desktop plasma-workspace bluedevil powerdevil qt6-tools:qdbus6 python:python3) ;;
    codex) specs+=(openai-codex-bin:codex) ;;
    github) specs+=(github-cli:gh) ;;
    node) specs+=(nodejs:node npm:npm) ;;
    opencode) specs+=(opencode:opencode) ;;
    niri) specs+=(niri:niri fuzzel:fuzzel swaylock:swaylock) ;;
    vscode) specs+=(visual-studio-code-bin:code) ;;
    vscodium) specs+=(vscodium-bin:codium) ;;
    slack) specs+=(slack-desktop:slack) ;;
    cuda) specs+=(cuda:nvcc cudnn nccl) ;;
    xsecurelock) specs+=(xsecurelock:xsecurelock) ;;
    selenium)
      echo "The legacy Selenium installer is Debian-only. Manage Selenium/ChromeDriver separately on Arch." >&2
      exit 2 ;;
    *) echo "Unknown component: $component" >&2; exit 2 ;;
  esac
done
if [[ $target_shell != keep ]]; then
  specs+=(fish:fish zsh:zsh starship:starship)
  configure_shells=true
fi

log() { printf '%s\n' "$*"; }
existing_command() {
  local candidate
  local -a candidates
  IFS=, read -r -a candidates <<< "$1"
  for candidate in "${candidates[@]}"; do
    if command -v "$candidate" >/dev/null 2>&1; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done
  return 1
}
run() {
  printf '+'
  printf ' %q' "$@"
  printf '\n'
  if ! $dry_run; then "$@"; fi
}

# pacman -T recognizes installed providers (e.g. rustup supplies rust).
# Check every target before starting a transaction. Never refresh databases
# independently of a full system upgrade, and never upgrade installed targets.
repo_packages=()
aur_packages=()
declare -A seen=()
for spec in "${specs[@]}"; do
  package=${spec%%:*}
  executable=
  [[ $spec != *:* ]] || executable=${spec#*:}
  [[ -z ${seen[$package]:-} ]] || continue
  seen[$package]=1
  if pacman -T "$package" >/dev/null 2>&1; then
    log "Keeping installed package/provider: $package"
  elif [[ -n $executable ]] && installed_command=$(existing_command "$executable"); then
    log "Keeping existing executable: $installed_command"
  elif pacman -Si "$package" >/dev/null 2>&1; then
    repo_packages+=("$package")
  else
    aur_packages+=("$package")
  fi
done

aur_helper=
if (( ${#aur_packages[@]} )); then
  if ! $allow_aur; then
    echo "Packages absent from enabled repositories: ${aur_packages[*]}" >&2
    echo "Use --aur to permit installation through an existing yay/paru helper." >&2
    exit 1
  fi
  for helper in yay paru; do
    if command -v "$helper" >/dev/null 2>&1; then aur_helper=$helper; break; fi
  done
  [[ -n $aur_helper ]] || { echo "Install yay or paru first to use --aur." >&2; exit 1; }
fi
if (( ${#repo_packages[@]} )); then run sudo pacman -S --needed -- "${repo_packages[@]}"; fi
if (( ${#aur_packages[@]} )); then run "$aur_helper" -S --needed -- "${aur_packages[@]}"; fi

link_config() {
  local source=$1 destination=$2
  if [[ -L $destination && $(readlink -- "$destination") == "$source" ]]; then
    log "Already linked: $destination"
  elif [[ -e $destination || -L $destination ]]; then
    log "Keeping existing configuration: $destination"
  else
    run mkdir -p -- "$(dirname -- "$destination")"
    run ln -s -- "$source" "$destination"
  fi
}

append_hook() {
  local file=$1 hook=$2
  # A multiline grep pattern matches ANY line, including a common 'end'.
  # Use the distinctive first line to detect an already-installed hook.
  local hook_start=${hook%%$'\n'*}
  if [[ -L $file ]]; then
    # Never edit the target of someone else's symlink (possibly under /usr).
    if grep -qF -- "$hook_start" "$file" 2>/dev/null; then
      log "Shell hook already present: $file"
    else
      log "Keeping symlink: $file; add this hook at the end of its configuration:"
      log "$hook"
    fi
  elif [[ -e $file && ! -f $file ]]; then
    echo "Cannot add a shell hook to a non-file: $file" >&2
    return 1
  elif grep -qF -- "$hook_start" "$file" 2>/dev/null; then
    log "Shell hook already present: $file"
  else
    log "Append shell hook: $file"
    if ! $dry_run; then
      mkdir -p -- "$(dirname -- "$file")"
      printf '\n# dotfiles: shared shell environment and Starship\n%s\n' "$hook" >> "$file"
    fi
  fi
}

if $configure_shells; then
  link_config "$repo_dir/.config/starship.toml" "$config_dir/starship.toml"
  link_config "$repo_dir/.config/dotfiles/shell.zsh" "$config_dir/dotfiles/shell.zsh"
  link_config "$repo_dir/.config/dotfiles/shell.fish" "$config_dir/dotfiles/shell.fish"
  append_hook "$zsh_dir/.zshrc" '[[ ! -r "${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/shell.zsh" ]] || source "${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/shell.zsh"'
  append_hook "$config_dir/fish/config.fish" 'if test -r "$__fish_config_dir/../dotfiles/shell.fish"
    source "$__fish_config_dir/../dotfiles/shell.fish"
end'
fi

if [[ $target_shell != keep ]]; then
  shell_path=$(command -v "$target_shell" || true)
  if $dry_run && [[ -z $shell_path ]]; then
    log "Would set login shell to $target_shell after installation."
  else
    [[ -n $shell_path ]] || { echo "Cannot find $target_shell after installation." >&2; exit 1; }
    shell_path=$(readlink -f -- "$shell_path")
    registered_shell=
    while IFS= read -r candidate; do
      [[ $candidate == /* ]] || continue
      if [[ $(readlink -f -- "$candidate") == "$shell_path" ]]; then registered_shell=$candidate; break; fi
    done < /etc/shells
    [[ -n $registered_shell ]] || { echo "$shell_path is not registered in /etc/shells." >&2; exit 1; }
    current_shell=$(getent passwd "$(id -un)" | cut -d: -f7)
    if [[ $(readlink -f -- "$current_shell") == "$shell_path" ]]; then
      log "Login shell already uses $target_shell."
    else
      run chsh -s "$registered_shell"
    fi
  fi
fi

if $dry_run; then
  log "Dry run complete; no changes made."
else
  log "Arch setup complete. Start a new fish or zsh session to load shell changes."
fi
