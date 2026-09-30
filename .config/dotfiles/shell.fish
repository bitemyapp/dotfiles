# Source from the end of config.fish, after any distribution configuration.
# fish cannot source POSIX shell files such as ~/.profile or ~/.cargo/env.
fish_add_path --path "$HOME/.local/bin" "$HOME/.cargo/bin" "$HOME/.bin" "$HOME/bin"
fish_add_path --path --append "$HOME/.npm-global/bin" "$HOME/.opencode/bin" \
    "$HOME/.local/share/JetBrains/Toolbox/scripts"

set -q CARGO_HOME; or set -gx CARGO_HOME "$HOME/.cargo"
set -q GOPATH; or set -gx GOPATH "$HOME/.local/goworkspace"
set -q PYENV_ROOT; or set -gx PYENV_ROOT "$HOME/.pyenv"
fish_add_path --path "$PYENV_ROOT/bin" "$CARGO_HOME/bin"

if status is-interactive
    if not set -q EDITOR; and type -q emacs
        set -gx EDITOR 'emacs -q -nw'
    end
    if set -q EDITOR; and not set -q GIT_EDITOR
        set -gx GIT_EDITOR "$EDITOR"
    end
    if isatty stdin
        set -gx GPG_TTY (tty)
    end
    if test -r "$HOME/.secrets.fish"
        source "$HOME/.secrets.fish"
    end
    if not set -q STARSHIP_CONFIG
        set -l config_dir "$HOME/.config"
        if set -q XDG_CONFIG_HOME
            set config_dir "$XDG_CONFIG_HOME"
        end
        if test -r "$config_dir/starship.toml"
            set -gx STARSHIP_CONFIG "$config_dir/starship.toml"
        end
    end
    if type -q starship; and not set -q __dotfiles_starship_initialized
        starship init fish | source
        set -g __dotfiles_starship_initialized 1
    end
end
