# Source from the end of .zshrc, after any distribution configuration.
typeset -U path PATH
for dotfiles_bin in "$HOME/.local/bin" "$HOME/.cargo/bin" "$HOME/.bin" "$HOME/bin"; do
  [[ ! -d $dotfiles_bin ]] || path=("$dotfiles_bin" $path)
done
for dotfiles_bin in "$HOME/.npm-global/bin" "$HOME/.opencode/bin" "$HOME/.local/share/JetBrains/Toolbox/scripts"; do
  [[ ! -d $dotfiles_bin ]] || path+=("$dotfiles_bin")
done
unset dotfiles_bin
export CARGO_HOME=${CARGO_HOME:-$HOME/.cargo}
export GOPATH=${GOPATH:-$HOME/.local/goworkspace}
export PYENV_ROOT=${PYENV_ROOT:-$HOME/.pyenv}
[[ ! -d $PYENV_ROOT/bin ]] || path=("$PYENV_ROOT/bin" $path)
[[ ! -d $CARGO_HOME/bin ]] || path=("$CARGO_HOME/bin" $path)

if [[ -o interactive ]]; then
  if [[ -z ${EDITOR:-} ]] && (( $+commands[emacs] )); then export EDITOR='emacs -q -nw'; fi
  [[ -z ${EDITOR:-} ]] || export GIT_EDITOR=${GIT_EDITOR:-$EDITOR}
  [[ ! -t 0 ]] || export GPG_TTY=$(tty)
  [[ ! -r $HOME/.secrets ]] || source "$HOME/.secrets"
  if [[ -z ${STARSHIP_CONFIG:-} && -r ${XDG_CONFIG_HOME:-$HOME/.config}/starship.toml ]]; then
    export STARSHIP_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/starship.toml"
  fi
  if (( $+commands[starship] )) && [[ -z ${_dotfiles_starship_initialized:-} ]]; then
    # CachyOS loads Powerlevel10k before this hook; disable its prompt hooks.
    if (( $+functions[prompt_powerlevel9k_teardown] )); then
      prompt_powerlevel9k_teardown
    fi
    eval "$(starship init zsh)"
    typeset -g _dotfiles_starship_initialized=1
  fi
fi
