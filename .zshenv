typeset -U path PATH
path=(~/bin ~/play $path)
export PYENV_ROOT="$HOME/.pyenv"
[[ ! -r "$HOME/.cargo/env" ]] || source "$HOME/.cargo/env"

[[ ! -d "$HOME/.foundry/bin" ]] || path+=("$HOME/.foundry/bin")
