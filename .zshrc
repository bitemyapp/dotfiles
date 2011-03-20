 function prompt_grayrest_setup {
     unset PS1 PS2 PS3 PS4 PROMPT RPROMPT
     autoload colors zsh/terminfo
     if [[ "$terminfo[colors]" -ge 8 ]]; then
       colors
     fi
     # Colours accepted as arguments
     local u_col=${1:-'green'}
     local h_col=${2:-'blue'}
     local d_col=${3:-'yellow'}
     local d_col_pre='blue'
     local job_color='yellow'
     local n_tru=${4:-'blue'}
     local n_fal=${5:-'yellow'}
     # looks like: ee1mpf@eepc-tsar16 ~ $
       ps1=(
       # "%{$fg_bold[$u_col]%}%n@"
       # "%{$fg_bold[$h_col]%}%m "
       "%1(j.%{$fg_bold[$job_color]%}.)"
       "%4(j.%{$reset_color$fg[$job_color]%}[%{$reset_color$fg_bold[$job_color]%}%j%{$reset_color$fg[$job_color]%}].%3(j.||| .%2(j.|| .%1(j.| .))))"
       "%{$reset_color%}"
       "%{%(?.$fg[$n_tru].$fg[$n_fal])%}$ "
       "%{$reset_color%}"
     )
     ps2=(
       "%_ %{$fg[$d_col]%}-> "
       "%{$reset_color%}"
     )
     rps1=(
       '%{$fg[$b_col]%}${vcs_info_msg_0_}%{$reset_color%}'
       " %{$fg[$d_col]%}%(4~.%{$fg[$d_col_pre]%}%-1~%{$fg[$d_col]%}
%2~.%3~) %{$reset_color%}"
     )
     PS2="${(j::)ps2}"
     PS1="${(j::)ps1}"
     RPS1="${(j::)rps1}"
}
