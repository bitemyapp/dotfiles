set fish_greeting

alias t  "tree --dirsfirst"
alias ll "ls -alGh"

# Git aliases
alias gs  "git status -sb"
alias ga  "git add"
alias gbr "git branch"
alias gc  "git commit"
alias gcm "git commit -m"
alias gd  "git diff"
alias gco "git checkout"

# Load paths
set PATH $PATH /usr/local/bin
set PATH $PATH /usr/local/sbin
set PATH $PATH /usr/bin
set PATH $PATH /usr/sbin
set PATH $PATH /bin
set PATH $PATH /sbin
set PATH $PATH /opt/bin
set PATH $PATH /usr/X11/bin
set PATH $PATH $HOME/bin
set PATH $PATH $HOME/.cabal/bin

# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch pink

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡ '
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function fish_prompt
       set last_status $status
       set_color $fish_color_cwd
       printf '%s' (prompt_pwd)
       set_color normal
       printf '%s ' (__fish_git_prompt)
       set_color normal
end

set -x LANG en_US.UTF-8
