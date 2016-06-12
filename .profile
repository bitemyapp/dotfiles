if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

LC_CTYPE=en_US.UTF-8
LANG=$LC_CTYPE
LC_ALL=$LC_CTYPE
LC_COLLATE=$LC_CTYPE
LC_MONETARY=$LC_CTYPE
LC_TIME=$LC_CTYPE

GTK_IM_MODULE=xim
LESSCHARSET=utf-8

PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.

export PATH HOME TERM LC_CTYPE LANG LC_ALL LC_COLLATE LC_MONETARY LC_TIME
