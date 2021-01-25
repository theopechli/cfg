#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	exec startx
fi

export VISUAL=nvim
export EDITOR="$VISUAL"
export LANG=en_US.UTF-8
export TERM=st-256color
export IDEA_JDK="/usr/lib/jvm/java-11-openjdk"

if [ -d "$HOME/bin" ] ; then
    PATH="$PATH:$HOME/bin"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$PATH:$HOME/.local/bin"
fi
