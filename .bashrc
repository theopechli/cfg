#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

alias ls='ls --color=auto'
alias la='ls -la'
alias vmpv='mpv --ytdl-format="bestvideo[height<?1080]+bestaudio/best"'
alias ampv='mpv --ytdl-format="bestaudio/best" --no-video'
alias pvmpv='prime-run mpv --ytdl-format="bestvideo[height<?1080]+bestaudio/best"'
alias ydla=$'youtube-dl --audio-quality 0 --extract-audio --audio-format m4a -o "./%(title)s.%(ext)s"'
alias rmpv='mpv av://v4l2:/dev/video0 --profile=low-latency --untimed'
alias gs='git status'
alias gc='git commit'
alias gcl='git clone'
alias gco='git checkout'
alias ga='git add'
alias gd='git diff'
alias gps='git push'
alias gpl='git pull'
alias gl='git log'
alias gb='git branch'
alias mpct='mpc toggle'
alias mpcn='mpc next'
alias mpcp='mpc prev'
alias mpcs='mpc single'
alias mpcr='mpc repeat'

mpca () {
    mpc ls | grep "${1}" | mpc add
}

man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

scf() {
    ffmpeg -f x11grab -video_size 1366x768 -i $DISPLAY -vframes 1 "$1".png
}

scv() {
    ffmpeg -f x11grab -video_size 1366x768 -i $DISPLAY -framerate 25 -c:v ffvhuff "$1".mkv
}

export VISUAL=emacs
export EDITOR="$VISUAL"
export LANG=en_US.UTF-8
export TERM=alacritty

blk='\[\033[01;30m\]'   # Black
red='\[\033[01;31m\]'   # Red
grn='\[\033[01;32m\]'   # Green
ylw='\[\033[01;33m\]'   # Yellow
blu='\[\033[01;34m\]'   # Blue
pur='\[\033[01;35m\]'   # Purple
cyn='\[\033[01;36m\]'   # Cyan
wht='\[\033[01;37m\]'   # White
clr='\[\033[00m\]'      # Reset

# Display the current Git branch in the Bash prompt.

function git_branch() {
    if [ -d .git ] ; then
        printf "%s" "($(git branch 2> /dev/null | awk '/\*/{print $2}'))";
    fi
}

# Set the prompt.

function bash_prompt(){
    PS1=${red}'[\u@\h]'${blu}'$(git_branch)'${pur}' \W'${grn}' \$ '${clr}
}

bash_prompt
