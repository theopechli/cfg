#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

alias ls='ls --color=auto'
alias la='ls -la'
alias vmpv='mpv --ytdl-format="bestvideo[height<?1080]+bestaudio/best"'
alias ampv='mpv --ytdl-format="bestaudio/best" --no-video'
alias pvmpv='prime-run mpv --ytdl-format="bestvideo[height<?1080]+bestaudio/best"'
alias ydla=$'youtube-dl --audio-quality 0 --extract-audio --audio-format m4a -o \'./%(title)s.%(ext)s\''
alias rmpv='mpv av://v4l2:/dev/video0 --profile=low-latency --untimed'
alias gs='git status'
alias gc='git commit -m'
alias ga='git add'
alias gd='git diff'

sc() {
    ffmpeg -f x11grab -video_size 1366x768 -i $DISPLAY -vframes 1 "$1".png
}
