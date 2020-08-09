#!/bin/bash

sudo pacman -Syu
sudo pacman -S $(cat packages)

cp -r .bash_profile .bashrc .config/ .xinitrc .Xresources .xserverrc .xsession .xmonad/ .xmobar/ ~/
sudo cp -r xorg.conf.d /etc/X11/
sudo cp custom.cfg /boot/grub/custom.cfg

sudo systemctl set-default multi-user.target

mkdir -p ~/Downloads
cd ~/Downloads
git clone https://git.suckless.org/st

cd ~/Downloads/st
curl -O https://st.suckless.org/patches/scrollback/st-scrollback-20200419-72e3f6c.diff
patch -i st-scrollback-20200419-72e3f6c.diff

sed -i -e 's/Liberation Mono:pixelsize=12/DejaVu Sans Mono:pixelsize=14/g' ~/Downloads/st/config.def.h
sed -i -e 's/XC_xterm/XC_left_ptr/g' ~/Downloads/st/config.def.h

sudo make clean install

cd ~/
