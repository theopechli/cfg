#!/bin/bash

sudo pacman -Syu
sudo pacman -S --needed $(cat packages-minimal)

cp -r .bash_profile .bashrc .config/ .xinitrc .xserverrc .xprofile dwm/ st/ ~/
sudo cp -r xorg.conf.d /etc/X11/

sudo systemctl set-default multi-user.target
