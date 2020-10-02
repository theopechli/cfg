#!/bin/bash

sudo pacman -Syu
sudo pacman -S $(cat packages)

cp -r .bash_profile .bashrc .config/ .xinitrc .Xresources .xserverrc .xsession .xprofile .xmonad/ .xmobar/ ~/
sudo cp -r xorg.conf.d /etc/X11/
sudo cp custom.cfg /boot/grub/custom.cfg

sudo systemctl set-default multi-user.target
