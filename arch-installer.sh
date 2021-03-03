#!/bin/bash


## Select the mirrors

dialog --yesno "Do you want to rank the mirrors?" 0 0
response=$?
if [ $response -eq 0 ]
then
    MIRRORLIST_URL_1="https://archlinux.org/mirrorlist/?country=GR&protocol=https&use_mirror_status=on"
    MIRRORLIST_URL_2="https://archlinux.org/mirrorlist/?country=DE&country=SE&protocol=https&use_mirror_status=on"

    pacman -Sy --noconfirm --needed pacman-contrib dialog

    echo "Backing up the mirror list"
    cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup

    echo "Updating the mirror list"
    curl -s "$MIRRORLIST_URL_1" | \
        sed -e 's/^#Server/Server/' -e '/^#/d' | \
        rankmirrors -n 5 - > /etc/pacman.d/mirrorlist
    curl -s "$MIRRORLIST_URL_2" | \
        sed -e 's/^#Server/Server/' -e '/^#/d' | \
        rankmirrors -n 5 - >> /etc/pacman.d/mirrorlist

    [ -s /etc/pacman.d/mirrorlist ] || cp /etc/pacman.d/mirrorlist.backup /etc/pacman.d/mirrorlist
fi

## Get basic information

region=$(dialog --stdout --inputbox "Enter region, e.g. 'Europe'" 0 0) || exit 1
clear
: ${region:?"region cannot be empty"}

city=$(dialog --stdout --inputbox "Enter city, e.g. 'Athens'" 0 0) || exit 1
clear
: ${city:?"city cannot be empty"}

hostname=$(dialog --stdout --inputbox "Enter hostname, e.g. 'archlinux'" 0 0) || exit 1
clear
: ${hostname:?"hostname cannot be empty"}

user=$(dialog --stdout --inputbox "Enter username" 0 0) || exit 1
clear
: ${user:?"username cannot be empty"}

password=$(dialog --stdout --passwordbox "Enter password" 0 0) || exit 1
clear
: ${password:?"password cannot be empty"}
password2=$(dialog --stdout --passwordbox "Enter password again" 0 0) || exit 1
clear
[[ "$password" == "$password2" ]] || ( echo "Passwords did not match"; exit 1; )

devicelist=$(lsblk -dplnx size -o name,size | grep -Ev "boot|rpmb|loop" | tac)
device=$(dialog --stdout --menu "Select installation disk" 0 0 0 ${devicelist}) || exit 1
clear


## Update the system clock

timedatectl set-ntp true


## Partition the disk

sed -e 's/\s*\([\+0-9a-zA-Z]*\).*/\1/' << EOF | fdisk "${device}"
  g # create a new empty GPT partition table
  n # add a new partition
  p # primary partition
  1 # partition number 1
    # default - start at beginning of disk
  +512M # 512 MB EFI System parttion
  n # add new partition
  p # primary partition
  2 # partion number 2
    # default, start immediately after preceding partition
  +32G # 32 GB Linux root partition
  n # add a new partition
  p # primary partition
  3 # partion number 3
    # default, start immediately after preceding partition
  +24G # 24 GB Linux swap partition
  n # add a new partition
  p # primary partition
  4 # partion number 4
    # default, start immediately after preceding partition
    # default, extend partition to end of disk
  t # change a partition type
  1 # partition number 1
  1 # EFI System
  t # change a partition type
  2 # partition number 2
  24 # Linux root (x86-64)
  t # change a partition type
  3 # partition number 3
  19 # Linux swap
  t # change a partition type
  4 # partition number 1
  28 # Linux home
  p # print the in-memory partition table
  w # write the partition table
  q # and we're done
EOF

part_efi="$(ls ${device}* | grep -E "^${device}p?1$")"
part_root="$(ls ${device}* | grep -E "^${device}p?2$")"
part_swap="$(ls ${device}* | grep -E "^${device}p?3$")"
part_home="$(ls ${device}* | grep -E "^${device}p?4$")"


## Format the partitions

mkfs.fat -F32 "${part_efi}"
mkfs.ext4 "${part_root}"
mkfs.ext4 "${part_home}"
mkswap "${part_swap}"


## Mount the file systems

swapon "${part_swap}"
mount "${part_root}" /mnt
mkdir /mnt/efi
mount "${part_efi}" /mnt/efi
mkdir /mnt/home
mount "${part_home}" /mnt/home


## Install essential packages

pacstrap /mnt base base-devel linux linux-firmware grub efibootmgr intel-ucode git dhcpcd networkmanager neovim


## Configure the system

# Fstab
genfstab -U /mnt >> /mnt/etc/fstab

# Time zone
arch-chroot /mnt ln -sf /usr/share/zoneinfo/"${region}"/"${city}" /etc/localtime
arch-chroot /mnt hwclock --systohc

# Localization
arch-chroot /mnt sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
arch-chroot /mnt sed -i 's/#el_GR.UTF-8 UTF-8/el_GR.UTF-8 UTF-8/g' /etc/locale.gen
arch-chroot /mnt locale-gen
echo "LANG=en_US.UTF-8" > /mnt/etc/locale.conf

# Network configuration
echo "${hostname}" > /mnt/etc/hostname
echo -e "127.0.0.1       localhost
::1             localhost
127.0.1.1       ${hostname}.localdomain	${hostname}" > /mnt/etc/hosts

# Add user
arch-chroot /mnt useradd -m -G wheel "$user"

# Set root and user password
echo "$user:$password" | chpasswd --root /mnt
echo "root:$password" | chpasswd --root /mnt

# Configure mkinitcpio
arch-chroot /mnt grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB --removable
arch-chroot /mnt grub-mkconfig -o /boot/grub/grub.cfg


## Reboot

arch-chroot /mnt git clone https://github.com/theopechli/cfg home/"${user}"/cfg
arch-chroot /mnt systemctl enable NetworkManager.service
arch-chroot /mnt ln -s usr/bin/nvim usr/bin/vi

umount -R /mnt/
reboot
