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


## Wipe the disk with dm-crypt

dialog --yesno "Do you want to wipe the disk with dm-crypt?" 0 0
response=$?
if [ $response -eq 0 ]
then
    cryptsetup open --type plain -d /dev/urandom ${device} to_be_wiped
    dd if=/dev/zero of=/dev/mapper/to_be_wiped status=progress
    cryptsetup close to_be_wiped
fi


## Update the system clock

timedatectl set-ntp true


## Partition the disk

sed -e 's/\s*\([\+0-9a-zA-Z]*\).*/\1/' << EOF | fdisk "${device}"
  g # create a new empty GPT partition table
  n # add a new partition
  1 # partition number 1
    # default - start at beginning of disk
  +512M # 512 MB EFI System parttion
  n # add a new partition
  2 # partion number 2
    # default, start immediately after preceding partition
    # default, extend partition to end of disk
  t # change a partition type
  1 # partition number 1
  1 # EFI System
  p # print the in-memory partition table
  w # write the partition table
EOF

part_efi="$(ls ${device}* | grep -E "^${device}p?1$")"
part_sys="$(ls ${device}* | grep -E "^${device}p?2$")"


## Format the partitions

cryptsetup luksFormat "${part_sys}"
cryptsetup open "${part_sys}" cryptlvm
pvcreate /dev/mapper/cryptlvm
vgcreate MyVolGroup /dev/mapper/cryptlvm
lvcreate -L 24G MyVolGroup -n swap
lvcreate -L 32G MyVolGroup -n root
lvcreate -l 100%FREE MyVolGroup -n home

mkfs.ext4 /dev/MyVolGroup/root
mkfs.ext4 /dev/MyVolGroup/home
mkswap /dev/MyVolGroup/swap


## Mount the file systems

mount /dev/MyVolGroup/root /mnt
mkdir /mnt/home
mount /dev/MyVolGroup/home /mnt/home
swapon /dev/MyVolGroup/swap

mkfs.fat -F32 "${part_efi}"
mkdir /mnt/boot
mount "${part_efi}" /mnt/boot


## Install essential packages

pacstrap /mnt base base-devel linux linux-firmware lvm2 cryptsetup intel-ucode dhcpcd networkmanager git neovim


## Configure the system

# Fstab
genfstab -U /mnt >> /mnt/etc/fstab

# Time zone
arch-chroot /mnt ln -sf /usr/share/zoneinfo/"${region}"/"${city}" /etc/localtime
arch-chroot /mnt hwclock --systohc

# Localization
arch-chroot /mnt sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
arch-chroot /mnt sed -i 's/#el_GR.UTF-8 UTF-8/el_GR.UTF-8 UTF-8/' /etc/locale.gen
arch-chroot /mnt locale-gen

echo -e "LANG=en_US.UTF-8" > /mnt/etc/locale.conf

# Network configuration
echo "${hostname}" > /mnt/etc/hostname

echo -e "127.0.0.1       localhost
::1             localhost
127.0.1.1       ${hostname}.localdomain	${hostname}" > /mnt/etc/hosts

# Add user
arch-chroot /mnt useradd -m -G wheel,video "$user"

# Root and User password
echo "$user:$password" | chpasswd --root /mnt
echo "root:$password" | chpasswd --root /mnt

# Configure mkinitcpio
arch-chroot /mnt sed -i '/^HOOKS=(.*)/ s/HOOKS=(base udev autodetect modconf block filesystems keyboard fsck)/HOOKS=(base udev autodetect keyboard modconf block encrypt lvm2 filesystems fsck)/' /etc/mkinitcpio.conf

# Initramfs
arch-chroot /mnt mkinitcpio -P

# Boot loader
arch-chroot /mnt bootctl install

# Configure systemd-boot
devUUID="$(lsblk -dno UUID ${part_sys})"

echo -e "default  arch.conf
timeout  3
editor   no" > /mnt/boot/loader/loader.conf

echo -e "title   Arch Linux
linux   /vmlinuz-linux
initrd  /intel-ucode.img
initrd  /initramfs-linux.img
options cryptdevice=UUID=${devUUID}:cryptlvm root=/dev/MyVolGroup/root rw" > /mnt/boot/loader/entries/arch.conf


## Reboot

arch-chroot /mnt git clone https://github.com/theopechli/cfg home/"${user}"/cfg
arch-chroot /mnt chown "${user}" home/"${user}"/cfg
arch-chroot /mnt systemctl enable NetworkManager.service
umount -R /mnt/
reboot
