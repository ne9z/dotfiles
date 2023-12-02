find /dev/disk/by-id/
DISK=/dev/disk/by-id/ata-MK0100GCTYU_BTTV508202HV100FGN
MY_DISK_PASS="923zqbgsa0v9z9lA?s"
MNT=$(mktemp -d)
SWAPSIZE=16
RESERVE=1
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
if ! command -v git; then nix-env -f '<nixpkgs>' -iA git; fi
if ! command -v partprobe;  then nix-env -f '<nixpkgs>' -iA parted; fi
partition_disk () {
    local disk="${1}";
    blkdiscard -f "${disk}" || true;
    parted --script --align=optimal  "${disk}" --  mklabel gpt  mkpart EFI 2MiB 1GiB  \
           mkpart bpool 1GiB 5GiB  mkpart rpool 5GiB -$((SWAPSIZE + RESERVE))GiB  \
           mkpart swap  -$((SWAPSIZE + RESERVE))GiB -"${RESERVE}"GiB  \
           mkpart BIOS 1MiB 2MiB  set 1 esp on  set 5 bios_grub on  set 5 legacy_boot on;
    partprobe "${disk}";
    udevadm settle;
}
for i in ${DISK};
do    partition_disk "${i}";
done
for i in ${DISK};
do    cryptsetup open --type plain --key-file /dev/random "${i}"-part4 "${i##*/}"-part4;
      mkswap /dev/mapper/"${i##*/}"-part4;
      swapon /dev/mapper/"${i##*/}"-part4;
done
for i in ${DISK};
do printf "${MY_DISK_PASS}" | cryptsetup luksFormat --type luks2 "${i}"-part3 -;
   printf "${MY_DISK_PASS}"  | cryptsetup luksOpen "${i}"-part3 luks-rpool-"${i##*/}"-part3 -;
done
zpool create -o compatibility=legacy \
-o ashift=12 \
  -o autotrim=on \
  -O acltype=posixacl \
  -O canmount=off \
  -O devices=off \
  -O normalization=formD \
  -O relatime=on \
  -O xattr=sa \
  -O mountpoint=/boot \
  -R "${MNT}" \
  bpool \
  $(for i in ${DISK}; do
 printf '%s ' "${i}-part2";
done)
zpool create \
  -o ashift=12 \
  -o autotrim=on \
  -R "${MNT}" \
  -O acltype=posixacl \
  -O canmount=off \
  -o compatibility=legacy \
 -O normalization=formD \
  -O relatime=on \
  -O xattr=sa \
  -O mountpoint=/ \
  rpool \
 $(for i in ${DISK}; do
printf '/dev/mapper/luks-rpool-%s ' "${i##*/}-part3";
  done)
zfs create  -o canmount=off  -o mountpoint=none rpool/nixos
zfs create -o mountpoint=legacy     rpool/nixos/root
mount -t zfs rpool/nixos/root "${MNT}"/
zfs create -o mountpoint=legacy rpool/nixos/home
mkdir "${MNT}"/home
mount -t zfs rpool/nixos/home "${MNT}"/home
zfs create -o mountpoint=none   rpool/nixos/var
zfs create -o mountpoint=legacy rpool/nixos/var/lib
zfs create -o mountpoint=legacy rpool/nixos/var/log
zfs create -o mountpoint=none bpool/nixos
zfs create -o mountpoint=legacy bpool/nixos/root
mkdir "${MNT}"/boot
mount -t zfs bpool/nixos/root "${MNT}"/boot
mkdir -p "${MNT}"/var/log
mkdir -p "${MNT}"/var/lib
mount -t zfs rpool/nixos/var/lib "${MNT}"/var/lib
mount -t zfs rpool/nixos/var/log "${MNT}"/var/log
zfs create -o mountpoint=legacy rpool/nixos/empty
zfs snapshot rpool/nixos/empty@start
for i in ${DISK};
do  mkfs.vfat -n EFI "${i}"-part1;
    mkdir -p "${MNT}"/boot/efis/"${i##*/}"-part1;
    mount -t vfat -o iocharset=iso8859-1 "${i}"-part1 "${MNT}"/boot/efis/"${i##*/}"-part1;
done
zfs set com.sun:auto-snapshot=true rpool/nixos/home
echo $MNT
# rsync -aP /oldroot/home/yc root@192.168.1.146:/tmp/tmp.qqxpHmVCLI/home/
nixos-install \
--root "${MNT}" \
--no-root-passwd \
--flake "git+file://${MNT}/home/yc/nixos-config#yinzhou"
umount -Rl "${MNT}"
zpool export -a
