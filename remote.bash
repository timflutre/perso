#!/usr/bin/env bash

# Usage: remote.bash <server> <task>
# Copyright (C) 2017 Timothée Flutre
# License: GPL-3+
# Inspiration: http://www.tecmint.com/sshfs-mount-remote-linux-filesystem-directory-using-ssh/

if [ ! $# -eq 2 ]; then
  echo -e "usage: remote.bash <server> <task>\n\tserver: cc2\n\ttask: mount/umount\n\nif device busy, use 'umount -l'" 1>&2
  exit 1
fi

server=$1
task=$2

## http://unix.stackexchange.com/a/111518/34919
if [[ ! "${server}" =~ ^(cc2)$ ]]; then
  echo "ERROR: unknown server '${server}'" 1>&2
  exit 1
fi  
if [[ ! "${task}" =~ ^(mount|umount)$ ]]; then
  echo "ERROR: unknown task '${task}'" 1>&2
  exit 1
fi

if [ ! -d ~/mnt ]; then
  echo "ERROR: '~/mnt' doesn't exist" 1>&2
  exit 1
fi
if [ ! -f ~/.ssh/id_rsa ]; then
  echo "ERROR: '~/.ssh/id_rsa' doesn't exist" 1>&2
  exit 1
fi

if [[ "${server}" == "cc2" ]]; then
  if [[ "${task}" == "mount" ]]; then
    sshfs -o IdentityFile=~/.ssh/id_rsa -o follow_symlinks \
          flutre@cc2-login.cirad.fr:/homedir/flutre \
          /home/tflutre/mnt/cc2_flutre
  fi
  if [[ "${task}" == "umount" ]]; then
    umount /home/tflutre/mnt/cc2_flutre # need "sudo"
    ## see also https://stackoverflow.com/a/25986155/597069
    ## fusermount -uz /home/tflutre/mnt/cc2_flutre
  fi
fi
