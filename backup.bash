#!/usr/bin/env bash

# Usage: cd; backup.bash >& backup.log &
# Copyright (C) 2013-2015 Timothée Flutre
# License: GPL-3+

set -e

date

RSYNC="empty"

SOURCE="${HOME}/"

TARGET="empty"

RSYNC_ARGS_GENERIC="-Cavzh"
RSYNC_ARGS_GENERIC+=" --delete"
RSYNC_ARGS_GENERIC+=" --delete-excluded"
RSYNC_ARGS_GENERIC+=" --stats"
RSYNC_ARGS_GENERIC+=" --progress"
RSYNC_ARGS_GENERIC+=" --exclude='.*'"
RSYNC_ARGS_GENERIC+=" --exclude='.*/'"
RSYNC_ARGS_GENERIC+=" --exclude='*~'"
RSYNC_ARGS_GENERIC+=" --exclude='bin'"
RSYNC_ARGS_GENERIC+=" --exclude='include'"
RSYNC_ARGS_GENERIC+=" --exclude='lib'"
RSYNC_ARGS_GENERIC+=" --exclude='share'"
RSYNC_ARGS_GENERIC+=" --exclude='src_ext'"

RSYNC_ARGS_SPECIFIC="empty"

# -----------------------------------------------------------------------------
# prepare backup depending on the host

if [ "$HOSTNAME" == "tflutre-laptop" ]; then
  RSYNC="rsync"
  TARGET="/media/tflutre/tflutre-backup/backup_${HOSTNAME}/"
  RSYNC_ARGS_SPECIFIC="--exclude='AeroFS'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='clusterpps'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Desktop'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Downloads'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Dropbox'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Public'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='tmp'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Ubuntu One'"
elif [ "$HOSTNAME" == "agap-flutre" ]; then
  if [ -d /media/tflutre/tflutre-backup/ ]; then
	  RSYNC="rsync"
	  TARGET="/media/tflutre/tflutre-backup/backup_${HOSTNAME}/"
  else
	  RSYNC="sudo rsync"
	  TARGET="${HOME}/backup-agap/"
	# if grep -qs "${TARGET}" /proc/mounts; then
	  if ! sudo mountpoint -q ${TARGET}; then
	    echo "[$0]: mount ${TARGET}"
	    # smbclient -L stocka2; nmblookup stocka2
	    sudo mount.cifs //stocka2/backup-agap ${TARGET} --verbose -o username=flutre,domain=MTP,rw
	  fi
  fi
  RSYNC_ARGS_SPECIFIC="--exclude='backup-agap'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Bureau'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Calibre Library'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='cluster-marmadais'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='cluster-pps'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Dropbox'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Public'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='R'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='Téléchargements'"
  RSYNC_ARGS_SPECIFIC+=" --exclude='tmp'"
  if [ ! -d /media/tflutre/tflutre-backup/ ]; then
    RSYNC_ARGS_SPECIFIC+=" --exclude='Vidéos'"
  fi
else
  echo "[$0]: can't recognize host name '$HOSTNAME'"
  exit 1
fi

# -----------------------------------------------------------------------------
# perform backup similarly for all hosts

RSYNC_CMD="${RSYNC}"
RSYNC_CMD+=" ${RSYNC_ARGS_GENERIC}"
RSYNC_CMD+=" ${RSYNC_ARGS_SPECIFIC}"
RSYNC_CMD+=" ${SOURCE}"
RSYNC_CMD+=" ${TARGET}"
echo ${RSYNC_CMD}
eval ${RSYNC_CMD}

# -----------------------------------------------------------------------------
# finish backup depending on the host

date

if [ -f backup.log ]; then
  if [ "$HOSTNAME" == "tflutre-laptop" ]; then
	  cp backup.log ${TARGET}
  elif [ "$HOSTNAME" == "agap-flutre" ]; then
	  if [ -d /media/tflutre/tflutre-backup/ ]; then
	    cp backup.log ${TARGET}
	  else
	    sudo cp backup.log ${TARGET}
	  fi
  fi
fi
if [ "$HOSTNAME" == "agap-flutre" ] && sudo mountpoint -q ${TARGET}; then
  echo "[$0]: unmount ${TARGET}"
  sudo umount ${TARGET}
fi
