#!/usr/bin/env bash
# Usage: cd; backup.bash >& backup.log &
# Author: Timothée Flutre
# License: GPL3+
set -e

date

RSYNC_ARGS_GENERIC="-Cavzh --delete --delete-excluded --stats --progress"
RSYNC_ARGS_GENERIC="${RSYNC_ARGS_GENERIC} --exclude='.*' --exclude='.*/' --exclude='*~'"
RSYNC_ARGS_GENERIC="${RSYNC_ARGS_GENERIC} --exclude='bin' --exclude='include' --exclude='lib' --exclude='share' --exclude='src_ext'"
RSYNC_ARGS_SPECIFIC="empty"
RSYNC="empty"
SOURCE="${HOME}/"
TARGET="empty"

#-----------------------------------------------------------------------------
# prepare backup depending on the host

if [ "$HOSTNAME" == "tflutre-laptop" ]; then
    RSYNC_ARGS_SPECIFIC="--exclude='AeroFS' --exclude='clusterpps' --exclude='Desktop' --exclude='Downloads' --exclude='Dropbox' --exclude='Public' --exclude='tmp/' --exclude='Ubuntu One'"
    RSYNC="rsync"
    TARGET="/media/tflutre/tflutre-backup/backup_${HOSTNAME}/"
elif [ "$HOSTNAME" == "agap-flutre" ]; then
    RSYNC_ARGS_SPECIFIC="--exclude='backup-agap' --exclude='Bureau' --exclude='clusterpps' --exclude='Dropbox' --exclude='Public' --exclude='Téléchargements' --exclude='tmp/'"
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
	    sudo mount.cifs //stocka2/backup-agap ${TARGET} -o user=flutre
	fi
    fi
else
    echo "[$0]: can't recognize host name '$HOSTNAME'"
    exit 1
fi

#-----------------------------------------------------------------------------
# perform backup similarly for all hosts

RSYNC_ARGS="${RSYNC_ARGS_GENERIC} ${RSYNC_ARGS_SPECIFIC}"
RSYNC_CMD="${RSYNC} ${RSYNC_ARGS} ${SOURCE} ${TARGET}"
echo ${RSYNC_CMD}
eval ${RSYNC_CMD}

#-----------------------------------------------------------------------------
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
