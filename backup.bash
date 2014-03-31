#!/usr/bin/env bash
# Usage: backup.bash >& backup.log &
# Author: Timothée Flutre
# License: GPL3+

date

cd $HOME

RSYNC_ARGS_1="-Cavzh --delete --delete-excluded --stats --progress"
RSYNC_ARGS_1="${RSYNC_ARGS_1} --exclude='.*' --exclude='.*/' --exclude='*~'"
RSYNC_ARGS_1="${RSYNC_ARGS_1} --exclude='bin' --exclude='include' --exclude='lib' --exclude='share' --exclude='src_ext'"
RSYNC_ARGS_2=""
SOURCE=""
TARGET=""

if [ "$HOSTNAME" == "tflutre-laptop" ]; then
    RSYNC_ARGS_2="--exclude='AeroFS' --exclude='clusterpps' --exclude='Desktop' --exclude='Downloads' --exclude='Dropbox' --exclude='Public' --exclude='tmp/' --exclude='Ubuntu One'"
    SOURCE="${HOME}/"
    TARGET="/media/tflutre/tflutre-backup/backup/"
fi

RSYNC_ARGS="${RSYNC_ARGS_1} ${RSYNC_ARGS_2}"
RSYNC="rsync ${RSYNC_ARGS} ${SOURCE} ${TARGET}"
echo ${RSYNC}
eval ${RSYNC}

date

if [ -f backup.log ]; then
    cp backup.log $TARGET
fi
