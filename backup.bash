#!/usr/bin/env bash

# Usage: nohup backup.bash >& backup.log &
# Copyright (C) 2013-2017 Timothée Flutre
# License: GPL-3+
# Inspiration: https://blog.interlinked.org/tutorials/rsync_time_machine.html

set -e

date

# -----------------------------------------------------------------------------
# prepare backup

SOURCE="${HOME}"
if [ ! -d "${SOURCE}" ]; then
  echo -e "ERROR: source directory ${SOURCE} doesn't exist";
  exit 1
fi

PATH_TO_TARGET="/media/tflutre/tflutre-backup"
if [ ! -d "${PATH_TO_TARGET}" ]; then
  echo -e "ERROR: path to target directory ${PATH_TO_TARGET} doesn't exist";
  exit 1
fi

PREVIOUS_BACKUP="backup_${HOSTNAME}_latest"
if [ ! -d "${PATH_TO_TARGET}/${PREVIOUS_BACKUP}" ]; then
  echo -e "ERROR: previous backup directory ${PATH_TO_TARGET}/${PREVIOUS_BACKUP} doesn't exist";
  exit 1
fi

CURRENT_BACKUP="backup_${HOSTNAME}_$(date "+%F_%H-%M-%S")"
if [ -d "${PATH_TO_TARGET}/${CURRENT_BACKUP}" ]; then
  echo -e "ERROR: current backup directory ${PATH_TO_TARGET}/${CURRENT_BACKUP} already exists";
  exit 1
fi

EXCLUDE_FILE="${SOURCE}/.rsync/exclude.txt"

RSYNC_ARGS="-axzChP"
RSYNC_ARGS+=" --stats"
RSYNC_ARGS+=" --delete"
RSYNC_ARGS+=" --delete-excluded"
if [ -f "${EXCLUDE_FILE}" ]; then
  echo -e "INFO: use exclude file ${EXCLUDE_FILE}";
  RSYNC_ARGS+=" --exclude-from=${EXCLUDE_FILE}"
fi
RSYNC_ARGS+=" --link-dest=${PATH_TO_TARGET}/${PREVIOUS_BACKUP}"

# -----------------------------------------------------------------------------
# perform backup

RSYNC_CMD="rsync"
RSYNC_CMD+=" ${RSYNC_ARGS}"
RSYNC_CMD+=" ${SOURCE}/"
RSYNC_CMD+=" ${PATH_TO_TARGET}/${CURRENT_BACKUP}"
echo ${RSYNC_CMD}
eval ${RSYNC_CMD}

# -----------------------------------------------------------------------------
# finish backup

rm -rf ${PATH_TO_TARGET}/${PREVIOUS_BACKUP}
ln -s ${PATH_TO_TARGET}/${CURRENT_BACKUP} ${PATH_TO_TARGET}/${PREVIOUS_BACKUP}

date
