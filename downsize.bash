#!/usr/bin/env bash

# Aim: use mogrify to downsize all images from a directory
# Copyright (C) 2024 Timothée Flutre
# License: GPL-3+
# Person: Timothée Flutre [cre,aut]
# Versioning: https://github.com/timflutre/perso

progVersion="0.1.0" # http://semver.org/

# Display the help on stdout.
# The format complies with help2man (http://www.gnu.org/s/help2man)
function help () {
  msg="\`${0##*/}' uses mogrify to downsize all images from a directory.\n"
  msg+="\n"
  msg+="Usage: ${0##*/} [OPTIONS] ...\n"
  msg+="\n"
  msg+="Options:\n"
  msg+="  -h, --help\tdisplay the help and exit\n"
  msg+="  -V, --version\toutput version information and exit\n"
  msg+="  -v, --verbose\tverbosity level (0/default=1/2/3)\n"
  msg+="  -s, --source\tpath to the source directory containing the original images\n"
  msg+="  -t, --target\tpath to the target directory that will contain the smaller images\n"
  msg+="\n"
  msg+="Examples:\n"
  msg+="  ${0##*/} -s ~/tmp/myBigImages / -t ~/tmp/mySmallImages\n"
  msg+="\n"
  msg+="Report bugs to <timflutre@gmail.com>."
  echo -e "$msg"
}

# Display version and license information on stdout.
# The person roles comply with R's guidelines (The R Journal Vol. 4/1, June 2012).
function version () {
  msg="${0##*/} ${progVersion}\n"
  msg+="\n"
  msg+="Copyright (C) 2024 Timothée Flutre.\n"
  msg+="License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n"
  msg+="\n"
  msg+="Written by Timothée Flutre [cre,aut]."
  echo -e "$msg"
}

# http://www.linuxjournal.com/content/use-date-command-measure-elapsed-time
function timer () {
  if [[ $# -eq 0 ]]; then
    echo $(date '+%s')
  else
    local startRawTime=$1
    endRawTime=$(date '+%s')
    if [[ -z "$startRawTime" ]]; then startRawTime=$endRawTime; fi
    elapsed=$((endRawTime - startRawTime)) # in sec
    nbDays=$((elapsed / 86400))
    nbHours=$(((elapsed / 3600) % 24))
    nbMins=$(((elapsed / 60) % 60))
    nbSecs=$((elapsed % 60))
    printf "%01dd %01dh %01dm %01ds" $nbDays $nbHours $nbMins $nbSecs
  fi
}

# Parse the command-line arguments.
# http://stackoverflow.com/a/4300224/597069
function parseCmdLine () {
  getopt -T > /dev/null # portability check (say, Linux or Mac OS?)
  if [ $? -eq 4 ]; then # GNU enhanced getopt is available
	  TEMP=`getopt -o hVv:s:t: -l help,version,verbose:,source:target: \
        -n "$0" -- "$@"`
  else # original getopt is available (no long options, whitespace, sorting)
	  TEMP=`getopt hVv:s:t: "$@"`
  fi
  if [ $? -ne 0 ]; then
	  echo "ERROR: "$(which getopt)" failed" 1>&2
	  getopt -T > /dev/null
	  if [ $? -ne 4 ]; then
	    echo "did you use long options? they are not handled \
on your system, use -h for help"
	  fi
	  exit 2
  fi
  eval set -- "$TEMP"
  while [ $# -gt 0 ]; do
    case "$1" in
      -h | --help) help; exit 0; shift;;
      -V | --version) version; exit 0; shift;;
      -v | --verbose) verbose=$2; shift 2;;
      -s | --source) sourceDir=$2; shift 2;;
      -t | --target) targetDir=$2; shift 2;;
      --) shift; break;;
      *) echo "ERROR: options parsing failed, use -h for help" 1>&2; exit 1;;
    esac
  done
  if [ -z "${sourceDir}" ]; then
    echo -e "ERROR: missing compulsory option --s, --source\n" 1>&2
    help
    exit 1
  fi
  if [ ! -d "${sourceDir}" ]; then
    echo -e "ERROR: can't find source directory '${sourceDir}'\n" 1>&2
    help
    exit 1
  fi
  if [ -z "${targetDir}" ]; then
    echo -e "ERROR: missing compulsory option --s, --target\n" 1>&2
    help
    exit 1
  fi
  if [ -d "${targetDir}" ]; then
    echo -e "ERROR: target directory '${targetDir}' already exists\n" 1>&2
    help
    exit 1
  fi
}

function run () {
  nbInputFiles=$(ls ${sourceDir}/* | wc -l)
  if [ $verbose -gt "0" ]; then
    msg="nb of input files: "${nbInputFiles}
    echo -e $msg
  fi
  if [ ${nbInputFiles} -gt 0 ]; then
    mkdir ${targetDir}
    ls ${sourceDir}/* | while read path2sourceFile; do
      fileSize=$(stat -c%s "${path2sourceFile}")
      baseFile="${path2sourceFile##*/}"
      baseFileSansExt="${baseFile%.*}"
      ext="${path2sourceFile##*.}"
      if [ ${fileSize} -gt 1000000 ]; then
        echo ${path2sourceFile}": "${fileSize}
        cp ${path2sourceFile} ${targetDir}/${baseFileSansExt}_small.${ext}
        mogrify -resize 30% ${targetDir}/${baseFileSansExt}_small.${ext}
      else
        cp ${path2sourceFile} ${targetDir}/${baseFile}
      fi
    done
  fi
}

verbose=1
sourceDir=""
targetDir=""
parseCmdLine "$@"

if [ $verbose -gt "0" ]; then
  startTime=$(timer)
  msg="INFO: start ${0##*/} ${progVersion} $(date +"%Y-%m-%d")"
  msg+=" $(date +"%H:%M:%S")"
  msg+="\nINFO: cmd-line: $0 "$@ # comment if an option takes a glob as argument
  msg+="\nINFO: cwd: $(pwd)"
  echo -e $msg
fi

run

if [ $verbose -gt "0" ]; then
  msg="INFO: end ${0##*/} ${progVersion} $(date +"%Y-%m-%d")"
  msg+=" $(date +"%H:%M:%S")"
  msg+=" ($(timer startTime))"
  echo $msg
fi

# Local Variables:
# mode: Shell-script
# coding: utf-8-unix
# tab-width: 2
# sh-basic-offset: 2
# sh-indentation: 2
# End:
