#!/usr/bin/env bash

# Aim: pair my mouse and keyboard to my laptop
# Copyright (C) 2014 Timothée Flutre
# License: GPL-3+
# Versioning: https://github.com/timflutre/perso

progVersion="1.0.1" # http://semver.org

# Display the help on stdout.
# The format complies with help2man (http://www.gnu.org/s/help2man)
function help () {
  msg="\`${0##*/}' pairs my mouse and keyboard to my laptop.\n"
  msg+="\n"
  msg+="Usage: ${0##*/} [OPTIONS] ...\n"
  msg+="\n"
  msg+="Options:\n"
  msg+="  -h, --help\tdisplay the help and exit\n"
  msg+="  -V, --version\toutput version information and exit\n"
  msg+="  -v, --verbose\tverbosity level (0/default=1/2/3)\n"
  msg+="  -m, --mouse\tpair my mouse\n"
  msg+="  -k, --keyboard\tpair my keyboard\n"
  msg+="  -f, --fast\tfast procedure (skip some checks)\n"
  msg+="\n"
  msg+="Examples:\n"
  msg+="  ${0##*/} -m -k -f\n"
  msg+="\n"
  msg+="Report bugs to <timflutre@gmail.com>."
  echo -e "$msg"
}

# Display version and license information on stdout.
function version () {
  msg="${0##*/} ${progVersion}\n"
  msg+="\n"
  msg+="Copyright (C) 2014 Timothée Flutre.\n"
  msg+="License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n"
  msg+="This is free software; see the source for copying conditions.  There is NO\n"
  msg+="warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
  msg+="\n"
  msg+="Written by Timothée Flutre."
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
	  TEMP=`getopt -o hVv:mkf -l help,version,verbose:,mouse,keyboard,fast \
        -n "$0" -- "$@"`
  else # original getopt is available (no long options, whitespace, sorting)
	  TEMP=`getopt hVv:mkf "$@"`
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
      -m | --mouse) pairMouse=true; shift;;
      -k | --keyboard) pairKeyboard=true; shift;;
      -f | --fast) fastProc=true; shift;;
      --) shift; break;;
      *) echo "ERROR: options parsing failed, use -h for help" 1>&2; exit 1;;
    esac
  done
}

function run () {
  
  # remember to click on some button to make blue light flash
  # source: http://unix.stackexchange.com/a/59204
  
  if $pairMouse; then
	  if ! $fastProc; then
	    echo "Is your mouse switched on? [Y/N]"
	    read -s -t 10 IsSwitchedOn
	    if [ "$IsSwitchedOn" != "Y" ]; then
		    exit 0
	    fi
	    #hcitool inq
	    hcitool scan
	  fi
	  sudo hidd --connect F0:65:DD:76:93:18
  fi
  
  if $pairKeyboard; then
	  if ! $fastProc; then
	    echo "Is your keyboard switched on? [Y/N]"
	    read -s -t 10 IsSwitchedOn
	    if [ "$IsSwitchedOn" != "Y" ]; then
		    exit 0
	    fi
	    #hcitool inq
	    hcitool scan
	  fi
	  sudo hidd --connect 90:7F:61:92:65:0E
  fi
  
}

verbose=1
pairMouse=false
pairKeyboard=false
fastProc=false
parseCmdLine "$@"

if [ $verbose -gt "0" ]; then
  startTime=$(timer)
  msg="START ${0##*/} $(date +"%Y-%m-%d") $(date +"%H:%M:%S")"
  msg+="\ncmd-line: $0 "$@ # comment if an option takes a glob as argument
  msg+="\ncwd: $(pwd)"
  echo -e $msg
fi

run pairMouse pairKeyboard fastProc verbose

if [ $verbose -gt "0" ]; then
  msg="END ${0##*/} $(date +"%Y-%m-%d") $(date +"%H:%M:%S")"
  msg+=" ($(timer startTime))"
  echo $msg
fi
