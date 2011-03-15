#!/bin/sh
#######################################################################
# timestamp.sh
#
# Copyright (C) 2008-2011 Klaus Alexander Seistrup <klaus@seistrup.dk>
# Released under GPLv3+.
#######################################################################

ME="${0##*/}"

if [ "${#}" -gt 0 ]; then
  if [ "${#}" -gt 1 ]; then
    echo "Usage: ${ME} [FORMAT]" >&2
    exit 1
  fi
  case "${1}" in
    -h | --help )
      echo "Usage: ${ME} [FORMAT]"
      exit 0
    ;;
    * )
      FORMAT="${1}"
    ;;
  esac
else
  FORMAT='%F %T'
fi

OLD_IFS="${IFS}"
IFS='
'
while read LINE; do
  printf '%s\t%s\n' "$(date "+${FORMAT}")" "${LINE}"
done
IFS="${OLD_IFS}"

:
# eof
