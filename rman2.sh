#!/bin/sh
#
# $Header: rman2.sh 26-jul-99.09:32:20 mjaeger Exp $
#
# Copyright (c) 1998, 1999, Oracle Corporation.  All rights reserved.
#
# rman2.sh
#
#    NAME
#      rman2.sh - duplexed backup of archivelogs
#
#    DESCRIPTION
#      This shell script dynamically constructs an RMAN backup job and then
#      executes it.  It will backup the archivelogs, creating 2 copies
#      of each archivelog.  This is intended for 8.0.x versions of RMAN.  With
#      8.1.3, the duplex feature makes this script unnecessary.
#
#    NOTES
#      Some customization is necessary.
#
#      Set "freq" to be the frequency in hours with which you execute this
#      script.  If your rate of redolog generation is very high, then you
#      might need to backup archivelogs every 2 hours.  In this case,
#      set freq=2.
#
#    MODIFIED   (MM/DD/YY)
#    mjaeger     07/26/99 - bug 808870: OCCS: convert tabs, no long lines
#    gpongrac    08/11/98 - Creation
#

# Initialize connect string variables.  Change these as necessary.

target='target /'     # default to "connect internal"
rcvcat='nocatalog'    # default to nocatalog mode
#rcvcat='rcvcat rman/rman@rcat_db'

freq=24    # The number of hours between executions of this script.
           # Default to once every 24 hours (once a day).
           # Change this as necessary.

# Get the current time for constructing a fairly unique filename in /tmp:
time=`date '+%H%M%S'`

# Construct filenames using $time for uniqueness:

cmdfile=/tmp/rman$time.rcv
msglog=/tmp/rman$time.log

# Get the current time-of-day in the format:
# YYYY MM DD HH24:MI:SS

tod=`date '+%Y %m %d %H:%M:%S'`

#
# CUSTOMIZATION:
#
# The RMAN script below should be customized as appropriate.
# Add "allocate channel" commands as needed so that the desired number of
# tape drives will be utilized.
#
# The format string can be changed if desired, but it must generate
# unique names.  %u is usually good enough.
#
# The RMAN invocation should be customized with the target database
# SYS password and TNS alias.  If you use a recovery catalog, replace
# "nocatalog" with "rcvcat", and follow it with the connect string
# for the recovery catalog.
#
# The "like" clause can be uncommented and edited to have RMAN backup
# only archivelogs whose filenames are LIKE the specified pattern.
# This can be used to backup only those archivelogs in a particular
# log_archive_dest.
#

cat << EOF > $cmdfile
run {
allocate channel c1 type 'sbt_tape';
allocate channel c2 type 'sbt_tape';
# allocate additional channels as necessary
backup archivelog
   from time="to_date('$tod', 'YYYY MM DD HH24:MI:SS')-$freq/24"
   until time="to_date('$tod', 'YYYY MM DD HH24:MI:SS')"
   # like 'primary_archivelog_destination_mount_point/%'
   format='%u';
backup archivelog
   from time="to_date('$tod', 'YYYY MM DD HH24:MI:SS')-$freq/24"
   until time="to_date('$tod', 'YYYY MM DD HH24:MI:SS')"
   # like 'primary_archivelog_destination_mount_point/%'
   format='%u'
   delete input;
}
EOF

rman $target $rcvcat cmdfile $cmdfile msglog $msglog

exit # rman2.sh

