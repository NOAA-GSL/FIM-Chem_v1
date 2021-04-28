#!/bin/sh --login

echo "****************************"
echo FIM_HOME: $FIM_HOME
echo FIM_RUN:  $FIM_RUN
echo DATE:     $yyyymmddhh
echo model:    $model
echo rev:      $rev

# Set up paths to unix commands
RM=/bin/rm
CP=/bin/cp
MV=/bin/mv
LN=/bin/ln
MKDIR=/bin/mkdir
CAT=/bin/cat
ECHO=/bin/echo
CUT=/bin/cut
WC=/usr/bin/wc
DATE=/bin/date
AWK="/bin/awk --posix"
SED=/bin/sed
TAIL=/usr/bin/tail

# Executable script and paths
W2='/lfs2/projects/fim/fiorino/w21'              ### jet
RUNCMD="$W2/run.cron.tcsh"
PYDIR="$FIM_HOME/util/tmtrkN"
PYCMD="w2.tc.tmtrkN-0p125.py"

# Set CWD to script location and execute redirecting stdout/stderr
cd $PYDIR
$RUNCMD "$PYDIR/$PYCMD $yyyymmddhh -m $model -r $rev"

# Check for exit status of script
error=$?
if [ ${error} -ne 0 ]; then
  ${ECHO} "ERROR: ${PYCMD} crashed  Exit status=${error}"
  exit ${error}
fi

# copy file back to tracker directories under realtime directory 
TRACKDIR=$W2/dat/tc/tmtrkN/$yyyymmddhh/fim8h
if [ -f ${TRACKDIR}/tctrk.atcf.${yyyymmddhh}.f8hj.txt ]; then
  DESTDIR=${FIM_RUN}/${yyyymmddhh}/posttracker_C/168
  mkdir -p ${DESTDIR}
  cmd="$CP ${TRACKDIR}/tctrk.atcf.${yyyymmddhh}.f8hj.txt ${DESTDIR}/track.${yyyymmddhh}00.FIM8"
  echo "${cmd}....."
  $cmd
  error=$?
  if [ ${error} -ne 0 ]; then
    ${ECHO} "ERROR with copy command. Exit status=${error}"
    exit ${error}
  fi
else
  ${ECHO} "${TRACKDIR}/tctrk.atcf.${yyyymmddhh}.fght.txt does not exist!"
fi
