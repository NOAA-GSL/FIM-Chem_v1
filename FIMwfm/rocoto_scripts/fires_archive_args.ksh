#!/bin/ksh -l

##   fires_archive_fdr.ksh
##      extracts fire data from mass store
##       /BMC/fdr/Permanent/${year}/{$month}/${day}/data/sat/firms/global
##          ${year}${month}${day}${hr}00.zip
##       /BMC/fdr/Permanent/${year}/{$month}/${day}/data/sat/ssec/goes-west/wf_abba
##          ${year}${month}${day}${hr}00.zip
##
##      and copies them to 
##
##    J.K.Henderson   10-2016 
##

FIREDIR=$1
YYYY=$2
MM=$3
DD=$4
HOUR=$5

# Print out value of required environment variables
echo
echo "FIREDIR      = ${FIREDIR}"
echo "YYYY         = ${YYYY}"
echo "MM           = ${MM}"
echo "DD           = ${DD}"
echo "HOUR         = ${HOUR}"
echo

# Set up paths to shell commands
MKDIR=/bin/mkdir
CP=/bin/cp 
RM=/bin/rm 
UNZIP=/usr/bin/unzip

# load HPSS module
module load hpss

# initialize
abba_dir="/BMC/fdr/Permanent/${YYYY}/${MM}/${DD}/data/sat/ssec/goes-west/wf_abba"
modis_dir="/BMC/fdr/Permanent/${YYYY}/${MM}/${DD}/data/sat/firms/global"
workdir=${FIREDIR}

# Set up a work directory and cd into it
echo ${workdir}
if [ ! -d ${workdir} ]; then
  ${MKDIR} -p ${workdir}
fi
cd ${workdir}

for dir in ${abba_dir} ${modis_dir}
do
  # Pull zip archive file from mass store
  archive=${YYYY}${MM}${DD}${HOUR}00.zip 
  echo Retrieving ${dir}/${archive}...
  hsi get ${dir}/${archive}
  status=$?
  if [ $status != 0 ]; then
    echo "error $status retrieving zip file $abba_dir/${archive}"
    return $status
  fi
  
  # extract files from archive file
  echo extracting files from $archive
  ${UNZIP} -o ${archive} 
  status=$?
  if [ $status != 0 ]; then
    echo "Can't extract file from ${archive}"
    return $status
  fi
  
  # remove archive file
  echo deleting $archive...
  rmcmd="${RM} $archive"
  echo "rmcmd:  $rmcmd"
  ${RM} $archive
done
exit $?
