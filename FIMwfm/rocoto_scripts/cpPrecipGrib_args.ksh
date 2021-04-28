#!/bin/ksh --login

## this script copies the GRIB2 file to /rt/fim/precip_gaussian 
##
##   J. Henderson    03/2014

# check for correct number of parameters
if [ $# -lt 3 ]; then
  echo "Usage:  $0 /pan2/projects/fim-njet/FIM  yyyymmddhh filename"
  exit 1
fi

# initialize variables
MEMBER_ID="C"
FIM_RUN=$1
yyyymmddhh=$2
filename=$3

#FIM_HOME=/pan2/projects/fim-njet/FIM 
#yyyymmddhh=2014030912
#filename=1406812000012
# Print out value of required environment variables
echo
echo "FIM_RUN          = ${FIM_RUN}"
echo "MEMBER_ID        = ${MEMBER_ID}"
echo "yyyymmddhh       = ${yyyymmddhh}"
echo "filename         = ${filename}"
echo

# initialize
CP=/bin/cp
grib2dir=${FIM_RUN}/${yyyymmddhh}/post_${MEMBER_ID}/129/NAT/grib2/
rtDir=/rt/fim/precip_gaussian/

# copy to /rt directory to be transferred to /public
echo "***copying to /rt...."
echo "$CP -p ${grib2dir}/${filename} $rtDir"
$CP -p ${grib2dir}/${filename} $rtDir/${filename}
status=$?
if [ $status != 0 ]; then
  echo "error $status copying ${grib2dir}/${filename} to ${rtDir}"
  return $status
fi
