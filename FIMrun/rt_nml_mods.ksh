#!/bin/ksh
#---------------------------------------------------------------
# Script to modify FIMnamelist for FIM real-time runs:
#
#  1) Create a clean copy of FIMnamelist.jet without comments 
#     and whitespace to use as a comparison, using nml tool
#  2) Modify namelist with mods common to all namelists
#  2) Modify namelist with partition-specific changes
#
# The nml tool is used since this option keeps this script short,
# readable, easy to edit, and less prone to error.  The disadvantage
# is that the output namelist is in a not so readable format:
#
# Usage:   ./rt_nml_mods.ksh ${BUILD}
# Example: ./rt_nml_mods.ksh jet
#---------------------------------------------------------------
if [[ $# -ne 1 ]];then
   print "Enter a build name (jet or theia)"
   exit
fi
BUILD=$1
nml_in=./FIMnamelist.${BUILD}  # Input namelist file with path, from repo
nml_tool=./nml                 # nml tool with path
nml_prefix=FIMnamelist         # base namelist name to append partitions onto

# Print a simplified, ordered copy of the input namelist, for comparing
cat ${nml_in} | ${nml_tool} > ${nml_in}.${BUILD}.ordered.unmodified

if [[ ${BUILD} == "jet" ]];then
  set -A PARTITIONS sjet ujet xjet
#------------------------------------------------------------
# Namelist common to all jet partitions:
#------------------------------------------------------------
  nml_out=${nml_in}.common
  ${nml_tool} --in ${nml_in}  --out ${nml_out} \
     --set QUEUEnamelist:ComputeTasks="'240'" \
     --set QUEUEnamelist:MaxQueueTime="'08:00:00'" \
     --set QUEUEnamelist:DATADIR="'/whome/rtfim/fimdata'" \
     --set QUEUEnamelist:DATADR2="'/public/data/grids/gfs/spectral_t1534'" \
     --set LANDnamelist:landdatdir="'/pan2/projects/fim-njet/fimdata/'" \
     --set CNTLnamelist:glvl="8" \
     --set PREPnamelist:mtnvar_file="'global_mtnvar.t574'" \
     --set DIAGnamelist:PrintDiagProgVars="24" \
     --set OUTPUTnamelist:TotalTime="336" \
     --set OUTPUTnamelist:restart_freq="480" \
     --set WRITETASKnamelist:check_omp_consistency=".true." \
     --set WRITETASKnamelist:max_write_tasks_per_node="2" \
     --set WRITETASKnamelist:num_write_tasks="1" \
     --set POSTnamelist:datadir="'../fim_C'" \
     --set POSTnamelist:grid_id="4" \
     --set POSTnamelist:var_list="'hgtP', 'tmpP', 'rp3P', 'up3P', 'vp3P', 'pr3D', 'ph3D', 'tk3D', 'td3D', 'ws3D', 'rh3D', 'us3D', 'vs3D', 'rn2D', 'rc2D', 'r12D', 'r22D', 'rg2D', 'pq2D', 'pw2D', 'ts2D', 'w080', 'us2D', 'hfss', 'hfls', 'rsds', 'rlds', 'ms2D', 'sa2D', 'sn2D', 's12D', 'cb2D', 'ct2D', 'u12D', 'v12D','rp2D', 'q22D', 't22D', 'rlut', 'oz3D', 'vv3P', 'prpv', 'thpv', 'uspv', 'vspv', 'rsus', 'rlus'" \
     --set POSTnamelist:nsmooth_var="4, 1, 1, 1, 1, 0, 4, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0" \
     --set POSTnamelist:fimout=".true." \
     --set POSTnamelist:gribout=".false." \
     --set POSTnamelist:only_write_var_list_entries=".true." \
     --set POSTnamelist:t2="336" 
#------------------------------------------------------------
# Create a namelist for each partition
#------------------------------------------------------------
  for PARTITION in ${PARTITIONS[@]} ; do
  #------------------------------------------------------------
  # Namelist modifications specific to sjet or vjet:
  #------------------------------------------------------------
    if [[ ${PARTITION} == "sjet" ]];then
      ${nml_tool} --in ${nml_out}  --out ${nml_prefix}.${PARTITION} \
        --set QUEUEnamelist:SRCDIR="'/pan2/projects/fim-njet/FIM/FIMsrc_sjet'" \
        --set PREPnamelist:NumPostGrids="8" \
        --set PREPnamelist:PostGridIds="4 236 201 244 130 174 83 129" \
        --set WRITETASKnamelist:cpn="16" \
        --set WRITETASKnamelist:mpipn="16" \
        --set WRITETASKnamelist:nthreads="16"
    fi    # sjet
  #------------------------------------------------------------
  # Namelist modifications specific to tjet or ujet:
  #------------------------------------------------------------
    if [[ ${PARTITION} == "ujet" ]];then
      ${nml_tool} --in ${nml_out}  --out ${nml_prefix}.${PARTITION} \
        --set QUEUEnamelist:SRCDIR="'/pan2/projects/fim-njet/FIM/FIMsrc_mvapich'" \
        --set PREPnamelist:NumPostGrids="8" \
        --set PREPnamelist:PostGridIds="4 236 201 244 130 174 83 129" \
        --set WRITETASKnamelist:cpn="12" \
        --set WRITETASKnamelist:mpipn="12" \
        --set WRITETASKnamelist:nthreads="12"
    fi    # ujet
  #------------------------------------------------------------
  # Namelist modifications specific to xjet:
  #------------------------------------------------------------
    if [[ ${PARTITION} == "xjet" ]];then
      ${nml_tool} --in ${nml_out}  --out ${nml_prefix}.${PARTITION} \
        --set QUEUEnamelist:SRCDIR="'/pan2/projects/fim-njet/FIM/FIMsrc_xjet'" \
        --set PREPnamelist:NumPostGrids="8" \
        --set PREPnamelist:PostGridIds="4 236 201 244 130 174 83 129" \
        --set WRITETASKnamelist:cpn="24" \
        --set WRITETASKnamelist:mpipn="24" \
        --set WRITETASKnamelist:nthreads="24"
    fi    # xjet
  done    # Loop over partitions
fi      # jet common

# Clean up intermediate common namelist
rm -f ${nml_out}
