#!/bin/ksh --login

## this script deletes a subset of fim_out files from fim+_C diredtory for specified 
##    forecast hour, T
##
## for fIMX runs, need to keep following files for next cycle to link to
        fim_out_d1st000${T}
        fim_out_d2st000${T}
        fim_out_d3st000${T}
        fim_out_d4st000${T}
        fim_out_d5st000${T}
        fim_out_dms1000${T}
        fim_out_obc1000${T}
        fim_out_obc2000${T}
        fim_out_pbc1000${T}
        fim_out_pbc2000${T}
        fim_out_pp10000${T}
        fim_out_pp25000${T}
        fim_out_pmsa000${T}
        fim_out_pso2000${T}
        fim_out_s1ea000${T}
        fim_out_s2ea000${T}
        fim_out_s3ea000${T}
        fim_out_s4ea000${T}
        fim_out_sulf000${T}
        fim_out_pno2000${T}
        fim_out_ppno000${T}
        fim_out_ppo3000${T}
        fim_out_hno3000${T}
        fim_out_h2o2000${T}
        fim_out_pald000${T}
        fim_out_hcho000${T}
        fim_out_pop1000${T}
        fim_out_pop2000${T}
        fim_out_ppaa000${T}
        fim_out_ora1000${T}
        fim_out_ora2000${T}
        fim_out_pnh3000${T}
        fim_out_n2o5000${T}
        fim_out_pno3000${T}
        fim_out_ppan000${T}
        fim_out_phc3000${T}
        fim_out_phc3000${T}
        fim_out_phc8000${T}
        fim_out_peth000${T}
        fim_out_ppco000${T}
        fim_out_pete000${T}
        fim_out_polt000${T}
        fim_out_poli000${T}
        fim_out_ptol000${T}
        fim_out_pxyl000${T}
        fim_out_aco3000${T}
        fim_out_tpan000${T}
        fim_out_hono000${T}
        fim_out_hno4000${T}
        fim_out_pket000${T}
        fim_out_pgly000${T}
        fim_out_mgly000${T}
        fim_out_pdcb000${T}
        fim_out_onit000${T}
        fim_out_pcsl000${T}
        fim_out_piso000${T}
        fim_out_pco2000${T}
        fim_out_pch4000${T}
        fim_out_pudd000${T}
        fim_out_hket000${T}
        fim_out_papi000${T}
        fim_out_plim000${T}
        fim_out_dien000${T}
        fim_out_macr000${T}
        fim_out_ppho000${T}
        fim_out_pho2000${T}
##
## delete fim_out*D* fim_out*P0*
##
##   J. Henderson    02/2015

# Print out value of required environment variables
echo entering purge_fimchem_fimout.ksh....
echo "FIMRUN         = ${FIMRUN}"
echo "yyyymmddhh     = ${yyyymmddhh}"
echo "T              = ${T}"

# Delete subset of fim_out files for specified cycle, yyyymmddhh
 cd ${FIMRUN}/${yyyymmddhh}/fim_C
 fimout_files=`find ./ \( -name "fim_out*D*${T}hr" -o -name "fim*P0*${T}hr" -a -not -name "fim_out*hgtP*${T}hr" \)`
 cmd=`rm -f $fimout_files`
 echo "Removing $fimout_files for ${FIMRUN}/${yyyymmddhh}/fim_C"
 $cmd
 status=$?
 if [ $status != 0 ]; then
   echo "error $status deleting fimout files for time $T "
   return $status
 fi
