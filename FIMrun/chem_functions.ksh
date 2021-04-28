CONTEXT="chem_functions.ksh"

function chem_on
{
  re="^[ \t]*chem_opt[ \t]*=[ \t]*"
  grep "$re" FIMnamelist | sed "s/$re//;s/[ \t].*$//" | read chem_opt
  test -n "$chem_opt" -a "$chem_opt" -gt 0 && true || false
}

function get_chem_opt_value
{
  re="^[ \t]*chem_opt[ \t]*=[ \t]*"
  grep "$re" FIMnamelist | sed "s/$re//;s/[ \t].*$//"
}

function get_chem_in_opt_value
{
  re="^[ \t]*chem_in_opt[ \t]*=[ \t]*"
  grep "$re" FIMnamelist | sed "s/$re//;s/[ \t].*$//"
}

function get_kemit_value
{
  re="^[ \t]*kemit[ \t]*=[ \t]*"
  grep "$re" FIMnamelist | sed "s/$re//;s/[ \t].*$//"
}

function test_suite
{
  print $PWD | grep -q "/FIMtest/"
}

function get_fcst_len
{
  re="^[ \t]*TotalTime[ \t]*=[ \t]*"
  grep "$re" FIMnamelist | sed "s/$re//;s/[ \t].*$//"
}

function chem_fim_setup
{
  print  "IN chem_fim_setup"
  CO2_DATA_DIR="/mnt/lfs0/projects/co2/andy/FIM/CT1x1_to_G7/2011_fluxes_G7/binary"
  CO2_FILE_ROOT="co2_flux."

  # get current run time
  get_runtime
  print "in chem_fim_setup: CHEMFLAG: $CHEMFLAG"
  if [[ $CHEMFLAG == "true" ]]
  then
    test -z "$CHEM_DATADIR" && get_nl_value $NLFILE QUEUEnamelist chem_datadir CHEM_DATADIR
    # Link "chem" files
   for x in erod1.dat erod2.dat erod3.dat prep_chem_sources_template.inp htapLL_to_fimN.bin clay.dat sand.dat \
            glvl.dat icos_grid_info_level.dat icos_grid_level.dat p_gocart.dat #dm0.dat #no3.dat oh.dat h2o2.dat
    do
      linksafe $CHEM_DATADIR/$x
    done

   # Link prep_chem_sources executable depending on compiler
   BUILD_CONFIG=$1
   print "in chem_fim_setup: BUILD_CONFIG: $BUILD_CONFIG"
   if [[ $BUILD_CONFIG =~ "lahey" ]] ; then
      linksafe $CHEM_DATADIR/prep_chem_sources.lahey prep_chem_sources
   else
      linksafe $CHEM_DATADIR/prep_chem_sources
   fi

   # Link anth emiss files with monthly variation
   for x in e_bc.dat e_oc.dat e_pm_10.dat e_pm_25.dat e_so2.dat e_sulf.dat \
            e_ald.dat e_co.dat e_csl.dat e_dms.dat e_eth.dat e_hc3.dat e_hc5.dat e_hc8.dat e_hcho.dat e_iso.dat e_ket.dat e_nh3.dat e_no2.dat e_no.dat e_oli.dat e_olt.dat e_ora2.dat e_tol.dat e_xyl.dat no3.dat oh.dat h2o2.dat dm0.dat
    do
      linksafe $CHEM_DATADIR/$mm/$x
    done

    test_suite && linksafe $CHEM_DATADIR/volcanic.dat

    # link files for co2
    chem_opt_value=$(get_chem_opt_value)
    if [[ $chem_opt_value == "500" ]] 
    then
      kemit=$(get_kemit_value)
      print "kemit:  $kemit"
      if [[ $kemit == 1 ]]
      then
        fcst_len=$(get_fcst_len)  # ASSUMING THIS IS IN HOURS!
        print "fcst_len:  $fcst_len"
        fcst=0
        while [[ $fcst -le $fcst_len ]] 
        do
          fileDate=$(date +%Y%m%d%H -u -d "$mm/$dd/$yr $hh:00 $fcst hours")
          print "fileDate: $fileDate"
          fileName="${fileDate}.bin" 
          print "cmd:  linksafe $CO2_DATA_DIR/$CO2_FILE_ROOT$fileName"
          linksafe $CO2_DATA_DIR/$CO2_FILE_ROOT$fileName
          fcst=$(expr ${fcst} + 3) 
        done
      fi
    fi 
   # Link "prep" files
#    for x in dm0.dat no3.dat oh.dat h2o2.dat
#    for x in icos_grid_info_level.dat icos_grid_level.dat
#    do
#      linksafe $PREP/$x
#    done
    chem_opt_value=$(get_chem_opt_value)
    # get current run time
    get_runtime

    # get date of initialization files
    get_last_run_date
    emiss_date="$yr-$mm-$dd-$hh" # default value for branch testing
    test -n "$WFM" && emiss_date="$yr-$mm-$dd-$hh"
    print "emiss_date: $emiss_date"
    print "yr: $yr mm: $mm dd: $dd hh: $hh"
    print "*** init_date_dir: $init_date_dir fcst: $fcst"
    # put date in input file
    sed_safe_chem_datadir=$(print $CHEM_DATADIR | sed 's/\//\\\//g')
    sed "s/\(ihour=\),/\1${hh},/;
         s/\(iday=\),/\1${dd},/;
         s/\(imon=\),/\1${mm},/;
         s/\(iyear=\),/\1${yr},/;
         s/__CHEM_DATADIR__/$sed_safe_chem_datadir/g" \
           prep_chem_sources_template.inp > prep_chem_sources.inp
    ./prep_chem_sources || fail "ERROR: prep_chem_sources failed."
######link anthropogenic emissions:
#    linksafe FIM-T-${emiss_date}0000-BC-ab.bin e_bc.dat    
#    linksafe FIM-T-${emiss_date}0000-OC-ab.bin e_oc.dat    
#    linksafe FIM-T-${emiss_date}0000-URBAN3-ab.bin e_pm_10.dat 
#    linksafe FIM-T-${emiss_date}0000-URBAN2-ab.bin e_pm_25.dat 
#    linksafe FIM-T-${emiss_date}0000-SO2-ab.bin e_so2.dat   
#    linksafe FIM-T-${emiss_date}0000-SO4-ab.bin e_sulf.dat     
#    linksafe FIM-T-${emiss_date}0000-ALD-ab.bin e_ald.dat     
#    linksafe FIM-T-${emiss_date}0000-ASH-ab.bin e_ash.dat     
#    linksafe FIM-T-${emiss_date}0000-CO-ab.bin e_co.dat     
#    linksafe FIM-T-${emiss_date}0000-CSL-ab.bin e_csl.dat     
#    linksafe FIM-T-${emiss_date}0000-DMS-ab.bin e_dms.dat     
#    linksafe FIM-T-${emiss_date}0000-ETH-ab.bin e_eth.dat     
#    linksafe FIM-T-${emiss_date}0000-HC3-ab.bin e_hc3.dat     
#    linksafe FIM-T-${emiss_date}0000-HC5-ab.bin e_hc5.dat     
#    linksafe FIM-T-${emiss_date}0000-HC8-ab.bin e_hc8.dat     
#    linksafe FIM-T-${emiss_date}0000-HCHO-ab.bin e_hcho.dat     
#    linksafe FIM-T-${emiss_date}0000-ISO-ab.bin e_iso.dat     
#    linksafe FIM-T-${emiss_date}0000-KET-ab.bin e_ket.dat     
#    linksafe FIM-T-${emiss_date}0000-NH3-ab.bin e_nh3.dat     
#    linksafe FIM-T-${emiss_date}0000-NO2-ab.bin e_no2.dat     
#    linksafe FIM-T-${emiss_date}0000-NO-ab.bin e_no.dat     
#    linksafe FIM-T-${emiss_date}0000-OLI-ab.bin e_oli.dat     
#    linksafe FIM-T-${emiss_date}0000-OLT-ab.bin e_olt.dat     
#    linksafe FIM-T-${emiss_date}0000-ORA2-ab.bin e_ora2.dat     
#    linksafe FIM-T-${emiss_date}0000-TOL-ab.bin e_tol.dat     
#    linksafe FIM-T-${emiss_date}0000-XYL-ab.bin e_xyl.dat     
######link BG emissions
#    linksafe FIM-T-${emiss_date}0000-g1-gocartBG-DMS.bin dm0.dat     
######link fire emissions
    linksafe FIM-T-${emiss_date}0000-plume.bin plumestuff.dat
    linksafe FIM-T-${emiss_date}0000-OC-bb.bin ebu_oc.dat
    linksafe FIM-T-${emiss_date}0000-BC-bb.bin ebu_bc.dat
    linksafe FIM-T-${emiss_date}0000-BBURN2-bb.bin ebu_pm25.dat 
    linksafe FIM-T-${emiss_date}0000-BBURN3-bb.bin ebu_pm10.dat 
    linksafe FIM-T-${emiss_date}0000-SO2-bb.bin ebu_so2.dat
    linksafe FIM-T-${emiss_date}0000-SO4-bb.bin ebu_sulf.dat
    linksafe FIM-T-${emiss_date}0000-ALD-bb.bin ebu_ald.dat    
#    linksafe FIM-T-${emiss_date}0000-ASH-bb.bin ebu_ash.dat    
    linksafe FIM-T-${emiss_date}0000-CO-bb.bin ebu_co.dat    
    linksafe FIM-T-${emiss_date}0000-CSL-bb.bin ebu_csl.dat    
    linksafe FIM-T-${emiss_date}0000-DMS-bb.bin ebu_dms.dat    
    linksafe FIM-T-${emiss_date}0000-ETH-bb.bin ebu_eth.dat    
    linksafe FIM-T-${emiss_date}0000-HC3-bb.bin ebu_hc3.dat    
    linksafe FIM-T-${emiss_date}0000-HC5-bb.bin ebu_hc5.dat    
    linksafe FIM-T-${emiss_date}0000-HC8-bb.bin ebu_hc8.dat    
    linksafe FIM-T-${emiss_date}0000-HCHO-bb.bin ebu_hcho.dat    
    linksafe FIM-T-${emiss_date}0000-ISO-bb.bin ebu_iso.dat    
    linksafe FIM-T-${emiss_date}0000-KET-bb.bin ebu_ket.dat    
    linksafe FIM-T-${emiss_date}0000-NH3-bb.bin ebu_nh3.dat    
    linksafe FIM-T-${emiss_date}0000-NO2-bb.bin ebu_no2.dat    
    linksafe FIM-T-${emiss_date}0000-NO-bb.bin ebu_no.dat    
    linksafe FIM-T-${emiss_date}0000-OLI-bb.bin ebu_oli.dat    
    linksafe FIM-T-${emiss_date}0000-OLT-bb.bin ebu_olt.dat    
    linksafe FIM-T-${emiss_date}0000-ORA2-bb.bin ebu_ora2.dat    
    linksafe FIM-T-${emiss_date}0000-TOL-bb.bin ebu_tol.dat    
    linksafe FIM-T-${emiss_date}0000-XYL-bb.bin ebu_xyl.dat    

    if [[ $test_suite -eq 0 ]]
    then
      if [[ -s "FIM-T-${emiss_date}0000-g1-volc.bin" ]]
      then
        linksafe FIM-T-${emiss_date}0000-g1-volc.bin volcanic.dat
      fi
    fi

    for x in glvl.dat icos_grid_info_level.dat icos_grid_level.dat
    do
      linksafe $PREP/$x
    done
    print "chem_opt_value: $chem_opt_value"
    chem_in_opt_value=$(get_chem_in_opt_value)
    print "chem_in_opt_value: $chem_in_opt_value"
    if [[ $chem_in_opt_value == "1" ]]
    then
    if [[ -n "$WFM" && $init_date_dir != "NOT FOUND" ]] 
    then
      if [[ $chem_opt_value == "317" ]] 
      then
        linksafe ${init_date_dir}/fim_out_ash1000${fcst}${ARCHVTIMEUNIT} vash1.in
        linksafe ${init_date_dir}/fim_out_ash2000${fcst}${ARCHVTIMEUNIT} vash2.in
        linksafe ${init_date_dir}/fim_out_ash3000${fcst}${ARCHVTIMEUNIT} vash3.in
        linksafe ${init_date_dir}/fim_out_ash4000${fcst}${ARCHVTIMEUNIT} vash4.in
      fi
      if [[ $chem_opt_value == "502" ]] 
      then
        linksafe ${init_date_dir}/fim_out_ash1000${fcst}${ARCHVTIMEUNIT} vash1.in
        linksafe ${init_date_dir}/fim_out_ash2000${fcst}${ARCHVTIMEUNIT} vash2.in
        linksafe ${init_date_dir}/fim_out_ash3000${fcst}${ARCHVTIMEUNIT} vash3.in
        linksafe ${init_date_dir}/fim_out_ash4000${fcst}${ARCHVTIMEUNIT} vash4.in
      fi
      if [[ $chem_opt_value -ge "300"  && $chem_opt_value -lt "400" ]] 
      then
        linksafe ${init_date_dir}/fim_out_s4ea000${fcst}${ARCHVTIMEUNIT} seas4.in
        linksafe ${init_date_dir}/fim_out_s3ea000${fcst}${ARCHVTIMEUNIT} seas3.in
        linksafe ${init_date_dir}/fim_out_s2ea000${fcst}${ARCHVTIMEUNIT} seas2.in
        linksafe ${init_date_dir}/fim_out_s1ea000${fcst}${ARCHVTIMEUNIT} seas1.in
        linksafe ${init_date_dir}/fim_out_sulf000${fcst}${ARCHVTIMEUNIT} sulf.in
        linksafe ${init_date_dir}/fim_out_pso2000${fcst}${ARCHVTIMEUNIT} so2.in
        linksafe ${init_date_dir}/fim_out_pbc2000${fcst}${ARCHVTIMEUNIT} bc2.in
        linksafe ${init_date_dir}/fim_out_pbc1000${fcst}${ARCHVTIMEUNIT} bc1.in
        linksafe ${init_date_dir}/fim_out_obc2000${fcst}${ARCHVTIMEUNIT} oc2.in
        linksafe ${init_date_dir}/fim_out_obc1000${fcst}${ARCHVTIMEUNIT} oc1.in
        linksafe ${init_date_dir}/fim_out_d5st000${fcst}${ARCHVTIMEUNIT} dust5.in
        linksafe ${init_date_dir}/fim_out_d4st000${fcst}${ARCHVTIMEUNIT} dust4.in
        linksafe ${init_date_dir}/fim_out_d3st000${fcst}${ARCHVTIMEUNIT} dust3.in
        linksafe ${init_date_dir}/fim_out_d2st000${fcst}${ARCHVTIMEUNIT} dust2.in
        linksafe ${init_date_dir}/fim_out_d1st000${fcst}${ARCHVTIMEUNIT} dust1.in
        linksafe ${init_date_dir}/fim_out_pp25000${fcst}${ARCHVTIMEUNIT} p25.in
        linksafe ${init_date_dir}/fim_out_pp10000${fcst}${ARCHVTIMEUNIT} p10.in
        linksafe ${init_date_dir}/fim_out_dms1000${fcst}${ARCHVTIMEUNIT} dms.in
        linksafe ${init_date_dir}/fim_out_pmsa000${fcst}${ARCHVTIMEUNIT} msa.in
      fi

      if [[ $chem_opt_value == "301" ]] 
      then
        linksafe ${init_date_dir}/fim_out_pno2000${fcst}${ARCHVTIMEUNIT} no2.in
        linksafe ${init_date_dir}/fim_out_ppno000${fcst}${ARCHVTIMEUNIT} no.in
        linksafe ${init_date_dir}/fim_out_ppo3000${fcst}${ARCHVTIMEUNIT} o3.in
        linksafe ${init_date_dir}/fim_out_hno3000${fcst}${ARCHVTIMEUNIT} hno3.in
        linksafe ${init_date_dir}/fim_out_h2o2000${fcst}${ARCHVTIMEUNIT} h2o2.in
        linksafe ${init_date_dir}/fim_out_pald000${fcst}${ARCHVTIMEUNIT} ald.in
        linksafe ${init_date_dir}/fim_out_hcho000${fcst}${ARCHVTIMEUNIT} hcho.in
        linksafe ${init_date_dir}/fim_out_pop1000${fcst}${ARCHVTIMEUNIT} op1.in
        linksafe ${init_date_dir}/fim_out_pop2000${fcst}${ARCHVTIMEUNIT} op2.in
        linksafe ${init_date_dir}/fim_out_ppaa000${fcst}${ARCHVTIMEUNIT} paa.in
        linksafe ${init_date_dir}/fim_out_ora1000${fcst}${ARCHVTIMEUNIT} ora1.in
        linksafe ${init_date_dir}/fim_out_ora2000${fcst}${ARCHVTIMEUNIT} ora2.in
        linksafe ${init_date_dir}/fim_out_pnh3000${fcst}${ARCHVTIMEUNIT} nh3.in
        linksafe ${init_date_dir}/fim_out_n2o5000${fcst}${ARCHVTIMEUNIT} n2o5.in
        linksafe ${init_date_dir}/fim_out_pno3000${fcst}${ARCHVTIMEUNIT} no3.in
        linksafe ${init_date_dir}/fim_out_ppan000${fcst}${ARCHVTIMEUNIT} pan.in
        linksafe ${init_date_dir}/fim_out_phc3000${fcst}${ARCHVTIMEUNIT} hc3.in
        linksafe ${init_date_dir}/fim_out_phc5000${fcst}${ARCHVTIMEUNIT} hc5.in
        linksafe ${init_date_dir}/fim_out_phc8000${fcst}${ARCHVTIMEUNIT} hc8.in
        linksafe ${init_date_dir}/fim_out_peth000${fcst}${ARCHVTIMEUNIT} eth.in
        linksafe ${init_date_dir}/fim_out_ppco000${fcst}${ARCHVTIMEUNIT} co.in
        linksafe ${init_date_dir}/fim_out_pete000${fcst}${ARCHVTIMEUNIT} ete.in
        linksafe ${init_date_dir}/fim_out_polt000${fcst}${ARCHVTIMEUNIT} olt.in
        linksafe ${init_date_dir}/fim_out_poli000${fcst}${ARCHVTIMEUNIT} oli.in
        linksafe ${init_date_dir}/fim_out_ptol000${fcst}${ARCHVTIMEUNIT} tol.in
        linksafe ${init_date_dir}/fim_out_pxyl000${fcst}${ARCHVTIMEUNIT} xyl.in
        linksafe ${init_date_dir}/fim_out_aco3000${fcst}${ARCHVTIMEUNIT} aco3.in
        linksafe ${init_date_dir}/fim_out_tpan000${fcst}${ARCHVTIMEUNIT} tpan.in
        linksafe ${init_date_dir}/fim_out_hono000${fcst}${ARCHVTIMEUNIT} hono.in
        linksafe ${init_date_dir}/fim_out_hno4000${fcst}${ARCHVTIMEUNIT} hno4.in
        linksafe ${init_date_dir}/fim_out_pket000${fcst}${ARCHVTIMEUNIT} ket.in
        linksafe ${init_date_dir}/fim_out_pgly000${fcst}${ARCHVTIMEUNIT} gly.in
        linksafe ${init_date_dir}/fim_out_mgly000${fcst}${ARCHVTIMEUNIT} mgly.in
        linksafe ${init_date_dir}/fim_out_pdcb000${fcst}${ARCHVTIMEUNIT} dcb.in
        linksafe ${init_date_dir}/fim_out_onit000${fcst}${ARCHVTIMEUNIT} onit.in
        linksafe ${init_date_dir}/fim_out_pcsl000${fcst}${ARCHVTIMEUNIT} csl.in
        linksafe ${init_date_dir}/fim_out_piso000${fcst}${ARCHVTIMEUNIT} iso.in
        linksafe ${init_date_dir}/fim_out_pco2000${fcst}${ARCHVTIMEUNIT} co2.in
        linksafe ${init_date_dir}/fim_out_pch4000${fcst}${ARCHVTIMEUNIT} ch4.in
        linksafe ${init_date_dir}/fim_out_pudd000${fcst}${ARCHVTIMEUNIT} udd.in
        linksafe ${init_date_dir}/fim_out_hket000${fcst}${ARCHVTIMEUNIT} hket.in
        linksafe ${init_date_dir}/fim_out_papi000${fcst}${ARCHVTIMEUNIT} api.in
        linksafe ${init_date_dir}/fim_out_plim000${fcst}${ARCHVTIMEUNIT} lim.in
        linksafe ${init_date_dir}/fim_out_dien000${fcst}${ARCHVTIMEUNIT} dien.in
        linksafe ${init_date_dir}/fim_out_macr000${fcst}${ARCHVTIMEUNIT} macr.in
        linksafe ${init_date_dir}/fim_out_ppho000${fcst}${ARCHVTIMEUNIT} ho.in
        linksafe ${init_date_dir}/fim_out_pho2000${fcst}${ARCHVTIMEUNIT} ho2.in
      fi

      if [[ $chem_opt_value == "108" ]]
      then
        linksafe ${init_date_dir}/fim_out_sulf000${fcst}${ARCHVTIMEUNIT} sulf.in
        linksafe ${init_date_dir}/fim_out_pso2000${fcst}${ARCHVTIMEUNIT} so2.in
        linksafe ${init_date_dir}/fim_out_pno2000${fcst}${ARCHVTIMEUNIT} no2.in
        linksafe ${init_date_dir}/fim_out_ppno000${fcst}${ARCHVTIMEUNIT} no.in
        linksafe ${init_date_dir}/fim_out_ppo3000${fcst}${ARCHVTIMEUNIT} o3.in
        linksafe ${init_date_dir}/fim_out_hno3000${fcst}${ARCHVTIMEUNIT} hno3.in
        linksafe ${init_date_dir}/fim_out_h2o2000${fcst}${ARCHVTIMEUNIT} h2o2.in
        linksafe ${init_date_dir}/fim_out_pald000${fcst}${ARCHVTIMEUNIT} ald.in
        linksafe ${init_date_dir}/fim_out_hcho000${fcst}${ARCHVTIMEUNIT} hcho.in
        linksafe ${init_date_dir}/fim_out_pop1000${fcst}${ARCHVTIMEUNIT} op1.in
        linksafe ${init_date_dir}/fim_out_pop2000${fcst}${ARCHVTIMEUNIT} op2.in
        linksafe ${init_date_dir}/fim_out_ppaa000${fcst}${ARCHVTIMEUNIT} paa.in
        linksafe ${init_date_dir}/fim_out_ora1000${fcst}${ARCHVTIMEUNIT} ora1.in
        linksafe ${init_date_dir}/fim_out_ora2000${fcst}${ARCHVTIMEUNIT} ora2.in
        linksafe ${init_date_dir}/fim_out_pnh3000${fcst}${ARCHVTIMEUNIT} nh3.in
        linksafe ${init_date_dir}/fim_out_n2o5000${fcst}${ARCHVTIMEUNIT} n2o5.in
        linksafe ${init_date_dir}/fim_out_pno3000${fcst}${ARCHVTIMEUNIT} no3.in
        linksafe ${init_date_dir}/fim_out_ppan000${fcst}${ARCHVTIMEUNIT} pan.in
        linksafe ${init_date_dir}/fim_out_phc3000${fcst}${ARCHVTIMEUNIT} hc3.in
        linksafe ${init_date_dir}/fim_out_phc5000${fcst}${ARCHVTIMEUNIT} hc5.in
        linksafe ${init_date_dir}/fim_out_phc8000${fcst}${ARCHVTIMEUNIT} hc8.in
        linksafe ${init_date_dir}/fim_out_peth000${fcst}${ARCHVTIMEUNIT} eth.in
        linksafe ${init_date_dir}/fim_out_ppco000${fcst}${ARCHVTIMEUNIT} co.in
        linksafe ${init_date_dir}/fim_out_pete000${fcst}${ARCHVTIMEUNIT} ete.in
        linksafe ${init_date_dir}/fim_out_polt000${fcst}${ARCHVTIMEUNIT} olt.in
        linksafe ${init_date_dir}/fim_out_poli000${fcst}${ARCHVTIMEUNIT} oli.in
        linksafe ${init_date_dir}/fim_out_ptol000${fcst}${ARCHVTIMEUNIT} tol.in
        linksafe ${init_date_dir}/fim_out_pxyl000${fcst}${ARCHVTIMEUNIT} xyl.in
        linksafe ${init_date_dir}/fim_out_aco3000${fcst}${ARCHVTIMEUNIT} aco3.in
        linksafe ${init_date_dir}/fim_out_tpan000${fcst}${ARCHVTIMEUNIT} tpan.in
        linksafe ${init_date_dir}/fim_out_hono000${fcst}${ARCHVTIMEUNIT} hono.in
        linksafe ${init_date_dir}/fim_out_hno4000${fcst}${ARCHVTIMEUNIT} hno4.in
        linksafe ${init_date_dir}/fim_out_pket000${fcst}${ARCHVTIMEUNIT} ket.in
        linksafe ${init_date_dir}/fim_out_pgly000${fcst}${ARCHVTIMEUNIT} gly.in
        linksafe ${init_date_dir}/fim_out_mgly000${fcst}${ARCHVTIMEUNIT} mgly.in
        linksafe ${init_date_dir}/fim_out_pdcb000${fcst}${ARCHVTIMEUNIT} dcb.in
        linksafe ${init_date_dir}/fim_out_onit000${fcst}${ARCHVTIMEUNIT} onit.in
        linksafe ${init_date_dir}/fim_out_pcsl000${fcst}${ARCHVTIMEUNIT} csl.in
        linksafe ${init_date_dir}/fim_out_piso000${fcst}${ARCHVTIMEUNIT} iso.in
        linksafe ${init_date_dir}/fim_out_pco2000${fcst}${ARCHVTIMEUNIT} co2.in
        linksafe ${init_date_dir}/fim_out_pch4000${fcst}${ARCHVTIMEUNIT} ch4.in
        linksafe ${init_date_dir}/fim_out_pudd000${fcst}${ARCHVTIMEUNIT} udd.in
        linksafe ${init_date_dir}/fim_out_hket000${fcst}${ARCHVTIMEUNIT} hket.in
        linksafe ${init_date_dir}/fim_out_papi000${fcst}${ARCHVTIMEUNIT} api.in
        linksafe ${init_date_dir}/fim_out_plim000${fcst}${ARCHVTIMEUNIT} lim.in
        linksafe ${init_date_dir}/fim_out_dien000${fcst}${ARCHVTIMEUNIT} dien.in
        linksafe ${init_date_dir}/fim_out_macr000${fcst}${ARCHVTIMEUNIT} macr.in
        linksafe ${init_date_dir}/fim_out_ppho000${fcst}${ARCHVTIMEUNIT} ho.in
        linksafe ${init_date_dir}/fim_out_pho2000${fcst}${ARCHVTIMEUNIT} ho2.in
        linksafe ${init_date_dir}/fim_out_hace000${fcst}${ARCHVTIMEUNIT} hace.in
        linksafe ${init_date_dir}/fim_out_ishp000${fcst}${ARCHVTIMEUNIT} ishp.in
        linksafe ${init_date_dir}/fim_out_ison000${fcst}${ARCHVTIMEUNIT} ison.in
        linksafe ${init_date_dir}/fim_out_mahp000${fcst}${ARCHVTIMEUNIT} mahp.in
        linksafe ${init_date_dir}/fim_out_mpan000${fcst}${ARCHVTIMEUNIT} mpan.in
        linksafe ${init_date_dir}/fim_out_nald000${fcst}${ARCHVTIMEUNIT} nald.in
        linksafe ${init_date_dir}/fim_out_sesq000${fcst}${ARCHVTIMEUNIT} sesq.in
        linksafe ${init_date_dir}/fim_out_pmbo000${fcst}${ARCHVTIMEUNIT} mbo.in
        linksafe ${init_date_dir}/fim_out_cva1000${fcst}${ARCHVTIMEUNIT} cva1.in
        linksafe ${init_date_dir}/fim_out_cva2000${fcst}${ARCHVTIMEUNIT} cva2.in
        linksafe ${init_date_dir}/fim_out_cva3000${fcst}${ARCHVTIMEUNIT} cva3.in
        linksafe ${init_date_dir}/fim_out_cva4000${fcst}${ARCHVTIMEUNIT} cva4.in
        linksafe ${init_date_dir}/fim_out_cvb1000${fcst}${ARCHVTIMEUNIT} cvb1.in
        linksafe ${init_date_dir}/fim_out_cvb2000${fcst}${ARCHVTIMEUNIT} cvb2.in
        linksafe ${init_date_dir}/fim_out_cvb3000${fcst}${ARCHVTIMEUNIT} cvb3.in
        linksafe ${init_date_dir}/fim_out_cvb4000${fcst}${ARCHVTIMEUNIT} cvb4.in
        linksafe ${init_date_dir}/fim_out_soaj000${fcst}${ARCHVTIMEUNIT} soaj.in
        linksafe ${init_date_dir}/fim_out_soai000${fcst}${ARCHVTIMEUNIT} soai.in
        linksafe ${init_date_dir}/fim_out_nhaj000${fcst}${ARCHVTIMEUNIT} nhaj.in
        linksafe ${init_date_dir}/fim_out_nhai000${fcst}${ARCHVTIMEUNIT} nhai.in
        linksafe ${init_date_dir}/fim_out_n3aj000${fcst}${ARCHVTIMEUNIT} n3aj.in
        linksafe ${init_date_dir}/fim_out_n3ai000${fcst}${ARCHVTIMEUNIT} n3ai.in
        linksafe ${init_date_dir}/fim_out_naaj000${fcst}${ARCHVTIMEUNIT} naaj.in
        linksafe ${init_date_dir}/fim_out_naai000${fcst}${ARCHVTIMEUNIT} naai.in
        linksafe ${init_date_dir}/fim_out_claj000${fcst}${ARCHVTIMEUNIT} claj.in
        linksafe ${init_date_dir}/fim_out_clai000${fcst}${ARCHVTIMEUNIT} clai.in
        linksafe ${init_date_dir}/fim_out_as1j000${fcst}${ARCHVTIMEUNIT} as1j.in
        linksafe ${init_date_dir}/fim_out_as1i000${fcst}${ARCHVTIMEUNIT} as1i.in
        linksafe ${init_date_dir}/fim_out_as2j000${fcst}${ARCHVTIMEUNIT} as2j.in
        linksafe ${init_date_dir}/fim_out_as2i000${fcst}${ARCHVTIMEUNIT} as2i.in
        linksafe ${init_date_dir}/fim_out_as3j000${fcst}${ARCHVTIMEUNIT} as3j.in
        linksafe ${init_date_dir}/fim_out_as3i000${fcst}${ARCHVTIMEUNIT} as3i.in
        linksafe ${init_date_dir}/fim_out_as4j000${fcst}${ARCHVTIMEUNIT} as4j.in
        linksafe ${init_date_dir}/fim_out_as4i000${fcst}${ARCHVTIMEUNIT} as4i.in
        linksafe ${init_date_dir}/fim_out_bs1j000${fcst}${ARCHVTIMEUNIT} bs1j.in
        linksafe ${init_date_dir}/fim_out_bs1i000${fcst}${ARCHVTIMEUNIT} bs1i.in
        linksafe ${init_date_dir}/fim_out_bs2j000${fcst}${ARCHVTIMEUNIT} bs2j.in
        linksafe ${init_date_dir}/fim_out_bs2i000${fcst}${ARCHVTIMEUNIT} bs2i.in
        linksafe ${init_date_dir}/fim_out_bs3j000${fcst}${ARCHVTIMEUNIT} bs3j.in
        linksafe ${init_date_dir}/fim_out_bs3i000${fcst}${ARCHVTIMEUNIT} bs3i.in
        linksafe ${init_date_dir}/fim_out_bs4j000${fcst}${ARCHVTIMEUNIT} bs4j.in
        linksafe ${init_date_dir}/fim_out_bs4i000${fcst}${ARCHVTIMEUNIT} bs4i.in
        linksafe ${init_date_dir}/fim_out_opaj000${fcst}${ARCHVTIMEUNIT} opaj.in
        linksafe ${init_date_dir}/fim_out_opai000${fcst}${ARCHVTIMEUNIT} opai.in
        linksafe ${init_date_dir}/fim_out_pecj000${fcst}${ARCHVTIMEUNIT} ecj.in
        linksafe ${init_date_dir}/fim_out_peci000${fcst}${ARCHVTIMEUNIT} eci.in
        linksafe ${init_date_dir}/fim_out_p25j000${fcst}${ARCHVTIMEUNIT} p25j.in
        linksafe ${init_date_dir}/fim_out_p25i000${fcst}${ARCHVTIMEUNIT} p25i.in
        linksafe ${init_date_dir}/fim_out_atha000${fcst}${ARCHVTIMEUNIT} atha.in
        linksafe ${init_date_dir}/fim_out_seas000${fcst}${ARCHVTIMEUNIT} seas.in
        linksafe ${init_date_dir}/fim_out_sila000${fcst}${ARCHVTIMEUNIT} sila.in
        linksafe ${init_date_dir}/fim_out_pnu0000${fcst}${ARCHVTIMEUNIT} nu0.in
        linksafe ${init_date_dir}/fim_out_pac0000${fcst}${ARCHVTIMEUNIT} ac0.in
        linksafe ${init_date_dir}/fim_out_corn000${fcst}${ARCHVTIMEUNIT} corn.in
      fi

      if [[ $chem_opt_value == "500" ]] 
      then
        linksafe ${init_date_dir}/fim_out_c13D000${fcst}${ARCHVTIMEUNIT} tr1.in
        linksafe ${init_date_dir}/fim_out_c23D000${fcst}${ARCHVTIMEUNIT} tr2.in
      fi
    fi
  fi
  fi
}

function get_runtime
{
  yr=${yyyymmddhhmm:0:4}
  mm=${yyyymmddhhmm:4:2}
  dd=${yyyymmddhhmm:6:2}
  hh=${yyyymmddhhmm:8:2}
}

function get_last_run_date
{
  get_runtime
  print "in get_last_run_date: yr: $yr mm: $mm dd: $dd hh: $hh"
  typeset -Z3 tmp_fcst
  tmp_fcst=0
  found=0
  while [[ $found -eq 0 ]]
  do
    tmp_fcst=$(expr ${tmp_fcst} + 12) 
    dirDate=$(date +%Y%m%d%H -u -d "$mm/$dd/$yr $hh:00 $tmp_fcst hours ago")
    dir=$FIM_RUN/fim_${GLVL}_${NVL}_${PES}_${dirDate}00/fim_${MEMBER_ID}
    print "checking: ${dir}/fim_out_dms1000${tmp_fcst}${ARCHVTIMEUNIT}"
    if [[ -s ${dir}/fim_out_hgtP000${tmp_fcst}${ARCHVTIMEUNIT} ]]
    then
      found=1
      init_date_dir=$dir
      print "FOUND init_date_dir: $init_date_dir"
    fi
    fcst=$tmp_fcst
    if [[ $fcst -gt 120 ]]
    then
      print "ERROR "
      init_date_dir="NOT FOUND"
      found=1
    fi
  done
}

function chem_prep_newname
{
  test $CHEMFLAG == "true" && (./newname.exe || fail "newname failed")
}

function chem_prep_setup
{
  if [[ $CHEMFLAG == "true" ]]
  then
    test -z "$CHEM_DATADIR" && get_nl_value $NLFILE QUEUEnamelist chem_datadir CHEM_DATADIR
    test_suite && linksafe $CHEM_DATADIR/volcanic.dat
    for x in erod_binary gocart_backgd_littlee dm0_binary anthro_binary \
      chemltln.dat
    do
      linksafe $CHEM_DATADIR/$x
    done
    linksafe $BINDIR/newname.exe
  fi
}
