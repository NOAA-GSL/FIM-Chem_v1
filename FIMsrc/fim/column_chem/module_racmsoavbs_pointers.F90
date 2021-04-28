Module module_racmsoavbs_pointers
USE module_initial_chem_namelists,only:p_so2,p_sulf,p_no2,p_no,p_o3, &
       p_hno3,p_h2o2,p_ald,p_hcho,p_op1,p_op2,p_paa,p_ora1,p_ora2,   &
       p_nh3,p_n2o5,p_no3,p_pan,p_hc3,p_hc5,p_hc8,p_eth,p_co,p_ete,  &
       p_olt,p_oli,p_tol,p_xyl,p_aco3,p_tpan,p_hono,p_hno4,p_ket,    &
       p_gly,p_mgly,p_dcb,p_onit,p_csl,p_iso,p_co2,p_ch4,p_udd,      &
       p_hket,p_api,p_lim,p_dien,p_macr,p_hace,p_ishp,p_ison,p_mahp, &
       p_mpan,p_nald,p_sesq,p_mbo,p_cvasoa1,p_cvasoa2,p_cvasoa3,     &
       p_cvasoa4,p_cvbsoa1,p_cvbsoa2,p_cvbsoa3,p_cvbsoa4,p_ho,p_ho2, &
       p_so4aj,p_so4ai,p_nh4aj,p_nh4ai,p_no3aj,p_no3ai,p_naaj,p_naai,&
       p_claj,p_clai,p_asoa1j,p_asoa1i,p_asoa2j,p_asoa2i,p_asoa3j,   &
       p_asoa3i,p_asoa4j,p_asoa4i,p_bsoa1j,p_bsoa1i,p_bsoa2j,p_bsoa2i,&
       p_bsoa3j,p_bsoa3i,p_bsoa4j,p_bsoa4i,p_orgpaj,p_orgpai,p_ecj,  &
       p_eci,p_p25j,p_p25i,p_antha,p_seas,p_soila,p_nu0,p_ac0,p_corn,&
! anthropogenic emissions
       p_e_iso,p_e_so2,p_e_no,p_e_no2,p_e_co,p_e_eth,p_e_hc3,p_e_hc5,&
       p_e_hc8,p_e_xyl,p_e_ol2,p_e_olt,p_e_oli,p_e_tol,p_e_csl,      &
       p_e_hcho,p_e_ald,p_e_ket,p_e_ora2,p_e_nh3,p_e_pm_25,p_e_pm_10,&
       p_e_oc,p_e_sulf,p_e_bc,p_e_dms,                               &
! dust and seasalt
       p_edust1,p_edust2,p_edust3,p_edust4,p_edust5,                 &
       p_eseas1,p_eseas2,p_eseas3,p_eseas4,                          &
! biomass burning emissions
       p_ebu_sulf,p_ebu_dms,p_ebu_csl,p_ebu_iso,p_ebu_no,p_ebu_no2,  &
       p_ebu_ald,p_ebu_hcho,p_ebu_ora2,p_ebu_hc3,p_ebu_hc5,p_ebu_hc8,&
       p_ebu_eth,p_ebu_co,p_ebu_olt,p_ebu_oli,p_ebu_oc,p_ebu_bc,     &
       p_ebu_pm25,p_ebu_pm10,p_ebu_ete,p_ebu_dien,p_ebu_api,         &
       p_ebu_lim,p_ebu_tol,p_ebu_ket,p_ebu_macr,p_ebu_ora1,          &
       p_ebu_so2,p_ebu_nh3,p_ebu_xyl,                                &
       p_ebu_in_sulf,p_ebu_in_dms,p_ebu_in_csl,p_ebu_in_iso,         &
       p_ebu_in_no,p_ebu_in_no2,p_ebu_in_ald,p_ebu_in_hcho,          &
       p_ebu_in_ora2,p_ebu_in_hc3,p_ebu_in_hc5,p_ebu_in_hc8,         &
       p_ebu_in_eth,p_ebu_in_co,p_ebu_in_olt,p_ebu_in_oli,           &
       p_ebu_in_oc,p_ebu_in_bc,p_ebu_in_pm25,p_ebu_in_pm10,          &
       p_ebu_in_ete,p_ebu_in_dien,p_ebu_in_api, p_ebu_in_lim,        &
       p_ebu_in_tol,p_ebu_in_ket,p_ebu_in_macr,p_ebu_in_ora1,        &
       p_ebu_in_so2,p_ebu_in_nh3,p_ebu_in_xyl


CONTAINS
!
! this module will initialize variable pointers for chem_opt "108"
! June 2015
!package   gocartracm_kpp     chem_opt==108                   -
!(chem:so2,sulf,no2,no,o3,hno3,h2o2,ald,hcho,op1,op2,paa,ora1,ora2,nh3,n2o5,no3,pan,hc3,hc5,hc8,eth,co,ete,olt,oli,tol,xyl,aco3,tpan,hono,hno4,ket,gly,mgly,dcb,onit,csl,iso,co2,ch4,udd,hket,api,lim,dien,macr,ho,ho2,dms,msa,p25,bc1,bc2,oc1,oc2,dust_1,dust_2,dust_3,dust_4,dust_5,seas_1,seas_2,seas_3,seas_4,p10
   subroutine racmsoavbs_pointers
  IMPLICIT NONE
!
! 51 variabkes for RACM gas phase
!
p_so2  = 1
p_sulf  = 2
p_no2  = 3
p_no  = 4
p_o3  = 5
p_hno3  = 6
p_h2o2  = 7
p_ald  = 8
p_hcho  = 9
p_op1  = 10
p_op2  = 11
p_paa  = 12
p_ora1  = 13
p_ora2  = 14
p_nh3  = 15
p_n2o5  = 16
p_no3  = 17
p_pan  = 18
p_hc3  = 19
p_hc5  = 20
p_hc8  = 21
p_eth  = 22
p_co  = 23
p_ete  = 24
p_olt  = 25
p_oli  = 26
p_tol  = 27
p_xyl  = 28
p_aco3  = 29
p_tpan  = 30
p_hono  = 31
p_hno4  = 32
p_ket  = 33
p_gly  = 34
p_mgly  = 35
p_dcb  = 36
p_onit  = 37
p_csl  = 38
p_iso  = 39
p_co2  = 40
p_ch4  = 41
p_udd  = 42
p_hket  = 43
p_api  = 44
p_lim  = 45
p_dien = 46
p_macr = 47
p_hace = 48
p_ishp = 49
p_ison = 50
p_mahp = 51
p_mpan = 52
p_nald = 53
p_sesq = 54
p_mbo  = 55
p_cvasoa1 = 56
p_cvasoa2 = 57
p_cvasoa3 = 58
p_cvasoa4 = 59
p_cvbsoa1 = 60
p_cvbsoa2 = 61
p_cvbsoa3 = 62
p_cvbsoa4 = 63
p_ho  = 64
p_ho2 = 65
p_so4aj = 66
p_so4ai = 67
p_nh4aj = 68
p_nh4ai = 69
p_no3aj = 70
p_no3ai = 71
p_naaj  = 72
p_naai  = 73
p_claj  = 74
p_clai  = 75
p_asoa1j = 76
p_asoa1i = 77
p_asoa2j = 78
p_asoa2i = 79
p_asoa3j = 80
p_asoa3i = 81
p_asoa4j = 82
p_asoa4i = 83
p_bsoa1j = 84
p_bsoa1i = 85
p_bsoa2j = 86
p_bsoa2i = 87
p_bsoa3j = 88
p_bsoa3i = 89
p_bsoa4j = 90
p_bsoa4i = 91
p_orgpaj = 92
p_orgpai = 93
p_ecj    = 94
p_eci    = 95
p_p25j   = 96
p_p25i   = 97
p_antha  = 98
p_seas   = 99
p_soila  = 100
p_nu0    = 101
p_ac0    = 102
p_corn   = 103
!
! emissions
!package   ecptec          emiss_opt==5                   -
!emis_ant:e_iso,e_so2,e_no,e_no2,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,e_pm_25,e_pm_10,e_oc,e_sulf,e_bc
p_e_iso = 1
p_e_so2 = 2
p_e_no = 3
p_e_no2 = 4
p_e_co = 5
p_e_eth = 6
p_e_hc3 = 7
p_e_hc5 = 8
p_e_hc8 = 9
p_e_xyl = 10
p_e_olt = 11
p_e_oli = 12
p_e_tol = 13
p_e_csl = 14
p_e_hcho = 15
p_e_ald = 16
p_e_ket = 17
p_e_ora2 = 18
p_e_nh3 = 19
p_e_pm_25 = 20
p_e_pm_10 = 21
p_e_oc = 22
p_e_sulf = 23
p_e_bc = 24
p_e_dms = 25

! biomass burning
p_ebu_iso = 1
p_ebu_so2 = 2
p_ebu_no = 3
p_ebu_no2=4
p_ebu_co =5
p_ebu_eth=6
p_ebu_hc3=7
p_ebu_hc5=8
p_ebu_hc8=9
p_ebu_xyl=10
p_ebu_olt=11
p_ebu_oli=12
p_ebu_tol=13
p_ebu_csl=14
p_ebu_hcho=15
p_ebu_ald=16
p_ebu_ket=17
p_ebu_ora2=18
p_ebu_nh3=19
p_ebu_pm25=20
p_ebu_pm10=21
p_ebu_oc=22
p_ebu_sulf=23
p_ebu_bc=24
p_ebu_dms=25

p_ebu_in_iso = 1
p_ebu_in_so2 = 2
p_ebu_in_no = 3
p_ebu_in_no2=4
p_ebu_in_co =5
p_ebu_in_eth=6
p_ebu_in_hc3=7
p_ebu_in_hc5=8
p_ebu_in_hc8=9
p_ebu_in_xyl=10
p_ebu_in_olt=11
p_ebu_in_oli=12
p_ebu_in_tol=13
p_ebu_in_csl=14
p_ebu_in_hcho=15
p_ebu_in_ald=16
p_ebu_in_ket=17
p_ebu_in_ora2=18
p_ebu_in_nh3=19
p_ebu_in_pm25=20
p_ebu_in_pm10=21
p_ebu_in_oc=22
p_ebu_in_sulf=23
p_ebu_in_bc=24
p_ebu_in_dms=25
!dust and seasalt
! diagnostic dust and seasale stuff
p_edust1=1
p_edust2=2
p_edust3=3
p_edust4=4
p_edust5=5
p_eseas1=1
p_eseas2=2
p_eseas3=3
p_eseas4=4

END SUBROUTINE racmsoavbs_pointers
END MODULE module_racmsoavbs_pointers
