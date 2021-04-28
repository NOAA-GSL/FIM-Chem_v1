!*********************************************************************
module module_chem_alloc
!	This module allocates variables used in chem
!  	Henderson          November      2008
!*********************************************************************
contains


subroutine chem_alloc(chem_opt,aer_ra_feedback)

use fimnamelist,only: nvl
use module_control,only: nip
use module_wrf_control,only: num_emis_ant,num_emis_vol,nvl_gocart,num_chem,num_ebu_in,num_ebu
use module_chem_variables,only: emiss_ab1,emiss_ab,pm25,p10,rcav,ero1,ero2,ero3,dm0, &
                                emiss_oc,emiss_bc,emiss_sulf,oh_backgd,h2o2_backgd,  &
                                no3_backgd,emiss_abu,plumestuff,emiss_ash_mass,&
                                emiss_ash_height,emiss_ash_dt,ashfall,rnav,tr1_tavg, &
                                emiss_tr_height,emiss_tr_mass,emiss_tr_dt,trfall,    &
                                clayfrac,sandfrac,emiss_co2,emiss_pm25,emiss_pm10,   &
                                emiss_ch4,emiss_sf6,&
                                emi_d1,emi_d2,emi_d3,emi_d4,emi_d5,&
                                d1st_ave,d2st_ave,&
                                d3st_ave,d4st_ave,d5st_ave,emid1_ave,emid2_ave,emid3_ave,&
                                emid4_ave,emid5_ave,aod2d_ave,oh_bg,h2o2_bg,no3_bg,  &
                                wet_dep,dry_dep,ebu_oc !lzhang

implicit none

integer, intent(IN) :: chem_opt,aer_ra_feedback

! always allocate these because they are passed 
! through an arglist
!TODO:  Create chem_internal_state and avoid all this complication.  
!TODO:  Avoid allocating *any* arrays that are not used!
print *,'DEBUG chem_alloc():  allocating...'
  allocate(pm25(nvl,nip))
  pm25=0.
  allocate(p10(nvl,nip))
  p10=0.
  allocate(tr1_tavg(nvl,nip))
  tr1_tavg=0.
  allocate(d1st_ave(nvl,nip))
  d1st_ave=0.
  allocate(d2st_ave(nvl,nip))
  d2st_ave=0.
  allocate(d3st_ave(nvl,nip))
  d3st_ave=0.
  allocate(d4st_ave(nvl,nip))
  d4st_ave=0.
  allocate(d5st_ave(nvl,nip))
  d5st_ave=0.
  allocate(ebu_oc(nvl,nip))
  ebu_oc=0.
  allocate(oh_bg(nvl,nip))
  oh_bg=0.
  allocate(h2o2_bg(nvl,nip))
  h2o2_bg=0.
  allocate(no3_bg(nvl,nip))
  no3_bg=0.
  allocate(oh_backgd(nvl_gocart,nip))
  oh_backgd=0.
  allocate( h2o2_backgd(nvl_gocart,nip) )
  allocate( no3_backgd(nvl_gocart,nip) )
  allocate( rcav(nip) )
  rcav = 0.
  allocate( rnav(nip) )
  rnav = 0.
  allocate( ero1(nip) )
  allocate( ero2(nip) )
  allocate( ero3(nip) )
  allocate( clayfrac(nip) )
  allocate( sandfrac(nip) )
  allocate( ashfall(nip) )
!  allocate( aod2d(nip) )
!  aod2d = 0.
  allocate( wet_dep(nip,num_chem) )
  wet_dep = 0.
  allocate( dry_dep(nip,num_chem) )
  dry_dep = 0.
  allocate( plumestuff(nip,8) )
  plumestuff = 0.
  allocate( emiss_ab(nip,num_emis_ant) )
  emiss_ab = 0.
  allocate( emiss_ab1(nip,num_emis_ant) )
  emiss_ab1 = 0.
  allocate( emiss_abu(nip,num_ebu_in) )
  emiss_abu = 0.
  allocate( emiss_ash_mass(nip) )
  emiss_ash_mass = 0.
  allocate( emiss_ash_height(nip) )
  emiss_ash_height = 0.
  allocate( emiss_ash_dt(nip) )
  emiss_ash_dt = 0.
  allocate( emiss_co2(nip) )
  emiss_co2 = 0.
  allocate( emiss_ch4(nip) )
  emiss_ch4 = 0.
  allocate( emiss_sf6(nip) )
  emiss_sf6 = 0.

!if(chem_opt == 500)then
    allocate( emiss_tr_mass(nip) )
    emiss_tr_mass = 0.
    allocate( emiss_tr_height(nip) )
    emiss_tr_height = 0.
    allocate( emiss_tr_dt(nip) )
    emiss_tr_dt = 0.
    ALLOCATE( trfall( nip, num_chem ) )
    trfall = 0.
! endif

  allocate( emiss_oc(nip) )
  emiss_oc = 0.
  allocate( emiss_bc(nip) )
  emiss_bc = 0.
  allocate( emiss_sulf(nip) )
  emiss_sulf = 0.
  allocate( emiss_pm25(nip) )
  emiss_pm25 = 0.
  allocate( emiss_pm10(nip) )
  emiss_pm10 = 0.
  allocate(  dm0(nip) )
  dm0 = 0.
  allocate( emi_d1(nip) )
  emi_d1 = 0.  
  allocate( emi_d2(nip) )
  emi_d2 = 0. 
  allocate( emi_d3(nip) )
  emi_d3 = 0. 
  allocate( emi_d4(nip) )
  emi_d4 = 0. 
  allocate( emi_d5(nip) )
  emi_d5 = 0. 
  allocate( emid1_ave(nip) )
  emid1_ave = 0.  
  allocate( emid2_ave(nip) )
  emid2_ave = 0.  
  allocate( emid3_ave(nip) )
  emid3_ave = 0.  
  allocate( emid4_ave(nip) )
  emid4_ave = 0.  
  allocate( emid5_ave(nip) )
  emid5_ave = 0.  
  allocate( aod2d_ave(nip) )
  aod2d_ave = 0.  
return
end subroutine chem_alloc


!TODO:  combine with chem_alloc if practical
subroutine chem_alloc2(chem_opt,aer_ra_feedback,bio_emiss_opt,biomass_burn_opt,kemit)

  USE module_wrf_control, only: ims,ime,jms,jme,kms,kme,nbands,  &
                          num_emis_vol,num_moist,num_chem,num_emis_ant, &
                          num_ext_coef,num_bscat_coef,num_asym_par,num_ebu,num_ebu_in
  ! yes, use ALL of it
  use module_chemvars

  implicit none

  integer, intent(IN) :: chem_opt,aer_ra_feedback,kemit, &
                         bio_emiss_opt,biomass_burn_opt
  ALLOCATE( chem( ims:ime, kms:kme, jms:jme, num_chem ) )
  ALLOCATE( e_bio( ims:ime, jms:jme, ne_area ) )
  ALLOCATE( emis_ant( ims:ime, 1:kemit, jms:jme,num_emis_ant) )
emis_ant = 0.
  ALLOCATE( emis_vol( ims:ime, kms:kme, jms:jme,num_emis_vol) )
emis_vol=0.
  ALLOCATE( relhum( ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( dms_0( ims:ime, jms:jme) )
  ALLOCATE( erod( ims:ime, jms:jme,3) )
  ALLOCATE( emis_dust( ims:ime, 1, jms:jme,num_emis_dust) )
emis_dust = 0.
  ALLOCATE( srce_dust( ims:ime, 1, jms:jme,5) ) 
srce_dust = 0. 
  ALLOCATE( emis_seas( ims:ime, 1, jms:jme,num_emis_seas) )
emis_seas = 0.
  ALLOCATE( backg_oh( ims:ime, kms:kme, jms:jme ) )
backg_oh = 0.
  ALLOCATE( backg_h2o2( ims:ime, kms:kme, jms:jme ) )
backg_h2o2 = 0.
  ALLOCATE( backg_no3( ims:ime, kms:kme, jms:jme ) )
backg_no3 = 0.
  ALLOCATE( oh_t( ims:ime, kms:kme, jms:jme ) )
oh_t = 0.
  ALLOCATE( h2o2_t( ims:ime, kms:kme, jms:jme ) )
h2o2_t = 0.
  ALLOCATE( no3_t( ims:ime, kms:kme, jms:jme ) )
no3_t = 0.
  ALLOCATE( h2oai(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( h2oaj(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( nu3(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( ac3(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( cor3(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( asulf(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( ahno3(ims:ime, kms:kme, jms:jme ) )
  ALLOCATE( anh3(ims:ime, kms:kme, jms:jme ) )
!
  ALLOCATE( ebu_in( ims:ime, jms:jme, num_ebu_in ) )
  ebu_in=0.
  ALLOCATE( ebu( ims:ime, kms:kme, jms:jme,num_ebu ) )
  ebu=0.
  ALLOCATE( mean_fct_agtf( ims:ime,  jms:jme ) )
  ALLOCATE( mean_fct_agef( ims:ime,  jms:jme ) )
  ALLOCATE( mean_fct_agsv( ims:ime,  jms:jme ) )
  ALLOCATE( mean_fct_aggr( ims:ime,  jms:jme ) )
  ALLOCATE( firesize_agtf( ims:ime,  jms:jme ) )
  ALLOCATE( firesize_agef( ims:ime,  jms:jme ) )
  ALLOCATE( firesize_agsv( ims:ime,  jms:jme ) )
  ALLOCATE( firesize_aggr( ims:ime,  jms:jme ) )
  ALLOCATE( ash_fall( ims:ime,  jms:jme ) )
  ash_fall=0.
  ALLOCATE( dust_fall( ims:ime,  jms:jme ) )
  ALLOCATE( pm2_5_dry( ims:ime , kms:kme , jms:jme ) )
  ALLOCATE( pm2_5_water( ims:ime , kms:kme , jms:jme ) )
  pm2_5_water=0.
  ALLOCATE( aerwrf( ims:ime , kms:kme , jms:jme ) )
  aerwrf=0.
  ALLOCATE( pm2_5_dry_ec( ims:ime , kms:kme , jms:jme ) )
  ALLOCATE( pm10( ims:ime , kms:kme , jms:jme ) )
  ALLOCATE( tcosz( ims:ime , jms:jme ) )
  ALLOCATE( ttday( ims:ime , jms:jme ) )

  ALLOCATE( sebio_iso( ims:ime , jms:jme ) )
  ALLOCATE( sebio_oli( ims:ime , jms:jme ) )
  ALLOCATE( sebio_api( ims:ime , jms:jme ) )
  ALLOCATE( sebio_lim( ims:ime , jms:jme ) )
  ALLOCATE( sebio_xyl( ims:ime , jms:jme ) )
  ALLOCATE( sebio_hc3( ims:ime , jms:jme ) )
  ALLOCATE( sebio_ete( ims:ime , jms:jme ) )
  ALLOCATE( sebio_olt( ims:ime , jms:jme ) )
  ALLOCATE( sebio_ket( ims:ime , jms:jme ) )
  ALLOCATE( sebio_ald( ims:ime , jms:jme ) )
  ALLOCATE( sebio_hcho( ims:ime , jms:jme ) )
  ALLOCATE( sebio_eth( ims:ime , jms:jme ) )
  ALLOCATE( sebio_ora2( ims:ime , jms:jme ) )
  ALLOCATE( sebio_co( ims:ime , jms:jme ) )
  ALLOCATE( sebio_nr( ims:ime , jms:jme ) )
  ALLOCATE( noag_grow( ims:ime , jms:jme ) )
  ALLOCATE( noag_nongrow( ims:ime , jms:jme ) )
  ALLOCATE( nononag( ims:ime , jms:jme ) )
  ALLOCATE( slai( ims:ime , jms:jme ) )
  ALLOCATE( ebio_iso( ims:ime , jms:jme ) )
  ALLOCATE( ebio_oli( ims:ime , jms:jme ) )
  ALLOCATE( ebio_api( ims:ime , jms:jme ) )
  ALLOCATE( ebio_lim( ims:ime , jms:jme ) )
  ALLOCATE( ebio_xyl( ims:ime , jms:jme ) )
  ALLOCATE( ebio_hc3( ims:ime , jms:jme ) )
  ALLOCATE( ebio_ete( ims:ime , jms:jme ) )
  ALLOCATE( ebio_olt( ims:ime , jms:jme ) )
  ALLOCATE( ebio_ket( ims:ime , jms:jme ) )
  ALLOCATE( ebio_ald( ims:ime , jms:jme ) )
  ALLOCATE( ebio_hcho( ims:ime , jms:jme ) )
  ALLOCATE( ebio_eth( ims:ime , jms:jme ) )
  ALLOCATE( ebio_ora2( ims:ime , jms:jme ) )
  ALLOCATE( ebio_co( ims:ime , jms:jme ) )
  ALLOCATE( ebio_nr( ims:ime , jms:jme ) )
  ALLOCATE( ebio_no( ims:ime , jms:jme ) )
  if(bio_emiss_opt == 3)then
  ALLOCATE( EFmegan(ims:ime, jms:jme , nmegan) )

  ALLOCATE( msebio_isop(ims:ime, jms:jme ) )
  ALLOCATE( pftp_bt(ims:ime, jms:jme ) )
  ALLOCATE( pftp_nt(ims:ime, jms:jme ) )
  ALLOCATE( pftp_sb(ims:ime, jms:jme ) )
  ALLOCATE( pftp_hb(ims:ime, jms:jme ) )

  ALLOCATE( mlai(ims:ime, jms:jme, 12 ) )
  ALLOCATE( mtsa(ims:ime, jms:jme, 12 ) )
  ALLOCATE( mswdown(ims:ime, jms:jme, 12 ) )

  ALLOCATE( mebio_isop(ims:ime, jms:jme ) )
  ALLOCATE( mebio_apin(ims:ime, jms:jme ) )
  ALLOCATE( mebio_bpin(ims:ime, jms:jme ) )
  ALLOCATE( mebio_bcar(ims:ime, jms:jme ) )
  ALLOCATE( mebio_acet(ims:ime, jms:jme ) )
  ALLOCATE( mebio_mbo(ims:ime, jms:jme ) )
  ALLOCATE( mebio_no(ims:ime, jms:jme ) )
  endif
if(chem_opt == 301 .or.chem_opt== 108)then
   allocate( ph_o31d(ims:ime, kms:kme, jms:jme ) )
   ph_o31d=0.
   allocate( ph_o33p(ims:ime, kms:kme, jms:jme ) )
   ph_o33p=0.
   allocate( ph_no2(ims:ime, kms:kme, jms:jme ) )
   ph_no2=0.
   allocate( ph_no3o2(ims:ime, kms:kme, jms:jme ) )
   ph_no3o2=0.
   allocate( ph_no3o(ims:ime, kms:kme, jms:jme ) )
   ph_no3o=0.
   allocate( ph_hno2(ims:ime, kms:kme, jms:jme ) )
   ph_hno2=0.
   allocate( ph_hno3(ims:ime, kms:kme, jms:jme ) )
   ph_hno3=0.
   allocate( ph_hno4(ims:ime, kms:kme, jms:jme ) )
   ph_hno4=0.
   allocate( ph_h2o2(ims:ime, kms:kme, jms:jme ) )
   ph_h2o2=0.
   allocate( ph_ch2or(ims:ime, kms:kme, jms:jme ) )
   ph_ch2or=0.
   allocate( ph_ch2om(ims:ime, kms:kme, jms:jme ) )
   ph_ch2om=0.
   allocate( ph_ch3cho(ims:ime, kms:kme, jms:jme ) )
   ph_ch3cho=0.
   allocate( ph_ch3coch3(ims:ime, kms:kme, jms:jme ) )
   ph_ch3coch3=0.
   allocate( ph_ch3coc2h5(ims:ime, kms:kme, jms:jme ) )
   ph_ch3coc2h5=0.
   allocate( ph_hcocho(ims:ime, kms:kme, jms:jme ) )
   ph_hcocho=0.
   allocate( ph_ch3cocho(ims:ime, kms:kme, jms:jme ) )
   ph_ch3cocho=0.
   allocate( ph_hcochest(ims:ime, kms:kme, jms:jme ) )
   ph_hcochest=0.
   allocate( ph_ch3o2h(ims:ime, kms:kme, jms:jme ) )
   ph_ch3o2h=0.
   allocate( ph_ch3coo2h(ims:ime, kms:kme, jms:jme ) )
   ph_ch3coo2h=0.
   allocate( ph_ch3ono2(ims:ime, kms:kme, jms:jme ) )
   ph_ch3ono2=0.
   allocate( ph_hcochob(ims:ime, kms:kme, jms:jme ) )
   ph_hcochob=0.
   allocate( ph_macr(ims:ime, kms:kme, jms:jme ) )
   ph_macr=0.
   allocate( ph_n2o5(ims:ime, kms:kme, jms:jme ) )
   ph_n2o5=0.
   allocate( ph_o2(ims:ime, kms:kme, jms:jme ) )
   ph_o2=0.
   allocate( ph_pan(ims:ime, kms:kme, jms:jme ) )
   ph_pan=0.
   allocate( ph_acet(ims:ime, kms:kme, jms:jme ) )
   ph_acet=0.
   allocate( ph_mglo(ims:ime, kms:kme, jms:jme ) )
   ph_mglo=0.
   allocate( ph_hno4_2(ims:ime, kms:kme, jms:jme ) )
   ph_hno4_2=0.
   allocate( ph_n2o (ims:ime, kms:kme, jms:jme ) )
   ph_n2o=0.
   allocate( ph_pooh (ims:ime, kms:kme, jms:jme ) )
   ph_pooh=0.
   allocate( ph_mpan (ims:ime, kms:kme, jms:jme ) )
   ph_mpan=0.
   allocate( ph_mvk (ims:ime, kms:kme, jms:jme ) )
   ph_mvk=0.
   allocate( ph_etooh (ims:ime, kms:kme, jms:jme ) )
   ph_etooh=0.
   allocate( ph_prooh (ims:ime, kms:kme, jms:jme ) )
   ph_prooh=0.
   allocate( ph_onitr (ims:ime, kms:kme, jms:jme ) )
   ph_onitr=0.
   allocate( ph_acetol (ims:ime, kms:kme, jms:jme ) )
   ph_acetol=0.
   allocate( ph_glyald (ims:ime, kms:kme, jms:jme ) )
   ph_glyald=0.
   allocate( ph_hyac (ims:ime, kms:kme, jms:jme ) )
   ph_hyac=0.
   allocate( ph_mek (ims:ime, kms:kme, jms:jme ) )
   ph_mek=0.
   allocate( ph_open (ims:ime, kms:kme, jms:jme ) )
   ph_open=0.
   allocate( ph_gly (ims:ime, kms:kme, jms:jme ) )
   ph_gly=0.
   allocate( ph_acetp (ims:ime, kms:kme, jms:jme ) )
   ph_acetp=0.
   allocate( ph_xooh (ims:ime, kms:kme, jms:jme ) )
   ph_xooh=0.
   allocate( ph_isooh (ims:ime, kms:kme, jms:jme ) )
   ph_isooh=0.
   allocate( ph_alkooh (ims:ime, kms:kme, jms:jme ) )
   ph_alkooh=0.
   allocate( ph_mekooh (ims:ime, kms:kme, jms:jme ) )
   ph_mekooh=0.
   allocate( ph_tolooh (ims:ime, kms:kme, jms:jme ) )
   ph_tolooh=0.
   allocate( ph_terpooh (ims:ime, kms:kme, jms:jme ) )
   ph_terpooh=0.
   allocate( ph_cl2 (ims:ime, kms:kme, jms:jme ) )
   ph_cl2=0.
   allocate( ph_hocl (ims:ime, kms:kme, jms:jme ) )
   ph_hocl=0.
   allocate( ph_fmcl (ims:ime, kms:kme, jms:jme ) )
   ph_fmcl=0.

endif


  if(chem_opt == 2)then
     ALLOCATE( h2oai(ims:ime, kms:kme, jms:jme ) )
     ALLOCATE( h2oaj(ims:ime, kms:kme, jms:jme ) )
  endif
  if(aer_ra_feedback == 1)then
     ALLOCATE( extt(ims:ime, kms:kme, jms:jme,nbands) )
     ALLOCATE( ssca(ims:ime, kms:kme, jms:jme,nbands) )
     ALLOCATE( asympar(ims:ime, kms:kme, jms:jme,nbands) )
     ALLOCATE( aod(ims:ime, jms:jme ) )
     ALLOCATE( ext_coeff(ims:ime, kms:kme, jms:jme,1:num_ext_coef ) )
     ALLOCATE( bscat_coeff(ims:ime, kms:kme, jms:jme,1:num_bscat_coef ) )
     ALLOCATE( asym_par(ims:ime, kms:kme, jms:jme,1:num_asym_par ) )
     ALLOCATE( tauaerlw(ims:ime, kms:kme, jms:jme,1:16 ) )
     ALLOCATE( tauaersw(ims:ime, kms:kme, jms:jme,1:4 ) )
     ALLOCATE( gaersw(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( waersw(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( bscoefsw(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l2aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l3aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l4aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l5aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l6aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
     ALLOCATE( l7aer(ims:ime, kms:kme, jms:jme, 1:4 ) )
  endif
  if(chem_opt == 301 .or.chem_opt == 108 ) then

     ALLOCATE( addt(ims:ime, kms:kme, jms:jme ) )
     addt=0.
     ALLOCATE( addx(ims:ime, kms:kme, jms:jme ) )
     addx=0.
     ALLOCATE( addc(ims:ime, kms:kme, jms:jme ) )
     addc=0.
     ALLOCATE( etep(ims:ime, kms:kme, jms:jme ) )
     etep=0.
     ALLOCATE( oltp(ims:ime, kms:kme, jms:jme ) )
     oltp=0.
     ALLOCATE( olip(ims:ime, kms:kme, jms:jme ) )
     olip=0.
     ALLOCATE( cslp(ims:ime, kms:kme, jms:jme ) )
     cslp=0.
     ALLOCATE( limp(ims:ime, kms:kme, jms:jme ) )
     limp=0.
     ALLOCATE( hc5p(ims:ime, kms:kme, jms:jme ) )
     hc5p=0.
     ALLOCATE( hc8p(ims:ime, kms:kme, jms:jme ) )
     hc8p=0.
     ALLOCATE( tolp(ims:ime, kms:kme, jms:jme ) )
     tolp=0.
     ALLOCATE( xylp(ims:ime, kms:kme, jms:jme ) )
     xylp=0.
     ALLOCATE( apip(ims:ime, kms:kme, jms:jme ) )
     apip=0.
     ALLOCATE( isop(ims:ime, kms:kme, jms:jme ) )
     isop=0.
     ALLOCATE( hc3p(ims:ime, kms:kme, jms:jme ) )
     hc3p=0.
     ALLOCATE( ethp(ims:ime, kms:kme, jms:jme ) )
     ethp=0.
     ALLOCATE( o3p(ims:ime, kms:kme, jms:jme ) )
     o3p=0.
     ALLOCATE( tco3(ims:ime, kms:kme, jms:jme ) )
     tco3=0.
     ALLOCATE( mo2(ims:ime, kms:kme, jms:jme ) )
     mo2=0.
     ALLOCATE( o1d(ims:ime, kms:kme, jms:jme ) )
     o1d=0.
     ALLOCATE( olnn(ims:ime, kms:kme, jms:jme ) )
     olnn=0.
     ALLOCATE( olnd(ims:ime, kms:kme, jms:jme ) )
     olnd=0.
     ALLOCATE( rpho(ims:ime, kms:kme, jms:jme ) )
     rpho=0.
     ALLOCATE( xo2(ims:ime, kms:kme, jms:jme ) )
     xo2=0.
     ALLOCATE( ketp(ims:ime, kms:kme, jms:jme ) )
     ketp=0.
     ALLOCATE( xno2(ims:ime, kms:kme, jms:jme ) )
     xno2=0.
     ALLOCATE( ol2p(ims:ime, kms:kme, jms:jme ) )
     ol2p=0.
     ALLOCATE( oln(ims:ime, kms:kme, jms:jme ) )
     oln=0.
     ALLOCATE( macp(ims:ime, kms:kme, jms:jme ) )
     macp=0.
     ALLOCATE( hocoo(ims:ime, kms:kme, jms:jme ) )
     hocoo=0.
     ALLOCATE( bzno2_o(ims:ime, kms:kme, jms:jme ) )
     bzno2_o=0.
     ALLOCATE( bz_o(ims:ime, kms:kme, jms:jme ) )
     bz_o=0.
     ALLOCATE( tbu_o(ims:ime, kms:kme, jms:jme ) )
     tbu_o=0.
  endif

  if(chem_opt == 108 ) then
     ALLOCATE( cvaro1(ims:ime, kms:kme, jms:jme ) )
     cvaro1=0.
     ALLOCATE( cvaro2(ims:ime, kms:kme, jms:jme ) )
     cvaro2=0.
     ALLOCATE( cvalk1(ims:ime, kms:kme, jms:jme ) )
     cvalk1=0.
     ALLOCATE( cvole1(ims:ime, kms:kme, jms:jme ) )
     cvole1=0.
     ALLOCATE( cvapi1(ims:ime, kms:kme, jms:jme ) )
     cvapi1=0.
     ALLOCATE( cvapi2(ims:ime, kms:kme, jms:jme ) )
     cvapi2=0.
     ALLOCATE( cvlim1(ims:ime, kms:kme, jms:jme ) )
     cvlim1=0.
     ALLOCATE( cvlim2(ims:ime, kms:kme, jms:jme ) )
     cvlim2=0.
     ALLOCATE( mob(ims:ime, kms:kme, jms:jme ) )
     mob=0.
     ALLOCATE( cvasoaX(ims:ime, kms:kme, jms:jme ) )
     cvasoaX=0.
     ALLOCATE( cvasoa1(ims:ime, kms:kme, jms:jme ) )
     cvasoa1=0.
     ALLOCATE( cvasoa2(ims:ime, kms:kme, jms:jme ) )
     cvasoa2=0.
     ALLOCATE( cvasoa3(ims:ime, kms:kme, jms:jme ) )
     cvasoa3=0.
     ALLOCATE( cvasoa4(ims:ime, kms:kme, jms:jme ) )
     cvasoa4=0.
     ALLOCATE( cvbsoaX(ims:ime, kms:kme, jms:jme ) )
     cvbsoaX=0.
     ALLOCATE( cvbsoa1(ims:ime, kms:kme, jms:jme ) )
     cvbsoa1=0.
     ALLOCATE( cvbsoa2(ims:ime, kms:kme, jms:jme ) )
     cvbsoa2=0.
     ALLOCATE( cvbsoa3(ims:ime, kms:kme, jms:jme ) )
     cvbsoa3=0.
     ALLOCATE( cvbsoa4(ims:ime, kms:kme, jms:jme ) )
     cvbsoa4=0.
     ALLOCATE( asoa1j(ims:ime, kms:kme, jms:jme ) )
     asoa1j=0.
     ALLOCATE( asoa1i(ims:ime, kms:kme, jms:jme ) )
     asoa1i=0.
     ALLOCATE( asoa2j(ims:ime, kms:kme, jms:jme ) )
     asoa2j=0.
     ALLOCATE( asoa2i(ims:ime, kms:kme, jms:jme ) )
     asoa2i=0.
     ALLOCATE( asoa3j(ims:ime, kms:kme, jms:jme ) )
     asoa3j=0.
     ALLOCATE( asoa3i(ims:ime, kms:kme, jms:jme ) )
     asoa3i=0.
     ALLOCATE( asoa4j(ims:ime, kms:kme, jms:jme ) )
     asoa4j=0.
     ALLOCATE( asoa4i(ims:ime, kms:kme, jms:jme ) )
     asoa4i=0.
     ALLOCATE( bsoa1j(ims:ime, kms:kme, jms:jme ) )
     bsoa1j=0.
     ALLOCATE( bsoa1i(ims:ime, kms:kme, jms:jme ) )
     bsoa1i=0.
     ALLOCATE( bsoa2j(ims:ime, kms:kme, jms:jme ) )
     bsoa2j=0.
     ALLOCATE( bsoa2i(ims:ime, kms:kme, jms:jme ) )
     bsoa2i=0.
     ALLOCATE( bsoa3j(ims:ime, kms:kme, jms:jme ) )
     bsoa3j=0.
     ALLOCATE( bsoa3i(ims:ime, kms:kme, jms:jme ) )
     bsoa3i=0.
     ALLOCATE( bsoa4j(ims:ime, kms:kme, jms:jme ) )
     bsoa4j=0.
     ALLOCATE( bsoa4i(ims:ime, kms:kme, jms:jme ) )
     bsoa4i=0.
  endif
return
end subroutine chem_alloc2

end module module_chem_alloc
