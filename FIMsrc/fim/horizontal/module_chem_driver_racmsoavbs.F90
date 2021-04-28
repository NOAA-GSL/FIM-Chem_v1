MODULE MODULE_CHEM_DRIVER_RACMSOAVBS
   IMPLICIT NONE

#include <gptl.inc>

CONTAINS
     subroutine racmsoavbs(ktau)
!
! FIM version variables
!
  USE fimnamelist,    only: yyyymmddhhmm, nvl,readrestart
  USE module_control, only: dt, nvlp1,ntra,ntrb,CallRadiation,nip,numphr
  USE module_wrf_control, only: ims,ime,jms,jme,kms,kme,           &
                                ids,ide,jds,jde,kds,kde,           &
                                its,ite,jts,jte,kts,kte,           &
                                nvl_gocart,num_emis_ant,num_moist, &
                                num_ext_coef,num_bscat_coef,num_asym_par,&
                                num_chem,num_soil_layers,numgas,nbands,  &
                                num_emis_vol,CallChemistry,CallBiom,num_ebu, &
                                num_ebu_in
!TODO:  replace use-association of FIM dynamics variables with coupler
  USE module_variables
  USE module_chem_variables
  USE module_add_emis_cptec
  USE module_wrf_variables,only:exch,pb2d
  USE module_constants
  USE module_chem_constants ,only: p_gocart
  USE module_sfc_variables
! TBH:  Ignore these so PPP does not have to translate them
  USE module_plumerise1, only: plumerise_driver
  USE module_chem_prep_fim,only: chem_prep_fim
  USE module_gocart_seasalt,only: gocart_seasalt_driver
  USE gocart_dust,only: gocart_dust_driver
  USE gocart_dust_afwa,only: gocart_dust_afwa_driver
  USE module_gocart_settling,only: gocart_settling_driver
  USE module_gocart_opt,only: aero_opt
  USE module_vash_settling,only: vash_settling_driver,vashshort_settling_driver
  USE module_wetdep_ls,only: wetdep_ls
  USE module_phot_mad,only: madronich1_driver,photmad_init
  USE module_dms_emis,only: gocart_dmsemis
  USE module_ctrans_grell,only:grelldrvct
  USE module_data_soa_vbs, only: ldrog,ldrog_vbs
  USE module_racmsoavbs_driver, only: racm_soa_vbs_driver
  USE module_gocart_so2so4,only:so2so4
  USE module_initial_chem_namelists,only:p_qc,chem_opt,biomass_burn_opt,seas_opt,dust_opt,&
                            chem_conv_tr,dmsemis_opt,chem_in_opt,aer_ra_feedback,kemit,   &
                            phot_opt,emiss_opt,conv_tr_aqchem,                            &
!gas phase stuff
                            p_so2,p_sulf,p_no2,p_no,p_o3,p_dms,                           &
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

! aerosols
                            p_p25,p_bc1,p_bc2,p_oc1,p_oc2,p_dust_1,p_dust_2,p_dust_3,     &
                            p_dust_4,p_dust_5,p_seas_1,p_seas_2,p_seas_3,p_seas_4,p_p10,  &
! emisions
                            p_e_iso,p_e_so2,p_e_no,p_e_no2,p_e_co,p_e_eth,p_e_hc3,p_e_hc5,&
                            p_e_hc8,p_e_xyl,p_e_ol2,p_e_olt,p_e_oli,p_e_tol,p_e_csl,      &
                            p_e_hcho,p_e_ald,p_e_ket,p_e_ora2,p_e_nh3,p_e_pm_25,p_e_pm_10,&
                            p_e_oc,p_e_sulf,p_e_bc,p_e_dms,                               &
! biomass burning emissions
                            p_ebu_sulf,p_ebu_dms,p_ebu_csl,p_ebu_iso,p_ebu_no,p_ebu_no2,  &
                            p_ebu_ald,p_ebu_hcho,p_ebu_ora2,p_ebu_hc3,p_ebu_hc5,p_ebu_hc8,&
                            p_ebu_eth,p_ebu_co,p_ebu_olt,p_ebu_oli,p_ebu_oc,p_ebu_bc,     &
                            p_ebu_pm25,p_ebu_pm10,p_ebu_tol,p_ebu_xyl,p_ebu_ket,p_ebu_so2,&
                            p_ebu_nh3
  USE module_dry_dep_driver,only:dry_dep_driver
  !USE module_gocart_aerosols,only:sum_pm_gocart,gocart_aerosols_driver
  USE module_gocart_chem,only:gocart_chem_driver 
  !USE module_gocartracm_driver,only:gocartracm_driver
  USE module_aerosols_soa_vbs, only:soa_vbs_driver,sum_pm_soa_vbs,soa_vbs_addemiss
  USE module_data_gocart_chem,only:airmw,mw_so4_aer
  USE module_optical_driver, only: optical_driver
  USE module_aer_opt_out, only: aer_opt_out
  USE module_aer_ra, only: aer_ra
  USE module_chemvars
  USE module_wrfphysvars
  USE units, only: getunit, returnunit
  IMPLICIT NONE

  INTEGER, INTENT (IN) :: KTAU

   real :: var_rmv(ims:ime, jms:jme ,num_chem)
   real :: swdown(ims:ime, jms:jme )
   real :: dep_vel_o3(ims:ime, jms:jme )
   real :: e_co(ims:ime, jms:jme )
   real :: uvrad(ims:ime, jms:jme )
   real :: raincv_b(ims:ime, jms:jme )
   real :: cu_co_ten(ims:ime, kms:kme,jms:jme )
   REAL :: dusthelp(ims:ime, jms:jme ),seashelp(ims:ime,jms:jme )
   real :: tr_fall(ims:ime, jms:jme,num_chem )
   real :: vcsulf_old(ims:ime,kms:kme,jms:jme),             &
           vcso2_old(ims:ime,kms:kme,jms:jme),              &
           vch2o2_old(ims:ime,kms:kme,jms:jme)
   real :: serial1(nip)
   real ::vdrog3(ims:ime,kms:kme,jms:jme,ldrog),& 
           vdrog3_vbs(ims:ime,kms:kme,jms:jme,ldrog_vbs)
   !integer,save :: current_month,current_gmt,current_day,current_year,current_hour,day
   integer current_month,current_gmt,current_day,current_year,current_hour,day
   !shc end stuff for MEGAN v2.04

      REAL                     :: dtstep_plume,dtstep, dx, gmt
      REAL                     :: dust_alpha,dust_gamma
      REAL (kind=8) :: curr_secs,curr_mins,curr_days
      real, dimension(1,1) :: rate,nlong
!
! Local variables...
!
      INTEGER :: nbegin,ib,i, j, k, nv,nvv,ksub, dust_emiss_active, seasalt_emiss_active
      INTEGER :: call_plume,call_chem,julday
      INTEGER :: call_optics,call_phot
      REAL :: factor,factor2,conv,mwdry,xlv,maxv,minv
      CHARACTER (LEN=80) :: message,filename 
      CHARACTER(len=9 )                :: jdate 
      logical scale_fire_emiss,haveaer
      integer :: ret
      integer :: unitno
      integer :: ipn
      integer :: idat(8),mdat(8),YEARi,MONTHi,DAYi
      real*8  :: rinc(5)
      LOGICAL, SAVE :: firstfire=.TRUE.
      !integer :: days(12)
      !data (days(i),i=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
      haveaer=.false.
      vdrog3=0.
      ret = gptlstart ('chem_driver')
! nbegin is the start address (-1) of the first chem variable in tr3d
    scale_fire_emiss=.false.
! scale fire emiss is currently only for a few mozrt options
    nbegin=ntra+num_moist-3
    if(num_moist.gt.3)nbegin=ntra+num_moist-2
    dust_alpha=1.0
    dust_gamma=1.6
    call_plume=0
    call_optics=0
    call_phot=0
    call_chem=0
!    if(aer_ra_feedback > 0 ) then
       if ((mod(ktau,CallRadiation)==0).or.(ktau==1)) then
          call_optics=1
       endif
!    endif
    if(biomass_burn_opt > 0 ) then
       if ((mod(ktau,CallBiom)==0).or.(ktau==1).or.firstfire) then
         call_plume=1
         dtstep_plume=dt*CallBiom
         firstfire = .false.
       endif
    endif
    if(chem_opt == 0) then
     write(6,*)'Shouldnt be here with chem_opt = 0 '
    endif
       if ((mod(ktau,CallRadiation)==0).or.(ktau==1) .and. phot_opt==1) then
          call_phot=1
       endif



!sms$compare_var(st3d   , "begin chem_driver ")
!sms$compare_var(sm3d   , "begin chem_driver ")
!sms$compare_var(ts2d   , "begin chem_driver ")
!sms$compare_var(us2d   , "begin chem_driver ")
!sms$compare_var(rsds   , "begin chem_driver ")

     if ((mod(ktau,CallChemistry)==0).or.(ktau==1)) then
       call_chem=1
     endif
     dusthelp(:,:)=0.
     seashelp(:,:)=0.
     xlv=2.5e6
     dtstep=dt*CallChemistry
     curr_secs=ktau*dt
     curr_mins=curr_secs/60.
     if(ktau.le.1)then
         dtstep=dt
         READ(UNIT=yyyymmddhhmm(5:6), FMT='(I2)') current_month
!sms$ignore begin
         rcav(jts:jte)=rc2d(jts:jte)
         rnav(jts:jte)=rn2d(jts:jte)-rc2d(jts:jte)
!sms$ignore end
     else
!sms$ignore begin
         rcav(jts:jte)=rc2d(jts:jte)-rcav(jts:jte)
         rcav(jts:jte)=max(0.,rcav(jts:jte))
         rnav(jts:jte)=rn2d(jts:jte)-rc2d(jts:jte)-rnav(jts:jte)
         rnav(jts:jte)=max(0.,rnav(jts:jte))
!sms$ignore end
     endif
! make the fim varialbles conform with the chem_driver
    !if(ktau.le.1 .or.readrestart)then
!        READ(UNIT=yyyymmddhhmm(1:4), FMT='(I4)') current_year
!        READ(UNIT=yyyymmddhhmm(5:6), FMT='(I2)') current_month
!     READ(UNIT=yyyymmddhhmm(7:8), FMT='(I2)') current_day
!     READ(UNIT=yyyymmddhhmm(9:10), FMT='(I2)') current_gmt
!     current_hour= current_gmt
!     day         = current_day 
     !endif
    READ(yyyymmddhhmm(1:4),   FMT='(I4)') YEARi
    READ(yyyymmddhhmm(5:6),   FMT='(I2)') MONTHi
    READ(yyyymmddhhmm(7:8),   FMT='(I2)') DAYi
    READ(yyyymmddhhmm(9:10),   FMT='(I2)') current_gmt
    idat = 0
    idat(1) = YEARi
    idat(2) = MONTHi
    idat(3) = DAYi
     rinc(:) = 0
     rinc(2) = (float(ktau))/float(numphr)
     call w3movdat (rinc, idat, mdat)
     
    current_year =mdat(1)
    current_month=mdat(2)
    current_day  =mdat(3)
    current_hour   = mdat(5)
    if (current_day>1) then
    !day         = current_day-1
    day         = current_day
    else
    day = 1
    endif 
     call GetJdate(yyyymmddhhmm,jdate)                ! Julian date conversion
     READ(UNIT=jdate(3:5), FMT='(I3)')julday

     gmt=current_gmt
     call chem_prep_fim(ktau,dt,rh3d,tr3d,tk3d,st3d,sm3d,dp3d,mp3d,  &
             ts2d,us2d,rsds,pr3d,emiss_ash_mass,emiss_ash_height,    &
             emiss_ash_dt,dm0,emiss_tr_mass,emiss_tr_height,         &
             emiss_tr_dt, VFRAC2d,VTYPE2d,STYPE2d,us3d,vs3d,ws3d,    &
             slmsk2d,zorl2d,exch,pb2d,hf2d,th_pvsrf,oh_backgd,h2o2_backgd,    &
             no3_backgd,backg_oh,backg_h2o2,backg_no3,p_gocart,      &
             nvl_gocart,ttday,tcosz,gmt,julday,dtstep,ph3d,area,ero1,&
             ero2,ero3,rcav,raincv_b,deg_lat,deg_lon,nvl,nvlp1,ntra, &
             relhum,rri,t_phy,moist,u_phy,v_phy,p_phy,chem,tsk,ntrb, &
             grvity,rd,p1000,cp,erod,emis_ant,emis_vol,e_co,dms_0,   &
             u10,v10,ivgtyp,isltyp,gsw,vegfra,rmol,ust,znt,xland,dxy,&
             t8w,p8w,exch_h,pbl,hfx,xlat,xlong,convfac,z_at_w,zmid,dz8w,vvel,&
             rho_phy,smois,num_soil_layers,num_chem,numgas,num_moist,&
             emiss_abu,ebu_in,emiss_ab,num_ebu_in,num_emis_ant,      &
             num_emis_vol,kemit,call_chem,ids,ide, jds,jde, kds,     &
             kde,plumestuff,mean_fct_agtf,mean_fct_agef,             &
             mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,&
             firesize_agsv,firesize_aggr,chem_in_opt,                &
             ims,ime, jms,jme, kms,kme,                              &
             its,ite,jts,jte,kts,kte)
  mwdry=28.

!if(mod(ktau,CallChemistry)==0.or.ktau==1) then
! write(6,*)'in chem_driver, now do chemistry = ',ktau,dtstep,CallChemistry,kemit
!  if(seas_opt == 1 )then
!    print *,'get seasalt emissions'
!     call gocart_seasalt_driver(ktau,dt,rri,t_phy,moist,u_phy,  &
!         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,                  &
!         xland,xlat,xlong,area,grvity,emis_seas, &
!         seashelp,num_emis_seas,num_moist,num_chem,        &
!         ids,ide, jds,jde, kds,kde,                                        &
!         ims,ime, jms,jme, kms,kme,                                        &
!         its,ite, jts,jte, kts,kte)
!  endif
!  if(dust_opt == 1 )then
!     call gocart_dust_driver(ktau,dt,rri,t_phy,moist,u_phy,       &
!         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,ivgtyp,isltyp,&
!         vegfra,xland,xlat,xlong,gsw,area,grvity,emis_dust,srce_dust, &
!         dusthelp, num_emis_dust,num_moist,num_chem,num_soil_layers,  &
!         current_month,                                               &
!         ids,ide, jds,jde, kds,kde,                                   &
!         ims,ime, jms,jme, kms,kme,                                   &
!         its,ite, jts,jte, kts,kte)
!  else if(dust_opt == 3 )then
!     call gocart_dust_afwa_driver(ktau,dt,rri,t_phy,moist,u_phy,      &
!         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,ivgtyp,isltyp,&
!         vegfra,xland,xlat,xlong,gsw,area,grvity,emis_dust,srce_dust, &
!         dusthelp, ust,znt,clayfrac,sandfrac,dust_alpha,dust_gamma,   &
!         num_emis_dust,num_moist,num_chem,num_soil_layers,            &
!         ids,ide, jds,jde, kds,kde,                                   &
!         ims,ime, jms,jme, kms,kme,                                   &
!         its,ite, jts,jte, kts,kte)
!  endif
!endif
if(call_plume == 1 )  then
          call plumerise_driver (ktau,dtstep,num_chem,num_moist,num_ebu, &
           num_ebu_in,ebu,ebu_in,mean_fct_agtf,mean_fct_agef,            &
           mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,      &
           firesize_agsv,firesize_aggr,'RACMSOAVBS','BIOMASSB', t_phy,   &
           moist,rho_phy,vvel,u_phy,v_phy,p_phy,                         &
           z_at_w,scale_fire_emiss,                                      &
           ids,ide, jds,jde, kds,kde,                                    &
           ims,ime, jms,jme, kms,kme,                                    &
           its,ite, jts,jte, kts,kte                                     )

endif

!if(chem_opt >= 0 .and. (dust_opt == 1 .or. dust_opt ==3))then
!     do j=jts,jte
!     do i=its,ite
!       emi_d1(j)=emis_dust(i,1,j,1)
!       emi_d2(j)=emis_dust(i,1,j,2)
!       emi_d3(j)=emis_dust(i,1,j,3)
!       emi_d4(j)=emis_dust(i,1,j,4)
!       emi_d5(j)=emis_dust(i,1,j,5)
!     enddo
!     enddo
!endif

if(dmsemis_opt == 1 ) then
!    do j=jts,jte
!    do i=its,ite
!       diaga(5,j)=emis_dust(i,1,j,1)
!       diaga(6,j)=chem(i,1,j,p_dms)
!    enddo
!    enddo
   call gocart_dmsemis(dt,rri,t_phy,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,dms_0,tsk,                  &
         ivgtyp,isltyp,xland,area,grvity,mwdry, &
         num_chem,p_dms,ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte)
!!    do j=jts,jte
!!    do i=its,ite
!!       diaga(6,j)=-(diaga(6,j)-chem(i,1,j,p_dms))/dt
!!    enddo
!!    enddo
endif

!if(dust_opt == 1 .or. seas_opt == 1 .or. dust_opt == 3)then

!   call gocart_settling_driver(dt,t_phy,moist,  &
!         chem,rho_phy,dz8w,p8w,p_phy,         &
!         dusthelp,seashelp,area,grvity,                         &
!         num_moist,num_chem,                 &
!         ids,ide, jds,jde, kds,kde,                                        &
!         ims,ime, jms,jme, kms,kme,                                        &
!         its,ite, jts,jte, kts,kte)
!endif
!
! 10 volcanic size bins
!
!
! add biomass burning emissions at every timestep
!
if(BIOMASS_BURN_OPT == 1 )  then

  do i=its,ite
     do k=kts,kte-2
       do j=jts,jte
!factro for pm emissions, factor2 for burn emissions
         !factor=dt*rri(i,k,j)/dz8w(i,k,j)
         !chem(i,k,j,p_oc1)=chem(i,k,j,p_oc1)+(ebu(i,k,j,p_ebu_oc))*factor
         !chem(i,k,j,p_bc1)=chem(i,k,j,p_bc1)+(ebu(i,k,j,p_ebu_bc))*factor
         !chem(i,k,j,p_p25)=chem(i,k,j,p_p25)+(ebu(i,k,j,p_ebu_pm25))*factor
         !chem(i,k,j,p_p10)=chem(i,k,j,p_p10)+(ebu(i,k,j,p_ebu_pm10))*factor

! gas phase next

        factor2=4.828e-4*dt*rri(i,k,j)/(60.*dz8w(i,k,j))
        chem(i,k,j,p_so2)=chem(i,k,j,p_so2)+ebu(i,k,j,p_ebu_so2)*factor2
        chem(i,k,j,p_sulf)  =  chem(i,k,j,p_sulf)                        &
                         +ebu(i,k,j,p_ebu_sulf)*factor2
!        chem(i,k,j,p_dms)  =  chem(i,k,j,p_dms)                        &
!                         +ebu(i,k,j,p_ebu_dms)*factor2
        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +ebu(i,k,j,p_ebu_csl)*factor2
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                     +ebu(i,k,j,p_ebu_iso)*factor2
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +ebu(i,k,j,p_ebu_no)*factor2
        chem(i,k,j,p_no2)  = chem(i,k,j,p_no2)                         &
                         +ebu(i,k,j,p_ebu_no2)*factor2
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
                         +ebu(i,k,j,p_ebu_nh3)*factor2
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +ebu(i,k,j,p_ebu_ald)*factor2
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +ebu(i,k,j,p_ebu_hcho)*factor2
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +ebu(i,k,j,p_ebu_ora2)*factor2
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +ebu(i,k,j,p_ebu_hc3)*factor2
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +ebu(i,k,j,p_ebu_hc5)*factor2
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +ebu(i,k,j,p_ebu_hc8)*factor2
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +ebu(i,k,j,p_ebu_eth)*factor2
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +ebu(i,k,j,p_ebu_co)*factor2
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +ebu(i,k,j,p_ebu_olt)*factor2
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +ebu(i,k,j,p_ebu_oli)*factor2
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +ebu(i,k,j,p_ebu_tol)*factor2
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
                         +ebu(i,k,j,p_ebu_xyl)*factor2
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
                         +ebu(i,k,j,p_ebu_ket)*factor2
       enddo
     enddo
  enddo
endif
!
! next add anthropogenic emissions, currently limited to one (surface) level
!
  do i=its,ite
       do j=jts,jte
        k=1
!factro for pm emissions, factor2 for burn emissions
        factor=dt*rri(i,k,j)/dz8w(i,k,j)
        factor2=4.828e-4*dt*rri(i,k,j)/(60.*dz8w(i,k,j))
!        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                         &
!                         +emis_ant(i,k,j,p_e_so2)*factor2
!        chem(i,k,j,p_sulf)  = chem(i,k,j,p_sulf)                         &
!                         +emis_ant(i,k,j,p_e_sulf)*factor2
!        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
!                         +emis_ant(i,k,j,p_e_co)*factor2
!        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
!                         +emis_ant(i,k,j,p_e_no)*factor2
!!        chem(i,k,j,p_no2)   = chem(i,k,j,p_no2)                          &
!                         +emis_ant(i,k,j,p_e_no2)*factor2
!        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
!                         +emis_ant(i,k,j,p_e_nh3)*factor2
!!       if(p_ch4 .gt. 0 .and. p_e_ch4 .gt. 0) then
!!          chem(i,k,j,p_ch4)  = chem(i,k,j,p_ch4)                      &
!!                        +emis_ant(i,k,j,p_e_ch4)*factor2
!!       endif
!        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
!                         +emis_ant(i,k,j,p_e_csl)*factor2  
         if (xlong(i,j)>180) then
             nlong=xlong(i,j)-360.
         else
             nlong=xlong(i,j)
         endif            
     call get_diurnal_rate(ktau,dt,current_gmt,current_month,current_day,current_year,xlat(i,j),nlong,rate)

        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +emis_ant(i,k,j,p_e_iso)*factor2*rate(1,1)*86400./dt
  
        emiss_ab1(j,p_e_iso)=emis_ant(i,k,j,p_e_iso)*rate(1,1)*86400./dt
!        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
!                         +emis_ant(i,k,j,p_e_ald)*factor2
!        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
!                         +emis_ant(i,k,j,p_e_hcho)*factor2
!        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
!                         +emis_ant(i,k,j,p_e_ora2)*factor2
!        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
!                         +emis_ant(i,k,j,p_e_hc3)*factor2
!        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
!                         +emis_ant(i,k,j,p_e_hc5)*factor2
!        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
!                         +emis_ant(i,k,j,p_e_hc8)*factor2
!        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
!                         +emis_ant(i,k,j,p_e_eth)*factor2
!        !if(p_ete.gt.1)chem(i,k,j,p_ete)  = chem(i,k,j,p_ete)           &
!        !                 +emis_ant(i,k,j,p_e_ol2)*factor2
!!        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
!                         +emis_ant(i,k,j,p_e_olt)*factor2
!        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
!                         +emis_ant(i,k,j,p_e_oli)*factor2
!        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
!                         +emis_ant(i,k,j,p_e_tol)*factor2
!        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
!                         +emis_ant(i,k,j,p_e_xyl)*factor2
!        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
!                         +emis_ant(i,k,j,p_e_ket)*factor2
        !factor = rri(i,k,j)*dt/dz8w(i,k,j)
        !chem(i,k,j,p_p10) = chem(i,k,j,p_p10) + factor*emis_ant(i,k,j,p_e_pm_10)
        !chem(i,k,j,p_p25) = chem(i,k,j,p_p25) + factor*emis_ant(i,k,j,p_e_pm_25)
        !chem(i,k,j,p_oc1) = chem(i,k,j,p_oc1) + factor*emis_ant(i,k,j,p_e_oc)
        !chem(i,k,j,p_bc1) = chem(i,k,j,p_bc1) + factor*emis_ant(i,k,j,p_e_bc)
       enddo
  enddo

              call add_emis_cptec(dt,ktau,dz8w,      &
            rho_phy,chem,num_chem,num_emis_ant,current_hour,            &
            julday,gmt,xlat,xlong,t_phy,p_phy,emis_ant, emiss_ab1,      &
!         ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
!          ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
!          ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
!          ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,                          &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                              )

 
! subgrid convective transport
!!
   if(CHEM_OPT == 108)  then
    call soa_vbs_addemiss(  dt, u10, v10, rri, dz8w, xland, chem,&
            ebu,clayfrac,sandfrac,vegfra,znt,gsw,                       &
            slai,ust,smois,ivgtyp,isltyp,                               &
            emis_ant,dust_emiss_active,                                 &
            seas_opt,kemit,                                 &
            biomass_burn_opt,                                           &
            num_soil_layers,emiss_opt,                                  &
            dust_opt,                                                   &
            ktau,p8w,u_phy,v_phy,rho_phy,grvity,area,erod,                &
            num_ebu, num_chem,num_emis_ant,                             &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )

   endif

   if(chem_conv_tr == 2 )then
   call grelldrvct(DT,ktau,                       &
              rho_phy,RAINCV_b,chem,tr_fall,              &
              U_phy,V_phy,t_phy,moist,dz8w,p_phy,                       &
              XLV,CP,grvity,rv,z_at_w,cu_co_ten,                         &
              numgas,chem_opt,                                   &
              num_chem,num_moist,                               &
              ids,ide, jds,jde, kds,kde,                        &
              ims,ime, jms,jme, kms,kme,                        &
              its,ite, jts,jte, kts,kte                         )
    endif
!    do j=jts,jte
!    do i=its,ite
!       diaga(3,j)=chem(i,1,j,p_p25)
!    enddo
!    enddo
    call dry_dep_driver(ktau,dt,julday,current_month,t_phy,p_phy,         &
               moist,p8w,rmol,rri,gmt,t8w,rcav,                          &
               chem,rho_phy,dz8w,exch_h,hfx,                              &
               ivgtyp,tsk,gsw,vegfra,pbl,ust,znt,zmid,z_at_w,        &
               xland,xlat,xlong,h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,     &
               anh3,dep_vel_o3,grvity,                                    &
               e_co,kemit,snowh,numgas,                                         &
               num_chem,num_moist,                                        &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!    do j=jts,jte
!    do i=its,ite
!       diaga(3,j)=(chem(i,1,j,p_p25)-diaga(3,j))/dt
!       diaga(4,j)=dep_vel_o3(i,j)
!    enddo
!    enddo
!
! ls wet deposition
!
     call wetdep_ls(dt,chem,rnav,moist,rho_phy,var_rmv,num_moist, &
         num_chem,numgas,p_qc,dz8w,vvel,chem_opt,                 &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
      if(phot_opt==1)then    
      if(ktau == 1)then    
! get climatology aerosols, will  not be necessary
       write(0,*)'call photinit'
      call photmad_init(z_at_w,aerwrf,grvity,ids,ide,jds,jde,kds,kde,ims,ime, &
          jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
      endif
!write(0,*)'call phot'
      if(call_optics == 1 )then
      call madronich1_driver(ktau,dt,haveaer,                           &
               gmt,julday,t_phy,moist,aerwrf,p8w,t8w,p_phy,             &
               chem,rho_phy,dz8w,                                       &
               xlat,xlong,z_at_w,gd_cloud,gd_cloud2,                    &
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2, &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,     &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,          &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob, &
               pm2_5_dry,pm2_5_water,uvrad,                             &
               chem_opt,num_chem,num_moist,                             &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
write(0,*)'called phot'
      endif
      endif
if(call_chem == 1)then
write(0,*)'callchem = ',call_chem,ktau,phot_opt
       vcsulf_old(its:ite,kts:kte,jts:jte) = &
            max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
   call  racm_soa_vbs_driver(  &
            chem, dtstep, &
            p_phy,t_phy,rho_phy,moist,     &
            vdrog3, ldrog, vdrog3_vbs, ldrog_vbs,            &

             addt, addx, addc, etep, oltp, &
             olip, cslp, limp, hc5p, hc8p, &
             tolp, xylp, apip, isop, hc3p, &
             ethp, o3p, tco3, mo2, o1d, &
             olnn, olnd, rpho, xo2, ketp, &
             xno2, ol2p, oln, macp, hocoo, &
             bzno2_o, bz_o, tbu_o,  &
             ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, &
             ph_hno2, ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, &
             ph_ch2om, ph_ch3cho, ph_ch3coch3, ph_ch3coc2h5, ph_hcocho, &
             ph_ch3cocho, ph_hcochest, ph_ch3o2h, ph_ch3coo2h, ph_ch3ono2, &
             ph_hcochob, ph_macr, ph_n2o5, ph_o2, ph_pan, &
             ph_acet, ph_mglo, ph_hno4_2, ph_n2o, ph_pooh, &
             ph_mpan, ph_mvk, ph_etooh, ph_prooh, ph_onitr, &
             ph_acetol, ph_glyald, ph_hyac, ph_mek, ph_open, &
             ph_gly, ph_acetp, ph_xooh, ph_isooh, ph_alkooh, &
             ph_mekooh, ph_tolooh, ph_terpooh, ph_cl2, ph_hocl, &
             ph_fmcl,  &
              ids,ide, jds,jde, kds,kde,         &
              ims,ime, jms,jme, kms,kme,         &
              its,ite, jts,jte, kts,kte         )
       !vcsulf_old(its:ite,kts:kte,jts:jte) = &
       !     max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
!       vcso2_old(its:ite,kts:kte,jts:jte) = &
!            max(chem(its:ite,kts:kte,jts:jte,p_so2),epsilc)
!       vch2o2_old(its:ite,kts:kte,jts:jte) = &
!            max(chem(its:ite,kts:kte,jts:jte,p_h2o2),epsilc)

   IF(conv_tr_aqchem == 0 ) THEN
         CALL  so2so4(0,chem,p_so2,p_sulf,p_h2o2,T_PHY,MOIST,           &
              gd_cloud, gd_cldfr,                                    &
              NUM_CHEM,NUM_MOIST, &
              ids,ide, jds,jde, kds,kde, &
              ims,ime, jms,jme, kms,kme, &
              its,ite, jts,jte, kts,kte)
   else IF(conv_tr_aqchem == 1 ) THEN
         CALL  so2so4(1,chem,p_so2,p_sulf,p_h2o2,T_PHY,MOIST,           &
              gd_cloud, gd_cldfr,                                            &
              NUM_CHEM,NUM_MOIST, &
              ids,ide, jds,jde, kds,kde, &
              ims,ime, jms,jme, kms,kme, &
              its,ite, jts,jte, kts,kte)
   ENDIF


   call soa_vbs_driver (ktau,dtstep,t_phy,moist,aerwrf,p8w,t8w, &
            rri,p_phy,chem,rho_phy,dz8w,relhum,z_at_w,                 &
            h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,               &
            vcsulf_old,vdrog3_vbs,                                   &
            kemit,& !brch_ratio,                                        &
            ids,ide, jds,jde, kds,kde,                               &
            ims,ime, jms,jme, kms,kme,                               &
            its,ite, jts,jte, kts,kte                                )

endif
! write(6,*)'in chem_driver, did gocart chemistry and aod '
      if(call_optics == 1 )then
      if(aer_ra_feedback == 2 )then
         call aero_opt('sw',dz8w,chem             &
                   ,rri,relhum,aod                       &
                   ,extt,ssca,asympar,num_chem                &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte )

      endif
      if(aer_ra_feedback == 1 )then
              call optical_driver(curr_secs,dtstep,          &
               chem,dz8w,rri,relhum,                               &
               h2oai,h2oaj,                                        &
               tauaersw,gaersw,waersw,bscoefsw,tauaerlw,           &
               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                &
               num_chem,chem_opt,ids,ide, jds,jde, kds,kde,        &      
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte)
              call aer_opt_out(aod,dz8w,                           &
                    ext_coeff,bscat_coeff,asym_par,                &
                    tauaersw,gaersw,waersw,tauaerlw,               &
                    num_ext_coef,num_bscat_coef,num_asym_par,      &
                    ids,ide, jds,jde, kds,kde,                     &
                    ims,ime, jms,jme, kms,kme,                     &
                    its,ite, jts,jte, kts,kte )
              call aer_ra(dz8w                                     &
                   ,extt,ssca,asympar,nbands                       &
                   ,tauaersw,gaersw,waersw,tauaerlw                &
                   ,ids,ide, jds,jde, kds,kde                      &
                   ,ims,ime, jms,jme, kms,kme                      &
                   ,its,ite, jts,jte, kts,kte )


      endif
      endif   ! optics

       CALL sum_pm_soa_vbs (                                           &
            rri, chem, h2oaj, h2oai,                                   &
            pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                &
            dust_opt,ids,ide, jds,jde, kds,kde,                        &
            ims,ime, jms,jme, kms,kme,                                 &
            its,ite, jts,jte, kts,kte                                  )
!
!      store aerosol optical variables for feedback in radiation
!
       if(aer_ra_feedback >= 1 )then
          do ib=1,nbands
          do j=jts,jte
          do k=kts,kte
          do i=its,ite
           ext_cof(k,j,ib)=extt(i,k,j,ib)
           sscal(k,j,ib)=ssca(i,k,j,ib)
           asymp(k,j,ib)=asympar(i,k,j,ib)
          enddo
          enddo
          enddo
          enddo
!         do j=jts,jte
!          diaga(7,j)=0.
!          diaga(8,j)=0.
!          diaga(9,j)=0.
!         do k=kts,kte
!         do i=its,ite
!          diaga(7,j)=diaga(7,j)+tauaersw(i,k,j,2)
!          diaga(8,j)=diaga(8,j)+tauaersw(i,k,j,3)
!          diaga(9,j)=diaga(9,j)+tauaersw(i,k,j,4)
!         enddo
!         enddo
!         enddo
!         print *,'in chem_driver ',nbands,maxval(extt),maxval(ext_cof)
! aod only output
          do j=jts,jte
          do i=its,ite
           aod2d(j)=aod(i,j)
!          aod2d(j)=dep_vel_o3(i,j)
          enddo
          enddo
!         print *,maxval(aod2d)
       endif   ! feedback to radiation
!
!
       do j=jts,jte
       do k=kts,kte
       do i=its,ite
        pm25(k,j)=pm2_5_dry(i,k,j)
        p10(k,j)=pm10(i,k,j)
!       if(call_chem.eq.1)then
!        oh_bg(k,j)=max(0.,backg_oh(i,k,j))
!        h2o2_bg(k,j)=max(0.,backg_h2o2(i,k,j))
!        no3_bg(k,j)=max(0.,backg_no3(i,k,j))
!       endif
       enddo
       enddo
       enddo
!
! put chem stuff back into tracer array
!
      do nv=1,num_chem
         nvv=nbegin+nv
         do j=jts,jte
            do i=its,ite
               do k=kts,kte
                tr3d(k,j,nvv)=max(epsilc,chem(i,k,j,nv))
                trdp(k,j,nvv)=tr3d(k,j,nvv)*dp3d(k,j)
               enddo
               !wet_dep(j,nv) = var_rmv(i,j,nv)
            enddo
         enddo
       enddo

!!sms$compare_var(st3d   , "end chem_driver ")
!!sms$compare_var(sm3d   , "end chem_driver ")
!!sms$compare_var(ts2d   , "end chem_driver ")
!!sms$compare_var(us2d   , "end chem_driver ")
!!sms$compare_var(rsds   , "end chem_driver ")

      ret = gptlstop ('chem_driver')

     END subroutine racmsoavbs

SUBROUTINE get_diurnal_rate (it,dtlt,ihour,imonth1,idate1,iyear1,glat,glon,rate)
integer, parameter :: mxp=1,myp=1,ia=1,iz=mxp,ja=1,jz=myp ! horizontal grid domain
real, parameter :: pi180 = 3.1415/180. ,emiss_cycle_time = 86400.
   integer :: imonth1,idate1,iyear1,itime1,ihour
   real :: dtlt,current_time
   real, dimension(mxp,myp) :: glat,glon,rate,dcnorma_inv,cosz
   integer :: it,its
   real :: check_normalization ! the sum over 24hours must be = 1 

   itime1 = ihour*100
   !dtlt = 60.

   check_normalization=0.0
    DO its=1,it ! NINT(86400./dtlt)

    !- current time to calculate the diurnal variation rate of biogenic emission
    current_time = (its-1) * dtlt

    IF(MOD(current_time+0.001,emiss_cycle_time) .LT. dtlt .OR. current_time .LT..01)     then
         !print*,"get diurnal variation of cosz",it
         CALL get_diurnal_cycle_normalized(mxp,myp,ia,iz,ja,jz,dtlt,glat,&
                                           glon,imonth1,idate1,iyear1,itime1,&
                                           pi180,dcnorma_inv)
    ENDIF

    CALL get_current_cosz(mxp,myp,ia,iz,ja,jz,dtlt,glat, &
                         glon,imonth1,idate1,iyear1,itime1, &
                         pi180,cosz,current_time)

    !- 
    rate(:,:)= dcnorma_inv(:,:) * cosz(:,:)
    !- in the emission model:
    !  emission (kg/s) = emission (k/day) * rate /86400.

    check_normalization=check_normalization+rate(1,1)
!    print*,"dc=",rate(:,:),check_normalization
   ENDDO



END subroutine get_diurnal_rate



SUBROUTINE get_diurnal_cycle_normalized(m2,m3,ia,iz,ja,jz,dtlt,glat, &
                                          glon,imonth1,idate1,iyear1,itime1, &
                                         pi180,dcnorma_inv)
    IMPLICIT NONE
    ! original
    INTEGER , INTENT(IN) :: m2
    INTEGER , INTENT(IN) :: m3
    INTEGER , INTENT(IN) :: ia
    INTEGER , INTENT(IN) :: iz
    INTEGER , INTENT(IN) :: ja
    INTEGER , INTENT(IN) :: jz
    REAL    , INTENT(IN) :: dtlt
    REAL    , INTENT(IN) :: glat(m2,m3)
    REAL    , INTENT(IN) :: glon(m2,m3)
    
    ! mem_grid
    INTEGER , INTENT(IN) :: imonth1
    INTEGER , INTENT(IN) :: idate1
    INTEGER , INTENT(IN) :: iyear1
    INTEGER , INTENT(IN) :: itime1
      
    ! rconstants
    REAL    , INTENT(IN) :: pi180

    real, dimension(m2,m3), intent(out) :: dcnorma_inv

    INTEGER :: jday,i,j
    INTEGER :: julianday

    REAL :: solfac,tdec,sdec,cdec,declin,d0,d02,dayhr,radlat,cslcsd,snlsnd, &
            gglon,dayhrr,hrangl

    !- local var
    INTEGER it
    REAL time_x,dt_x,cosz
    REAL dcnorma(m2,m3)
    REAL xxx

    dt_x=0.
    time_x=0.
    dcnorma(:,:)=0.


    DO it=1,NINT(86400./dtlt) 

       call julday(imonth1,idate1,iyear1,julianday) 
       jday=julianday
       jday = jday + NINT(time_x/86400.)
       !      sdec - sine of declination, cdec - cosine of declination
       declin = -23.5 * COS(6.283 / 365. * (jday + 9)) * pi180
       sdec = SIN(declin)
       cdec = COS(declin)

       ! Find the factor, solfac, to multiply the solar constant to correct
       ! for Earth s varying distance to the sun.

       !d0 = 6.2831853 * float(jday-1) / 365.
       !d02 = d0 * 2.
       !solfac = 1.000110 + 0.034221 * cos (d0) + 0.001280 * sin(d0)  &
       !     + 0.000719 * cos(d02) + 0.000077 * sin(d02)

       ! Find the hour angle, THEN get cosine of zenith angle.

       dayhr = time_x / 3600. + float(itime1/100) + float(MOD(itime1,100)) / 60.

       DO j = ja,jz
          DO i = ia,iz
             radlat = glat(i,j) * pi180
             !IF (lonrad .eq. 0) radlat = centlat(1) * pi180
             !IF (radlat .eq. declin) radlat = radlat + 1.e-5
             cslcsd = COS(radlat) * cdec
             snlsnd = SIN(radlat) * sdec
             !gglon = glon(i,j)
             !IF (lonrad .eq. 0) gglon = centlon(1)
             dayhrr = MOD(dayhr+glon(i,j)/15.+24.,24.)
             hrangl = 15. * (dayhrr - 12.) * pi180
             cosz = snlsnd + cslcsd * COS(hrangl)
             
             cosz = min(max(cosz,0.), 1.0) 
             
             dcnorma(i,j)=dcnorma(i,j)+cosz
             if (dcnorma(i,j).eq.0.) dcnorma(i,j)=1.0E-20
             !print*,"dc=",time_x,dcnorma(i,j),cosz,jday

          END DO
       END DO
       time_x=time_x+dtlt
    END DO
    
    DO j = ja,jz
       DO i = ia,iz
           !- invert dcnorma to save computation time
           dcnorma_inv(i,j)=1./(dcnorma(i,j))
    ENDDO; ENDDO


  END SUBROUTINE get_diurnal_cycle_normalized
 !------------------------------------------------------------------
  SUBROUTINE get_current_cosz(m2,m3,ia,iz,ja,jz,dtlt,glat, &
                              glon,imonth1,idate1,iyear1,itime1, &
                              pi180,cosz,time_x)
      
    IMPLICIT NONE
    ! original
    INTEGER , INTENT(IN) :: m2
    INTEGER , INTENT(IN) :: m3
    INTEGER , INTENT(IN) :: ia
    INTEGER , INTENT(IN) :: iz
    INTEGER , INTENT(IN) :: ja
    INTEGER , INTENT(IN) :: jz
    REAL    , INTENT(IN) :: dtlt
    REAL    , INTENT(IN) :: glat(m2,m3)
    REAL    , INTENT(IN) :: glon(m2,m3)
    
    ! mem_grid
    INTEGER , INTENT(IN) :: imonth1
    INTEGER , INTENT(IN) :: idate1
    INTEGER , INTENT(IN) :: iyear1
    INTEGER , INTENT(IN) :: itime1
      
    ! rconstants
    REAL    , INTENT(IN) :: pi180

    real, dimension(m2,m3), intent(out) ::cosz

    REAL,intent(in) :: time_x
    INTEGER :: jday,i,j
    INTEGER :: julianday

    REAL :: solfac,tdec,sdec,cdec,declin,d0,d02,dayhr,radlat,cslcsd,snlsnd, &
            gglon,dayhrr,hrangl

    call julday(imonth1,idate1,iyear1,julianday) ! do not change the position of this line
    jday = julianday
    jday = jday + NINT(time_x/86400.)
    ! sdec - sine of declination, cdec - cosine of declination
    declin = -23.5 * COS(6.283 / 365. * (jday + 9)) * pi180
    sdec = SIN(declin)
    cdec = COS(declin)

    ! Find the factor, solfac, to multiply the solar constant to correct
    ! for Earth s varying distance to the sun.

    !d0 = 6.2831853 * float(jday-1) / 365.
    !d02 = d0 * 2.
    !solfac = 1.000110 + 0.034221 * cos (d0) + 0.001280 * sin(d0)  &
    !     + 0.000719 * cos(d02) + 0.000077 * sin(d02)
    ! Find the hour angle, THEN get cosine of zenith angle.

    dayhr = time_x / 3600. + float(itime1/100) + float(MOD(itime1,100)) / 60.

    DO j = ja,jz
          DO i = ia,iz
             radlat = glat(i,j) * pi180
             !IF (lonrad .eq. 0) radlat = centlat(1) * pi180
             !IF (radlat .eq. declin) radlat = radlat + 1.e-5
             cslcsd = COS(radlat) * cdec
             snlsnd = SIN(radlat) * sdec
             !gglon = glon(i,j)
             !IF (lonrad .eq. 0) gglon = centlon(1)
             dayhrr = MOD(dayhr+glon(i,j)/15.+24.,24.)
             hrangl = 15. * (dayhrr - 12.) * pi180
             cosz(i,j) = snlsnd + cslcsd * COS(hrangl)
             cosz(i,j) = min(max(cosz(i,j),0.), 1.0) 
         END DO
     END DO
    
  END SUBROUTINE get_current_cosz

  SUBROUTINE JULDAY (imonth,iday,iyear,julianday)

    ! returns which day of the year is the input date

    integer, intent(in) :: imonth
    integer, intent(in) :: iday
    integer, intent(in) :: iyear
    integer, intent(out) :: julianday

    julianday= iday  &
         + min(1,max(0,imonth-1))*31  &
         + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4))))  &
         + min(1,max(0,imonth-3))*31  &
         + min(1,max(0,imonth-4))*30  &
         + min(1,max(0,imonth-5))*31  &
         + min(1,max(0,imonth-6))*30  &
         + min(1,max(0,imonth-7))*31  &
         + min(1,max(0,imonth-8))*31  &
         + min(1,max(0,imonth-9))*30  &
         + min(1,max(0,imonth-10))*31  &
         + min(1,max(0,imonth-11))*30  &
         + min(1,max(0,imonth-12))*31
  END SUBROUTINE JULDAY

END MODULE MODULE_CHEM_DRIVER_RACMSOAVBS

