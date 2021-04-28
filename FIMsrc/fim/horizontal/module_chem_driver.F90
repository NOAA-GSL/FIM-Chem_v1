MODULE MODULE_CHEM_DRIVER_GOCART
   IMPLICIT NONE

#include <gptl.inc>

CONTAINS
     subroutine gocart(ktau)
!
! FIM version variables
!
  USE module_control, only: dt,nip, nvlp1,ntra,ntrb,CallRadiation,numphr
  USE fimnamelist, only:  yyyymmddhhmm, nvl
  USE module_wrf_control, only: ims,ime,jms,jme,kms,kme,           &
                                ids,ide,jds,jde,kds,kde,           &
                                its,ite,jts,jte,kts,kte,           &
                                nvl_gocart,num_emis_ant,num_moist, &
                                num_ext_coef,num_bscat_coef,num_asym_par,&
                                num_chem,num_soil_layers,numgas,nbands,  &
                                num_emis_vol,CallChemistry,CallBiom,     &
                                num_ebu_in,num_ebu
!TODO:  replace use-association of FIM dynamics variables with coupler
  USE module_variables
  USE module_chem_variables
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
  USE module_dms_emis,only: gocart_dmsemis
  USE module_ctrans_grell,only:grelldrvct
  USE module_initial_chem_namelists,only:p_qc,p_dms,p_seas_1,chem_opt,biomass_burn_opt,p_oc1,   &
                                         p_bc1,p_p25,p_p10,p_sulf,p_so2,seas_opt,dust_opt,      &
                                         dmsemis_opt,chem_in_opt,aer_ra_feedback,kemit,         &
                                         p_ebu_oc,p_ebu_bc,p_ebu_pm25,p_ebu_pm10,p_ebu_so2,     &
                                         p_tr1,p_tr2,chem_conv_tr
  USE module_dry_dep_driver,only:dry_dep_driver
  USE module_gocart_aerosols,only:sum_pm_gocart,gocart_aerosols_driver
  USE module_gocart_chem,only:gocart_chem_driver 
  USE module_data_gocart_chem,only:airmw,mw_so4_aer
  USE module_optical_driver, only: optical_driver
  USE module_aer_opt_out, only: aer_opt_out
  USE module_aer_ra, only: aer_ra
  USE module_chemvars
  USE module_wrfphysvars
  USE units, only: getunit, returnunit
  USE fimnamelist,only: readrestart

  IMPLICIT NONE

  INTEGER, INTENT (IN) :: KTAU

   real :: var_rmv(ims:ime, jms:jme ,num_chem)
   real :: swdown(ims:ime, jms:jme )
   real :: dep_vel_o3(ims:ime, jms:jme )
   real :: e_co(ims:ime, jms:jme )
   real :: raincv_b(ims:ime, jms:jme )
   real :: cu_co_ten(ims:ime, kms:kme,jms:jme )
   REAL :: dusthelp(ims:ime, jms:jme ),seashelp(ims:ime,jms:jme )
   real :: tr_fall(ims:ime, jms:jme,num_chem )
   real :: serial1(nip)
   !integer,save :: current_month,current_gmt,current_day,current_year,current_hour,day
   integer current_month,current_gmt,current_day,current_year,current_hour,day
   !shc end stuff for MEGAN v2.04

      REAL                     :: dtstep_plume,dtstep, dx, gmt
      REAL                     :: dust_alpha,dust_gamma
      REAL (kind=8) :: curr_secs,curr_mins,curr_days
!
! Local variables...
!
      INTEGER :: nbegin,ib,i, j, k, nv,nvv,ksub, dust_emiss_active, seasalt_emiss_active
      INTEGER :: call_plume,call_gocart,call_radiation,julday,hour
      REAL :: factor,factor2,conv,mwdry,xlv,maxv,minv
      CHARACTER (LEN=80) :: message,filename 
      CHARACTER(len=9 )                :: jdate 
      integer :: ret
      integer :: unitno
      integer :: ipn
      logical scale_fire_emiss
      integer :: idat(8),mdat(8),YEARi,MONTHi,DAYi
      real*8  :: rinc(5)
      LOGICAL, SAVE :: firstfire=.TRUE.
      !integer :: days(12)
      !data (days(i),i=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
      ret = gptlstart ('chem_driver')
! nbegin is the start address (-1) of the first chem variable in tr3d
    scale_fire_emiss=.false.
    nbegin=ntra+num_moist-3
    if(num_moist.gt.3)nbegin=ntra+num_moist-2
    dust_alpha=1.0
    dust_gamma=1.6
     call_plume=0

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
    dusthelp(:,:)=0.
    seashelp(:,:)=0.

!sms$compare_var(st3d   , "begin chem_driver ")
!sms$compare_var(sm3d   , "begin chem_driver ")
!sms$compare_var(ts2d   , "begin chem_driver ")
!sms$compare_var(us2d   , "begin chem_driver ")
!sms$compare_var(rsds   , "begin chem_driver ")

     call_gocart=0
     call_radiation=0

     if ((mod(ktau,CallChemistry)==0).or.(ktau==1)) then
          call_gocart=1
     endif
     if ((mod(ktau,CallRadiation)==0).or.(ktau==1)) then
          call_radiation=1
     endif
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
             num_emis_vol,kemit,call_gocart,ids,ide, jds,jde, kds,   &
             kde,plumestuff,mean_fct_agtf,mean_fct_agef,             &
             mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,&
             firesize_agsv,firesize_aggr,chem_in_opt,                &
             ims,ime, jms,jme, kms,kme,                              &
             its,ite,jts,jte,kts,kte)
     mwdry=28.
!if(mod(ktau,CallChemistry)==0.or.ktau==1) then
! write(6,*)'in chem_driver, now do chemistry = ',ktau,dtstep,CallChemistry,kemit
  if(seas_opt == 1 )then
!    print *,'get seasalt emissions'
     call gocart_seasalt_driver(ktau,dt,rri,t_phy,moist,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,                  &
         xland,xlat,xlong,area,grvity,emis_seas, &
         seashelp,num_emis_seas,num_moist,num_chem,        &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte)
  endif
  if(dust_opt == 1 )then
     call gocart_dust_driver(ktau,dt,rri,t_phy,moist,u_phy,       &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,ivgtyp,isltyp,&
         vegfra,xland,xlat,xlong,gsw,area,grvity,emis_dust,srce_dust, & 
         dusthelp,num_emis_dust,num_moist,num_chem,num_soil_layers,   &
         current_month,                                               &
         ids,ide, jds,jde, kds,kde,                                   &
         ims,ime, jms,jme, kms,kme,                                   &
         its,ite, jts,jte, kts,kte)
  endif
  if(dust_opt == 3 )then
     call gocart_dust_afwa_driver(ktau,dt,rri,t_phy,moist,u_phy,      &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,ivgtyp,isltyp,&
         vegfra,xland,xlat,xlong,gsw,area,grvity,emis_dust,srce_dust, &
         dusthelp,ust,znt,clayfrac,sandfrac,dust_alpha,dust_gamma,    &
         num_emis_dust,num_moist,num_chem,num_soil_layers,            &
         ids,ide, jds,jde, kds,kde,                                   &
         ims,ime, jms,jme, kms,kme,                                   &
         its,ite, jts,jte, kts,kte)
  endif
!endif

if(call_plume == 1 )  then
          call plumerise_driver (ktau,dtstep,num_chem,num_moist,num_ebu, &
           num_ebu_in,ebu,ebu_in,mean_fct_agtf,mean_fct_agef,mean_fct_agsv,mean_fct_aggr, &
           firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr, &
           'GOCART','BIOMASSB', t_phy,moist,                                     &
           rho_phy,vvel,u_phy,v_phy,p_phy,                              &
           z_at_w,scale_fire_emiss,                                     &
           ids,ide, jds,jde, kds,kde,                                        &
           ims,ime, jms,jme, kms,kme,                                        &
           its,ite, jts,jte, kts,kte                                         )
endif
!lzhang
if(chem_opt >= 300 .and. (dust_opt == 1 .or. dust_opt ==3))then
     do j=jts,jte
     do i=its,ite
       emi_d1(j)=emis_dust(i,1,j,1)
       emi_d2(j)=emis_dust(i,1,j,2)
       emi_d3(j)=emis_dust(i,1,j,3)
       emi_d4(j)=emis_dust(i,1,j,4)
       emi_d5(j)=emis_dust(i,1,j,5)
     enddo
     enddo
endif

if(dmsemis_opt == 1 ) then
   call gocart_dmsemis(dt,rri,t_phy,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,dms_0,tsk,                  &
         ivgtyp,isltyp,xland,area,grvity,mwdry, &
         num_chem,p_dms,ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte)
!    do j=jts,jte
!    do i=its,ite
!       diaga(6,j)=-(diaga(6,j)-chem(i,1,j,p_dms))/dt
!    enddo
!    enddo
endif

if(dust_opt == 1 .or. seas_opt == 1 .or. dust_opt == 3)then

   call gocart_settling_driver(dt,t_phy,moist,  &
         chem,rho_phy,dz8w,p8w,p_phy,         &
         dusthelp,seashelp,area,grvity,                         &
         num_moist,num_chem,                 &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte)
endif
!
! 10 volcanic size bins
!
if(CHEM_OPT == 316 )  then
    call vash_settling_driver(dt,t_phy,moist,                              &
         chem,rho_phy,dz8w,p8w,p_phy,area,                                 &
         ash_fall,grvity,num_moist,num_chem    ,                                &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
     do j=jts,jte
     do i=its,ite
        ashfall(j)=ash_fall(i,j)
     enddo
     enddo
endif
!
! 4 volcanic size bins
!
!    do j=jts,jte
!    do i=its,ite
!       diaga(2,j)=chem(i,1,j,p_p25)
!    enddo
!    enddo
    call vashshort_settling_driver(dt,t_phy,moist,                              &
         chem,rho_phy,dz8w,p8w,p_phy,area,                                 &
         ash_fall,grvity,num_moist,num_chem    ,                                &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!    do j=jts,jte
!    do i=its,ite
!       diaga(2,j)=(chem(i,1,j,p_p25)-diaga(2,j))/dt
!    enddo
!    enddo
     do j=jts,jte
     do i=its,ite
        ashfall(j)=ash_fall(i,j)
     enddo
     enddo
!
! add biomass burning emissions at every timestep
!
if(BIOMASS_BURN_OPT == 1 )  then
  do i=its,ite
     do k=kts,kte-2
       do j=jts,jte
!factro for pm emissions, factor2 for burn emissions
         factor=dt*rri(i,k,j)/dz8w(i,k,j)
         factor2=4.828e-4*dt*rri(i,k,j)/(60.*dz8w(i,k,j))
         chem(i,k,j,p_oc1)=chem(i,k,j,p_oc1)+(ebu(i,k,j,p_ebu_oc))*factor
         chem(i,k,j,p_bc1)=chem(i,k,j,p_bc1)+(ebu(i,k,j,p_ebu_bc))*factor
         chem(i,k,j,p_p25)=chem(i,k,j,p_p25)+(ebu(i,k,j,p_ebu_pm25))*factor
         chem(i,k,j,p_p10)=chem(i,k,j,p_p10)+(ebu(i,k,j,p_ebu_pm10))*factor
         chem(i,k,j,p_so2)=chem(i,k,j,p_so2)+(ebu(i,k,j,p_ebu_so2))*factor2
       enddo
     enddo
  enddo
endif
!
! subgrid convective transport
!
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

       call dry_dep_driver(ktau,dt,julday,current_month,t_phy,p_phy,&
               moist,p8w,rmol,rri,gmt,t8w,rcav,                          &
               chem,rho_phy,dz8w,exch_h,hfx,                              &
               ivgtyp,tsk,gsw,vegfra,pbl,ust,znt,zmid,z_at_w,        &
               xland,xlat,xlong,h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,     &
               anh3,dep_vel_o3,grvity,                                    &
               e_co,kemit,snowh,numgas,                                   &
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

if(call_gocart == 1)then
! write(6,*)'in chem_driver, now do gocart chemistry and aod '
       !call gocart_chem_driver(ktau,dtstep, gmt,julday,t_phy,moist,                                      &
       call gocart_chem_driver(ktau,dt,dtstep, gmt,julday,t_phy,moist,                                      &
         chem,rho_phy,dz8w,p8w,backg_oh,oh_t,backg_h2o2,h2o2_t,backg_no3,no3_t, &
         area,grvity,xlat,xlong,ttday,tcosz, &
         chem_opt,num_chem,num_moist,                                      &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
       call gocart_aerosols_driver(ktau,dtstep,t_phy,moist,  &
         chem,rho_phy,dz8w,p8w,area,grvity,         &
         chem_opt,num_chem,num_moist,                                      &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
endif

if(call_radiation == 1)then
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
endif
!      vcsulf_old(its:ite,kts:kte,jts:jte) = &
!           max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
!      vcso2_old(its:ite,kts:kte,jts:jte) = &
!           max(chem(its:ite,kts:kte,jts:jte,p_so2),epsilc)
!      vch2o2_old(its:ite,kts:kte,jts:jte) = &
!           max(chem(its:ite,kts:kte,jts:jte,p_h2o2),epsilc)

      call sum_pm_gocart (                                      &
            rri, chem,pm2_5_dry, pm2_5_dry_ec, pm10,                   &
            num_chem,chem_opt,ids,ide, jds,jde, kds,kde,                        &
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
! pm25 and pm10 for output , not for tracer options
!
       do j=jts,jte
       do k=kts,kte
       do i=its,ite
        pm25(k,j)=pm2_5_dry(i,k,j)
        p10(k,j)=pm10(i,k,j)
        ebu_oc(k,j)=ebu(i,k,j,p_ebu_oc)
        if(call_gocart.eq.1)then
        oh_bg(k,j)=max(0.,oh_t(i,k,j))
        h2o2_bg(k,j)=max(0.,h2o2_t(i,k,j))
        no3_bg(k,j)=max(0.,no3_t(i,k,j))
        endif
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
               wet_dep(j,nv) = var_rmv(i,j,nv)
            enddo
         enddo
       enddo

!sms$compare_var(st3d   , "end chem_driver ")
!sms$compare_var(sm3d   , "end chem_driver ")
!sms$compare_var(ts2d   , "end chem_driver ")
!sms$compare_var(us2d   , "end chem_driver ")
!sms$compare_var(rsds   , "end chem_driver ")

      ret = gptlstop ('chem_driver')


     END subroutine gocart

END MODULE MODULE_CHEM_DRIVER_GOCART
