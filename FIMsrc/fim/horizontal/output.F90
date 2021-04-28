module module_output

  use module_constants
  use module_control,           only: dt,nip,ntra,ntrb,nvar2d,nvarsig,nvarp,    &
                                      nvlsig,nvlp,nvlp1,ArchvStep,hrs_in_month, &
                                      itsDFI,ArchvStep6h
  use fimnamelist,              only: FixedGridOrder,nvl,ArchvTimeUnit,         &
                                      PrintDiags,restart_freq,yyyymmddhhmm,     &
                                      enkfio_out,readrestart,itsstart,digifilt, &
                                      cycle_freq,enkf_diag
  use module_variables,         only: potvor,th_pvsrf,pr_pvsrf,us_pvsrf,vs_pvsrf
  use module_core_setup,        only: use_write_tasks
  use module_op_diag,           only: op_diag
  use module_outFMTed,          only: outFMTed
  use module_outqv,             only: outqv
  use module_outqv_mn_lat,      only: outqv_mn_lat
  use module_outqv_mn_lat_land, only: outqv_mn_lat_land
  USE module_outvar_enkf_spectral,       ONLY: outvar_enkf_spectral
  USE module_outvar_enkf_sfcio,          ONLY: outvar_enkf_sfcio
  USE module_enkf_io, only : write_enkfio
  use module_printMAXMIN,       only: printMAXMIN
  use findmaxmin1
  use mdul_pvsurf
  use icosio_wrapper,           only: maybe_write
  use module_wrf_control,       only: num_moist,num_chem
  use global_bounds,            only: ims, ime, ips, ipe
  use module_sfc_variables,     only: rsds_ave,rlds_ave,rsus_ave,rlus_ave,rsdt_ave,rsut_ave,rlut_ave,	&
				      qf_ave,hf_ave,rn2d6_0,rc2d6_0,rg2d6_0,cld_hi,cld_md,cld_lo,       &
                                      cld_tt,cld_bl,cld_hi_ave,cld_md_ave,cld_lo_ave,cld_tt_ave,cld_bl_ave

  implicit none

#include <gptl.inc>

contains

!*********************************************************************
!	output
!	Output program for fim global model
!	Alexander E. MacDonald  12/27/2004
!	J. Lee                  September, 2005
!*********************************************************************

subroutine output (its, nts,				& ! index time step, final timestep
                   us3d, vs3d, dp3d, sdot,		& ! west wind, south wind, delta pres, vert.velocity
                   pr3d, ex3d, mp3d,			& ! pressure, Exner, mont pot,
                   tr, rh3d, relvor, ws3d,		& ! tracers, humidity, vorticity, omega
                   chem_opt,diaga, diagb,		& ! diagnostic arrays
                   ph3d, tk3d, rn2d, sn2d, rc2d, pw2d, pq2d,  &
                   ts2d, us2d, rsds, rlds, rsus, rlus,	&
                   rsdt, rsut, rlut, qf2d, hf2d,	&
                   st3d, sm3d, t2m2d, q2m2d, canopy2d,	& ! geopotential, accumulated precip/rainfall
                   fice2d, hice2d, sheleg2d, slmsk2d,	&
		   u10m, v10m, rn2d0, rc2d0, rg2d0,	&
                   TimingBarriers, curr_write_time,	&
                   wt_int_rev,k_int_rev)

! Arguments
    integer, intent(in) :: its      ! current time step
    integer, intent(in) :: nts      ! final time step
    integer, intent(in) :: chem_opt ! for chem pressure level files we need to know chem_opt
    real, intent(in)    :: us3d(nvl,  ims:ime)
    real, intent(in)    :: vs3d(nvl,  ims:ime)
    real, intent(in)    :: dp3d(nvl,  ims:ime)
    real, intent(in)    :: sdot(nvlp1,ims:ime)
    real, intent(in)    :: pr3d(nvlp1,ims:ime)
    real, intent(in)    :: mp3d(nvl,  ims:ime)
    real, intent(in)    :: diaga(nvl, ims:ime)
    real, intent(in)    :: diagb(nvl, ims:ime)
    real, intent(in)    :: relvor(nvl,ims:ime)
    real, intent(in)    :: ws3d(nvl  ,ims:ime)
    real, intent(in)    :: ph3d(nvlp1,ims:ime)
    real, intent(in)    :: ex3d(nvlp1,ims:ime)
    real, intent(in)    :: tr(nvl,    ims:ime,ntra+ntrb)
    real, intent(inout) :: rh3d(nvl,  ims:ime)
    real, intent(inout) :: tk3d(nvl,  ims:ime)
    real, intent(in)    :: rn2d(ims:ime)
    real, intent(inout) :: sn2d(ims:ime)
    real, intent(in)    :: rc2d(ims:ime)
    real, intent(in)    :: u10m(ims:ime)
    real, intent(in)    :: v10m(ims:ime)
!JR Moved these 5 things to arg list so they can be written to the restart file.
    real, intent(inout) :: rn2d0(ims:ime)
    real, intent(inout) :: rc2d0(ims:ime)
    real, intent(inout) :: rg2d0(ims:ime)
    real, intent(inout) :: pw2d(ims:ime)
    real, intent(inout) :: pq2d(ims:ime)
    real, intent(in)    :: ts2d(ims:ime)
    real, intent(in)    :: us2d(ims:ime)
    real, intent(in)    :: hf2d(ims:ime)
    real, intent(in)    :: qf2d(ims:ime)
    real, intent(in)    :: rsds(ims:ime)
    real, intent(in)    :: rlds(ims:ime)
    real, intent(in)    :: rsus(ims:ime)
    real, intent(in)    :: rlus(ims:ime)
    real, intent(in)    :: rsdt(ims:ime)
    real, intent(in)    :: rsut(ims:ime)
    real, intent(in)    :: rlut(ims:ime)
    real, intent(in)    :: st3d(4,ims:ime)
    real, intent(in)    :: sm3d(4,ims:ime)
    REAL, INTENT(IN)    :: T2M2D(IMS:IME)
    REAL, INTENT(IN)    :: Q2M2D(IMS:IME)
!JR ADDED ITEMS FROM MODULE_SFC_VARIABLES SO EVERYTHING COMES FROM INPUT ARG LIST
!JR RATHER THAN SOME USED FROM MODULE
    REAL, INTENT(IN) :: CANOPY2D(IMS:IME)
    REAL, INTENT(IN) :: FICE2D(IMS:IME)
    REAL, INTENT(IN) :: HICE2D(IMS:IME)
    REAL, INTENT(IN) :: SHELEG2D(IMS:IME)
    REAL, INTENT(IN) :: SLMSK2D(IMS:IME)

    LOGICAL, INTENT(IN)    :: TIMINGBARRIERS
    INTEGER, INTENT(INOUT) :: CURR_WRITE_TIME ! MOST RECENT TIME VARS. WERE WRITTEN

    REAL, INTENT(inout) :: wt_int_rev(nvl,ims:ime)! weights for reverse interpolation from
! gfs sig levels back to hybrid levels
! for fim ensemble data assimila\tion
    INTEGER, INTENT(inout) :: k_int_rev(nvl,ims:ime)! k levs for reverse interpolation from 
!GFS sig levels back to hybrid levels

! LOCAL VARIABLES
    REAL :: TD3D(NVL,IMS:IME)
    REAL :: MSLP(IMS:IME)
    REAL :: RG2D(IMS:IME)
    REAL :: RN_XH(IMS:IME)
    REAL :: RC_XH(IMS:IME)
    REAL :: RG_XH(IMS:IME)
    REAL :: RN_6H(IMS:IME)
    REAL :: SN_6H(IMS:IME)
    REAL :: RC_6H(IMS:IME)
    REAL :: RG_6H(IMS:IME)
    REAL :: G3P(NVLP,IMS:IME,NVARP)
    !      (NVARP=6 + NUM_CHEM)
    !      1=HEIGHT,2=TEMP,3=RH (W.R.T. WATER),4=U WIND,5=V WIND
    REAL :: G3P_chem(NVLP,IMS:IME,num_chem+2)
!   REAL :: G3P_chem(NVLP,IMS:IME,num_chem)
    !      (NVARP=6 + NUM_CHEM)
    !      1=HEIGHT,2=TEMP,3=RH (W.R.T. WATER),4=U WIND,5=V WIND
    REAL, allocatable :: G3SIG (:,:,:)
    REAL :: G2D(IMS:IME,NVAR2D)
    !       ADDITIONAL DIAGNOSTIC 2D VARIABLES FROM OP_DIAG.F90
    REAL :: SPD10M_DIF(IMS:IME)
    REAL :: T2M_DIF(IMS:IME)
    real :: sn_xh  (ims:ime)  !  snow accumulation since last xh time

    integer :: time, ipn, laststep,ichemstart
    integer :: accum_start     ! value of "time" from previous output call
    integer :: accum_start_6h  ! value of "time" 6 hours ago 
    real*8  :: toutput, tmaxmin
    INTEGER :: ret,k
    logical, parameter:: flux_avg=.false.	! T: write fluxes averaged over "ArchvIntvl"
    logical, parameter:: output2d=.false.	! T: write 2d & g3p fields only to save diskspace

    real, external :: its2time

    allocate (G3SIG(NVLSIG+1,IMS:IME,NVARSIG))
    time = nint(its2time(its))

! there is extra varibles in nvarp, so chem starts one later than ntra!!
! need to recover original nvarp, assuming there is some sort of chemistry in this run
! so FROM FIMsrc/cntl/module_wrf_control
!   if(num_moist.lt.4)then
!    nvarp=nvarp+num_chem+num_moist - 3
! else
!    nvarp=nvarp+num_chem+num_moist - 2       ! nvarp includes all variables for pressure level output
! endif
! THEREFORE:

!    ichemstart=(nvarp -num_chem-num_moist +3) +num_moist - 3 ! as of sep 2012, this should be 6
!   if (num_moist > 3) then
!     ichemstart=(nvarp-num_chem-num_moist +2) + num_moist - 2
!   end if

!SB - 1/26/2014 - chem vars on isobaric levels are now in separate g3p_chem array
!      to avoid awkward indexing
    ichemstart=0

    if (its == 0) then
!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
      do ipn=ips,ipe
	rsds_ave(ipn) = 0.
	rlds_ave(ipn) = 0.
	rsus_ave(ipn) = 0.
	rlus_ave(ipn) = 0.
	rsdt_ave(ipn) = 0.
	rsut_ave(ipn) = 0.
	rlut_ave(ipn) = 0.
	  qf_ave(ipn) = 0.
	  hf_ave(ipn) = 0.
	   sn2d (ipn) = 0.
	   rn2d0(ipn) = 0.
	   rc2d0(ipn) = 0.
	   rg2d0(ipn) = 0.
	 rn2d6_0(ipn) = 0.
	 rc2d6_0(ipn) = 0.
	 rg2d6_0(ipn) = 0.
      cld_hi_ave(ipn) = 0.
      cld_md_ave(ipn) = 0.
      cld_lo_ave(ipn) = 0.
      cld_tt_ave(ipn) = 0.
     !cld_bl_ave(ipn) = 0.
          cld_hi(ipn) = 0.
          cld_md(ipn) = 0.
          cld_lo(ipn) = 0.
          cld_tt(ipn) = 0.
         !cld_bl(ipn) = 0.
      end do
!$OMP END PARALLEL DO
!sms$ignore end
    end if			! (its == 0)

! --- accumulate fields averaged over "ArchvIntvl"
!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
      do ipn=ips,ipe
	rsds_ave(ipn) = rsds_ave(ipn) + rsds(ipn)
	rlds_ave(ipn) = rlds_ave(ipn) + rlds(ipn)
	rsus_ave(ipn) = rsus_ave(ipn) + rsus(ipn)
	rlus_ave(ipn) = rlus_ave(ipn) + rlus(ipn)
	rsdt_ave(ipn) = rsdt_ave(ipn) + rsdt(ipn)
	rsut_ave(ipn) = rsut_ave(ipn) + rsut(ipn)
	rlut_ave(ipn) = rlut_ave(ipn) + rlut(ipn)
	  qf_ave(ipn) =   qf_ave(ipn) + qf2d(ipn)
	  hf_ave(ipn) =   hf_ave(ipn) + hf2d(ipn)
      cld_hi_ave(ipn) = cld_hi_ave(ipn) + cld_hi(ipn)
      cld_md_ave(ipn) = cld_md_ave(ipn) + cld_md(ipn)
      cld_lo_ave(ipn) = cld_lo_ave(ipn) + cld_lo(ipn)
      cld_tt_ave(ipn) = cld_tt_ave(ipn) + cld_tt(ipn)
     !cld_bl_ave(ipn) = cld_bl_ave(ipn) + cld_bl(ipn)
      end do
!$OMP END PARALLEL DO
!sms$ignore end

!JR Write history info every so often (mod(its,archvstep) == 0)
    IF (MOD(its,ArchvStep) == 0) THEN
      accum_start = curr_write_time
      curr_write_time = time
      if (ips.eq.1) print *,'(subr.output) archiving data, time =',time

!sms$ignore begin
!$OMP PARALLEL DO SCHEDULE (static)
      do ipn=ips,ipe
        rg2d(ipn) = rn2d(ipn) - rc2d(ipn)
        ! Calculate x-hour interval precip (difference from total precip at the last
        ! output time)
        rn_xh(ipn) = rn2d(ipn) - rn2d0(ipn)
        rc_xh(ipn) = rc2d(ipn) - rc2d0(ipn)
        rg_xh(ipn) = rg2d(ipn) - rg2d0(ipn)
        rn2d0(ipn) = rn2d(ipn)
        rc2d0(ipn) = rc2d(ipn)
        rg2d0(ipn) = rg2d(ipn)
        spd10m_dif(ipn) = sqrt(u10m(ipn)**2 + v10m(ipn)**2) - &
                          sqrt(us3d(1,ipn)**2 + vs3d(1,ipn)**2)
        rsds_ave(ipn) = rsds_ave(ipn) / ArchvStep
        rlds_ave(ipn) = rlds_ave(ipn) / ArchvStep
        rsus_ave(ipn) = rsus_ave(ipn) / ArchvStep
        rlus_ave(ipn) = rlus_ave(ipn) / ArchvStep
        rsdt_ave(ipn) = rsdt_ave(ipn) / ArchvStep
        rsut_ave(ipn) = rsut_ave(ipn) / ArchvStep
        rlut_ave(ipn) = rlut_ave(ipn) / ArchvStep
          qf_ave(ipn) =   qf_ave(ipn) / ArchvStep
          hf_ave(ipn) =   hf_ave(ipn) / ArchvStep
       cld_hi_ave(ipn)=cld_hi_ave(ipn) / ArchvStep
       cld_md_ave(ipn)=cld_md_ave(ipn) / ArchvStep
       cld_lo_ave(ipn)=cld_lo_ave(ipn) / ArchvStep
       cld_tt_ave(ipn)=cld_tt_ave(ipn) / ArchvStep
      !cld_bl_ave(ipn)=cld_bl_ave(ipn) / ArchvStep
      end do
!$OMP END PARALLEL DO
!sms$ignore end

      IF (MOD(its,ArchvStep6h) == 0 .and. ArchvTimeUnit=='hr') THEN
        accum_start_6h = max(0, its-6)
!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
        DO ipn=ips,ipe
          rn_6h(ipn) = rn2d(ipn) - rn2d6_0(ipn)
          rc_6h(ipn) = rc2d(ipn) - rc2d6_0(ipn)
          rg_6h(ipn) = rg2d(ipn) - rg2d6_0(ipn)
          rn2d6_0(ipn) = rn2d(ipn)
          rc2d6_0(ipn) = rc2d(ipn)
          rg2d6_0(ipn) = rg2d(ipn)
        ENDDO
!$OMP END PARALLEL DO
!sms$ignore end
      ELSE
!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
        DO ipn=ips,ipe
          rn_6h(ipn) = 0.0
        ENDDO
!$OMP END PARALLEL DO
!sms$ignore end
      ENDIF

!---------------------------------------------------------------------------------
!         Output diagnostics of additional variables - op_diag
!---------------------------------------------------------------------------------
      ! Calculate various 3-d and 2-d diagnostic variables for outputting below.
      ! These are generally multivariate diagnostics, thus not do-able by the
      ! scalar FIMpost, which can only do horizontal interpolation (icos to
      ! lat/lon) one variable at a time.

      call op_diag (its, time,                     &
                    us3d, vs3d, dp3d, sdot, pr3d,  &
                    ex3d, mp3d, tr, ws3d,          &
                    ph3d, rn2d, rc2d, ts2d, us2d,  &
                    hf2d, qf2d, rsds, rlds, st3d,  &
                    sm3d, t2m2d,rn_xh,rn_6h,       &
                    ! Below are output variables from op_diag
                    tk3d, rh3d, td3d, pw2d, pq2d, mslp,  &
                    g3p, g3p_chem, g3sig, g2d, t2m_dif,  &
                    sn_xh,sn_6h, wt_int_rev, k_int_rev          &
                    )
!---------------------------------------------------------------------------------

    if (its.ne. 0) then
!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
        DO ipn=ips,ipe
          sn2d (ipn) = sn2d(ipn) + sn_xh(ipn)
        ENDDO
!$OMP END PARALLEL DO
!sms$ignore end
    ENDIF

      call pvsurf(its, relvor, dp3d, tr, potvor, th_pvsrf, pr_pvsrf,	&
                  us_pvsrf, vs_pvsrf)

      call outqv        (us3d,       1,   deg_lat, deg_lon,     1., 'U wind comp at k=1 - max/min')
      call outqv_mn_lat (us3d,       1,   deg_lat, deg_lon, 50.,1., 'U wind comp at k=1 - max/min')
      call outqv        (u10m,       1,   deg_lat, deg_lon,     1., 'U10M - max/min')
      call outqv_mn_lat (u10m,       1,   deg_lat, deg_lon, 50.,1., 'U10M - max/min')
      call outqv        (vs3d,       1,   deg_lat, deg_lon,     1., 'V wind comp at k=1 - max/min')
      call outqv_mn_lat (vs3d,       1,   deg_lat, deg_lon, 50.,1., 'V wind comp at k=1 - max/min')
      call outqv        (v10m,       1,   deg_lat, deg_lon,     1., 'V10M - max/min')
      call outqv_mn_lat (v10m,       1,   deg_lat, deg_lon, 50.,1., 'V10M - max/min')
      call outqv        (spd10m_dif, 1,   deg_lat, deg_lon,     1., 'spd_dif(wind-10M- wind(k=1)) - max/min')
      call outqv_mn_lat (spd10m_dif, 1,   deg_lat, deg_lon, 50.,1., 'spd_dif(wind-10M- wind(k=1)) - max/min')
      call outqv        (t2m2d,      1,   deg_lat, deg_lon,     1., 'T2M - max/min')
      call outqv_mn_lat (t2m2d,      1,   deg_lat, deg_lon, 50.,1., 'T2M - max/min')
      call outqv        (t2m_dif,    1,   deg_lat, deg_lon,     1., 'T2M - T(k=1)) - max/min')
      call outqv_mn_lat (t2m_dif,    1,   deg_lat, deg_lon, 50.,1., 'T2M - T(k=1)) - max/min')
      call outqv        (q2m2d,      1,   deg_lat, deg_lon,     1., 'Q2M - max/min')
      call outqv_mn_lat (q2m2d,      1,   deg_lat, deg_lon, 50.,1., 'Q2M - max/min')
      call outqv        (g2d(:,7),   1,   deg_lat, deg_lon,     1., 'Wind at 80 m - max/min')
      call outqv_mn_lat (g2d(:,7),   1,   deg_lat, deg_lon, 50.,1., 'Wind at 80 m - max/min')

      call outqv        (tr(:,:,4), nvl, deg_lat, deg_lon,     1.E5, 'O3 - max/min')
      call outqv_mn_lat (tr(:,:,4), nvl, deg_lat, deg_lon, 50.,1.E5, 'O3 - max/min')

      call outqv        (sheleg2d,   1,   deg_lat, deg_lon,     1., 'Snow water equivalent - max/min')
      call outqv_mn_lat (sheleg2d,   1,   deg_lat, deg_lon, 50.,1., 'Snow water equivalent - max/min')
      call outqv        (canopy2d,   1,   deg_lat, deg_lon,     1., 'Canopy water - max/min')
      call outqv_mn_lat (canopy2d,   1,   deg_lat, deg_lon, 30.,1., 'Canopy water - max/min')
      call outqv_mn_lat_land (canopy2d, 1, deg_lat, deg_lon, 30., slmsk2d, 1, 1., ' Canopy water - mean - land only')

      call outqv_mn_lat_land (g2d(:,5),1,deg_lat, deg_lon, 30., slmsk2d, 1, 1., ' RH w.r.t. PW - mean - land only')
      call outqv_mn_lat_land (pw2d,    1,deg_lat, deg_lon, 30., slmsk2d, 1, 1., ' PW - mean - land only')
      call outqv_mn_lat_land (pq2d,    1,deg_lat, deg_lon, 30., slmsk2d, 1, 1., ' Integ condensate - mean - land only')

      call outqv        (rn_xh,      1,   deg_lat, deg_lon,     1., 'Rain - since last output - max/min')
      call outqv        (rc_xh,      1,   deg_lat, deg_lon,     1., 'Rain-conv - since last output - max/min')
      call outqv        (rg_xh,      1,   deg_lat, deg_lon,     1., 'Rain-grid-scale - since last output - max/min')
      call outqv        (sn_xh,      1,   deg_lat, deg_lon,     1., 'Snow - since last output - max/min')
      if (mod(its,ArchvStep6h) == 0) then
        call outqv       (rn_6h, 1, deg_lat, deg_lon, 1., 'Rain -6h-total  - max/min')
        call outqv       (sn_6h, 1, deg_lat, deg_lon, 1., 'Snow -6h-total  - max/min')
      endif
      call outqv       (rn2d, 1, deg_lat, deg_lon, 1., 'Rain -run-total  - max/min')
      call outqv       (sn2d, 1, deg_lat, deg_lon, 1., 'Snow -run-total  - max/min')
      ! call outqv       (rc2d0, 1, deg_lat, deg_lon, 1., 'Rain0-conv  - max/min')
      if (its == itsStart-1 .or. (its==itsDFI-1.and.digifilt)) then
        call outqv        (hice2d,   1,   deg_lat, deg_lon,     1., 'Sea ice-hice - max/min')
        call outqv_mn_lat (hice2d,   1,   deg_lat, deg_lon, 50.,1., 'Sea ice-hice - max/min')
        call outqv        (fice2d,   1,   deg_lat, deg_lon,     1., 'Sea ice-fice - max/min')
        call outqv_mn_lat (fice2d,   1,   deg_lat, deg_lon, 50.,1., 'Sea ice-fice - max/min')
      end if
      IF (PrintDiags) THEN
        call outFMTed (its,pr3d,ph3d,us3d,vs3d,dp3d,mp3d,relvor,tr,rh3d,tk3d,ws3d, &
                       st3d,sm3d,rn2d,pw2d,pq2d,ts2d,us2d,hf2d,qf2d,rsds,rlds,time)
      else

        IF (enkfio_out .AND. .NOT. use_write_tasks .AND. .NOT. FixedGridOrder) THEN
! write only fields needed for EnKF DA cycle to two files.
! 3d dynamical and surface fields. Only supported if no write task, and FixedGridOrder
! = .false.
          CALL outvar_enkf_spectral(time,g3sig,ph3d(1,:))
          CALL outvar_enkf_sfcio(time,ph3d(1,:))

          IF ( time == cycle_freq ) CALL write_enkfio ()

        ENDIF			! enkfio_out

        IF (.NOT. enkfio_out .OR. enkf_diag) THEN

!JR Redo order for testing convenience to match specification in FIMnamelist ("wgrib" will give 
!JR same order). Optional argument "scalefactor" is scaling factor to be applied when GRIB file 
!JR is written. Optional argument "accum_start" specifies an accumulation start time different 
!JR from default when GRIB file is written.

          call maybe_write (its, 'hgtP', g3p(:,:,1), nvlp)
          call maybe_write (its, 'tmpP', g3p(:,:,2), nvlp)
          call maybe_write (its, 'rp3P', g3p(:,:,3), nvlp)
          call maybe_write (its, 'up3P', g3p(:,:,4), nvlp)
          call maybe_write (its, 'vp3P', g3p(:,:,5), nvlp)
          call maybe_write (its, 'vv3P', g3p(:,:,6), nvlp)
          call maybe_write (its, 'qc3P', g3p(:,:,7), nvlp)

          call maybe_write (its, 'rn2D', rn2d, 1, accum_start=0, twodfile=.true.)
          call maybe_write (its, 'sn2D', sn2d, 1, accum_start=0, twodfile=.true.)
          call maybe_write (its, 'rc2D', rc2d, 1, accum_start=0, twodfile=.true.)
          call maybe_write (its, 'rg2D', rg2d, 1, accum_start=0, twodfile=.true.)
          call maybe_write (its, 'r12D', rn_xh,1, accum_start=accum_start, twodfile=.true.)
          call maybe_write (its, 's12D', sn_xh,1, accum_start=accum_start, twodfile=.true.)
          call maybe_write (its, 'r22D', rc_xh,1, accum_start=accum_start, twodfile=.true.)
          call maybe_write (its, 'r32D', rg_xh,1, accum_start=accum_start, twodfile=.true.)
 
          if (mod(its,ArchvStep6h) == 0 .and. ArchvTimeUnit=='hr') then
            call maybe_write (its, 'r42D', rn_6h,1,accum_start=accum_start_6h,twodfile=.true.)
            call maybe_write (its, 's62D', sn_6h,1,accum_start=accum_start_6h,twodfile=.true.)
            call maybe_write (its, 'r52D', rc_6h,1,accum_start=accum_start_6h,twodfile=.true.)
            call maybe_write (its, 'r62D', rg_6h,1,accum_start=accum_start_6h,twodfile=.true.)
          endif

          call maybe_write (its, 'pw2D', pw2d, 1, twodfile=.true.)
          call maybe_write (its, 'pq2D', pq2d, 1, twodfile=.true.)
          call maybe_write (its, 'ts2D', ts2d, 1, twodfile=.true.)
          call maybe_write (its, 'us2D', us2d, 1, twodfile=.true.)
          call maybe_write (its, 'ms2D', mslp, 1, twodfile=.true.)
          call maybe_write (its, 'sa2D', sheleg2d ,1, twodfile=.true.)
          call maybe_write (its, 'cb2D', g2d(:,1),1, twodfile=.true.)
          call maybe_write (its, 'ct2D', g2d(:,3),1, twodfile=.true.)
          call maybe_write (its, 'u12D', u10m, 1, twodfile=.true.)
          call maybe_write (its, 'v12D', v10m, 1, twodfile=.true.)
          call maybe_write (its, 'rp2D', g2d(:,5),1, twodfile=.true.)

          call maybe_write (its, 'cn2D', canopy2d,  1, twodfile=.true.)
          call maybe_write (its, 'st3D', st3d,      4, twodfile=.true.)
          call maybe_write (its, 'sm3D', sm3d,      4, twodfile=.true.)
          call maybe_write (its, 't22D', t2m2d,     1, twodfile=.true.)
          call maybe_write (its, 'q22D', q2m2d,     1, twodfile=.true.)
          if (flux_avg) then
            call maybe_write (its, 'rsds', rsds_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rlds', rlds_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rsus', rsus_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rlus', rlus_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rsdt', rsdt_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rsut', rsut_ave, 1, twodfile=.true.)
            call maybe_write (its, 'rlut', rlut_ave, 1, twodfile=.true.)
            call maybe_write (its, 'hfls', qf_ave,   1, twodfile=.true.)
            call maybe_write (its, 'hfss', hf_ave,   1, twodfile=.true.)
          else
            call maybe_write (its, 'rsds', rsds,     1, twodfile=.true.)
            call maybe_write (its, 'rlds', rlds,     1, twodfile=.true.)
            call maybe_write (its, 'rsus', rsus,     1, twodfile=.true.)
            call maybe_write (its, 'rlus', rlus,     1, twodfile=.true.)
            call maybe_write (its, 'rsdt', rsdt,     1, twodfile=.true.)
            call maybe_write (its, 'rsut', rsut,     1, twodfile=.true.)
            call maybe_write (its, 'rlut', rlut,     1, twodfile=.true.)
            call maybe_write (its, 'hfls', qf2d,     1, twodfile=.true.)
            call maybe_write (its, 'hfss', hf2d,     1, twodfile=.true.)
          end if			! (flux_avg)
          call maybe_write (its, 'w080', g2d(:,7), 1, twodfile=.true.)
          call maybe_write (its, 'thpv', th_pvsrf , 1, twodfile=.true.)
          call maybe_write (its, 'prpv', pr_pvsrf , 1, twodfile=.true.)
          call maybe_write (its, 'uspv', us_pvsrf , 1, twodfile=.true.)
          call maybe_write (its, 'vspv', vs_pvsrf , 1, twodfile=.true.)
          call maybe_write (its, 'clhi', cld_hi_ave,1, twodfile=.true.)
          call maybe_write (its, 'clmd', cld_md_ave,1, twodfile=.true.)
          call maybe_write (its, 'cllo', cld_lo_ave,1, twodfile=.true.)
          call maybe_write (its, 'cltt', cld_tt_ave,1, twodfile=.true.)
         !call maybe_write (its, 'clbl', cld_bl_ave,1, twodfile=.true.)

          IF (.NOT. output2d) THEN         ! write out 3d fields
        
            call maybe_write (its, 'pr3D', pr3d, nvlp1)
            call maybe_write (its, 'ph3D', ph3d, nvlp1, scalefactor=1./9.8)
            call maybe_write (its, 'tk3D', tk3d, nvl)
            call maybe_write (its, 'td3D', td3d, nvl)
            call maybe_write (its, 'ws3D', ws3d, nvl)
            call maybe_write (its, 'rh3D', rh3d, nvl)
            call maybe_write (its, 'us3D', us3d, nvl)
            call maybe_write (its, 'vs3D', vs3d, nvl)

!JR   These arent specified in default FIMnamelist

            call maybe_write (its, 'dp3D', dp3d,      nvl)
            call maybe_write (its, 'mp3D', mp3d,      nvl)
            call maybe_write (its, 'th3D', tr(:,:,1),nvl)
            call maybe_write (its, 'qv3D', tr(:,:,2),nvl, scalefactor=1000.)
            call maybe_write (its, 'qw3D', tr(:,:,3),nvl, scalefactor=1000.)
            call maybe_write (its, 'oz3D', tr(:,:,4),nvl, scalefactor=1000.)
            call maybe_write (its, 'vo3D', relvor,    nvl)
            call maybe_write (its, 'pv3D', potvor,    nvl)
            call maybe_write (its, 'da3D', diaga,     nvl)
            call maybe_write (its, 'db3D', diagb,     nvl)
          end if   ! (.NOT. output2d)

!sms$ignore begin
!$OMP PARALLEL DO schedule (static)
      do ipn=ips,ipe
	rsds_ave(ipn) = 0.
	rlds_ave(ipn) = 0.
	rsus_ave(ipn) = 0.
	rlus_ave(ipn) = 0.
	rsdt_ave(ipn) = 0.
	rsut_ave(ipn) = 0.
	rlut_ave(ipn) = 0.
	  qf_ave(ipn) = 0.
	  hf_ave(ipn) = 0.
      cld_hi_ave(ipn) = 0.
      cld_md_ave(ipn) = 0.
      cld_lo_ave(ipn) = 0.
      cld_tt_ave(ipn) = 0.
     !cld_bl_ave(ipn) = 0.
      end do
!$OMP END PARALLEL DO
!sms$ignore end

! so far only outputting pressure level chem data for GOCART option
!
!          IF (num_chem > 0 .AND. chem_opt == 300)THEN
!            CALL maybe_write (its, 'so2P', g3p_chem(:,:,1), nvlp)
!            CALL maybe_write (its, 'slfP', g3p_chem(:,:,2), nvlp)
!            CALL maybe_write (its, 'dmsP', g3p_chem(:,:,3), nvlp)
!            CALL maybe_write (its, 'msaP', g3p_chem(:,:,4), nvlp)
!            CALL maybe_write (its, 'p25P', g3p_chem(:,:,5), nvlp)
!            CALL maybe_write (its, 'bc1P', g3p_chem(:,:,6), nvlp)
!            CALL maybe_write (its, 'bc2P', g3p_chem(:,:,7), nvlp)
!            CALL maybe_write (its, 'oc1P', g3p_chem(:,:,8), nvlp)
!            CALL maybe_write (its, 'oc2P', g3p_chem(:,:,9), nvlp)
!            CALL maybe_write (its, 'd1sP', g3p_chem(:,:,10), nvlp)
!            CALL maybe_write (its, 'd2sP', g3p_chem(:,:,11), nvlp)
!            CALL maybe_write (its, 'd3sP', g3p_chem(:,:,12), nvlp)
!            CALL maybe_write (its, 'd4sP', g3p_chem(:,:,13), nvlp)
!            CALL maybe_write (its, 'd5sP', g3p_chem(:,:,14), nvlp)
!            CALL maybe_write (its, 's1sP', g3p_chem(:,:,15), nvlp)
!            CALL maybe_write (its, 's2sP', g3p_chem(:,:,16), nvlp)
!            CALL maybe_write (its, 's3sP', g3p_chem(:,:,17), nvlp)
!            CALL maybe_write (its, 's4sP', g3p_chem(:,:,18), nvlp)
!            CALL maybe_write (its, 'p10P', g3p_chem(:,:,19), nvlp)
!!           CALL maybe_write (its, 'slfP', g3p(:,:,ichemstart+2), nvlp)
!!           CALL maybe_write (its, 'dmsP', g3p(:,:,ichemstart+3), nvlp)
!!           CALL maybe_write (its, 'msaP', g3p(:,:,ichemstart+4), nvlp)
!!           CALL maybe_write (its, 'p25P', g3p(:,:,ichemstart+5), nvlp)
!!           CALL maybe_write (its, 'bc1P', g3p(:,:,ichemstart+6), nvlp)
!!           CALL maybe_write (its, 'bc2P', g3p(:,:,ichemstart+7), nvlp)
!           CALL maybe_write (its, 'oc1P', g3p(:,:,ichemstart+8), nvlp)
!!           CALL maybe_write (its, 'oc2P', g3p(:,:,ichemstart+9), nvlp)
!!           CALL maybe_write (its, 'd1sP', g3p(:,:,ichemstart+10), nvlp)
!!           CALL maybe_write (its, 'd2sP', g3p(:,:,ichemstart+11), nvlp)
!!           CALL maybe_write (its, 'd3sP', g3p(:,:,ichemstart+12), nvlp)
!!           CALL maybe_write (its, 'd4sP', g3p(:,:,ichemstart+13), nvlp)
!!           CALL maybe_write (its, 'd5sP', g3p(:,:,ichemstart+14), nvlp)
!!           CALL maybe_write (its, 's1sP', g3p(:,:,ichemstart+15), nvlp)
!!           CALL maybe_write (its, 's2sP', g3p(:,:,ichemstart+16), nvlp)
!!           CALL maybe_write (its, 's3sP', g3p(:,:,ichemstart+17), nvlp)
!!           CALL maybe_write (its, 's4sP', g3p(:,:,ichemstart+18), nvlp)
!!           CALL maybe_write (its, 'p10P', g3p(:,:,ichemstart+19), nvlp)
!          END IF		! (num_chem > 0 .AND. chem_opt == 300)
           
        END IF			! (.NOT. enkfio_out .OR. enkf_diag)
         
      END IF			! (PrintDiags)

      ret = gptlstart ('maxmin')
      call printMAXMIN(its,nvl,nip,ntra+ntrb,tr,dp3d)
      ret = gptlstop ('maxmin')
      if (ips.eq.1) print *,'(subr.output) archiving completed'

    end if			! (MOD(its,ArchvStep) == 0)

    if (digifilt) then
      laststep = itsDFI + nts
    else
      laststep = itsStart + nts
    end if

!TODO: Fix this to print max, min times and keep separate from OMP
!    if (its == laststep-1) then
!      ret = gptlget_wallclock ('Output', 0, toutput)  ! The "0" is thread number
!      ret = gptlget_wallclock ('maxmin', 0, tmaxmin)  ! The "0" is thread number
!      print "(' OUTPUT time, maxmin time:',2F10.1)", toutput, tmaxmin
!    endif

    deallocate (G3SIG)
    return

  end subroutine output

end module module_output
