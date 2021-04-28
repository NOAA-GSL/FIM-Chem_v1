module module_wrf_control
  USE module_initial_chem_namelists ,only:chem_opt,mp_physics,p_qv,p_qc,p_qr, &
      p_qi,p_qs,p_qg,p_qni,p_qnr,p_qnc,p_qnwfa,p_qnifa,p_ho2

implicit none

!********************************************************************
!	This module specifies control variables for wrf physics 
!       and chemistry.  
!********************************************************************

! standard WRF index bounds
integer            ::  ims    ! =1
integer            ::  ime    ! =1
integer            ::  ids    ! =1
integer            ::  ide    ! =1
integer            ::  its    ! =1
integer            ::  ite    ! =1
integer            ::  jms    ! =1
integer            ::  jme    ! =nip
integer            ::  jds    ! =1
integer            ::  jde    ! =nip
integer            ::  jts    ! =1
integer            ::  jte    ! =nip
integer            ::  kms    ! =1
integer            ::  kme    ! =nvl+1
integer            ::  kds    ! =1
integer            ::  kde    ! =nvl+1
integer            ::  kts    ! =1
integer            ::  kte    ! =nvl

! WRF physics and chem parameters
!integer, parameter :: numgas=1        ! # of tracers for gas phase chemistry
integer :: DoForce                    ! Timestep interval for averagin forcing for GF
integer :: DoForce_ave_length=820    ! Timestep interval n seconds for averagin forcing for GF
logical :: AveCuforcingNow

!
! GFS physics _ gocart very light for fim
!
!integer, parameter :: num_moist=2+1
!integer, parameter :: num_chem=13
!integer, parameter :: num_emis_ant = 6
!
! following for Lin et al. + regular GOCART
!
!integer, parameter :: num_moist=6+1
!integer, parameter :: num_chem=18
!integer, parameter :: num_emis_ant = 6
!integer, parameter :: num_emis_vol = 0
!
! volcanic ash only
!integer, parameter :: num_moist=2+1
!integer, parameter :: num_chem=23
!integer, parameter :: num_emis_ant = 6
!integer, parameter :: num_emis_vol = 10
!
! light gocart + reduced volcanic ash only (4 size bins)
!!!!!!!!!! REG TEST SETUP !!!!
!integer, parameter :: num_moist=2+1
!integer, parameter :: num_chem=17
!integer, parameter :: num_emis_ant = 6
!integer, parameter :: num_emis_vol = 4
 integer            :: num_chem,num_moist,num_emis_ant,   &
                       num_emis_vol,num_ebu,num_ebu_in,numgas
!integer            :: num_moist =0
!integer            :: num_emis_ant =0
!integer            :: num_emis_vol =0
! volcanic ash only (4 size bins)
!integer, parameter :: num_moist=2+1
!integer, parameter :: num_chem=4
!integer, parameter :: num_emis_ant = 0
!integer, parameter :: num_emis_vol = 4
! Pure GOCART (volcanic ash included in p25 and p10)
!!!!! CURRENT REAL-TIME
!integer, parameter :: num_moist=2+1
!integer, parameter :: num_chem=19
!integer, parameter :: num_emis_ant = 6
!integer, parameter :: num_emis_vol = 4
! Pure GOCART (volcanic ash included in p25 and p10) + mp_phys=4 (qv,qc,qr,qi,qs)
!integer, parameter :: num_moist=4+1
!integer, parameter :: num_chem=19
!integer, parameter :: num_emis_ant = 6
!integer, parameter :: num_emis_vol = 4
!
!
integer            :: num_emis_season_bb=0
integer            :: num_emis_season_ant=0
integer, parameter :: nbands=14
integer, parameter :: nbandlw=16
integer, parameter :: num_soil_layers=4
integer, parameter :: num_scalar=1
integer, parameter :: nvl_gocart=55  ! number of input levels from gocart file
integer, parameter :: num_ext_coef = 5
integer, parameter :: num_bscat_coef = 3
integer, parameter :: num_asym_par = 3

! namelist variables
! not yet used
integer            :: ChemistryInterval = 0  ! Interval in seconds to call chemistry, 0 => every time step
!Control variables calculated in init.F90 from namelist variables
integer            :: CallChemistry          ! Timestep interval to call chemistry
integer            :: CallBiom               ! Timestep interval to call biomass burning plumerise

contains

!subroutine wrf_control(nvarp,ntrb,num_chem,num_moist,num_emis_ant,num_emis_vol)
!  integer, intent(inout) :: nvarp,ntrb,num_chem,num_moist,num_emis_ant,num_emis_vol
subroutine wrf_control(nvarp,ntrb)
  integer, intent(inout) :: nvarp,ntrb
  ! add WRF variables to ntrb-dimensioned arrays
  numgas=1
  num_ebu=0
  num_ebu_in=0
  if(mp_physics.eq.0 .and. chem_opt .eq. 317)then
     num_chem=17
     num_moist=3
     num_emis_ant=6
     num_emis_vol=4
     ntrb=ntrb+num_moist+num_chem-3           ! # of tracers + num_moist-3, num_chem - no ice variable transported
     num_ebu=30
     num_ebu_in=30
!    nvarp=nvarp+num_chem+num_moist - 3
  elseif (chem_opt .eq. 300 .and. mp_physics.eq.0 )then
     num_ebu=7
     num_ebu_in=7
     num_chem=19
     num_moist=3
     num_emis_ant=7
     num_emis_vol=0
     num_emis_season_bb=7
     num_emis_season_ant=7
     ntrb=ntrb+num_moist+num_chem-3           ! # of tracers + num_moist-3, num_chem - no ice variable transported
  elseif (chem_opt .eq. 301 .and. mp_physics.eq.0 )then
     num_chem=66
     numgas=49
     num_moist=3
     num_emis_ant=25
     num_ebu=25
     num_ebu_in=25
     num_emis_vol=0
     num_emis_season_bb=0
     num_emis_season_ant=0
     ntrb=ntrb+num_moist+num_chem-3           ! # of tracers + num_moist-3, num_chem - no ice variable transported
!    nvarp=nvarp+num_chem+num_moist - 2       ! nvarp includes all variables for pressure level output
   elseif (chem_opt .eq. 108 .and. mp_physics.eq.0 )then
     num_chem=103
     numgas=65
     num_moist=3
     num_emis_ant=25
     num_ebu=25
     num_ebu_in=25
     num_emis_vol=0
     num_emis_season_bb=0
     num_emis_season_ant=0
     ntrb=ntrb+num_moist+num_chem-3           ! # of tracers + num_moist-3,num_chem - no ice variable transported
!    nvarp=nvarp+num_chem+num_moist - 2       ! nvarp includes all variables for pressure level output
  elseif (chem_opt .eq. 0 .and. mp_physics.eq.28 )then
     num_chem=0
     num_moist=12
     num_emis_ant=0
     num_emis_vol=0
     ntrb=ntrb+num_moist+num_chem-2           ! # of tracers + num_moist-2, num_chem - transport of qi
  elseif (chem_opt .eq. 317 .and. mp_physics.eq.3 )then
     num_ebu=30
     num_ebu_in=30
     num_chem=17
     num_moist=5
     num_emis_ant=6
     num_emis_vol=4
     ntrb=ntrb+num_moist+num_chem-2           ! # of tracers + num_moist-2, num_chem - transport of qi
  elseif (chem_opt .eq. 500 .and. mp_physics.eq.0 )then
     num_chem=5
     num_moist=3
     num_emis_ant=5
     num_emis_vol=0
     ntrb=ntrb+num_moist+num_chem-2           ! # of tracers + num_moist-2, num_chem - transport of qi
!  elseif (thompson )then
!    num_chem=0
!    num_moist=9
!    num_emis_ant=0
!    num_emis_vol=0
!    p_qv = 1
!    p_qc = 2
!    p_qr = 3
!    p_qi = 4
!    p_qs = 5
!    p_qg = 6
!    p_qni = 7
!    p_qnr = 8
!    p_qnc = 9
!    ntrb=7
!  elseif (thompsaero )then
!    num_chem=0
!    num_moist=11
!    num_emis_ant=0
!    num_emis_vol=0
!    p_qv = 1
!    p_qc = 2
!    p_qr = 3
!    p_qi = 4
!    p_qs = 5
!    p_qg = 6
!    p_qni = 7
!    p_qnr = 8
!    p_qnc = 9
!    p_qnwfa= 10
!    p_qnifa = 11
!    ntrb=9
  endif
end subroutine wrf_control

end module module_wrf_control
