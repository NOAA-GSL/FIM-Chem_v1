module module_sfc_variables
!*********************************************************************
!       Single-precision storage for dynamics versions of physics 
!       variables passed from PHY to DYN via CPL for FIM diagnostics 
!       and output *only*.  
!*********************************************************************

save

!SMS$DISTRIBUTE (dh,1) BEGIN
!JR Moved these 5 things from output.F90 so they can be written to the restart file.
real,allocatable :: rn2d0(:)      ! ?
!real,allocatable :: sn2d0(:)      ! ?
real,allocatable :: rc2d0(:)      ! ?
real,allocatable :: rg2d0(:)      ! ?
real,allocatable :: rn2d6_0(:)      ! ?
!real,allocatable :: sn2d6_0(:)      ! ?
real,allocatable :: rc2d6_0(:)      ! ?
real,allocatable :: rg2d6_0(:)      ! ?
real,allocatable :: rn2d(:)		! accumulated total precipitation/rainfall
real,allocatable :: sn2d(:)		! accumulated total snowfall
real,allocatable :: rc2d(:)		! accumulated convective precipitation/rainfall
real,allocatable :: ts2d(:)		! skin temperature
real,allocatable :: us2d(:)		! friction velocity/equivalent momentum flux
real,allocatable :: strs2d(:)		! surface wind stress
real,allocatable :: sheleg2d(:)
real,allocatable :: canopy2d(:)
real,allocatable :: hice2d(:)
real,allocatable :: fice2d(:)
real,allocatable :: sst_prev(:)		! skin temperature previous month (sst holder)
real,allocatable :: sst_next(:)		! skin temperature next month (sst holder)
real,allocatable :: fice2d_prev(:)	! holder for previous months ice fraction
real,allocatable :: fice2d_next(:)	! holder for next months ice fra
real,allocatable :: t2m2d(:)		! 2-meter temp.
real,allocatable :: q2m2d(:)		! 2-meter spfh 
real,allocatable :: slmsk2d(:)		! surface land mask, 0/1/2: ocean/land/ice
real,allocatable :: alb2d(:)		! surface albedo
!!!  added for digital filter
real,allocatable :: zorl2d(:)
real,allocatable :: vfrac2d(:)
real,allocatable :: vtype2d(:)
real,allocatable :: stype2d(:)
real,allocatable :: srflag2d(:)
real,allocatable :: tg32d(:)
real,allocatable :: cv2d(:)
real,allocatable :: cvb2d(:)
real,allocatable :: cvt2d(:)
real,allocatable :: alvsf2d(:)
real,allocatable :: alvwf2d(:)
real,allocatable :: alnsf2d(:)
real,allocatable :: alnwf2d(:)
real,allocatable :: f10m2d(:)
!JR Added u10m, v10m for proper dyn<->phy handling
real,allocatable :: u10m(:)
real,allocatable :: v10m(:)
real,allocatable :: facsf2d(:)
real,allocatable :: facwf2d(:)
real,allocatable :: uustar2d(:)
real,allocatable :: ffmm2d(:)
real,allocatable :: ffhh2d(:)
real,allocatable :: slc2d(:)
real,allocatable :: snwdph2d(:)
real,allocatable :: shdmin2d(:)
real,allocatable :: shdmax2d(:)
real,allocatable :: slope2d(:)
real,allocatable :: snoalb2d(:)
real,allocatable :: tprcp2d(:)  ! precip rate (1000*kg/m**2)
real,allocatable :: aod2d(:)   !  aerosol optical depth
! fluxes (W/m2) at surface & TOA
real,allocatable :: rsds(:),rsds_ave(:)	! radiation SW downward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: rlds(:),rlds_ave(:)	! radiation LW downward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: rsus(:),rsus_ave(:)	! radiation SW   upward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: rlus(:),rlus_ave(:)	! radiation LW   upward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: rsdt(:),rsdt_ave(:)	! radiation SW downward        TOA, snapshot/averaged over ArchvIntvl
real,allocatable :: rsut(:),rsut_ave(:)	! radiation SW   upward        TOA, snapshot/averaged over ArchvIntvl
real,allocatable :: rlut(:),rlut_ave(:)	! radiation LW   upward        TOA, snapshot/averaged over ArchvIntvl
real,allocatable :: qf2d(:),  qf_ave(:) !   latent heatflux, snapshot/averaged over ArchvIntvl (W/m2)
real,allocatable :: hf2d(:),  hf_ave(:) ! sensible heatflux, snapshot/averaged over ArchvIntvl (W/m2)
real,allocatable :: cld_hi(:),cld_md(:),cld_lo(:),cld_tt(:),cld_bl(:) !high/mid/low/total/boundary cloud cover
real,allocatable :: cld_hi_ave(:),cld_md_ave(:),cld_lo_ave(:),cld_tt_ave(:),cld_bl_ave(:)        !avethly mean

!SMS$DISTRIBUTE END
!SMS$DISTRIBUTE (dh,2) BEGIN
real,allocatable :: st3d(:,:)   ! soil temperature
real,allocatable :: sm3d(:,:)   ! soil moisture
real,allocatable :: slc3d(:,:)  ! soil liquid content
real,allocatable :: hprm2d(:,:) ! hprm2d(14,nip)
!SMS$DISTRIBUTE END

end module module_sfc_variables
