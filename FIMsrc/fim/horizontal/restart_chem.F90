module restart_chem

contains

  subroutine read_restart_chem (unitno)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read required Chemical fields from the restart file. SMS will modify the code to do the
! appropriate single-task reading of the restart file, and subsequent scattering of the
! data to other MPI tasks.
!
! CRITICAL: If you modify this file, you MUST also modify write_restart_chem.F90 in the
! same way. Otherwise restart will be broken.
!
! readarr32_[i|r] assumes 32-bit data.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    USE module_wrf_variables,      ONLY: exch,pb2d
    USE MODULE_WRFPHYSVARS,        ONLY: gd_cloud,gd_cldfr
!    USE module_variables,          ONLY: conv_act
    USE module_chem_variables,      ONLY: rcav,rnav
    USE module_chemvars, ONLY: aerwrf,backg_oh,backg_h2o2,backg_no3,&
                                      addt, addx, addc, etep, oltp, &
                                      olip, cslp, limp, hc5p, hc8p, &
                                      tolp, xylp, apip, isop, hc3p, &
                                         ethp, o3p, tco3, mo2, o1d, &
                                       olnn, olnd, rpho, xo2, ketp, &
                                      xno2, ol2p, oln, macp, hocoo, &
                            bzno2_o, bz_o, tbu_o,extt,ssca,asympar, &
                       ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, &
                      ph_hno2, ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, &
                    ph_ch2om, ph_ch3cho, ph_ch3coch3, ph_ch3coc2h5, &
                     ph_hcocho,ph_ch3cocho, ph_hcochest, ph_ch3o2h, &
                      ph_ch3coo2h, ph_ch3ono2, ph_hcochob, ph_macr, &
                                     ph_n2o5, ph_o2, ph_pan,ph_acet,&
                               ph_mglo, ph_hno4_2, ph_n2o, ph_pooh, &
                     ph_mpan, ph_mvk, ph_etooh, ph_prooh, ph_onitr, &
                    ph_acetol, ph_glyald, ph_hyac, ph_mek, ph_open, &
                    ph_gly, ph_acetp, ph_xooh, ph_isooh, ph_alkooh, &
                          ph_mekooh, ph_tolooh, ph_terpooh, ph_cl2, &
                                                  ph_hocl, ph_fmcl, &
                            h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3
    USE module_wrf_control, only:nbands
    use fimnamelist,               only: nvl
    USE module_initial_chem_namelists,only: chem_opt,aer_ra_feedback
    implicit none

    integer, intent(in) :: unitno ! unit number to read from
    integer :: n

      read (unitno, err=90) rcav
      read (unitno, err=90) rnav
      read (unitno, err=90) pb2d
      !read (unitno, err=90) conv_act
      call readarr32_r (exch, nvl,unitno)
!      call readarr32_r (backg_oh, nvl+1,unitno)
!      call readarr32_r (backg_h2o2, nvl+1,unitno)
!      call readarr32_r (backg_no3, nvl+1,unitno)
     if(aer_ra_feedback >= 1 )then     
      do n=1,nbands
      call readarr32_r (extt(:,:,:,n), nvl+1,unitno)
      call readarr32_r (ssca(:,:,:,n), nvl+1,unitno)
      call readarr32_r (asympar(:,:,:,n), nvl+1,unitno)
      enddo
    endif
      
    if(chem_opt == 301 .or.chem_opt == 108 ) then
      call readarr32_r (aerwrf, nvl+1,unitno)
      call readarr32_r (gd_cloud, nvl+1,unitno)
      call readarr32_r (gd_cldfr, nvl+1,unitno)
      call readarr32_r (addt, nvl+1,unitno)
      call readarr32_r (addx, nvl+1,unitno)
      call readarr32_r (addc, nvl+1,unitno)
      call readarr32_r (etep, nvl+1,unitno)
      call readarr32_r (oltp, nvl+1,unitno)
      call readarr32_r (olip, nvl+1,unitno)
      call readarr32_r (cslp, nvl+1,unitno)
      call readarr32_r (limp, nvl+1,unitno)
      call readarr32_r (hc5p, nvl+1,unitno)
      call readarr32_r (hc8p, nvl+1,unitno)
      call readarr32_r (tolp, nvl+1,unitno)
      call readarr32_r (xylp, nvl+1,unitno)
      call readarr32_r (apip, nvl+1,unitno)
      call readarr32_r (isop, nvl+1,unitno)
      call readarr32_r (hc3p,  nvl+1,unitno)
      call readarr32_r (ethp, nvl+1,unitno)
      call readarr32_r (o3p, nvl+1,unitno)
      call readarr32_r (tco3, nvl+1,unitno)
      call readarr32_r (mo2, nvl+1,unitno)
      call readarr32_r (o1d, nvl+1,unitno)
      call readarr32_r (olnn, nvl+1,unitno)
      call readarr32_r (olnd, nvl+1,unitno)
      call readarr32_r (rpho, nvl+1,unitno)
      call readarr32_r (xo2, nvl+1,unitno)
      call readarr32_r (ketp, nvl+1,unitno)
      call readarr32_r (xno2, nvl+1,unitno)
      call readarr32_r (ol2p, nvl+1,unitno)
      call readarr32_r (oln, nvl+1,unitno)
      call readarr32_r (macp, nvl+1,unitno)
      call readarr32_r (hocoo, nvl+1,unitno)
      call readarr32_r (bzno2_o, nvl+1,unitno)
      call readarr32_r (bz_o, nvl+1,unitno)
      call readarr32_r (tbu_o, nvl+1,unitno)
      call readarr32_r (ph_macr, nvl+1,unitno)
      call readarr32_r (ph_o31d, nvl+1,unitno)
      call readarr32_r (ph_o33p, nvl+1,unitno)
      call readarr32_r (ph_no2, nvl+1,unitno)
      call readarr32_r (ph_no3o2, nvl+1,unitno)
      call readarr32_r (ph_no3o, nvl+1,unitno)
      call readarr32_r (ph_hno2, nvl+1,unitno)
      call readarr32_r (ph_hno3, nvl+1,unitno)
      call readarr32_r (ph_hno4, nvl+1,unitno)
      call readarr32_r (ph_h2o2, nvl+1,unitno)
      call readarr32_r (ph_ch2or, nvl+1,unitno)
      call readarr32_r (ph_ch2om, nvl+1,unitno)
      call readarr32_r (ph_ch3cho, nvl+1,unitno)
      call readarr32_r (ph_ch3coch3, nvl+1,unitno)
      call readarr32_r (ph_ch3coc2h5, nvl+1,unitno)
      call readarr32_r (ph_hcocho, nvl+1,unitno)
      call readarr32_r (ph_ch3cocho, nvl+1,unitno)
      call readarr32_r (ph_hcochest, nvl+1,unitno)
      call readarr32_r (ph_ch3o2h, nvl+1,unitno)
      call readarr32_r (ph_ch3coo2h, nvl+1,unitno)
      call readarr32_r (ph_ch3ono2, nvl+1,unitno)
      call readarr32_r (ph_hcochob, nvl+1,unitno)
       call readarr32_r (ph_n2o5, nvl+1,unitno)
       call readarr32_r (ph_o2, nvl+1,unitno)
       call readarr32_r (ph_pan, nvl+1,unitno)
       call readarr32_r (ph_acet, nvl+1,unitno)
       call readarr32_r (ph_mglo, nvl+1,unitno)
       call readarr32_r (ph_hno4_2, nvl+1,unitno)
       call readarr32_r (ph_n2o, nvl+1,unitno)
       call readarr32_r (ph_pooh, nvl+1,unitno)
       call readarr32_r (ph_mpan, nvl+1,unitno)
       call readarr32_r (ph_mvk, nvl+1,unitno)
       call readarr32_r (ph_etooh, nvl+1,unitno)
       call readarr32_r (ph_prooh, nvl+1,unitno)
       call readarr32_r (ph_onitr, nvl+1,unitno)
       call readarr32_r (ph_acetol, nvl+1,unitno)
       call readarr32_r (ph_glyald, nvl+1,unitno)
       call readarr32_r (ph_hyac, nvl+1,unitno)
       call readarr32_r (ph_mek, nvl+1,unitno)
       call readarr32_r (ph_open, nvl+1,unitno)
       call readarr32_r (ph_gly, nvl+1,unitno)
       call readarr32_r (ph_acetp, nvl+1,unitno)
       call readarr32_r (ph_xooh, nvl+1,unitno)
       call readarr32_r (ph_isooh, nvl+1,unitno)
       call readarr32_r (ph_alkooh, nvl+1,unitno)
       call readarr32_r (ph_mekooh, nvl+1,unitno)
       call readarr32_r (ph_tolooh, nvl+1,unitno)
       call readarr32_r (ph_terpooh, nvl+1,unitno)
       call readarr32_r (ph_cl2, nvl+1,unitno)
       call readarr32_r (ph_hocl, nvl+1,unitno)
       call readarr32_r (ph_fmcl, nvl+1,unitno)
 !     endif
 !   if(chem_opt == 108 ) then
      call readarr32_r (h2oaj, nvl+1,unitno)
      call readarr32_r (h2oai, nvl+1,unitno)
      call readarr32_r (nu3, nvl+1,unitno)
      call readarr32_r (ac3, nvl+1,unitno)
      call readarr32_r (cor3, nvl+1,unitno)
      call readarr32_r (asulf, nvl+1,unitno)
      call readarr32_r (ahno3, nvl+1,unitno)
      call readarr32_r (anh3, nvl+1,unitno)
    endif
  
    write(6,*) 'read_restart_chem: successfully read chemical fields from restart file'
    return

90  write(6,*)'read_restart_chem: Error reading from unit ', unitno, '. Stopping'
    stop

  end subroutine read_restart_chem

  subroutine write_restart_chem (unitno)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Write required chemical fields to the restart file. SMS will modify the code to do the
! appropriate single-task writing of the restart file, after gathering the
! data from other MPI tasks.
!
! CRITICAL: If you modify this file, you MUST also modify read_restart_chem.F90 in the
! same way. Otherwise restart will be broken.
!
! writearr32_[i|r] assumes 32-bit data.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    USE module_wrf_variables,      ONLY: exch,pb2d
    USE MODULE_WRFPHYSVARS,        ONLY: gd_cloud,gd_cldfr
    !USE module_variables,          ONLY: conv_act
    USE module_chem_variables,      ONLY: rcav,rnav
    USE module_chemvars, ONLY: aerwrf,backg_oh,backg_h2o2,backg_no3,&
                                      addt, addx, addc, etep, oltp, &
                                      olip, cslp, limp, hc5p, hc8p, &
                                      tolp, xylp, apip, isop, hc3p, &
                                         ethp, o3p, tco3, mo2, o1d, &
                                       olnn, olnd, rpho, xo2, ketp, &
                                      xno2, ol2p, oln, macp, hocoo, &
                             bzno2_o, bz_o, tbu_o,extt,ssca,asympar,&
                       ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, &
                      ph_hno2, ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, &
                    ph_ch2om, ph_ch3cho, ph_ch3coch3, ph_ch3coc2h5, &  
                     ph_hcocho,ph_ch3cocho, ph_hcochest, ph_ch3o2h, &
                      ph_ch3coo2h, ph_ch3ono2, ph_hcochob, ph_macr, &
                                     ph_n2o5, ph_o2, ph_pan,ph_acet,&
                               ph_mglo, ph_hno4_2, ph_n2o, ph_pooh, &
                     ph_mpan, ph_mvk, ph_etooh, ph_prooh, ph_onitr, &
                    ph_acetol, ph_glyald, ph_hyac, ph_mek, ph_open, &
                    ph_gly, ph_acetp, ph_xooh, ph_isooh, ph_alkooh, &
                          ph_mekooh, ph_tolooh, ph_terpooh, ph_cl2, & 
                                                  ph_hocl, ph_fmcl, & 
                            h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3
    USE module_wrf_control, only:nbands
    USE module_chemvars,           ONLY: aerwrf,backg_oh, backg_h2o2,backg_no3
    use fimnamelist,               only: nvl
    USE module_initial_chem_namelists,only: chem_opt,aer_ra_feedback
    implicit none

    integer, intent(in) :: unitno ! unit number to read from
    integer :: n
      write (unitno, err=90) rcav
      write (unitno, err=90) rnav
      write (unitno, err=90) pb2d
      !write (unitno, err=90) conv_act
      call writearr32_r (exch, nvl,unitno)
!      call writearr32_r (backg_oh, nvl+1,unitno)
!      call writearr32_r (backg_h2o2, nvl+1,unitno)
!      call writearr32_r (backg_no3, nvl+1,unitno)
    if(aer_ra_feedback >= 1 )then
      do n=1,nbands
      call writearr32_r (extt(:,:,:,n), nvl+1,unitno)
      call writearr32_r (ssca(:,:,:,n), nvl+1,unitno)
      call writearr32_r (asympar(:,:,:,n), nvl+1,unitno)
      enddo
    endif
      if(chem_opt == 301 .or.chem_opt == 108) then
      call writearr32_r (aerwrf, nvl+1,unitno)
      call writearr32_r (gd_cloud, nvl+1,unitno)
      call writearr32_r (gd_cldfr, nvl+1,unitno)
      call writearr32_r (addt, nvl+1,unitno)
      call writearr32_r (addx, nvl+1,unitno)
      call writearr32_r (addc, nvl+1,unitno)
      call writearr32_r (etep, nvl+1,unitno)
      call writearr32_r (oltp, nvl+1,unitno)
      call writearr32_r (olip, nvl+1,unitno)
      call writearr32_r (cslp, nvl+1,unitno)
      call writearr32_r (limp, nvl+1,unitno)
      call writearr32_r (hc5p, nvl+1,unitno)
      call writearr32_r (hc8p, nvl+1,unitno)
      call writearr32_r (tolp, nvl+1,unitno)
      call writearr32_r (xylp, nvl+1,unitno)
      call writearr32_r (apip, nvl+1,unitno)
      call writearr32_r (isop, nvl+1,unitno)
      call writearr32_r (hc3p,  nvl+1,unitno)
      call writearr32_r (ethp, nvl+1,unitno)
      call writearr32_r (o3p, nvl+1,unitno)
      call writearr32_r (tco3, nvl+1,unitno)
      call writearr32_r (mo2, nvl+1,unitno)
      call writearr32_r (o1d, nvl+1,unitno)
      call writearr32_r (olnn, nvl+1,unitno)
      call writearr32_r (olnd, nvl+1,unitno)
      call writearr32_r (rpho, nvl+1,unitno)
      call writearr32_r (xo2, nvl+1,unitno)
      call writearr32_r (ketp, nvl+1,unitno)
      call writearr32_r (xno2, nvl+1,unitno)
      call writearr32_r (ol2p, nvl+1,unitno)
      call writearr32_r (oln, nvl+1,unitno)
      call writearr32_r (macp, nvl+1,unitno)
      call writearr32_r (hocoo, nvl+1,unitno)
      call writearr32_r (bzno2_o, nvl+1,unitno)
      call writearr32_r (bz_o, nvl+1,unitno)
      call writearr32_r (tbu_o, nvl+1,unitno)
      call writearr32_r (ph_macr, nvl+1,unitno)
      call writearr32_r (ph_o31d, nvl+1,unitno)
      call writearr32_r (ph_o33p, nvl+1,unitno)
      call writearr32_r (ph_no2, nvl+1,unitno)
      call writearr32_r (ph_no3o2, nvl+1,unitno)
      call writearr32_r (ph_no3o, nvl+1,unitno)
      call writearr32_r (ph_hno2, nvl+1,unitno)
      call writearr32_r (ph_hno3, nvl+1,unitno)
      call writearr32_r (ph_hno4, nvl+1,unitno)
      call writearr32_r (ph_h2o2, nvl+1,unitno)
      call writearr32_r (ph_ch2or, nvl+1,unitno)
      call writearr32_r (ph_ch2om, nvl+1,unitno)
      call writearr32_r (ph_ch3cho, nvl+1,unitno)
      call writearr32_r (ph_ch3coch3, nvl+1,unitno)
      call writearr32_r (ph_ch3coc2h5, nvl+1,unitno)
      call writearr32_r (ph_hcocho, nvl+1,unitno)
      call writearr32_r (ph_ch3cocho, nvl+1,unitno)
      call writearr32_r (ph_hcochest, nvl+1,unitno)
      call writearr32_r (ph_ch3o2h, nvl+1,unitno)
      call writearr32_r (ph_ch3coo2h, nvl+1,unitno)
      call writearr32_r (ph_ch3ono2, nvl+1,unitno)
      call writearr32_r (ph_hcochob, nvl+1,unitno)
       call writearr32_r (ph_n2o5, nvl+1,unitno)
       call writearr32_r (ph_o2, nvl+1,unitno)
       call writearr32_r (ph_pan, nvl+1,unitno)
       call writearr32_r (ph_acet, nvl+1,unitno)
       call writearr32_r (ph_mglo, nvl+1,unitno)
       call writearr32_r (ph_hno4_2, nvl+1,unitno)
       call writearr32_r (ph_n2o, nvl+1,unitno)
       call writearr32_r (ph_pooh, nvl+1,unitno)
       call writearr32_r (ph_mpan, nvl+1,unitno)
       call writearr32_r (ph_mvk, nvl+1,unitno)
       call writearr32_r (ph_etooh, nvl+1,unitno)
       call writearr32_r (ph_prooh, nvl+1,unitno)
       call writearr32_r (ph_onitr, nvl+1,unitno)
       call writearr32_r (ph_acetol, nvl+1,unitno)
       call writearr32_r (ph_glyald, nvl+1,unitno)
       call writearr32_r (ph_hyac, nvl+1,unitno)
       call writearr32_r (ph_mek, nvl+1,unitno)
       call writearr32_r (ph_open, nvl+1,unitno)
       call writearr32_r (ph_gly, nvl+1,unitno)
       call writearr32_r (ph_acetp, nvl+1,unitno)
       call writearr32_r (ph_xooh, nvl+1,unitno)
       call writearr32_r (ph_isooh, nvl+1,unitno)
       call writearr32_r (ph_alkooh, nvl+1,unitno)
       call writearr32_r (ph_mekooh, nvl+1,unitno)
       call writearr32_r (ph_tolooh, nvl+1,unitno)
       call writearr32_r (ph_terpooh, nvl+1,unitno)
       call writearr32_r (ph_cl2, nvl+1,unitno)
       call writearr32_r (ph_hocl, nvl+1,unitno)
       call writearr32_r (ph_fmcl, nvl+1,unitno)
    !  endif
    !if(chem_opt == 108 ) then
      call writearr32_r (h2oaj, nvl+1,unitno)
      call writearr32_r (h2oai, nvl+1,unitno)
      call writearr32_r (nu3, nvl+1,unitno)
      call writearr32_r (ac3, nvl+1,unitno)
      call writearr32_r (cor3, nvl+1,unitno)
      call writearr32_r (asulf, nvl+1,unitno)
      call writearr32_r (ahno3, nvl+1,unitno)
      call writearr32_r (anh3, nvl+1,unitno)
    endif
    write(6,*) 'write_restart_chem: successfully wrote chemical fields to restart file'
    return

90  write(6,*)'write_restart_chem: Error writing to unit ', unitno, '. Stopping'
    stop

  end subroutine write_restart_chem

end module restart_chem
