Module module_phot1_pointers
USE
module_initial_chem_namelists,only:p_ph_o31d,p_ph_o33p,p_ph_no2,p_ph_no3o2, &
      ph_no3o,p_ph_hno2,p_ph_hno3,p_ph_hno4,p_ph_h2o2,p_ph_ch2or,p_ph_ch2om2,   &
      ph_ch3cho,p_ph_ch3coch3,p_ph_ch3coc2h5,p_ph_hcocho,p_ph_ch3cocho,     &
      ph_hcochest,p_ph_ch3o2h,p_ph_ch3ono2,p_ph_hcochob,p_ph_macr,p_ph_n2o5,  &
      ph_o2,p_ph_pan,p_ph_acet,p_ph_mglo,p_ph_hno4_2
CONTAINS
!
! this module will initialize variable pointers for photolysis options
! June 2015
!
   subroutine phot1_pointers
  IMPLICIT NONE
p_ph_o31d  = 1
p_ph_o33p  = 2
p_ph_no2   = 3
p_ph_no3o2 = 4
p_ph_no3o  = 5
p_ph_hno2  = 6
p_ph_hno3  = 7
p_ph_hno4  = 8
p_ph_h2o2  = 9
p_ph_ch2or = 10
p_ph_ch2om2 = 11
p_ph_ch3cho  = 12
p_ph_ch3coch3 = 13
p_ph_ch3coc2h5 = 14
p_ph_hcocho  = 15
p_ph_ch3cocho = 16
p_ph_hcochest = 17
p_ph_ch3o2h  = 18
p_ph_ch3coo2h = 19
p_ph_ch3ono2 = 20
p_ph_hcochob = 21
p_ph_macr = 22
p_ph_n2o5 = 23
p_ph_o2   = 24
p_ph_pan  = 25
p_ph_acet = 26
p_ph_mglo = 27
p_ph_hno4_2 = 28

END SUBROUTINE phot1_pointers
END MODULE module_phot1_pointers
