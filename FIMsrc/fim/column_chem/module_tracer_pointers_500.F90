Module module_tracer_pointers_500
USE module_initial_chem_namelists,only:p_tr1,p_tr2,p_tr3,p_tr4,p_tr5,p_e_tr1, &
p_e_tr2,p_e_tr3,p_e_tr4,p_e_tr5
CONTAINS
!
! this module will initialize variable pointers for chem_opt "500", used
! for tracer only transport (originally implemented for "earth
! analyzer", June 2015
!
   subroutine tracer_pointers_500
  IMPLICIT NONE
p_tr1=1
p_tr2=2
p_tr3=3
p_tr4=4
p_tr5=5
p_e_tr1=1
p_e_tr2=2
p_e_tr3=3
p_e_tr4=4
p_e_tr5=5
END SUBROUTINE tracer_pointers_500
END MODULE module_tracer_pointers_500
