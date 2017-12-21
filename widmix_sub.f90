MODULE WIDMIX_SUB
CONTAINS
SUBROUTINE WIDMIX ( WNL, WNU )
!
! VERSION
!   01MAY17 AD Original. Based on part of F77 module SPCWID. Checked.
!
! DESCRIPTION
!   Adjust Wno limits to avoid splitting mixing datasets
!   Called by SPCINI for each spectral range if MIX flag enabled
!   Rounds up/down to the nearest wavenumber
!   Use WNLSET, WNUSET in MIXDAT - spectral range of each mixing dataset
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE MIXDAT_DAT, ONLY: NSET, WNLSET, WNUSET ! CO2 line-mixing datasets
!
  IMPLICIT NONE
!
! ARGUMENTS
    REAL(R8), INTENT(INOUT) :: WNL ! Lower Wno limit
    REAL(R8), INTENT(INOUT) :: WNU ! Upper Wno limit
!
! LOCAL VARIABLES
    INTEGER(I4) :: ISET ! Mixing set counter
    REAL(R4)    :: RWNL ! Real version of WNL 
    REAL(R4)    :: RWNU ! Real version of WNU
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  RWNL = REAL ( WNL )
  RWNU = REAL ( WNU ) 
  DO ISET = 1, NSET
    IF ( RWNL .GT. WNLSET(ISET) .AND. RWNL .LE. WNUSET(ISET) ) &
      WNL = DBLE ( FLOOR ( WNLSET(ISET) ) )  
    IF ( RWNU .GE. WNLSET(ISET) .AND. RWNU .LT. WNUSET(ISET) ) &
      WNU = DBLE ( CEILING ( WNUSET(ISET) ) )  
  END DO
!
END SUBROUTINE WIDMIX
END MODULE WIDMIX_SUB


