MODULE ATMAIR_SUB
CONTAINS
SUBROUTINE ATMAIR
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Set VMR profile values for air
!   Called once by DRVATM if REX flag enabled.
!   This routine should only be called after 'air' loaded as an absorber
!   Also sets appropriate array element of GOTGAS to .TRUE.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE FLGCOM_DAT, ONLY: GRAFLG ! T=2D atmosphere
    USE IDXCON_DAT, ONLY: IDXAIR ! RFM 'molecular' index for air
!
! SUBROUTINES
    USE IDXGAS_FNC ! Index in GAS arrays of molecule,isotope
    USE PRFGRA_SUB ! Copy atm profile from 1D to 2D field
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IVMR ! Index of 'air' in VMR profiles
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IVMR = IDXGAS ( IDXAIR ) 
  IF ( IVMR .EQ. 0 ) STOP 'F-ATMAIR: Logical error'
!
  VMRATM(:,IVMR) = 1.0E6 ! 'air' mixing ratio = 1ppv.
  SETVMR(IVMR) = .TRUE.
!
! If using horizontal gradients, ensure that Air VMR defined at one (arbitrary)
! location 
  IF ( GRAFLG ) CALL PRFGRA ( 'VMR', IVMR ) 
!
END SUBROUTINE ATMAIR
END MODULE ATMAIR_SUB

