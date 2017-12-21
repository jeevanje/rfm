MODULE CHKABS_SUB
CONTAINS
SUBROUTINE CHKABS ( FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion of abschk.for. Checked.
!
! DESCRIPTION
!   Check information available for all absorbers
!   Called once by DRVCHK.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE LFLCOM_DAT ! Look-Up Table files
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IGAS ! Absorber counter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  DO IGAS = 1, NGAS
    IF ( NLFL .GT. 0 ) THEN                      ! Using (some) LUTs
      IF ( ALL ( IDXLFL(IGAS,:) .GT. 0 ) ) CYCLE ! LUTs for all spectral ranges
    END IF
    IF ( .NOT. GAS(IGAS)%HIT .AND. .NOT. GAS(IGAS)%XSC .AND. &
         GAS(IGAS)%COD .NE. 'air' ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-CHKABS: No spectroscopic data for ' // GAS(IGAS)%COD 
      RETURN
    END IF
  END DO 
!
! Normal exit
  FAIL = .FALSE.
!
END SUBROUTINE CHKABS
END MODULE CHKABS_SUB
