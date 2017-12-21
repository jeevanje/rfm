MODULE RFMPTH_SUB
CONTAINS
SUBROUTINE RFMPTH ( FAIL, ERRMSG ) 
!
! VERSION
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION
!   Set up RFM path calculations
!   Called once by RFM
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
!
! SUBROUTINES
    USE CIAPTH_SUB ! Set CIA paths
    USE FLXPTH_SUB ! Set up paths for flux calculations
    USE HOMPTH_SUB ! Set Homogeneous paths
    USE JACPTH_SUB ! Set Jacobian paths
    USE LIMPTH_SUB ! Set Limb-viewing paths
    USE PTHCLC_SUB ! Determine which path absorption calculations can be scaled
    USE PTHWRT_SUB ! Write RFM path diagnostics
    USE REXPTH_SUB ! Set paths for Rayleigh Extinction
    USE TABPTH_SUB ! Set paths for TABulated abs.coeff. calculations
    USE VRTPTH_SUB ! Set Vertical-viewing paths
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error occurs
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  IF ( HOMFLG ) THEN
    CALL HOMPTH
  ELSE IF ( FLXFLG ) THEN
    CALL FLXPTH
  ELSE IF ( NADFLG .OR. ZENFLG ) THEN
    CALL VRTPTH
  ELSE IF ( TABFLG ) THEN
    CALL TABPTH
  ELSE
    CALL LIMPTH
    CALL PTHCLC
  END IF
!
  IF ( CIAFLG ) CALL CIAPTH
!
  IF ( REXFLG ) CALL REXPTH
!
  IF ( PTHFLG ) CALL PTHWRT ( FAIL, ERRMSG ) 
!
  IF ( JACFLG ) CALL JACPTH
!
END SUBROUTINE RFMPTH
END MODULE RFMPTH_SUB
