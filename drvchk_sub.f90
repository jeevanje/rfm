MODULE DRVCHK_SUB
CONTAINS
SUBROUTINE DRVCHK ( FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion of inpchk.for. Checked.
!
! DECSRIPTION
!   Cross-check driver table inputs
!   Called once by RFMDRV after all driver table info is loaded.
!   This is used to perform checks where inputs come from sections which 
!   can be in arbitrary order, so only meaningful after all driver table
!   data has been loaded.
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE TANCOM_DAT, ONLY: LIMTAN ! T=limb-viewing mode
!
! SUBROUTINES
    USE CHKABS_SUB ! Check information available for all absorbers
    USE CHKFOV_SUB ! Check FOV tangent heights
    USE CHKLEV_SUB ! Set up tangent paths for intermediate output levels
    USE CHKNAM_SUB ! Check RFM output filename templates
    USE CHKTAN_SUB ! Check limb-viewing tangent paths
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Check output filename templates and convert .asc to .bin or v/v if required
  CALL CHKNAM ( FAIL, ERRMSG )
  IF ( FAIL ) RETURN

! Check spectral information is available for all absorbers
  CALL CHKABS ( FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
! Check limb-viewing tangent paths
  IF ( LIMTAN ) THEN
    CALL CHKTAN ( FAIL, ERRMSG )
    IF ( FAIL ) RETURN
    IF ( FOVFLG ) THEN
      CALL CHKFOV ( FAIL, ERRMSG )
      IF ( FAIL ) RETURN
    END IF
  END IF
!
! Set tangent paths/Jacobians for intermediate output levels
  IF ( LEVFLG ) CALL CHKLEV  
!
END SUBROUTINE DRVCHK
END MODULE DRVCHK_SUB

