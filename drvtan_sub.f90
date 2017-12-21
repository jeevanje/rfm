MODULE DRVTAN_SUB
CONTAINS
SUBROUTINE DRVTAN ( LUNDRV, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion of inptan.for. Tested.
!
! DESCRIPTION
!   Read RFM driver table *TAN section
!   Called by RFMDRV once.
!   Reads inputs for tangent heights, sec zenith angles, matrix levels
!   according to flags in *FLG section. A separate module is used to read
!   tabulation axes for the TAB flag.
!
! VARIABLE KINDS
    USE KIND_DAT
! 
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE LENREC_DAT ! Max length of input text record
    USE TANCOM_DAT, ONLY: NTAN   ! No.tangent paths
!
! SUBROUTINES
    USE NXTFFL_SUB ! Load next field from rfm.drv, expanding filenames
    USE TANCHK_SUB ! Check if string is valid Tangent Height
    USE TANFLX_SUB ! Initialise spectral flux calculation
    USE TANMTX_SUB ! Set matrix output levels
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNDRV ! LUN for Driver File
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: LENGTH ! Length of field read from driver table
    CHARACTER(LENREC) :: FIELD  ! Tangent height or Tan.Hgt filename
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  CALL WRTLOG ( 'I-DRVTAN: ', .TRUE. ) 
!
! Read each field in *TAN section
  DO
    CALL NXTFFL ( LUNDRV, FIELD, LENGTH, FAIL, ERRMSG )
    IF ( FAIL ) RETURN
    IF ( LENGTH .EQ. 0 ) EXIT
    CALL TANCHK ( FIELD, FAIL, ERRMSG ) 
    IF ( FAIL ) RETURN
    CALL WRTLOG ( ' '//FIELD, .TRUE. ) 
  END DO
!
  IF ( FLXFLG ) CALL TANFLX
  IF ( MTXFLG ) CALL TANMTX
!
! Check at least one tangent height supplied
  IF ( NTAN .EQ. 0 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVTAN: No entries in *TAN section'
  ELSE IF ( LOSFLG .AND. NTAN .EQ. 1 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVTAN: LOS flag requires at least 2 tangent heights'
  ELSE IF ( MTXFLG .AND. NTAN .EQ. 1 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVTAN: MTX flag requires at least 2 height levels'
  ELSE
    CALL WRTLOG ( '', .FALSE. )
    IF ( LOSFLG .AND. NTAN .EQ. 2 ) CALL WRTLOG ( 'W-INPTAN: Only 2 tan.hts, ' &
                              // 'so LOS Jacobians from linear interpolation' )
  END IF
!
! Normal exit
!
END SUBROUTINE DRVTAN
END MODULE DRVTAN_SUB

