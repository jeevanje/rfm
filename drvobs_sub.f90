MODULE DRVOBS_SUB
CONTAINS
SUBROUTINE DRVOBS ( LUNDRV, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Read RFM driver table *OBS section
!   Called by RFMDRV following *OBS marker in Driver table.
!   If OBSFLG is TRUE read observer altitude [km]
!   If GRAFLG is also TRUE read observer horizontal angle.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE LENREC_DAT ! Max length of input text record
    USE OBSCOM_DAT ! Observer location data
    USE FLGCOM_DAT, ONLY: GRAFLG ! T = use horizontal gradients
!
! SUBROUTINES
    USE C9REAL_GEN ! Write real number as C*9 string
    USE ENDCHK_SUB ! Check end of Driver Table section has been reached
    USE NXTFLD_SUB ! Load next field from section of driver file
    USE OBSCHK_SUB ! Check and set observer altitude
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNDRV ! LUN for driver table
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: PSIMAX = 90.0 ! Max abs.val[deg]  of obs horiz.angle 
!                                          relative to reference profile
! LOCAL VARIABLES
    INTEGER(I4)       :: IOS    ! Saved value of IOSTAT for error message
    INTEGER(I4)       :: LENGTH ! No.of characters written in FIELD 
    CHARACTER(LENREC) :: FIELD  ! Data field from driver file
    CHARACTER(80)     :: MESSGE ! Message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Read first field in *OBS section
  CALL NXTFLD ( LUNDRV, FIELD, LENGTH, FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
  IF ( LENGTH .EQ. 0 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVOBS: No data in *OBS section of Driver Table'
    RETURN
  END IF
!
! Read altitude from 1st field
  READ ( FIELD, *, IOSTAT=IOS ) ALTOBS
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-DRVOBS: Error reading Observer Altitude.' &
         // ' IOSTAT=', IOS
    RETURN
  END IF 
!
  MESSGE = 'I-DRVOBS: Read Observer Altitude = ' // TRIM ( C9REAL(ALTOBS) ) &
           // ' [km]'
  CALL WRTLOG ( MESSGE )
!
! Check valid altitude and insert extra profile level in atmosphere if reqd.
  CALL OBSCHK ( FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
! Check next field (should only be PSIOBS - Observer Horizontal angle).
  IF ( GRAFLG ) THEN
    CALL NXTFLD ( LUNDRV, FIELD, LENGTH, FAIL, ERRMSG )
    IF ( FAIL ) RETURN
!
    IF ( LENGTH .EQ. 0 ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-DRVOBS: No observer horizontal angle supplied ' // &
               '(required with GRA flag)'
      RETURN
    ENDIF
!
    READ ( FIELD, *, IOSTAT=IOS ) PSIOBS
    IF ( IOS .NE. 0 ) THEN
      FAIL = .TRUE.
      WRITE ( ERRMSG, * ) 'F-DRVOBS: Error reading Obs.horiz.angle, IOSTAT=',IOS
    ELSE IF ( ABS ( PSIOBS ) .GT. PSIMAX ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-DRVOBS: Abs(Obs.Horiz.angle)=' // C9REAL ( ABS(PSIOBS) ) // &
               ' > PSIMAX=' // C9REAL ( PSIMAX ) // ' [deg]'
    ELSE 
      MESSGE = 'I-DRVOBS: Read Observer Horiz.Angle =' //C9REAL(PSIOBS)//' [deg]'
      CALL WRTLOG ( MESSGE )
    END IF
    IF ( FAIL ) RETURN
!
    IF ( PSIOBS .LT. 0.0 .OR. PSIOBS .GT. 45.0 ) &
      CALL WRTLOG ( 'W-DRVOBS: Unusual value of Obs.Horiz.Angle' )
  END IF
!
! Check for any superfluous fields in section         
  CALL ENDCHK ( LUNDRV, '*OBS', FAIL, ERRMSG )
!
END SUBROUTINE DRVOBS
END MODULE DRVOBS_SUB

