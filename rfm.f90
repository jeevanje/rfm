PROGRAM RFM
!
! VERSION (update VIDHDR)
!   02OCT17 AD v5.0 F90 conversion. Tested.
!
! DESCRIPTION
!   Reference Forward Model
!   This is the F90 version.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE RUN_ID_DAT ! RFM Run ID string
    USE SPCCOM_DAT ! Spectral range data
    USE HDRCOM_DAT, ONLY: VIDHDR ! RFM version identifier
    USE RFMLUN_DAT, ONLY: LUNLOG ! LUN for log file
!
! SUBROUTINES
    USE RFMDAL_SUB ! Deallocate program level pointers
    USE RFMDRV_SUB ! Read RFM driver table
    USE RFMPRF_SUB ! Write out RFM internal atmospheric profile
    USE RFMPTH_SUB ! Set up RFM path calculations
    USE RFMSPC_SUB ! Calculations for one spectral range
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! LOCAL CONSTANTS
    LOGICAL, PARAMETER :: PROMPT = .FALSE. ! T=prompt user for appended ID
!
! LOCAL VARIABLES
    LOGICAL       :: FAIL   ! Set TRUE if a fatal error is detected
    INTEGER(I4)   :: IOS    ! Value of IOSTAT on OPEN  
    INTEGER(I4)   :: ISPC   ! Counter for spectral ranges
    CHARACTER(80) :: ERRMSG ! Error message if FAIL is TRUE
    CHARACTER(80) :: LOGMSG ! Text message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  VIDHDR = '02OCT17_NJ'
  LOGMSG = 'R-RFM: Running RFM v' // VIDHDR    
  WRITE ( *, '(A)' ) LOGMSG
!
  IF ( PROMPT ) THEN
    WRITE ( *, '(A)' ) 'Optional ID to be appended to filenames (<CR>=none):'
    READ ( *, '(A)' ) RUN_ID
    IF ( RUN_ID .NE. '' ) &
      WRITE ( *, '(A)' ) 'R-RFM:    Filename append string=' // RUN_ID
  END IF
!
! Open log file
  OPEN ( UNIT=LUNLOG, FILE='rfm.log'//RUN_ID, ACTION='WRITE', &
         STATUS='REPLACE', IOSTAT=IOS )
  IF ( IOS .NE. 0 ) THEN
    WRITE ( *, '(A)' ) 'F-RFM: Error opening rfm.log file. IOSTAT=', IOS
    STOP
  END IF
  CALL WRTLOG ( LOGMSG ) 
!
! Read driver table contents
  CALL RFMDRV ( FAIL, ERRMSG ) 
  IF ( FAIL ) GOTO 900
!
! If PRF flag, output atmospheric profile being used
  IF ( PRFFLG ) THEN 
    CALL RFMPRF ( FAIL, ERRMSG )
    IF ( FAIL ) GOTO 900
  END IF
!
! Calculate equivalent CG paths
  CALL RFMPTH ( FAIL, ERRMSG )
  IF ( FAIL ) GOTO 900
!
! Loop over required spectral ranges
  DO ISPC = 1, NSPC
    IF ( NSPC .GT. 1 ) THEN 
      LOGMSG = 'I-RFM: Calculation for spectral range: ' // SPC(ISPC)%LAB
      IF ( .NOT. SHHFLG ) WRITE ( *, '(A)' ) LOGMSG
      CALL WRTLOG ( LOGMSG ) 
    END IF
    CALL RFMSPC ( ISPC, FAIL, ERRMSG ) 
    IF ( FAIL ) GOTO 900
  END DO
!
! Deallocate pointers
  CALL RFMDAL 
!
900 CONTINUE
  IF ( FAIL ) THEN
    LOGMSG = ERRMSG
  ELSE
    LOGMSG = 'R-RFM: Successful completion'
  END IF
  CALL WRTLOG ( LOGMSG )
  WRITE ( *, '(A)' ) LOGMSG
!
END PROGRAM RFM
