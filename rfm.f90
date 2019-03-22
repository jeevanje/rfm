PROGRAM RFM
!
! VERSION (update VIDHDR)
!   08FEB19 AD Bug#16
!   04FEB19 AD Bug#14,15
!   14JAN19 AD Bug#13
!   30NOV18 AD Bug#12
!   21NOV18 AD Bug#11
!   02NOV18 AD New H2O continuum model 
!   12OCT18 AD Bug#10
!   28JUN18 AD Bug#9
!   01JUN18 AD v5.01
!   31MAY18 AD Bug#8
!   31MAY18 AD Bug#7
!   10MAY18 AD Redefine molecules#48,#50-53
!   05MAY18 AD Bug#6
!   04MAY18 AD Bug#3, Bug#4, Bug#5
!   02MAY18 AD Bug#1, Bug#2
!   29JAN18 AD v5.00 F90 version. Tested.
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
  VIDHDR = '5.02_08FEB'
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
