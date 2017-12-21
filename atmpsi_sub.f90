MODULE ATMPSI_SUB
CONTAINS
SUBROUTINE ATMPSI ( NAMATM, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 F90 conversion. Checked.
!
! DESCRIPTION
!   Extract Psi angle from brackets following .atm filename
!   Called by DRVATM, DRVNTE (also for .nte filename)
!   If called with GRA flag this returns the index IGRA for the Psi angle.
!   If called without GRA flag this returns IGRA=0
!   Also strips off '(...)' from NAMATM leaving just the filename.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT, ONLY: GRAFLG ! T = use horizontal gradients
    USE GRACOM_DAT, ONLY: PSIATM ! Horiz angle for next profiles
!
! SUBROUTINES
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(INOUT) :: NAMATM ! Name of file 
    LOGICAL,       INTENT(OUT)   :: FAIL   ! Set TRUE if fatal error detected
    CHARACTER(80), INTENT(OUT)   :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: PSIMAX = 90.0 ! Max horizontal angle [deg]
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IEND   ! Location of end of text field in RECORD 
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error message
    INTEGER(I4)   :: ISTA   ! Location of start of text field in RECORD 
    CHARACTER(80) :: MESSGE ! Text message for LOG file
    CHARACTER(LEN(NAMATM)) :: PSISTR ! Substring containing horiz. angle
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
!
! Establish if there is any qualifier string appended to filename containing
! the horizontal angle
  ISTA = INDEX (  NAMATM, '(' )
  IF ( ISTA .EQ. 0 ) THEN
    IF ( GRAFLG ) THEN
      CALL WRTLOG ( 'W-ATMPSI: PSI angle not specified - assuming PSI=0' )
      PSIATM = 0.0
    END IF      
    RETURN       ! Normal exit if no appended angle
  END IF
!
! Separate filename and qualifier string
  PSISTR = NAMATM(ISTA:)
  NAMATM = NAMATM(1:ISTA-1)
!      
  IF ( .NOT. GRAFLG ) THEN
    MESSGE = 'W-ATMPSI: GRA flag disabled so profile Horiz.angles ignored.'
    CALL WRTLOG ( MESSGE ) 
    RETURN               ! Exit just removing appended horiz angle
  END IF
!
! Read Psi angle from qualifier string and check valid range
  IEND = INDEX ( PSISTR, ')' )
  IF ( IEND .EQ. 0 ) THEN
    ERRMSG= 'F-ATMPSI: No '')'' following ''('' for hor.angle of file: ' // &
            NAMATM(1:25)
    FAIL = .TRUE.
    RETURN
  END IF
  READ ( PSISTR(2:IEND-1), *, IOSTAT=IOS ) PSIATM
  IF ( IOS .NE. 0 ) THEN
    WRITE ( ERRMSG, * ) 'F-ATMPSI: Unable to read horiz.angle from ''' // &
      PSISTR(1:9) // '''. IOSTAT=', IOS
    FAIL = .TRUE.
    RETURN
  ELSE IF ( ABS(PSIATM) .GT. PSIMAX ) THEN
    WRITE ( ERRMSG, '(A,F7.3,A)' ) &
      'F-ATMPSI: |Psi angle| > allowed maximum magnitude=', PSIMAX, ' [deg]'
    FAIL = .TRUE.
    RETURN
  END IF
!
  WRITE ( MESSGE, '(A,F7.3)' ) &
    'I-ATMPSI: Loading horizontal gradient profiles at PSI=', PSIATM
  CALL WRTLOG ( MESSGE )
!
END SUBROUTINE ATMPSI
END MODULE ATMPSI_SUB
