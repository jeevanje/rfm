MODULE ATMPAR_SUB
CONTAINS
SUBROUTINE ATMPAR ( PARAM, VALUE, FAIL, ERRMSG )
!
! VERSION
!   21JUN17 AD Original.
!
! DESCRIPTION
!   Assign atmospheric profile to single value
!   Called by DRVATM if PARAM=VALUE listed in *ATM section of driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! SUBROUTINES
    USE ATMPRF_SUB ! Load profile into ATMCOM
    USE WRTLOG_SUB ! Write text message to log file
    USE ATMCOM_DAT, ONLY: LENATM ! Max length of .atm profile label
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: PARAM  ! Profile type
    CHARACTER(*),  INTENT(IN)  :: VALUE  ! Profile value
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    LOGICAL           :: USEPAR ! T=parameter is required, F=not required
    INTEGER(I4)       :: IOS    ! Saved value of IOSTAT for error message
    REAL(R4)          :: RVAL   ! VALUE converted to real number
    CHARACTER(LENATM) :: LABEL  ! Label identifying profile
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  LABEL = PARAM
  IF ( LABEL(1:1) .NE. '*') LABEL = '*'//LABEL(1:LENATM-1)
!
  READ ( VALUE, *, IOSTAT=IOS ) RVAL
  IF ( IOS .NE. 0 ) THEN
    WRITE ( ERRMSG, * ) 'F-ATMPAR: Error reading value from ' // PARAM // &
      '=' // VALUE // '. IOS=', IOS
    FAIL = .TRUE.
    RETURN
  END IF
!  
  CALL ATMPRF ( LABEL, (/ 1.0 /), (/ RVAL /), USEPAR, FAIL, ERRMSG ) 
  IF ( FAIL ) RETURN
  IF ( .NOT. USEPAR ) CALL WRTLOG ( 'W-ATMPAR: Parameter ' // TRIM(LABEL) // &
    ' ignored - not required' ) 
!
END SUBROUTINE ATMPAR
END MODULE ATMPAR_SUB

