MODULE DRVPHY_SUB
CONTAINS
SUBROUTINE DRVPHY ( LUNDRV, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Tested.
!
! DESCRIPTION
!   Read RFM driver table *PHY section
!   Read modified values of adjustable physical constants
!   Called by RFMDRV once if *PHY key listed in RFM Driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE PHYADJ_DAT ! Adjustable physical constants used within the RFM
    USE LENREC_DAT ! Max length of input text record
!
! SUBROUTINES
    USE NXTREC_SUB ! Load next record from input file
    USE PARFLD_SUB ! Extract Parameter=Value string from record
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
    LOGICAL           :: ENDSEC ! T=reached end of driver file *PHY section
    LOGICAL           :: GOTPAR ! T=identified PARAM=VALUE pair from record
    INTEGER(I4)       :: IOS    ! Saved value of IOSTAT for error messages
    CHARACTER(80)     :: LOGMSG ! Message sent to log file
    CHARACTER(6)      :: PARAM  ! Name of parameter to be adjusted
    CHARACTER(LENREC) :: RECORD ! Record read from RFM Driver file
    CHARACTER(10)     :: VALUE  ! Substring containing new value of parameter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  DO 
    CALL NXTREC ( LUNDRV, RECORD, ENDSEC, FAIL, ERRMSG ) 
    IF ( FAIL ) RETURN
    IF ( ENDSEC ) EXIT
    CALL PARFLD ( RECORD, GOTPAR, PARAM, VALUE ) 
    IF ( .NOT. GOTPAR ) THEN 
      FAIL = .TRUE.
      ERRMSG = 'F-DRVPHY: Unable to identify PARAM=VALUE from *PHY ' // &
               'section record'
      RETURN
    END IF
!
    SELECT CASE ( PARAM ) 
    CASE ( 'CPKMOL' ) ; READ ( VALUE, *, IOSTAT=IOS ) CPKMOL 
    CASE ( 'GRAVTY' ) ; READ ( VALUE, *, IOSTAT=IOS ) GRAVTY
    CASE ( 'WGTAIR' ) ; READ ( VALUE, *, IOSTAT=IOS ) WGTAIR
    CASE ( 'TEMSPA' ) ; READ ( VALUE, *, IOSTAT=IOS ) TEMSPA
    CASE ( 'RADCRV' ) ; READ ( VALUE, *, IOSTAT=IOS ) RADCRV
    CASE DEFAULT
       FAIL = .TRUE.
       ERRMSG = 'F-DRVPHY: Unrecognised adjustable parameter: '// PARAM
       RETURN
    END SELECT
    IF ( IOS .NE. 0 ) THEN
      FAIL = .TRUE. 
      WRITE ( ERRMSG, * ) 'F-DRVPHY: PARAM=' // PARAM // &
       ': error reading VALUE=' // VALUE // '. IOSTAT=', IOS
      RETURN
    END IF
!
    LOGMSG = 'I-DRVPHY: Setting new value for ' // TRIM(PARAM) // '=' // VALUE
    CALL WRTLOG ( LOGMSG )
!
  END DO
!
END SUBROUTINE DRVPHY
END MODULE DRVPHY_SUB

