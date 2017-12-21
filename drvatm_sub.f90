MODULE DRVATM_SUB
CONTAINS
SUBROUTINE DRVATM ( LUNDRV, FAIL, ERRMSG )
!
! VERSION
!   21JUN17 AD Add ATMPAR
!   01MAY17 AD F90 conversion of original module INPATM. Checked.
!
! DESCRIPTION
!   Read RFM driver table *ATM section
!   Called once by RFMDRV.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE LENREC_DAT ! Max length of input text record
!
! SUBROUTINES
    USE ATMAIR_SUB ! Set VMR profile values for air
    USE ATMAUX_SUB ! Set up auxiliary profiles of atmospheric parameters
    USE ATMCHK_SUB ! Check all required atmospheric profiles loaded
    USE ATMFIL_SUB ! Read file containing atmospheric profiles
    USE ATMGRA_SUB ! Interpolate to fill 2-D atmospheric profile field
    USE ATMPAR_SUB ! Assign atmospheric profile to single value
    USE ATMPSI_SUB ! Extract Psi angle from brackets following .atm filename
    USE NXTFLD_SUB ! Load next field from section of driver file
    USE PARFLD_SUB ! Extract Parameter=Value string from record
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNDRV ! LUN for Driver File
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    LOGICAL           :: GOTPAR ! T=extracted PARAM = VALUE from field
    INTEGER(I4)       :: LENGTH ! Length of filename read from driver file
    CHARACTER(LENREC) :: NAMATM ! Name of ATM file read from Driver Table
    CHARACTER(LENREC) :: PARAM  ! Parameter from PARAM=VALUE field
    CHARACTER(LENREC) :: VALUE  ! Value from PARAM=VALUE field
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! For each new field in *ATM section...
  DO 
    CALL NXTFLD ( LUNDRV, NAMATM, LENGTH, FAIL, ERRMSG )
    IF ( FAIL ) RETURN
    IF ( LENGTH .EQ. 0 ) EXIT
    CALL ATMPSI ( NAMATM, FAIL, ERRMSG )
    IF ( FAIL ) RETURN
    CALL PARFLD ( NAMATM, GOTPAR, PARAM, VALUE ) ! See if form PARAM=VALUE
    IF ( GOTPAR ) THEN 
      CALL ATMPAR ( PARAM, VALUE, FAIL, ERRMSG )
    ELSE
      CALL ATMFIL ( NAMATM(1:LENGTH), FAIL, ERRMSG )
    END IF
    IF ( FAIL ) RETURN
  END DO
!
! If Rayleigh extinction is modelled, set VMR profile=1ppv for 'air'
  IF ( REXFLG ) CALL ATMAIR 
!
! Check all required profiles have been loaded
  CALL ATMCHK ( FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
! Fill out 2D field with available data
  IF ( GRAFLG ) CALL ATMGRA
!
! Calculate auxiliary profiles
  CALL ATMAUX 
!
! Normal exit
  FAIL = .FALSE.
!
END SUBROUTINE DRVATM
END MODULE DRVATM_SUB

