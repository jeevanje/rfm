MODULE CHKNAM_SUB
CONTAINS
SUBROUTINE CHKNAM ( FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 original. Checked.
!
! DESCRIPTION
!   Check RFM output filename templates
!   Called by DRVCHK for any driver file sections defining output filenames.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE NAMCOM_DAT ! RFM output filenames
    USE GASCOM_DAT, ONLY: NGAS   ! No. different molecules
    USE GRACOM_DAT, ONLY: NPSI   ! No. different horiz.profile locations
    USE SPCCOM_DAT, ONLY: NSPC   ! No. different spectral ranges
    USE TANCOM_DAT, ONLY: NTAN   ! No. output tangent paths
!
! SUBROUTINES
    USE LOCASE_FNC ! Convert text string to lower case
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER, PARAMETER :: NTYP = 11
    CHARACTER(3), PARAMETER :: TYPLST(NTYP) = & ! Different file types
      (/ 'ABS', 'BBT', 'COO', 'OPT', 'PRF', 'PTH', 'RAD', 'RJT', 'TAB', &
         'TRA', 'WID' /)
!
! LOCAL VARIABLES
    LOGICAL           :: LMULTI ! T = multiple output files for type required
    INTEGER(I4)       :: IPT    ! Pointer to '*' in supplied filename
    INTEGER(I4)       :: ITYP   ! Counter for different output file types
    CHARACTER(LENNAM) :: NAMLOW ! Lower case version of NAMFIL
    CHARACTER(LENNAM), POINTER :: NAMFIL ! Output file template
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  DO ITYP = 1, NTYP
    SELECT CASE ( TYPLST(ITYP) )
    CASE ( 'ABS' ) 
      IF ( .NOT. ABSFLG ) CYCLE
      NAMFIL => NAMABS
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'BBT' ) 
      IF ( .NOT. BBTFLG ) CYCLE
      NAMFIL => NAMBBT
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'COO' ) 
      IF ( .NOT. COOFLG ) CYCLE
      NAMFIL => NAMCOO
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'OPT' ) 
      IF ( .NOT. OPTFLG ) CYCLE
      NAMFIL => NAMOPT
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'PRF' ) 
      IF ( .NOT. GRAFLG ) CYCLE
      NAMFIL => NAMPRF
      LMULTI = NPSI .GT. 1 
    CASE ( 'PTH' ) 
      IF ( .NOT. PTHFLG ) CYCLE
      NAMFIL => NAMPTH
      LMULTI = NTAN .GT. 1 .OR. NPSI .GT. 1
    CASE ( 'RAD' ) 
      IF ( .NOT. RADFLG ) CYCLE
      NAMFIL => NAMRAD
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'RJT' ) 
      IF ( .NOT. RJTFLG ) CYCLE
      NAMFIL => NAMRJT
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'TAB' ) 
      IF ( .NOT. TABFLG ) CYCLE
      NAMFIL => NAMTAB
      LMULTI = NGAS .GT. 1 .OR. NSPC .GT. 1
    CASE ( 'TRA' ) 
      IF ( .NOT. TRAFLG ) CYCLE
      NAMFIL => NAMTRA
      LMULTI = NTAN .GT. 1 .OR. NSPC .GT. 1 .OR. JACFLG .OR. LEVFLG
    CASE ( 'WID' ) 
      IF ( .NOT. WIDFLG ) CYCLE
      NAMFIL => NAMWID
      LMULTI = NGAS .GT. 1 .OR. NSPC .GT. 1
    CASE DEFAULT
      STOP 'F-CHKNAM: Logical error'
    END SELECT
!
! If multiple output files required, check filename has a '*' symbol
    IF ( LMULTI .AND. INDEX ( NAMFIL, '*') .EQ. 0 ) THEN
      FAIL = .TRUE. 
      ERRMSG = 'F-CHKNAM: ''*'' character required in ' // TYPLST(ITYP) // &
                ' filename template'
      RETURN
    END IF
!
    NAMLOW = LOCASE ( NAMFIL ) 
    IF ( BINFLG ) THEN
      IPT = INDEX ( NAMLOW, '.asc' ) 
      IF ( IPT .NE. 0 ) NAMFIL(IPT:IPT+3) = '.bin'
    ELSE
      IPT = INDEX ( NAMLOW, '.bin' ) 
      IF ( IPT .NE. 0 ) NAMFIL(IPT:IPT+3) = '.asc'
    END IF
!
  END DO
!
  NULLIFY ( NAMFIL )
!
END SUBROUTINE CHKNAM
END MODULE CHKNAM_SUB
