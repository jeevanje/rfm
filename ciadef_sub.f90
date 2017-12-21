MODULE CIADEF_SUB
CONTAINS
SUBROUTINE CIADEF ( NAMDEF, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Use default .cia filename to find any missing files
!   Called by DRVCIA if filename template found in *CIA section of driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE CIACOM_DAT ! Collision-induced absorption data
    USE LENREC_DAT ! Max length of input text record
!
! SUBROUTINES
    USE CIAFIL_SUB ! Read Collision-Induced Abs. data file
    USE CIAMOL_SUB ! Identify molecules from .cia file header record
    USE LEXIST_FNC ! Check if file exists
    USE USEMOL_FNC ! T = molecule is required
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMDEF ! Default name of CIAfile
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS          
    CHARACTER(20), PARAMETER :: CIALST(7) = & ! List of  CIA molecule pairs 
                             (/ 'N2-N2  ', 'O2-CO2 ', 'O2-N2  ', 'O2-O2  ', &
                                'CH4-CH4', 'CO2-CO2', 'N2-CH4 ' /)
! LOCAL VARIABLES
    INTEGER(I4)       :: ID1,ID2 ! RFM/HITRAN indices for molecule pairs
    INTEGER(I4)       :: ILST    ! Counter for listed pairs
    INTEGER(I4)       :: IPT     ! Location of '*' in NAMDEF
    CHARACTER(LENREC) :: NAMCIA  ! Name of CIA file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  CALL WRTLOG ( 'I-CIADEF: Checking default CIA files' )
!
  IPT = INDEX ( NAMDEF, '*' )
  IF ( IPT .EQ. 0 ) STOP 'F-CIADEF: Logical error'
!
  DO ILST = 1, SIZE ( CIALST ) 
    CALL CIAMOL ( CIALST(ILST), ID1, ID2, FAIL, ERRMSG ) 
! Only error should be if CIAMOL list inconsistent with those in CIALST
    IF ( FAIL ) STOP 'F-CIADEF: Logical Error'
    IF ( USEMOL(ID1) .AND. USEMOL(ID2) ) THEN ! using pair
      IF ( NGGCIA .GT. 0 ) THEN
        IF ( ANY ( IDDCIA(1,:) .EQ. ID1 .AND. IDDCIA(2,:) .EQ. ID2 ) ) CYCLE
      END IF
      NAMCIA = NAMDEF(1:IPT-1) // TRIM(CIALST(ILST)) // NAMDEF(IPT+1:)
      CALL WRTLOG ( 'I-CIADEF: looking for file: ' // NAMCIA )
      IF ( LEXIST ( NAMCIA ) ) THEN
        CALL CIAFIL ( NAMCIA, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      ELSE
        CALL WRTLOG ( '... file not found' )
      END IF
    END IF
  END DO
!
END SUBROUTINE CIADEF
END MODULE CIADEF_SUB
