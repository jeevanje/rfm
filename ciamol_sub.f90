MODULE CIAMOL_SUB
CONTAINS
SUBROUTINE CIAMOL ( MOLSTR, IDX1, IDX2, FAIL, ERRMSG ) 
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
! 
! DESCRIPTION    
!   Identify molecules from .cia file header record
!   Called by CIAFIL and CIADEF
!   The .cia header record contains a C*20 right-justified string containing
!   the pair of molecules, eg: '     O2-N2'
!   This will return IDX1 or IDX2 = 0 if molecule is not one of the list of
!   currently recognised CIA molecules: CO2, CH4, O2 or N2.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE IDXCON_DAT ! RFM/HITRAN indices for specific molecules
!                  
  IMPLICIT NONE
!
! ARGUMENTS      
    CHARACTER(*),  INTENT(IN)  :: MOLSTR ! String containing molecules
    INTEGER(I4),   INTENT(OUT) :: IDX1   ! HITRAN/RFM index of first molecule
    INTEGER(I4),   INTENT(OUT) :: IDX2   ! HITRAN/RFM index of second molecule
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: I ! Location of '-' in MOLSTR
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Locate '-' character in MOLSTR
  I = INDEX ( MOLSTR, '-' ) 
  IF ( I .LE. 1 .OR. I .EQ. LEN ( MOLSTR ) ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-CIAMOL: Unable to read molecules from .cia header:' // MOLSTR
    RETURN
  END IF
!
! These molecules should be consistent with those in ciadef.for
  IDX1 = 0
  IF ( MOLSTR(1:I-1) .EQ. 'CO2' ) IDX1 = IDXCO2
  IF ( MOLSTR(1:I-1) .EQ. 'CH4' ) IDX1 = IDXCH4
  IF ( MOLSTR(1:I-1) .EQ. 'O2' )  IDX1 = IDXO2
  IF ( MOLSTR(1:I-1) .EQ. 'N2' )  IDX1 = IDXN2
!
  IDX2 = 0
  IF ( MOLSTR(I+1:) .EQ. 'CO2' ) IDX2 = IDXCO2
  IF ( MOLSTR(I+1:) .EQ. 'CH4' ) IDX2 = IDXCH4
  IF ( MOLSTR(I+1:) .EQ. 'O2' )  IDX2 = IDXO2
  IF ( MOLSTR(I+1:) .EQ. 'N2' )  IDX2 = IDXN2
!
! Normal exit
  FAIL = .FALSE.
! 
END SUBROUTINE CIAMOL
END MODULE CIAMOL_SUB
