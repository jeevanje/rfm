MODULE LUTINF_SUB
CONTAINS
SUBROUTINE LUTINF ( LUNLUT, NAMLUT, TYP, IDXMOL, IDXISO, V1, V2, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Identify LUT file type, molecule and spectral range
!   Called by LUTFIL for each file in *LUT section of driver table
!
! VARIABLE KINDS
    USE KIND_DAT
!
! SUBROUTINES
    USE SVDINF_SUB ! Extract info from SVD-LUT header
    USE TABINF_SUB ! Extract info from TAB-LUT header record
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNLUT ! LUN for LUT File
    CHARACTER(*),  INTENT(IN)  :: NAMLUT ! Name of LUT file 
    CHARACTER(3),  INTENT(OUT) :: TYP    ! Type of LUT file: ASC, BIN or SVD
    INTEGER(I4),   INTENT(OUT) :: IDXMOL ! RFM/HITRAN ID of gas
    INTEGER(I4),   INTENT(OUT) :: IDXISO ! Isotopic ID of gas, or -1
    REAL(R8),      INTENT(OUT) :: V1     ! Lower Wno limit [cm-1] of data
    REAL(R8),      INTENT(OUT) :: V2     ! Upper Wno limit [cm-1] of data
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER(I4),  PARAMETER :: NTYP = 3 ! No different LUT file types
    CHARACTER(3), PARAMETER :: ASBLST(NTYP) = (/ 'ASC', 'SVD', 'BIN' /)
    CHARACTER(3), PARAMETER :: BASLST(NTYP) = (/ 'BIN', 'ASC', 'SVD' /)
    CHARACTER(3), PARAMETER :: SBALST(NTYP) = (/ 'SVD', 'BIN', 'ASC' /)
!
! LOCAL VARIABLES
    LOGICAL      :: LREAD        ! File successfully identified and read
    INTEGER(I4)  :: ITYP         ! Index of LUT type in TYPLST
    CHARACTER(3) :: TYPLST(NTYP) ! List of types to be tested in sequence
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Use filename to guess probable type and put first in trial list
  IF ( INDEX ( NAMLUT, '.bin' ) .GT. 0 ) THEN
    TYPLST = BASLST
  ELSE IF ( INDEX ( NAMLUT, 'CS_' ) .GT. 0 ) THEN
    TYPLST = SBALST
  ELSE
    TYPLST = ASBLST
  END IF
!
! Try all different types starting with guessed type
  DO ITYP = 1, NTYP
    TYP = TYPLST(ITYP) 
    SELECT CASE ( TYP )
!
    CASE ( 'ASC' )
      CALL TABINF ( LUNLUT, NAMLUT, .FALSE., &
                    LREAD, IDXMOL, IDXISO, V1, V2, FAIL, ERRMSG )
      IF ( FAIL ) RETURN
      IF ( LREAD ) RETURN
!
    CASE ( 'BIN' )                 ! could interpret SVD LUT as .bin !
      CALL TABINF ( LUNLUT, NAMLUT, .TRUE., &
                    LREAD, IDXMOL, IDXISO, V1, V2, FAIL, ERRMSG )
      IF ( FAIL ) RETURN
      IF ( LREAD ) RETURN
!
    CASE ( 'SVD' )
      CALL SVDINF ( LUNLUT, NAMLUT, &
                    LREAD, IDXMOL, IDXISO, V1, V2, FAIL, ERRMSG ) 
      IF ( FAIL ) RETURN
      IF ( LREAD ) RETURN
!
    CASE DEFAULT
      STOP 'F-LUTINF: Logical error'
    END SELECT
  END DO
!
  FAIL = .TRUE.
  ERRMSG = 'F-LUTINF: Unable to identify LUT file type'
!
END SUBROUTINE LUTINF
END MODULE LUTINF_SUB
