MODULE SVDINF_SUB
CONTAINS
SUBROUTINE SVDINF ( LUNLUT, NAMLUT, LREAD, IDXMOL, IDXISO, V1, V2, &
                    FAIL, ERRMSG )
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Extract info from SVD-LUT header 
!   Called by LUTINF for each SVD-LUT file
!   If file appears not to be an SVD-LUT file, LREAD is returned as FALSE.
!
! VARIABLE KINDS
    USE KIND_DAT 
!
! SUBROUTINES
    USE SVDHDR_SUB ! Read SVD-LUT header record
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNLUT ! LUN for reading SVD-LUT file
    CHARACTER(*),  INTENT(IN)  :: NAMLUT ! Name of SVD-LUT file
    LOGICAL,       INTENT(OUT) :: LREAD  ! T=file identified as SVD-LUT
    INTEGER(I4),   INTENT(OUT) :: IDXMOL ! RFM/HITRAN ID of gas
    INTEGER(I4),   INTENT(OUT) :: IDXISO ! Isotopic ID of gas, or -1
    REAL(R8),      INTENT(OUT) :: V1     ! Lower Wno limit [cm-1] of data
    REAL(R8),      INTENT(OUT) :: V2     ! Upper Wno limit [cm-1] of data
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IDUM   ! Dummy integer (NL, no singular vectors)
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)   :: NV     ! No.spectral points in SVD-LUT file
    REAL(R8)      :: DV     ! Wavenumber [cm-1] interval for SVD-LUT file
    CHARACTER(80) :: RECORD ! Record read from SVD-LUT file
    CHARACTER(3)  :: TAB    ! Tabulation function
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IOS = 0
  LREAD = .FALSE.
!
  OPEN ( UNIT=LUNLUT, FILE=NAMLUT, STATUS='OLD', ACTION='READ', &
         IOSTAT=IOS, ERR=900 )
  READ ( LUNLUT, '(A)', ERR=800 ) RECORD
  READ ( LUNLUT, '(A)', ERR=800 ) RECORD
  IF ( RECORD(1:1) .NE. '#' ) GOTO 800
! File now identified as SVD-LUT file. From here any I/O errors are fatal
  CALL WRTLOG ( RECORD )
!
  DO WHILE ( RECORD(1:1) .EQ. '#' )
    READ ( LUNLUT, '(A)', IOSTAT=IOS, ERR=900 ) RECORD
  END DO
!
  CALL SVDHDR ( RECORD, TAB, IDXMOL, IDXISO, FAIL, ERRMSG ) 
  IF ( FAIL ) RETURN
!
  READ ( LUNLUT, *, IOSTAT=IOS, ERR=900 ) IDUM, NV, V1, DV
!
  IF ( NV .LT. 1 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-SVDINF: Irreg spectral grid LUT-SVD files not yet implemented'
    RETURN
  END IF
!
  V2 = V1 + ( NV - 1 ) * DV
!
  LREAD = .TRUE.
!
800 CONTINUE
  CLOSE ( LUNLUT, IOSTAT=IOS, ERR=900 )
!
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-SVDINF: Error reading LUT file header. IOSTAT=', IOS
!
END SUBROUTINE SVDINF
END MODULE SVDINF_SUB
