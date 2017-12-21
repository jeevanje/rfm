MODULE LUTDEF_SUB
CONTAINS
SUBROUTINE LUTDEF ( NAMDEF, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Use default LUT filename to find any missing files
!   Called once by DRVLUT if filename template found in *LUT section of 
!   driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
! 
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE LENREC_DAT ! Max length of input text record
    USE SPCCOM_DAT ! Spectral range data
    USE LFLCOM_DAT, ONLY: IDXLFL ! Indices of assigned LFL files
!
! SUBROUTINES
    USE IDGOLD_FNC ! Convert new RFM index for .xsc data to old value
    USE LEXIST_FNC ! Check if file exists
    USE LUTFIL_SUB ! Check if LUT file is required
    USE NAMGAS_FNC ! Return molecule name + (iso) associated with GASCOM index
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMDEF ! Default name of LUT file
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: IGAS   ! Absorber counter
    INTEGER(I4)       :: IPT    ! Location of '*' character in NAMDEF
    INTEGER(I4)       :: ISPC   ! Counter for spectral ranges
    CHARACTER(2)      :: IDSTR  ! String containing molecule index
    CHARACTER(LENREC) :: NAMLUT ! Name of LUT file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  CALL WRTLOG ( 'I-LUTDEF: Checking default LUT files' )
!
  IPT = INDEX ( NAMDEF, '*' )
  IF ( IPT .EQ. 0 ) STOP 'F-LUTDEF: Logical error'
!      
  DO ISPC = 1, NSPC
    DO IGAS = 1, NGAS                           ! Loop over gases
      IF ( IDXLFL(IGAS,ISPC) .GT. 0 ) CYCLE     ! LUT already found
!
! At this point, spectral range/gas found requiring LUT data but no data loaded
! Construct spec/gas part of LUT filename, which depends on whether this is
! a SVD compressed LUT (SVDFIL=TRUE) or a direct tabulation (FALSE)
!
      IF ( INDEX ( NAMDEF, 'CS_' ) .GT. 0 ) THEN      ! SVD files
        WRITE ( IDSTR, '(I2.2)' ) IDGOLD ( GAS(IGAS)%IDM ) 
! for historical reasons SF6 has ID=30 not 64 for MIPAS LUTs
        IF ( IDSTR .EQ. '64' ) IDSTR = '30'
        NAMLUT = NAMDEF(1:IPT-1) // TRIM ( SPC(ISPC)%LAB ) // '_' // & 
                 IDSTR // NAMDEF(IPT+1:)
      ELSE                                        ! TAB files
        NAMLUT = NAMDEF(1:IPT-1) // TRIM ( SPC(ISPC)%LAB ) // &
                 TRIM ( NAMGAS(IGAS) ) // NAMDEF(IPT+1:)
      END IF
!
      CALL WRTLOG ( 'I-LUTDEF: looking for file:' // NAMLUT )
      IF ( LEXIST ( NAMLUT ) ) THEN
        CALL LUTFIL ( NAMLUT, FAIL, ERRMSG )
        IF ( FAIL ) RETURN
      ELSE
        CALL WRTLOG ( '... file not found' )
      END IF
    END DO
  END DO
!
END SUBROUTINE LUTDEF
END MODULE LUTDEF_SUB
