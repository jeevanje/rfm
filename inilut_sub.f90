MODULE INILUT_SUB
CONTAINS
SUBROUTINE INILUT ( ISPC, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion of spclut.for. Checked.
!
! DESCRIPTION
!   Initialise LUT data for each new spectral range
!   Called by SPCINI at the start of each spectral range.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE LFLCOM_DAT ! Look-Up Table files
    USE LUTCOM_DAT ! TAB LUT data
    USE SVDCOM_DAT ! SVD-compressed LUT data
    USE RFMLUN_DAT, ONLY: LUNTMP ! LUN for temporarily open files
!
! SUBROUTINES
    USE LUTSVD_SUB ! Load SVD-compressed LUT data from file
    USE LUTTAB_SUB ! Initialise LUT-TAB file
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: ISPC   ! Current spectral range
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IGAS   ! Counter, index for absorbers
    INTEGER(I4) :: ILFL   ! Index of LUT files
    INTEGER(I4) :: ILUT   ! Counter for TAB-LUT files
    INTEGER(I4) :: ISVD   ! Counter for SVD-LUT files
    LOGICAL, SAVE, ALLOCATABLE :: HITGAS(:) ! Flags indicating HITRAN gases
    LOGICAL, SAVE, ALLOCATABLE :: XSCGAS(:) ! Flags indicating x/s gases
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
!
! Save HIT, XSC flags for each molecule since these may be set FALSE if
! LUT table data used for current spectral range
  IF ( ISPC .EQ. 1 ) THEN
    ALLOCATE ( HITGAS(NGAS) ) 
    ALLOCATE ( XSCGAS(NGAS) ) 
    HITGAS = GAS%HIT
    XSCGAS = GAS%XSC
  ELSE                     ! Reset to original values for next spectral range
    GAS%HIT = HITGAS
    GAS%XSC = XSCGAS
  END IF
!
  NSVD = 0
  NLUT = 0
! First calculate how many SVD and LUT files are to be used for spectral range
  DO IGAS = 1, NGAS
    ILFL = IDXLFL(IGAS,ISPC) 
    IF ( ILFL .EQ. 0 ) CYCLE
    IF ( LFL(ILFL)%TYP .EQ. 'SVD' ) THEN
      NSVD = NSVD + 1
    ELSE                  ! TYP .eq. 'ASC' or 'BIN'
      NLUT = NLUT + 1
    END IF
  END DO
!
  IF ( NSVD .GE. 1 ) ALLOCATE ( SVD(NSVD) ) 
  IF ( NLUT .GE. 1 ) ALLOCATE ( LUT(NLUT) ) 
!
  IF ( NSVD .EQ. 0 .AND. NLUT .EQ. 0 ) RETURN ! No LUTs for this spec.range
!
  ISVD = 0
  ILUT = 0
! Next pass is to fill data
  DO IGAS = 1, NGAS
    ILFL = IDXLFL(IGAS,ISPC) 
    IF ( ILFL .EQ. 0 ) CYCLE
    GAS(IGAS)%HIT = .FALSE.    ! set flag=F since replaced by LUT
    GAS(IGAS)%XSC = .FALSE.
    IF ( LFL(ILFL)%TYP .EQ. 'SVD' ) THEN
      ISVD = ISVD + 1
      CALL LUTSVD ( LUNTMP, LFL(ILFL)%NAM, ISVD, LFL(ILFL)%NL, FAIL, ERRMSG )
    ELSE 
      ILUT = ILUT + 1
      CALL LUTTAB ( LFL(ILFL)%LUN, LFL(ILFL)%NAM, (LFL(ILFL)%TYP .EQ. 'BIN'), &
                    ILUT, LFL(ILFL)%NDP, LFL(ILFL)%NDT, FAIL, ERRMSG )
    END IF
    IF ( FAIL ) RETURN
  END DO
!
! Normal exit
!
END SUBROUTINE INILUT
END MODULE INILUT_SUB
