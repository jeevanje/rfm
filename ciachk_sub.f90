MODULE CIACHK_SUB
CONTAINS
SUBROUTINE CIACHK ( ID1, ID2, TEM, WNL, WNU, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Tested.
!
! DESCRIPTION
!   Check CIA data consistent with other RFM options.
!   Called by CIAFIL for each .cia file loaded
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE CIACOM_DAT ! Collision-induced absorption data
    USE GASCOM_DAT ! Molecule and isotope data
    USE IDXCON_DAT ! RFM/HITRAN indices for specific molecules
!
! SUBROUTINES
    USE C9REAL_GEN ! Write real number as C*9 string
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: ID1    ! Index of first molecule
    INTEGER(I4),   INTENT(IN)  :: ID2    ! Index of second molecule
    REAL(R4),      INTENT(IN)  :: TEM    ! Temperature [K] of dataset
    REAL(R8),      INTENT(IN)  :: WNL    ! Lower Wno [cm-1] of dataset
    REAL(R8),      INTENT(IN)  :: WNU    ! Upper Wno [cm-1] of dataset
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: TEMTOL = 0.1 ! Tolerance [K] to separate temperatures
!
! LOCAL VARIABLES
    INTEGER(I4) :: ICIA ! Indices of CIA data sets
    INTEGER(I4) :: IGAS ! Index of molecule in /GASCOM/ arrays
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  FAIL = .TRUE.
!
! Check CIA not being combined with continua representing same feature
  IF ( ID1 .EQ. IDXO2 .AND. ID2 .EQ. IDXO2 ) THEN
    IGAS = IGSMOL(IDXO2)
    IF ( GAS(IGAS)%CTM ) THEN
      ERRMSG = 'F-CIACHK: O2-O2 CIA incompatible with O2 continuum'
      RETURN
    END IF
  ELSE IF ( ID1 .EQ. IDXN2 .AND. ID2 .EQ. IDXN2 ) THEN
    IGAS = IGSMOL(IDXN2)
    IF ( GAS(IGAS)%CTM ) THEN
      ERRMSG = 'F-CIACHK: N2-N2 CIA incompatible with N2 continuum'
      RETURN
    END IF
  END IF
!
! Check no overlaps with previously loaded data
  DO ICIA = 1, NCIA
    IF ( ID1 .NE. CIA(ICIA)%ID1 .OR. ID2 .NE. CIA(ICIA)%ID2 ) CYCLE
    IF ( WNL .LE. CIA(ICIA)%WNU .AND. WNU .GT. CIA(ICIA)%WNL .AND. &
         ABS ( TEM - CIA(ICIA)%TEM ) .LT. TEMTOL ) THEN
      ERRMSG = 'F-CIACHK: Overlapping CIA datasets for ' // &
               TRIM ( C9REAL ( WNL ) ) // '-' // &
               TRIM ( C9REAL ( WNU ) ) //' cm-1, T=' // C9REAL ( TEM ) 
      RETURN
    END IF
  END DO     
!
! Normal exit
  FAIL = .FALSE.
!
END SUBROUTINE CIACHK
END MODULE CIACHK_SUB

