MODULE OBSCHK_SUB
CONTAINS
SUBROUTINE OBSCHK ( FAIL, ERRMSG )
!
! VERSION
!   02OCT17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Check and set observer altitude
!   Called by DRVOBS if OBS flag enabled
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE OBSCOM_DAT ! Observer location data
    USE TANCOM_DAT ! Tangent path data
    USE FLGCOM_DAT, ONLY: NADFLG, ZENFLG ! T = nadir,zenith-viewing plane atm.
!
! SUBROUTINES
    USE ATMLEV_SUB ! Find/insert atmospheric level for given altitude
    USE C9REAL_GEN ! Write real number as C*9 string
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES 
    INTEGER(I4) :: IATM ! Index of atmospheric level for observer
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
!
  IF ( ALTOBS .LT. HGTSFC ) THEN
    ERRMSG = 'F-OBSCHK: Observer Altitude below base of atmosphere, =' // &
             C9REAL(HGTSFC) // ' [km]'              
    FAIL = .TRUE.
  ELSE IF ( ALTOBS .GT. HGTTOA ) THEN ! Observer above top of atm.
    IF ( ZENFLG ) THEN                      ! Fatal error if looking upward
      ERRMSG = 'F-OBSCHK: Observer Altitude above top of atmosphere, =' // &
               C9REAL(HGTTOA) // ' [km]'              
      FAIL = .TRUE.
    ELSE                                    ! OK for other cases
      IATOBS = NATM
    END IF
  ELSE 
! Note that ATMLEV itself adjusts IATOBS so cannot use this as argument
    CALL ATMLEV ( ALTOBS, IATM )  
    IATOBS = IATM
  END IF
!
  IF ( ZENFLG .OR. NADFLG ) TAN%IAT = IATOBS
!
END SUBROUTINE OBSCHK
END MODULE OBSCHK_SUB

