MODULE OBSCOM_DAT
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Observer location data
!   ALTOBS set in DRVOBS if OBS flag T.
!   IATOBS set in OBSCHK if OBS flag T.
!   PSIOBS set in DRVOBS if OBS+GRA flags T.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
! GLOBAL VARIABLES
    INTEGER(I4) :: IATOBS = 0 ! Atm level# of observer (1:NATM)
    REAL(R4)    :: ALTOBS     ! Observer Altitude [km]
    REAL(R4)    :: PSIOBS     ! Observer horizontal angle [deg]
!
END MODULE OBSCOM_DAT
