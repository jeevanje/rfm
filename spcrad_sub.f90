MODULE SPCRAD_SUB
CONTAINS
SUBROUTINE SPCRAD 
!
! VERSION
!   01JUL17 AD F90 version. Checked.
!
! DESCRIPTION
!   Radiative transfer calculation
!   Called by RFMSPC
!   Calculation starts at remote side of atmosphere and moves towards observer
!   (NB reverse direction to RFM v4)
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FINCOM_DAT ! Finemesh data
    USE FLGCOM_DAT ! Option flags
    USE FULCOM_DAT ! Full grid data
    USE TANCOM_DAT ! Tangent path data
    USE ATMCOM_DAT, ONLY: NATM   ! No. atmos.profile levels (1+No.layers)
    USE OBSCOM_DAT, ONLY: IATOBS ! Atmos.profile level of observert
    USE PHYADJ_DAT, ONLY: TEMSPA ! Cosmic background temperature
    USE SFCCOM_DAT, ONLY: RFLSFC ! T=reflecting surface
!
! SUBROUTINES
    USE PLANCK_FNC ! Planck Function
    USE RADLAY_SUB ! Radiative transfer calculation through a layer
    USE RADLEV_SUB ! Save radiances at intermediate output levels
    USE RADSFC_SUB ! Surface contribution to radiative transfer calculation
!
  IMPLICIT NONE
!
! LOCAL CONSTANTS
    INTEGER, PARAMETER :: NQAD = 4 ! No.pts for Gaussian quadrature
!
! LOCAL VARIABLES
    INTEGER(I4) :: IATM         ! Index of atmospheric level
    INTEGER(I4) :: ILOW         ! Lower atm.level for calculation
    INTEGER(I4) :: ITAN         ! Counter for tangent paths
    INTEGER(I4) :: IUPP         ! Upper atm.level for calculation
    REAL(R8)    :: BBFSPA(NFIN) ! Blackbody space emitted radiance
    REAL(R8)    :: OPTLAY(NFIN) ! Optical thickness of atmos layer
    REAL(R8), POINTER :: OPT(:) ! Optical depth
    REAL(R8), POINTER :: RAD(:) ! Radiance
    REAL(R8), POINTER :: TRA(:) ! Path transmittance
!       
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( TEMSPA .GT. TINY(0.0) ) THEN
    BBFSPA = PLANCK ( TEMSPA, WNOFIN )
  ELSE
    BBFSPA = 0.0D0
  END IF
!
  DO ITAN = 1, MTAN
    IF ( .NOT. TAN(ITAN)%CLC ) CYCLE
!
    OPT => OPTFUL(IFUL1:IFUL2,ITAN) ; OPT = 0.0D0
    TRA => TRAFUL(IFUL1:IFUL2,ITAN) ; TRA = 1.0D0
    RAD => RADFUL(IFUL1:IFUL2,ITAN) ; RAD = BBFSPA
!
! Downward path from space into atmosphere lowest point
    ILOW = TAN(ITAN)%IAT
!
! Skip downward surface paths if black surface and no intermediate o/p reqd
    IF ( TAN(ITAN)%SFC .AND. .NOT. RFLSFC .AND. .NOT. LEVFLG ) ILOW = NATM
    IUPP = MAX ( NATM-1, 1 )           ! Max to allow for NATM=1 for HOM flag
!  
    IF ( LEVFLG ) CALL RADLEV ( ITAN, NATM, -1 ) 
!
    DO IATM = IUPP, ILOW, -1   ! Loop over layers
      CALL RADLAY ( ITAN, TAN(ITAN)%ITN, IATM, -1, RAD, OPTLAY )  
      OPT = OPT + OPTLAY
      TRA = EXP ( -OPT )
      IF ( LEVFLG ) CALL RADLEV ( ITAN, IATM, -1 ) 
    END DO
!
! If zenith view, no further calculation required
    IF ( ZENFLG ) CYCLE
    IF ( HOMFLG .AND. .NOT. SFCFLG ) CYCLE   ! single pass for HOM flag
!
! Surface contribution
    IF ( TAN(ITAN)%SFC ) THEN
      CALL RADSFC ( OPT, RAD, TAN(ITAN)%JDX ) 
      TRA = EXP ( -OPT ) 
    END IF
!
! Upward path from lowest point to observer
    ILOW = TAN(ITAN)%IAT
    IF ( OBSFLG ) THEN
      IUPP = IATOBS - 1
    ELSE 
      IUPP = MAX ( NATM-1, 1 )     ! max to allow for NATM=1 for HOM flag
    END IF
!
    DO IATM = ILOW, IUPP
      IF ( LEVFLG ) CALL RADLEV ( ITAN, IATM, 1 ) 
      CALL RADLAY ( ITAN, TAN(ITAN)%ITN, IATM, 1, RAD, OPTLAY )  
      OPT = OPT + OPTLAY
      TRA = EXP ( -OPT )
    END DO      
    IF ( LEVFLG ) CALL RADLEV ( ITAN, IATM, 1 ) 
!
  END DO
!
  NULLIFY ( OPT, RAD, TRA ) 
!
END SUBROUTINE SPCRAD
END MODULE SPCRAD_SUB

