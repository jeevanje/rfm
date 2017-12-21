MODULE TANFLX_SUB
CONTAINS
SUBROUTINE TANFLX 
!
! VERSION
!   01JUN17 AD F90 original. Checked.
!
! DESCRIPTION
!   Initialise spectral flux calculation
!   Called by DRVTAN if FLX flag enabled.
!   Assumes that atmospheric profile now fixed in size.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE QADCOM_DAT ! Gaussian quadrature data
    USE TANCOM_DAT ! Tangent path data
    USE ATMCOM_DAT, ONLY: NATM, ITNATM   ! Indices of Flux ouput levels (or 0)
    USE FLGCOM_DAT, ONLY: COOFLG, VRTFLG ! Option flags
    USE PHYCON_DAT, ONLY: PI
!
! SUBROUTINES
    USE COOWGT_SUB ! Calculate cooling rate weights
    USE GAUQAD_SUB ! Values and Weights for Gaussian First Moment Quadrature
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IATM ! Index of atmospheric level
    INTEGER(I4) :: ITAN ! Counter for output levels
!       
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( VRTFLG ) THEN    
    NQAD = 1            ! alter default value of NQAD in QADCOM_DAT
    RPIQAD = 1.0D0
    ALLOCATE ( XQAD(1), WQAD(1) ) 
    XQAD(1) = 1.0D0
    WQAD(1) = 1.0D0
  ELSE                  ! use default value of NQAD in QADCOM_DAT
    RPIQAD = 1.0D0 / PI
    ALLOCATE ( XQAD(NQAD), WQAD(NQAD) ) 
    CALL GAUQAD ( NQAD, XQAD, WQAD ) 
    WQAD = WQAD * 2 * PI
  END IF
!
  ALLOCATE ( ITNATM(NATM) )
  ITNATM = 0
  DO ITAN = 1, NTAN 
    IATM = TAN(ITAN)%IAT
    ITNATM(IATM) = ITAN
  END DO
!
  IF ( COOFLG ) CALL COOWGT 
!
END SUBROUTINE TANFLX
END MODULE TANFLX_SUB
