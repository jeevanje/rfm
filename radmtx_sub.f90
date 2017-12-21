MODULE RADMTX_SUB
CONTAINS
SUBROUTINE RADMTX 
!
! VERSION
!   01JUN17 AD F90 conversion of rfmflx.for. Checked.
!
! DESCRIPTION
!   Calculate radiance matrix
!   Called by SPCFLX if MTX and (RAD or COO) flags enabled.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE FULCOM_DAT ! Full grid data
    USE LEVCOM_DAT ! Intermediate output levels
    USE QADCOM_DAT ! Gaussian quadrature data
    USE ATMCOM_DAT ! Atmospheric profile data
    USE FINCOM_DAT, ONLY: NFIN, WNOFIN ! Finemesh grid
    USE TANCOM_DAT, ONLY: NTAN         ! No. nominal output levels
!
! SUBROUTINES
    USE FLXLAY_SUB ! Radiative flux calculation through a layer
    USE FLXSFC_SUB ! Surface radiance flux
    USE FLXSPA_SUB ! Space radiance flux
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    LOGICAL     :: PTBSRC ! T=perturbed source function
    INTEGER(I4) :: IATM   ! Index of atmospheric level
    INTEGER(I4) :: ILEV   ! Counter for output levels
    INTEGER(I4) :: ITAN   ! Counter for output levels
    INTEGER(I4) :: IWGT   ! Counter for cooling rate weights
    INTEGER(I4) :: JTAN   ! Index of flux matrix elements
    REAL(R8)    :: OPT(NFIN)       ! Cumulative optical path
    REAL(R8)    :: OPTLAY(NFIN)    ! Optical depth of single atm layer
    REAL(R8)    :: RQAD(NFIN,NQAD) ! Radiances for quadrature paths
!       
! EXECUTABLE CODE -------------------------------------------------------------
!
  PTBSRC = .FALSE.
  DO ILEV = 1, NLEV         ! Perturbed source fn levels
    CALL FLXSPA ( WNOFIN, RQAD )    ! Initialise with space radiance
    OPT = 0.0D0
!
! Downward path 
    DO IATM = NATM, 1, -1               ! Loop over atmospheric profile levels
      PTBSRC = IATM .EQ. LEV(ILEV)%IAT
      CALL FLXLAY ( IATM, -1, PTBSRC, XQAD, RQAD, OPTLAY )
      IF ( NADFLG ) CYCLE               ! nadflg=only include upward components
      ITAN = ITNATM(IATM) 
      IF ( ITAN .GT. 0 ) THEN           ! output level
        JTAN = ITNLEV(ITAN,ILEV)
! Negative radiance for downward path
        RADFUL(IFUL1:IFUL2,JTAN) = RADFUL(IFUL1:IFUL2,JTAN) - MATMUL(RQAD,WQAD) 
      END IF
      IF ( COOFLG ) THEN
        DO IWGT = 1, NCOATM(IATM)     ! NCOATM = 0 if not wgt for output level
          ITAN = ICOATM(IWGT,IATM) 
          JTAN = ITNLEV(ITAN,ILEV)
          COOFUL(IFUL1:IFUL2,JTAN) = COOFUL(IFUL1:IFUL2,JTAN) &
                                     - MATMUL(RQAD,WQAD) * WCOATM(IWGT,IATM)
        END DO
      END IF
    END DO
!
    IF ( ZENFLG ) CYCLE   ! only consider downwelling radiances
!
! Incorporate surface contribution to bottom-of-atmosphere reflected radiances
    CALL FLXSFC ( WNOFIN, WQAD, RQAD ) 
!
! Upward path
    OPT = 0.0D0
    DO IATM = 1, NATM
      IF ( ILEV .GT. 0 ) PTBSRC = IATM .EQ. LEV(ILEV)%IAT
      ITAN = ITNATM(IATM)
      IF ( ITAN .GT. 0 ) THEN          ! output level
        JTAN = ITNLEV(ITAN,ILEV) 
! Positive radiance for upward path
        RADFUL(IFUL1:IFUL2,JTAN) = RADFUL(IFUL1:IFUL2,JTAN) + MATMUL(RQAD,WQAD) 
      END IF
      IF ( COOFLG ) THEN
        DO IWGT = 1, NCOATM(IATM)
          ITAN = ICOATM(IWGT,IATM) 
          JTAN = ITNLEV(ITAN,ILEV)
          COOFUL(IFUL1:IFUL2,JTAN) = COOFUL(IFUL1:IFUL2,JTAN) &
                                     + MATMUL(RQAD,WQAD) * WCOATM(IWGT,IATM)
        END DO
      END IF
      PTBSRC = IATM .EQ. LEV(ILEV)%IAT
      CALL FLXLAY ( IATM, 1, PTBSRC, XQAD, RQAD, OPTLAY ) 
    END DO
  END DO
!
! Subtract unperturbed path values (stored 1:NTAN)
  DO ITAN = 1, NTAN
    DO ILEV = 1, NLEV
      JTAN = ITNLEV(ITAN,ILEV)
      RADFUL(IFUL1:IFUL2,JTAN) = RADFUL(IFUL1:IFUL2,JTAN) - &
                                 RADFUL(IFUL1:IFUL2,ITAN) 
      IF ( COOFLG ) COOFUL(IFUL1:IFUL2,JTAN) = COOFUL(IFUL1:IFUL2,JTAN) - &
                                               COOFUL(IFUL1:IFUL2,ITAN) 
    END DO
  END DO
!
END SUBROUTINE RADMTX
END MODULE RADMTX_SUB
