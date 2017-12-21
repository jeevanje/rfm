MODULE SPCFLX_SUB
CONTAINS
SUBROUTINE SPCFLX 
!
! VERSION
!   01JUN17 AD F90 conversion of rfmflx.for. Checked.
!
! DESCRIPTION
!   Spectral flux calculation
!   Called by RFMSPC if FLX flag enabled.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE FLGCOM_DAT ! Option flags
    USE FULCOM_DAT ! Full grid data
    USE QADCOM_DAT ! Gaussian quadrature data
    USE FINCOM_DAT, ONLY: NFIN, WNOFIN ! Finemesh data
!
! SUBROUTINES
    USE FLXLAY_SUB ! Radiative flux calculation through a layer
    USE FLXSFC_SUB ! Surface radiance flux
    USE FLXSPA_SUB ! Space radiance flux
    USE RADMTX_SUB ! Calculate radiance matrix
    USE TRAMTX_SUB ! Calculate transmittance matrix
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IATM   ! Index of atmospheric level
    INTEGER(I4) :: IQAD   ! Counter for quadrature points
    INTEGER(I4) :: ITAN   ! Counter for output levels
    INTEGER(I4) :: IWGT   ! Counter for cooling rate weights
    REAL(R8)    :: OPT(NFIN)        ! Cumulative optical path
    REAL(R8)    :: OPTLAY(NFIN)     ! Optical depth of single atm layer
    REAL(R8)    :: RQAD(NFIN,NQAD)  ! Radiances for quadrature paths
    REAL(R8)    :: TQAD(NFIN,NQAD)  ! Transmittances for quadrature paths
!       
! EXECUTABLE CODE -------------------------------------------------------------
!
! Initialise space radiance
  CALL FLXSPA ( WNOFIN, RQAD ) 
  OPT = 0.0D0
!
! Downward path 
  DO IATM = NATM, 1, -1                ! Loop over levels
    CALL FLXLAY ( IATM, -1, .FALSE., XQAD, RQAD, OPTLAY )
    IF ( NADFLG ) CYCLE                ! nadflg=only include upward components
    OPT = OPT + OPTLAY
    ITAN = ITNATM(IATM)
    IF ( ITAN .GT. 0 ) THEN
! Negative radiance for downward path
      RADFUL(IFUL1:IFUL2,ITAN) = RADFUL(IFUL1:IFUL2,ITAN) - MATMUL(RQAD,WQAD) 
      OPTFUL(IFUL1:IFUL2,ITAN) = OPT
      DO IQAD = 1, NQAD
        TQAD(:,IQAD) = EXP ( - OPT / XQAD(IQAD) ) * RPIQAD
      END DO
      TRAFUL(IFUL1:IFUL2,ITAN) = MATMUL ( TQAD, WQAD ) 
    END IF
    IF ( COOFLG ) THEN
      DO IWGT = 1, NCOATM(IATM)
        ITAN = ICOATM(IWGT,IATM) 
        COOFUL(IFUL1:IFUL2,ITAN) = COOFUL(IFUL1:IFUL2,ITAN) &
                                   - MATMUL(RQAD,WQAD) * WCOATM(IWGT,IATM)
      END DO
    END IF
  END DO
!
  IF ( ZENFLG ) RETURN   ! only consider downwelling radiances
!
! Surface contribution to reflected radiance
  CALL FLXSFC ( WNOFIN, WQAD, RQAD ) 
!
! Upward path
  OPT = 0.0D0
  DO IATM = 1, NATM
    ITAN = ITNATM(IATM)
    IF ( ITAN .GT. 0 ) THEN              
! Positive radiance for upward path
      RADFUL(IFUL1:IFUL2,ITAN) = RADFUL(IFUL1:IFUL2,ITAN) + MATMUL(RQAD,WQAD) 
      OPTFUL(IFUL1:IFUL2,ITAN) = OPT
      DO IQAD = 1, NQAD
        TQAD(:,IQAD) = EXP ( - OPT / XQAD(IQAD) ) * RPIQAD
      END DO
      TRAFUL(IFUL1:IFUL2,ITAN) = MATMUL ( TQAD, WQAD ) 
    END IF
    IF ( COOFLG ) THEN
      DO IWGT = 1, NCOATM(IATM)
        ITAN = ICOATM(IWGT,IATM) 
        COOFUL(IFUL1:IFUL2,ITAN) = COOFUL(IFUL1:IFUL2,ITAN) &
                                   + MATMUL(RQAD,WQAD) * WCOATM(IWGT,IATM)
      END DO
    END IF
    CALL FLXLAY ( IATM, 1, .FALSE., XQAD, RQAD, OPTLAY ) 
    OPT = OPT + OPTLAY
  END DO
!
  IF ( MTXFLG ) THEN
    IF ( ABSFLG .OR. TRAFLG ) CALL TRAMTX
    IF ( RADFLG .OR. COOFLG ) CALL RADMTX
  END IF
!
END SUBROUTINE SPCFLX
END MODULE SPCFLX_SUB
