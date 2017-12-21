MODULE LIMPTH_SUB
CONTAINS
SUBROUTINE LIMPTH 
!
! VERSION
!   01JUL17 AD F90 conversion of tanpth.for. Checked.
!
! DESCRIPTION
!   Set Limb-viewing paths
!   Called by RFMPTH, JACPTH
!   A path segment is defined for each combination of absorber and atmos layer.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE PTHCOM_DAT ! Path segment data
    USE TANCOM_DAT ! Tangent path data
    USE FLGCOM_DAT, ONLY: GRAFLG ! T = use horizontal gradients
    USE PHYCON_DAT, ONLY: DG2RAD ! [rad]/[deg] conversion factor
!
! SUBROUTINES
    USE GRASUM_SUB ! Construct ray paths through 2D atmosphere
    USE RAYSUM_SUB ! CG integrals in ray path segment
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IATM ! Atmospheric layer counter
    INTEGER(I4) :: IDIR ! Direction pointer for path 
    INTEGER(I4) :: IDIR1 ! Initial value of IDIR (-1 for GRAFLG, 1 otherwise)
    INTEGER(I4) :: IPTH ! Path segment coutner
    INTEGER(I4) :: ITAN ! Tangent path counter
    INTEGER(I4) :: IVMR ! Gas counter
    REAL(R4)    :: LEN  ! Path length [km]
    REAL(R4)    :: PBOT ! Horiz.angle [deg] at base of layer
    REAL(R4)    :: PTOP ! Horiz.angle [deg] at top of layer
    REAL(R4)    :: TBOT ! Zenith angle [deg] at base of layer
    REAL(R4)    :: TTOP ! Zenith angle [deg] at top of layer
    REAL(R4)    :: ZBOT ! Altitude [km] at base of layer
    REAL(R4)    :: ZTOP ! Altitude [km] at top of layer
    REAL(R8)    :: DSNBOT ! Sine of zenith angle at base of layer
    REAL(R8)    :: DSNTOP ! Sine of zenith angle at top of layer
    REAL(R4), ALLOCATABLE :: AMT(:) ! Absorber amounts [kmol/cm2]
    REAL(R4), ALLOCATABLE :: ECG(:) ! CG Pressures [atm]
    REAL(R4), ALLOCATABLE :: PCG(:) ! CG Part.Pres [atm]
    REAL(R4), ALLOCATABLE :: PSU(:) ! Absorber-weighted horiz.angle [deg]
    REAL(R4), ALLOCATABLE :: TCG(:) ! CG Temperatures [K]
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Calculate no of paths required
  USEDIR = GRAFLG
  NPTH = 0
  DO ITAN = 1, MTAN           
    IF ( TAN(ITAN)%CLC ) NPTH = NPTH + NVMR * ( NATM - TAN(ITAN)%IAT ) 
  END DO 
!   
  IF ( GRAFLG ) THEN
    NPTH = 2 * NPTH
    IDIR1 = -1
  ELSE
    IDIR1 = 1
  END IF
!
  ALLOCATE ( PTH(NPTH) )
!
  IPTH = 0
!
  ALLOCATE ( AMT(NVMR), ECG(NVMR), PCG(NVMR), TCG(NVMR), PSU(NVMR) )
  DO ITAN = 1, MTAN
    IF ( .NOT. TAN(ITAN)%CLC ) CYCLE
    DO IDIR = IDIR1, 1, 2
      IF ( GRAFLG ) THEN
! RFM ray paths from space towards observer
! If IDIR=+1, ray from t.p. towards observer, so T is 0:-90 (a/c from zen)
! If IDIR=-1, ray from t.p. away from observer, so T is 0:90 (c/w from zen)
        TTOP = + SIGN ( SNGL ( ASIN ( TAN(ITAN)%SZN ) ) / DG2RAD, FLOAT(IDIR) )
        PTOP = TAN(ITAN)%PSI
      ELSE
        DSNTOP = TAN(ITAN)%SZN
      END IF
      ZTOP = TAN(ITAN)%HGT 
      DO IATM = TAN(ITAN)%IAT, NATM-1
        ZBOT = ZTOP
        ZTOP = HGTATM(IATM+1) 
        IF ( GRAFLG ) THEN
          PBOT = PTOP
          TBOT = TTOP
          CALL GRASUM ( NVMR, TAN(ITAN)%HGT, ZBOT, PBOT, TBOT, ZTOP, &
                        PTOP, TTOP, TCG, PCG, ECG, AMT, LEN, PSU ) 
        ELSE
          DSNBOT = DSNTOP
          CALL RAYSUM ( NVMR, ZBOT, DSNBOT, ZTOP, DSNTOP, &
                        TCG, PCG, ECG, AMT, LEN )
        END IF
        DO IVMR = 1, NVMR
          IPTH = IPTH + 1
          PTH(IPTH)%IGS = IVMR
          PTH(IPTH)%ITN = ITAN
          PTH(IPTH)%IAT = IATM
          PTH(IPTH)%ICL = 0
          PTH(IPTH)%IDR = IDIR
          PTH(IPTH)%NTE = NTEVMR(IVMR)
          PTH(IPTH)%TEM = TCG(IVMR)
          PTH(IPTH)%PRE = PCG(IVMR)
          PTH(IPTH)%PPA = ECG(IVMR)
          PTH(IPTH)%AMT = AMT(IVMR)
          PTH(IPTH)%RAY = LEN
          IF ( GRAFLG ) THEN
            IF ( IDIR .EQ. -1 ) THEN
              PTH(IPTH)%PSI = PTOP ! Near=upper boundary for down path
            ELSE
              PTH(IPTH)%PSI = PBOT ! Near=lower boundary for up path
            END IF
            PTH(IPTH)%PSU = PSU(IVMR)
          ELSE
            PTH(IPTH)%PSI = SNGL ( ASIN ( DSNTOP ) ) / DG2RAD
          END IF
        END DO
      END DO
    END DO
  END DO
!
END SUBROUTINE LIMPTH
END MODULE LIMPTH_SUB
