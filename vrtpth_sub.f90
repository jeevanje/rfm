MODULE VRTPTH_SUB
CONTAINS
SUBROUTINE VRTPTH 
!
! VERSION
!   01MAY17 AD F90 conversion of nadpth.for. Checked.
!
! DESCRIPTION
!   Determine path segments along vertical path
!   Called by RFMPTH, JACPTH for plane-parallel geometry
!   A path segment is defined for each combination of absorber and atmos layer.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE FLGCOM_DAT ! Option flags
    USE PTHCOM_DAT ! Path segment data
    USE TANCOM_DAT ! Tangent path data
    USE OBSCOM_DAT, ONLY: IATOBS ! Observer location data
    USE PHYCON_DAT, ONLY: DG2RAD ! [rad]/[deg] conversion factor
!
! SUBROUTINES
    USE ADDCLC_SUB ! Set calculated paths
    USE VRTSUM_SUB ! Curtis-Godson integrals for vertical path segment
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IATM   ! Atmospheric layer counter
    INTEGER(I4) :: IATM1  ! Start for atmospheric layer integration
    INTEGER(I4) :: IPTH   ! Path segment coutner
    INTEGER(I4) :: ITAN   ! Tangent path counter
    INTEGER(I4) :: IVMR   ! Gas counter
    REAL(R4)    :: SECANG ! Sec(zenith angle)
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Calculate no of paths required
  IF ( ZENFLG .AND. OBSFLG ) THEN
    NPTH = MTAN * NVMR * ( NATM - IATOBS ) 
    IATM1 = IATOBS
  ELSE
    NPTH = MTAN * NVMR * ( NATM - 1 )
    IATM1 = 1
  END IF
!
  ALLOCATE ( PTH(NPTH) )
!
  IPTH = 0
!
  DO ITAN = 1, MTAN
    SECANG = TAN(ITAN)%USR   
    DO IATM = IATM1, NATM-1
      DO IVMR = 1, NVMR
        IPTH = IPTH + 1
        PTH(IPTH)%IGS = IVMR
        PTH(IPTH)%ITN = ITAN
        PTH(IPTH)%IAT = IATM
        PTH(IPTH)%NTE = NTEVMR(IVMR)
        CALL VRTSUM ( IVMR, IATM, PTH(IPTH)%TEM, PTH(IPTH)%PRE, PTH(IPTH)%PPA, &
                      PTH(IPTH)%AMT, PTH(IPTH)%RAY )
        PTH(IPTH)%AMT = PTH(IPTH)%AMT * SECANG  ! * sec(zenith angle)
! For non-GRA cases, PSI contains zenith angle of ray
        PTH(IPTH)%PSI = ACOS ( 1.0 / SECANG ) / DG2RAD
        IF ( ZENFLG ) PTH(IPTH)%PSI = 180.0 - PTH(IPTH)%PSI
        CALL ADDCLC ( PTH(IPTH) ) 
      END DO
    END DO
  END DO
!
END SUBROUTINE VRTPTH
END MODULE VRTPTH_SUB
