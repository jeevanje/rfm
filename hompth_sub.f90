MODULE HOMPTH_SUB
CONTAINS
SUBROUTINE HOMPTH 
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Set Homogeneous paths
!   Called by RFMPTH if HOM flag enabled.
!   Called by JACPTH if HOM+JAC flags enabled.
!   A path is defined for each combination of absorber and atmos layer.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
    USE PTHCOM_DAT ! Path segment data
    USE TANCOM_DAT ! Tangent path data
    USE PHYCON_DAT, ONLY: ATMB, AVOG ! mb/atm, Avogadro's number
!
! SUBROUTINES
    USE ADDCLC_SUB ! Set calculated paths
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER :: IPTH ! Path segment counter
    INTEGER :: ITAN ! Tangent path counter
    INTEGER :: IVMR ! Absorber counter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  NPTH = NVMR * NTAN 
  ALLOCATE ( PTH(NPTH) )
!
  IPTH = 0
!
  DO ITAN = 1, NTAN
    DO IVMR = 1, NVMR
      IPTH = IPTH + 1
      PTH(IPTH)%IGS = IVMR
      PTH(IPTH)%ITN = ITAN
      PTH(IPTH)%NTE = NTEVMR(IVMR)
      PTH(IPTH)%IAT = 1
      PTH(IPTH)%TEM = TEMATM(1)
      PTH(IPTH)%PRE = PREATM(1) / ATMB       ! mb to atm
      PTH(IPTH)%PPA = VMRATM(1,IVMR) * 1.0E-6 * PREATM(1) / ATMB ! ppmv to ppv
      PTH(IPTH)%RAY = TAN(ITAN)%USR
      PTH(IPTH)%AMT = 0.1 / AVOG * VMRATM(1,IVMR) * DNSATM(1) * TAN(ITAN)%USR
      PTH(IPTH)%PSI = 0.0
      CALL ADDCLC ( PTH(IPTH) ) 
      TAN(ITAN)%IAT = 1
    END DO
  END DO
!
END SUBROUTINE HOMPTH
END MODULE HOMPTH_SUB

