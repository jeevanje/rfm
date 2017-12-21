MODULE SPCCTM_SUB
CONTAINS
SUBROUTINE SPCCTM 
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
! 
! DESCRIPTION    
!   Calculate continuum absorption
!   Called by RFMSPC if CTM flag enabled.
!   Calculates continuum at wide mesh points.
!   Continuum data available for absorbers H2O, O2, N2, CO2.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE CLCCOM_DAT ! Calculated path segments
    USE GASCOM_DAT ! Molecule and isotope data
    USE IDXCON_DAT ! RFM/HITRAN indices for specific molecules
!
! SUBROUTINES
    USE CTMCKD_SUB ! CKD H2O continuum
    USE CTMCO2_SUB ! CO2 continuum
    USE CTMH2O_SUB ! H2O continuum
    USE CTMN2_SUB  ! N2 continuum
    USE CTMO2_SUB  ! O2 continuum
!
  IMPLICIT NONE
!
! LOCAL CONSTANTS
    LOGICAL, PARAMETER :: USECKD = .FALSE. ! F=use (new) MT_CKD H2O ctm, 
!                                            T=use old CKD
! LOCAL VARIABLES
    INTEGER(I4) :: IGAS   ! Gas counter
    INTEGER(I4) :: ICLC   ! Gas/segment Path counter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  DO ICLC = 1, NCLC
    IGAS = CLC(ICLC)%IGS
    IF ( GAS(IGAS)%CTM ) THEN
      SELECT CASE ( GAS(IGAS)%IDM )
      CASE ( IDXH2O ) 
        IF ( USECKD ) THEN 
          CALL CTMCKD ( ICLC )
        ELSE
          CALL CTMH2O ( ICLC )
        END IF
      CASE ( IDXCO2 ) 
        CALL CTMCO2 ( ICLC )
      CASE ( IDXO2 ) 
        CALL CTMO2 ( ICLC )
      CASE ( IDXN2 ) 
        CALL CTMN2 ( ICLC )
      END SELECT
    END IF
  END DO
!
END SUBROUTINE SPCCTM
END MODULE SPCCTM_SUB

