MODULE IDXPTH_FNC
CONTAINS
INTEGER(I4) PURE FUNCTION IDXPTH ( ITAN, IATM, IGAS, IDIR )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Index in PTHCOM of tan/atm/gas/dir
!   General purpose module
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE PTHCOM_DAT ! Path segment data
!
  IMPLICIT NONE 
!
! ARGUMENTS
    INTEGER(I4), INTENT(IN) :: ITAN ! Tangent Ray#
    INTEGER(I4), INTENT(IN) :: IATM ! Atmospheric Layer#
    INTEGER(I4), INTENT(IN) :: IGAS ! Absorber#
    INTEGER(I4), OPTIONAL, &
                 INTENT(IN) :: IDIR ! Direction (ignored unless USEDIR is TRUE)
! LOCAL VARIABLES
    INTEGER(I4) :: IPTH ! Counter
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( PRESENT ( IDIR ) .AND. USEDIR ) THEN
    DO IPTH = 1, NPTH
      IF ( PTH(IPTH)%ITN .EQ. ITAN .AND. &
           PTH(IPTH)%IAT .EQ. IATM .AND. &
           PTH(IPTH)%IGS .EQ. IGAS .AND. &
           PTH(IPTH)%IDR .EQ. IDIR         ) THEN
        IDXPTH = IPTH
        RETURN                  ! Exit with path found
      END IF
    END DO
  ELSE
    DO IPTH = 1, NPTH
      IF ( PTH(IPTH)%ITN .EQ. ITAN .AND. &
           PTH(IPTH)%IAT .EQ. IATM .AND. &
           PTH(IPTH)%IGS .EQ. IGAS         ) THEN
        IDXPTH = IPTH
        RETURN                  ! Exit with path found
      END IF
    END DO
  END IF
!
! Exit with path not found
  IDXPTH = 0 
!
END FUNCTION IDXPTH
END MODULE IDXPTH_FNC
