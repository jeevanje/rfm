MODULE SPCFOV_SUB
CONTAINS
SUBROUTINE SPCFOV 
!
! VERSION
!   01MAY17 AD F90 version. Checked.
!
! DESCRIPTION
!   Convolve spectra with FOV
!   Called by RFMSPC for each spectral range if FOV flag enabled.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FOVCOM_DAT ! Field of View data
    USE FULCOM_DAT ! Full grid data    
    USE JACCOM_DAT, ONLY: NJAC, ITNJAC  ! No.Jacobians, Jacobians tan.paths
    USE TANCOM_DAT, ONLY: MTAN, NTAN    ! No. Tot, nominal tangent paths
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: IFOV   ! Counter for FOV tabulation points
    INTEGER(I4) :: IJAC   ! Counter for Jacobian elements
    INTEGER(I4) :: ITAN   ! Counter for output tangent paths
    INTEGER(I4) :: JTAN   ! Index of tan.pth containing FOV contribution
    INTEGER(I4) :: KTAN   ! Counter for final tangent ray path
    REAL(R8), ALLOCATABLE :: RADFOV(:,:) ! Convolved radiances
    REAL(R8), ALLOCATABLE :: TRAFOV(:,:) ! Convolved transmittances
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Redefine MTAN to be nominal outputs * (1 + njac) (NJAC=0 unless JACFLG)
  MTAN = NTAN * ( 1 + NJAC ) 
  ALLOCATE ( RADFOV(NFUL,MTAN), TRAFOV(NFUL,MTAN) )
  RADFOV = 0.0D0
  TRAFOV = 0.0D0
!
  KTAN = 0
  DO IJAC = 0, NJAC    ! ijac=0 used for standard, non-Jacobian radiances
    DO ITAN = 1, NTAN
      KTAN = KTAN + 1
      DO IFOV = 1, NFOV
        JTAN = ITNFOV(ITAN,IFOV)
        IF ( IJAC .GT. 0 ) THEN
          JTAN = ITNJAC(JTAN,IJAC)  
          IF ( JTAN .EQ. 0 ) CYCLE
        END IF
        RADFOV(:,KTAN) = RADFOV(:,KTAN) + FOV(IFOV)%FNC * RADFUL(:,JTAN) 
        TRAFOV(:,KTAN) = TRAFOV(:,KTAN) + FOV(IFOV)%FNC * TRAFUL(:,JTAN) 
      END DO
    END DO
  END DO
!
! Reassign ITNJAC indices to reflect new structure for convolved paths
  IF ( NJAC .GT. 0 ) THEN
    DEALLOCATE ( ITNJAC ) 
    ALLOCATE ( ITNJAC(NTAN,NJAC) ) 
    KTAN = NTAN
    DO IJAC = 1, NJAC
      DO ITAN = 1, NTAN
        KTAN = KTAN + 1
        ITNJAC(ITAN,IJAC) = KTAN
      END DO
    END DO
  END IF
!
  CALL MOVE_ALLOC ( RADFOV, RADFUL ) 
  CALL MOVE_ALLOC ( TRAFOV, TRAFUL ) 
!
END SUBROUTINE SPCFOV
END MODULE SPCFOV_SUB

