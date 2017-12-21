MODULE SPCLOS_SUB
CONTAINS
SUBROUTINE SPCLOS 
!
! VERSION
!   01MAY17 AD F90 version. Checked.
!
! DESCRIPTION
!   Calculate LOS Jacobians
!   Called once by RFMSPC if LOS flag enabled.
!
! VARIABLE KINDS
    USE KIND_DAT
    USE PTBCON_DAT, ONLY: PTBLOS ! LOS perturbation [km]
!
! GLOBAL DATA
    USE FULCOM_DAT ! Full grid data    
    USE JACCOM_DAT ! Jacobian data
    USE TANCOM_DAT ! Tangent path data
!
  IMPLICIT NONE
!
! LOCAL VARIABLES
    INTEGER(I4) :: I1,I2,I3 ! Indices of tan.paths used for interpolation
    INTEGER(I4) :: ITAN   ! Counter for output tangent paths
    INTEGER(I4) :: JTAN   ! Index of tan.pth containing FOV contribution
    REAL(R8)    :: W      ! Weight for linear interpolation
    REAL(R8)    :: WGT(3) ! Weights for quadratic interpolation 
    REAL(R8)    :: Z21,Z32,Z31 ! Differences between tangent heights
    INTEGER(I4),  ALLOCATABLE :: ITJSAV(:,:)  ! Saved ITNJAC during reallocation
    REAL(R8),     ALLOCATABLE :: OPTSAV(:,:)  ! Saved OPTFUL during reallocation
    REAL(R8),     ALLOCATABLE :: RADSAV(:,:)  ! Saved RADFUL during reallocation
    REAL(R8),     ALLOCATABLE :: TRASAV(:,:)  ! Saved TRAFUL during reallocation
    TYPE(JACTYP), ALLOCATABLE :: JACSAV(:)    ! Saved JAC during reallocation
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Set up extra 'Jacobian' element for 'los' perturbation
  IF ( ALLOCATED ( JAC ) ) CALL MOVE_ALLOC ( JAC, JACSAV ) 
  NJAC = NJAC + 1
  ALLOCATE ( JAC(NJAC) ) 
  IF ( ALLOCATED ( JACSAV ) ) JAC(1:NJAC-1) = JACSAV
  JAC(NJAC)%COD = 'los'
  JAC(NJAC)%COL = .TRUE.
!
! Add NTAN extra paths for LOS Jacobians
  CALL MOVE_ALLOC ( OPTFUL, OPTSAV ) 
  CALL MOVE_ALLOC ( RADFUL, RADSAV ) 
  CALL MOVE_ALLOC ( TRAFUL, TRASAV ) 
  ALLOCATE ( OPTFUL(NFUL,MTAN+NTAN) )  
  ALLOCATE ( RADFUL(NFUL,MTAN+NTAN) )  
  ALLOCATE ( TRAFUL(NFUL,MTAN+NTAN) ) 
  OPTFUL(:,1:MTAN) = OPTSAV
  RADFUL(:,1:MTAN) = RADSAV
  TRAFUL(:,1:MTAN) = TRASAV
!
  SELECT CASE ( NTAN ) 
  CASE ( 1 )                                ! NTAN=1 not allowed for LOS flag
    STOP 'F-SPCLOS: Logical error'
  CASE ( 2 ) 
    DO ITAN = 1, NTAN
      JTAN = MTAN + ITAN
      I1 = MAX ( ITAN - 1, 1 ) 
      I3 = MIN ( ITAN + 1, NTAN ) 
      W = PTBLOS / ( TAN(I3)%USR - TAN(I1)%USR ) 
      OPTFUL(:,JTAN) = ( OPTFUL(:,I3) - OPTFUL(:,I1) ) * W
      RADFUL(:,JTAN) = ( RADFUL(:,I3) - RADFUL(:,I1) ) * W
      TRAFUL(:,JTAN) = ( TRAFUL(:,I3) - TRAFUL(:,I1) ) * W
    END DO
  CASE ( 3: ) 
    DO ITAN = 1, NTAN
      JTAN = MTAN + ITAN
      I1 = ITAN-1
      I2 = ITAN
      I3 = ITAN+1
      IF ( ITAN .EQ. 1 ) I1 = 3
      IF ( ITAN .EQ. NTAN ) I3 = NTAN-2
      Z21 = ( TAN(I2)%USR - TAN(I1)%USR ) / PTBLOS
      Z32 = ( TAN(I3)%USR - TAN(I2)%USR ) / PTBLOS
      Z31 = Z32 + Z21
      WGT(1) = -Z32 / Z21 / Z31 
      WGT(2) = (Z32-Z21) / Z21 / Z32
      WGT(3) = Z21 / Z31 / Z32
      OPTFUL(:,JTAN) = MATMUL ( OPTFUL(:,I1:I3),  WGT )
      RADFUL(:,JTAN) = MATMUL ( RADFUL(:,I1:I3),  WGT )
      TRAFUL(:,JTAN) = MATMUL ( TRAFUL(:,I1:I3),  WGT )
    END DO
  END SELECT
!
  IF ( NJAC .GT. 1 ) CALL MOVE_ALLOC ( ITNJAC, ITJSAV )
  ALLOCATE ( ITNJAC(NTAN,NJAC) ) 
  IF ( ALLOCATED ( ITJSAV ) ) ITNJAC(:,1:NJAC-1) = ITJSAV
  DO ITAN = 1, NTAN
    ITNJAC(ITAN,NJAC) = MTAN + ITAN
  END DO 
!
  MTAN = MTAN + NTAN
!
END SUBROUTINE SPCLOS
END MODULE SPCLOS_SUB
