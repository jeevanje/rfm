MODULE CHKFOV_SUB
CONTAINS
SUBROUTINE CHKFOV ( FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion of fovtan.for. Checked.
!
! DESCRIPTION
!   Check FOV tangent heights
!   Called once by DRVCHK if FOV flag is enabled. 
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE FOVCOM_DAT ! Field of View data
    USE TANCOM_DAT ! Tangent path data
!
! SUBROUTINES
    USE ADDTAN_SUB ! Add tangent ray path
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE CHKLIM_SUB ! Check limb-viewing tangent paths
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE 
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: TOLFAC = 0.01 ! Frac. tol. allowed on FOV spacing
!
! LOCAL VARIABLES
    LOGICAL       :: GEOFOV  ! T=Treat FOV as Geom.Tangent Heights.
    INTEGER(I4)   :: IFOV    ! Counter for FOV points
    INTEGER(I4)   :: ITAN    ! Counter for output tangent heights
    INTEGER(I4)   :: JTAN    ! Counter for all tangent heights
    REAL(R4)      :: FOVDIF  ! Min difference wrt tabulated tan paths
    REAL(R4)      :: TANFOV  ! Value of tangent path + FOV point
    CHARACTER(80) :: MESSGE  ! Text message for Log file
    REAL(R4), ALLOCATABLE :: TOLFOV(:) ! Hgt Tol for matching FOV convol pts
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Set up tangent height tolerances for FOV convolution
! NB NFOV must be at least 3 to accomodate two zeros plus one non-zero value.
  ALLOCATE ( TOLFOV(NFOV) )
  IF ( CLCFLG ) THEN         ! If CLC flag, only use exact matches
    TOLFOV = 0.0
  ELSE                       ! Otherwise use TOLFOV * spacing
    DO IFOV = 2, NFOV-1
      TOLFOV(IFOV) = TOLFAC * &
           MIN ( ABS ( FOV(IFOV+1)%ALT - FOV(IFOV)%ALT ), &
                 ABS ( FOV(IFOV)%ALT - FOV(IFOV-1)%ALT )  )  
    END DO
    TOLFOV(1)    = TOLFOV(2)
    TOLFOV(NFOV) = TOLFOV(NFOV-1)
  END IF
!
! Switch off calculation for set of nominal tangent paths - may be switched on
! again if path required for FOV convolution
  TAN(1:NTAN)%CLC = .FALSE.
!
! For each output tangent height (1:NTAN) check if any already listed will do
! for FOV convolution, otherwise add to list (NTAN+1:MTAN)
  GEOFOV = .NOT. ( ELEFOV .OR. FVZFLG )
  ALLOCATE ( ITNFOV(NTAN,NFOV) )
  DO ITAN = 1, NTAN
    DO IFOV = 1, NFOV
      IF ( ELEFOV ) THEN          ! FOV expressed as elevation angles ...
        TANFOV = TAN(ITAN)%ELE + FOV(IFOV)%ALT
        FOVDIF = MINVAL ( ABS ( TAN%ELE - TANFOV ) ) 
          JTAN = MINLOC ( ABS ( TAN%ELE - TANFOV ), 1 ) 
      ELSE IF ( GEOFOV ) THEN     ! FOV expressed as geom.tan.hts
        TANFOV = TAN(ITAN)%GEO + FOV(IFOV)%ALT
        FOVDIF = MINVAL ( ABS ( TAN%GEO - TANFOV ) )
          JTAN = MINLOC ( ABS ( TAN%GEO - TANFOV ), 1 ) 
      ELSE                        ! FOV expressed as ref.tan.hts (for OFM)
        TANFOV = TAN(ITAN)%HGT + FOV(IFOV)%ALT 
        FOVDIF = MINVAL ( ABS ( TAN%HGT - TANFOV ) )
          JTAN = MINLOC ( ABS ( TAN%HGT - TANFOV ), 1 ) 
      END IF
      IF ( FOVDIF .LE. TOLFOV(IFOV) ) THEN ! re-use existing tangent path
        TAN(JTAN)%CLC = .TRUE.
        ITNFOV(ITAN,IFOV) = JTAN
      ELSE                                 ! add new tangent path
        CALL ADDTAN ( 0, .TRUE. ) 
        TAN(MTAN)%USR = TANFOV
        ITNFOV(ITAN,IFOV) = MTAN
        CALL CHKLIM ( MTAN, ELEFOV, GEOFOV, FAIL, ERRMSG )
        IF ( FAIL ) RETURN
      END IF
    END DO
  END DO
!
  MESSGE = 'I-CHKFOV: No.extra tangent paths required for FOV Convolution=' &
           // C11INT ( MTAN - NTAN )
  CALL WRTLOG ( MESSGE )
!
END SUBROUTINE CHKFOV
END MODULE CHKFOV_SUB

