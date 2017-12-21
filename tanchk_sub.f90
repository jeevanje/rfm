MODULE TANCHK_SUB
CONTAINS
SUBROUTINE TANCHK ( TANSTR, FAIL, ERRMSG )
!
! VERSION
!   01MAY17 AD F90 conversion. Tested.
!
! DESCRIPTION
!   Check if string is valid Tangent Height
!   Called by DRVTAN for each field in *TAN section.
!   Note that this just loads reqd output tangent heights, more may be added
!   by FOVTAN (up to MTAN) for the FOV convolution.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE TANCOM_DAT ! Tangent heights
    USE FLGCOM_DAT ! Option flags
!
! SUBROUTINES
    USE ATMLEV_SUB ! Find/insert atmospheric level for given altitude.
    USE CHKFLX_SUB ! Check output levels for Flux calculations
    USE CHKHOM_SUB ! Check path length for HOMogeneous path calculation
    USE CHKNAD_SUB ! Check specification of Nadir/Zenith viewing paths
    USE HGTSTR_FNC ! Convert altitude to C*5 string
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: TANSTR ! Tangent string to be tested 
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: IDXATM ! Index of atm level matching tangent point
    INTEGER(I4)       :: IOS    ! Saved value of IOSTAT
    INTEGER(I4)       :: ITAN   ! Counter for listed tangent heights
    REAL(R4)          :: TANTST ! Tangent height read from TANSTR
    CHARACTER(LENTAN) :: STRTST ! Tan.value written as a string 
    TYPE(TANTYP), ALLOCATABLE :: TANSAV(:) ! Saved TAN during reallocation
!
! EXECUTABLE CODE --------------------------------------------------------------
!
! To be identified as a real number, the string TANSTR must be readable without
! an error
  READ ( TANSTR, *, IOSTAT=IOS ) TANTST 
  IF ( IOS .NE. 0 ) THEN
    ERRMSG = 'F-TANCHK: Unreadable value in *TAN section: ' // TANSTR
    FAIL = .TRUE.
    RETURN
  ELSE IF ( ABS ( TANTST ) .GE. 99999.5 ) THEN
    ERRMSG = 'F-TANCHK: Cannot handle values .GE. 99999.5'
    FAIL = .TRUE.
    RETURN
  END IF
!
  LIMTAN = .FALSE.
  IF ( HOMFLG ) THEN
    CALL CHKHOM ( TANTST, FAIL, ERRMSG ) 
    IF ( FAIL ) RETURN
  ELSE IF ( FLXFLG ) THEN
    CALL CHKFLX ( TANTST, FAIL, ERRMSG )    
    IF ( FAIL ) RETURN
  ELSE IF ( NADFLG .OR. ZENFLG ) THEN
    CALL CHKNAD ( TANTST, USRELE, FAIL, ERRMSG )
    IF ( FAIL ) RETURN
  ELSE
    LIMTAN = .TRUE.
  END IF
!
! Convert value to string that will form part of output filenames and check
! that this is unique
  STRTST = HGTSTR ( TANTST ) 
!
! Check that this is distinguishable from other USRTAN values, 
  IF ( ALLOCATED ( TAN ) ) THEN
    IF ( ANY ( TAN%STR .EQ. STRTST ) ) THEN
      FAIL = .TRUE.
      ERRMSG = 'F-TANCHK: Repeated Tan.Hgt=' // TANSTR
      RETURN
    END IF
    ITAN = COUNT ( TAN%USR .LT. TANTST ) + 1  ! insertion index
    CALL MOVE_ALLOC ( TAN, TANSAV ) 
  ELSE
    ITAN = 1
  END IF
  NTAN = NTAN + 1
  ALLOCATE ( TAN(NTAN) ) 
  TAN(ITAN)%USR = TANTST
  TAN(ITAN)%STR = STRTST
  TAN(ITAN)%CLC = .TRUE.
  TAN(ITAN)%IAT = 1    ! reset in CHKLIM for limb paths
  TAN(ITAN)%HGT = 0.0
  TAN(ITAN)%JDX = 0
  TAN(ITAN)%SFC = NADFLG .OR. ( HOMFLG .AND. SFCFLG ) ! reset in CHKLIM
  IF ( ALLOCATED ( TANSAV ) ) THEN
    TAN(1:ITAN-1) = TANSAV(1:ITAN-1)
    TAN(ITAN+1:NTAN) = TANSAV(ITAN:)
  END IF
!
! For flux calculations 'tan' represents atmospheric level for output
  IF ( FLXFLG ) THEN
    CALL ATMLEV ( TANTST, IDXATM )
    TAN(ITAN)%IAT = IDXATM
    TAN(ITAN)%HGT = TANTST
  END IF    
!
  MTAN = NTAN
!
END SUBROUTINE TANCHK
END MODULE TANCHK_SUB

