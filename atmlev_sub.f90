MODULE ATMLEV_SUB
CONTAINS
SUBROUTINE ATMLEV ( HGT, IDXATM )
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   Find/insert atmospheric level for given altitude
!   General purpose module.
!   If the altitude is above the top of the atmosphere or below the base of
!     the atmosphere a fatal error message results
!   If the altitude exactly matches an existing atmospheric level, the index
!     IATM of that level is returned. 
!   If the altitude matches an existing atmospheric level within a given 
!     tolerance (set by TOLLEV) the altitude is adjusted to that level and 
!     the index returned.
!   If the altitude does not match, a new level is inserted and its index is
!     returned
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE ATMCOM_DAT ! Atmospheric profile data
!
! SUBROUTINES
    USE ADDATM_SUB ! Add extra level to atm profiles in ATMCOM
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE C9REAL_GEN ! Write real number as C*9 string
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    REAL(R4),    INTENT(INOUT) :: HGT    ! Altitude [km] - may be adjusted
    INTEGER(I4), INTENT(OUT)   :: IDXATM ! Index of new atmospheric level
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: TOLLEV = 0.001 ! Fraction of layer thickness which 
!                                 can be ignored if HGT close to layer boundary
! LOCAL VARIABLES
    REAL(R4)      :: ALPHA  ! Profile interpolation fraction (0:1)
    REAL(R4)      :: DELHGT ! Difference between HGT and nearest profile level
    CHARACTER(80) :: WRNMSG ! Message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Argument HGT should be checked before this subroutine is called, so this 
! error message should never arise
  IF ( HGT .GT. HGTTOA .OR. HGT .LT. HGTSFC ) STOP 'F-ATMLEV: Logical error'
!
  IDXATM = MINLOC ( ABS ( HGTATM - HGT ), 1 ) 
  DELHGT = MINVAL ( ABS ( HGTATM - HGT ) ) 
  IF ( DELHGT .EQ. 0.0 ) RETURN              ! Normal exit with exact match
!
  IF ( IDXATM .EQ. 1 ) THEN 
    ALPHA = DELHGT / ( HGTATM(2) - HGTATM(1) )
  ELSE IF ( IDXATM .EQ. NATM ) THEN
    ALPHA = DELHGT / ( HGTATM(NATM) - HGTATM(NATM-1) ) 
  ELSE
    ALPHA = 2.0 * DELHGT / ( HGTATM(IDXATM+1) - HGTATM(IDXATM-1) )
  END IF
!
  IF ( ALPHA .LE. TOLLEV ) THEN  ! Adjust alt to match profile level
    WRNMSG = 'W-ATMLEV: Change altitude from ' // C9REAL(HGT) // ' km to' // & 
             C9REAL(HGTATM(IDXATM)) // ' km to match profile.'
    CALL WRTLOG ( WRNMSG )
    HGT = HGTATM(IDXATM)
    RETURN         ! normal exit with adjusted alt to match existing level
  END IF
!
! From this point onwards an extra level has to be inserted into profiles
  CALL ADDATM ( HGT, .TRUE., IDXATM )     ! T=new level specified by height
  WRNMSG = 'W-ATMLEV: Inserting extra profile level#' // C11INT(IDXATM)
  CALL WRTLOG ( WRNMSG )
!
! Normal exit with extra level inserted
!
END SUBROUTINE ATMLEV
END MODULE ATMLEV_SUB
