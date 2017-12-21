MODULE HGTSTR_FNC
CONTAINS
CHARACTER(5) PURE FUNCTION HGTSTR ( HGT ) 
!
! VERSION
!   01MAY17 AD F90 orginal. Checked.
!
! DESCRIPTION
!   Convert altitude to C*5 string
!   General purpose module.
!   Used for constructing altitude component of RFM output filenames.
!   This converts any altitude in range -99:999km to metres, altitudes outside
!   this range are kept as km.
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
!
! ARGUMENTS
    REAL(R4), INTENT(IN) :: HGT ! Altitude [km]
!
! EXECUTABLE CODE --------------------------------------------------------------
!
  IF ( NINT ( HGT * 1000.0 ) .LE. 99999 .AND. &
       NINT ( HGT * 1000.0 ) .GE. -9999          ) THEN
    WRITE ( HGTSTR, '(I5.5)' ) NINT ( HGT * 1000.0 ) 
  ELSE
    WRITE ( HGTSTR, '(I5.5)' ) NINT ( HGT ) 
  END IF
!
END FUNCTION HGTSTR
END MODULE HGTSTR_FNC

