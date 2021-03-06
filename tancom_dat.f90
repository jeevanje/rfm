MODULE TANCOM_DAT
!
! VERSION
!   30MAY18 AD Make TAN a TARGET
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   List of tangent heights required for RFM calculations.
!   Loaded by DRVTAN, FOVTAN.
!   SFCTAN set in CHKLIM
!   Output tangent heights are 1:NTAN (1=lowest, NTAN=highest) limb view
!   but not sorted for NAD, ZEN or HOM views.
!   Extra tangent heights for FOV calculation are in NTAN+1:MTAN
!   More may be added for Jacobian calculations
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
! GLOBAL CONSTANTS
    INTEGER, PARAMETER :: LENTAN = 6 ! Max length of tan.ht string in filenames
!
  TYPE :: TANTYP
    LOGICAL     :: CLC ! T = Radiance.calc reqd for Tan.Hgt
    LOGICAL     :: SFC ! T = limb ray intersects surface
    INTEGER(I4) :: IAT ! Atmospheric layer containing tangent pt 
    INTEGER(I4) :: ITN ! Index of original tan (or 0 if this is original).
    INTEGER(I4) :: JDX ! Target species for Jacobian perturbations, or 0
    REAL(R4)    :: ELE ! Elevation angles of paths [deg]
    REAL(R4)    :: GEO ! Projected (=Geom) tan.alt. [km]
    REAL(R4)    :: HGT ! Lowest altitude along paths [km] 
    REAL(R4)    :: PSI ! LOS angle of tan.pt. [deg] (with GRA flag)
    REAL(R4)    :: USR ! Tan.path values from Drv.Table
    REAL(R8)    :: SZN ! Sin(Zenith angle) at 'tangent point'/obs/surface
    CHARACTER(LENTAN) :: STR ! Strings for filenames
  END TYPE TANTYP
!
! GLOBAL VARIABLES
    TYPE(TANTYP), TARGET, ALLOCATABLE :: TAN(:) 
!
    LOGICAL     :: USRELE = .FALSE. ! T=USRTAN are elevation angles
    LOGICAL     :: USRGEO = .FALSE. ! T=USRTAN are Geom.Tan.points.
    LOGICAL     :: LIMTAN ! T=limb-viewing geometry, F=plane or homog.path
    INTEGER(I4) :: MTAN   ! No. ray paths reqd for calculation
    INTEGER(I4) :: NTAN   ! No. nominal tan pts for output
!
END MODULE TANCOM_DAT

