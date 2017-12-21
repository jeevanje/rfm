MODULE SPCOUT_SUB
CONTAINS
SUBROUTINE SPCOUT ( ISPC, FAIL, ERRMSG )
!
! VERSION
!   01JUN17 AD F90 original. Checked.
!
! DESCRIPTION
!   Write spectral output data
!   Called by RFMSPC
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE FLGCOM_DAT ! Option flags
    USE FULCOM_DAT ! Full grid data
    USE JACCOM_DAT ! Jacobian data
    USE LEVCOM_DAT ! Intermediate output levels
    USE NAMCOM_DAT ! RFM output filenames
    USE PHYCON_DAT, ONLY: C1,C2  ! Radiation constants
    USE TANCOM_DAT, ONLY: NTAN   ! No. of tangent paths for output
!
! SUBROUTINES
    USE BRIGHT_FNC ! Brightness Temperature calculation
    USE SPCWRT_SUB ! Write spectral data file
    USE WRTSTT_SUB ! Write widemesh statistics
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: ISPC   ! Spectral range number
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error occurs
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    LOGICAL, PARAMETER :: NOZERO = .FALSE. ! T=don't output zero Jacobian spectra
!
! LOCAL VARIABLES
    INTEGER(I4) :: IJAC ! Index of Jacobian
    INTEGER(I4) :: ILEV ! Index of intermediate output level
    INTEGER(I4) :: ISEC ! Counter for secondary output spectra
    INTEGER(I4) :: ITAN ! Counter for output tangent heights
    INTEGER(I4) :: JTAN ! Counter for tangent heights incl. Jacobian spectra
    INTEGER(I4) :: NSEC ! No. secondary output spectra per nominal spectrum
    REAL(R8), TARGET, ALLOCATABLE :: SPCFUL(:) ! Derived spectral outputs
    REAL(R8), POINTER             :: SPCPTR(:) ! Pointer for output spectrum
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  NULLIFY ( SPCPTR ) 
  ALLOCATE ( SPCFUL(NFUL) )
!
  IF ( NJAC .GT. 0 ) THEN 
    NSEC = NJAC
  ELSE IF ( NLEV .GT. 0 ) THEN
    NSEC = NLEV
  ELSE
    NSEC = 0
  END IF
!
  DO ITAN = 1, NTAN    
    DO ISEC = 0, NSEC     ! 0 = nominal spectra, 1:NSEC secondary spectra
      IF ( ISEC .EQ. 0 ) THEN        ! nominal spectrum
        IJAC = 0
        ILEV = 0
        JTAN = ITAN
      ELSE                           ! secondary spectrum
        IF ( NJAC .GT. 0 ) THEN
          IJAC = ISEC
          JTAN = ITNJAC(ITAN,IJAC)
        ELSE
          ILEV = ISEC
          JTAN = ITNLEV(ITAN,ILEV)
        END IF
        IF ( JTAN .EQ. 0 .AND. NOZERO ) CYCLE  ! No sec. output for this tan
      END IF
!
      IF ( ABSFLG ) THEN
        IF ( JTAN .EQ. 0 ) THEN
          SPCFUL = 0.0D0
        ELSE
          SPCFUL = 1.0D0 - TRAFUL(:,JTAN)
        END IF
        SPCPTR => SPCFUL
        CALL SPCWRT ( NAMABS, 'ABS', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( BBTFLG ) THEN
        IF ( IJAC .GT. 0 ) THEN
          IF ( JTAN .EQ. 0 ) THEN
            SPCFUL = 0.0D0
          ELSE
            SPCFUL = BRIGHT ( RADFUL(:,JTAN) + RADFUL(:,ITAN), WNOFUL ) - &
                     BRIGHT ( RADFUL(:,ITAN), WNOFUL )
          END IF
        ELSE
          SPCFUL = BRIGHT ( RADFUL(:,JTAN), WNOFUL ) 
        END IF
        SPCPTR => SPCFUL
        CALL SPCWRT ( NAMBBT, 'BBT', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( COOFLG ) THEN
        SPCPTR => COOFUL(:,JTAN)
        CALL SPCWRT ( NAMCOO, 'COO', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( OPTFLG ) THEN
        IF ( JTAN .EQ. 0 ) THEN
          SPCFUL = 0.0D0
          SPCPTR => SPCFUL
        ELSE
          SPCPTR => OPTFUL(:,JTAN)
        END IF
        CALL SPCWRT ( NAMOPT, 'OPT', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( RADFLG ) THEN
! For radiance flux, convert nW/cm2 to W/m2 
        IF ( JTAN .EQ. 0 ) THEN
          SPCFUL = 0.0
          SPCPTR => SPCFUL
        ELSE IF ( FLXFLG .AND. .NOT. VRTFLG ) THEN
          SPCFUL = RADFUL(:,JTAN) * 1.0E-5 
          SPCPTR => SPCFUL
        ELSE
          SPCPTR => RADFUL(:,JTAN)
        END IF
        CALL SPCWRT ( NAMRAD, 'RAD', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( RJTFLG ) THEN
        IF ( JTAN .EQ. 0 ) THEN
          SPCFUL = 0.0
        ELSE
          SPCFUL = C2 * RADFUL(:,JTAN) / C1 / WNOFUL**2
        END IF
        SPCPTR => SPCFUL
        CALL SPCWRT ( NAMRJT, 'RJT', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
      IF ( TRAFLG ) THEN
        IF ( JTAN .EQ. 0 ) THEN
          SPCFUL = 0
          SPCPTR => SPCFUL
        ELSE
          SPCPTR => TRAFUL(:,JTAN)
        END IF
        CALL SPCWRT ( NAMTRA, 'TRA', ISPC, ITAN, IJAC, ILEV, NFUL, IRRFUL, &
                      WNOFUL, SPCPTR, FAIL, ERRMSG ) 
        IF ( FAIL ) RETURN
      END IF
!
    END DO
  END DO
!
  IF ( WIDFLG ) CALL WRTSTT ( ISPC, FAIL, ERRMSG )
!
  NULLIFY ( SPCPTR ) 
!
END SUBROUTINE SPCOUT
END MODULE SPCOUT_SUB

