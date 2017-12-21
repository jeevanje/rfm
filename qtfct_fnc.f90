MODULE QTFCT_FNC
CONTAINS
REAL(R4) FUNCTION QTFCT ( IDGAS, ISO, TEM )
!
! VERSION
!   01MAY17 AD F90 conversion, originally a subroutine. Checked.
!
! DESCRIPTION
!   Calculate total internal partition sum
!   Called by ADJUST, NTECLC, QTNTE.
!   The ratio of total internal partition sums, Q(296K)/Q(Path Temp) is 
!   calculated for a given molecule and isotopic species. The line strength 
!   can then be adjusted by this factor.
!
!   Based on program TIPS_2003.FOR by R.R.GAMACHE
!   The original version used 4th order polynomials to span entire 
!   temperature ranges, however the 2003 version uses local 4th order 
!   Lagrangian interpolation which should be more accurate and spans a 
!   greater temperature range overall
!     
!   In general, Lagrangian interpolation of y(x) is given by
!       y(x) = sum_ijkl  yi(x-xj)(x-xk)(x-xl)/[(xi-xj)(xi-xk)(xi-xl)]
!   where ijkl are the four different points used for interpolation.
!
!   In this code (as in the Gamache version) these points are chosen such 
!   that x lies between j and k, except at the extreme ends of the tabulated
!   temperature where the points used are the edge points of the tabulation.
!   In this code it it assumed that the xi are equally spaced (25K intervals)
!   (although this is not checked) which allows for significant simplification
!   compared to the more general Gamache subroutine "AtoB"
!
! VARIABLE KINDS
    USE KIND_DAT 
!
! GLOBAL DATA
    USE TPSDAT_DAT ! TIPS Temp coefficient data
!
! SUBROUTINES
    USE QTWARN_SUB ! Warning messages from QTFCT
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4), INTENT(IN) :: IDGAS ! HITRAN gas ID
    INTEGER(I4), INTENT(IN) :: ISO   ! HITRAN isotope ID 
    REAL(R4),    INTENT(IN) :: TEM   ! Path temperature [K]
!
! LOCAL CONSTANTS
    REAL(R4), PARAMETER :: DTI3 = 1/DTEM**3    ! 6.4E-5 = (1/25)^3
!
! LOCAL VARIABLES
    INTEGER(I4) :: IT,JT,KT,LT  ! Indices of 4 consecutive tabulation points
    INTEGER(I4) :: ISPE         ! Counter for isotope species (all gases)
    REAL(R4)    :: DTI,DTJ,DTK,DTL ! Position of TEM relative to tab. points
    REAL(R4)    :: QT           ! Interpolated TIPS value at T
    REAL(R4)    :: SQ           ! Local value of QTFCT
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IF ( IDGAS .GE. MAXIDG .OR. &        ! Assume extra molecule
       IDGAS .EQ. 39     .OR. &        ! no data yet for CH3OH
       IDGAS .EQ. 47          ) THEN   ! no data yet for SO3
    SQ = ( TEMREF / TEM )**1.5
    CALL QTWARN ( IDGAS, -1, TEM )   ! -1 flags this as no TIPS for molec
  ELSE
    ISPE = ISPOFF(IDGAS) + ISO     ! Position in array of molecular isotope
! Check for isotopes not included in TIPS data
! Note that ISPOFF array extends to max IDGAS plus 1 to allow for this check
    IF ( ISPE .GT. ISPOFF(IDGAS+1) ) THEN
      CALL QTWARN ( IDGAS, ISO, TEM ) 
      ISPE = ISPOFF(IDGAS) + 1           ! Just use main isotope value instead 
    END IF 
!
! Find indices of 4 consecutive points for interpolation, IT,JT,KT,LT, aiming
! to bracket TEM between JT LE TEM LT KT, but ensuring that the 4 chosen points
! remain within the tabulation if TEM is near the edges
    JT = INT ( ( TEM - TEM1 ) / DTEM ) + 1
    IF ( JT .LT. 2 ) THEN 
      IF ( TEM .LT. TEM1 ) CALL QTWARN ( IDGAS, ISO, TEM, TEM1=TEM1 )
      JT = 2
    ELSE IF ( JT .GT. NT-2 ) THEN
      IF ( TEM .GT. TEM2 ) CALL QTWARN ( IDGAS, ISO, TEM, TEM2=TEM2 )
      JT = NT - 2
    END IF
    IT = JT - 1
    KT = JT + 1
    LT = JT + 2
!
    DTI = TEM - TEMVAL(IT)
    DTJ = TEM - TEMVAL(JT)
    DTK = TEM - TEMVAL(KT)
    DTL = TEM - TEMVAL(LT)
!
    QT = DTI3 * ( -1.0/6.0*DTJ*DTK*DTL * QTEM(IT,ISPE)  &
                  +1.0/2.0*DTI*DTK*DTL * QTEM(JT,ISPE)  &
                  -1.0/2.0*DTI*DTJ*DTL * QTEM(KT,ISPE)  &
                  +1.0/6.0*DTI*DTJ*DTK * QTEM(LT,ISPE)    )
!
    SQ = 1.00 / QT
  END IF
  QTFCT = SQ
!
END FUNCTION QTFCT
END MODULE QTFCT_FNC
