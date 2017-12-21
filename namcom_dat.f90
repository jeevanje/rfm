MODULE NAMCOM_DAT
!
! VERSION
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION
!   RFM output filenames
!   The following are the default filename templates where the '*' is replaced
!   by spectral range, path etc data to distinguish each file.
!   These templates can be changed by inserting appropriate sections into the
!   RFM driver table (*ABS, *BBT etc).
!   The '.asc' component will be changed to '.bin' if binary output files are
!   selected (BIN flag).
!
! VARIABLE KINDS
    USE KIND_DAT
!
  IMPLICIT NONE
  SAVE
!
! GLOBAL CONSTANTS
    INTEGER(I4), PARAMETER :: LENNAM = 200 ! Max length of any output filename 
!
! GLOBAL VARIABLES
    CHARACTER(LENNAM), TARGET :: NAMABS = 'abs_*.asc' ! ABS filename template
    CHARACTER(LENNAM), TARGET :: NAMBBT = 'bbt_*.asc' ! BBT filename template
    CHARACTER(LENNAM), TARGET :: NAMCOO = 'coo_*.asc' ! COO filename template
    CHARACTER(LENNAM), TARGET :: NAMOPT = 'opt_*.asc' ! OPT filename template
    CHARACTER(LENNAM), TARGET :: NAMPRF = 'prf*.asc'  ! PRF filename template
    CHARACTER(LENNAM), TARGET :: NAMPTH = 'pth_*.asc' ! PTH filename template
    CHARACTER(LENNAM), TARGET :: NAMRAD = 'rad_*.asc' ! RAD filename template
    CHARACTER(LENNAM), TARGET :: NAMRJT = 'rjt_*.asc' ! RJT filename template
    CHARACTER(LENNAM), TARGET :: NAMTAB = 'tab_*.asc' ! TAB filename template
    CHARACTER(LENNAM), TARGET :: NAMTRA = 'tra_*.asc' ! TRA filename template
    CHARACTER(LENNAM), TARGET :: NAMWID = 'wid_*.asc' ! WID filename template
!
END MODULE NAMCOM_DAT
