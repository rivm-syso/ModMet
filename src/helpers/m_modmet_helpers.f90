!------------------------------------------------------------------------------
! Module:     m_rc_gw
! Authors:    Marte Voorneveld, RIVM
! Created:    June 10 2026
! Updated:    June 11 2026
! Description:
!   This module provides helper functions for the ModMet library.
!------------------------------------------------------------------------------
module m_modmet_helpers
   implicit none (type, external)
   private
   public :: modmet_missing

   !! This module defines utility functions to check for missing values in real and integer data.
   !! -999 and -9999 are treated as missing values for both real and integer types,
   !!with a small tolerance for floating-point comparisons.
   interface modmet_missing
      module procedure missing_real, missing_int
   end interface
contains
   pure logical function missing_real(x)
      real, intent(in) :: x
      !! the real value to check
      real, parameter :: EPS = 1.0e-5
      missing_real = (abs(x + 999.) <= EPS .or. abs(x + 9999.) <= EPS)
   end function missing_real

   pure logical function missing_int(x)
      integer, intent(in) :: x
      !! the integer value to check
      missing_int = (x == -999) .or. (x == -9999)
   end function missing_int
end module m_modmet_helpers
