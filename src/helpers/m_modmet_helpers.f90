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
   interface modmet_missing
      module procedure missing_real, missing_int
   end interface
contains
   pure logical function missing_real(x)
   ! ===========================================================
   ! Function: missing_real
   ! Description: Checks if a real value is considered missing based on specific values.
   !              Returns .true. if the value is -999 or -9999, otherwise .false.
   ! input: x - the real value to check
   ! output: logical - .true. if x is considered missing, .false. otherwise
   ! ===========================================================
      real, intent(in) :: x
      real, parameter :: EPS = 1.0e-5
      missing_real = (abs(x + 999.) <= EPS .or. abs(x + 9999.) <= EPS)
   end function missing_real

   pure logical function missing_int(x)
   ! ===========================================================
   ! Function: missing_int
   ! Description: Checks if an integer value is considered missing based on specific values.
   !              Returns .true. if the value is -999 or -9999, otherwise .false.
   ! input: x - the integer value to check
   ! output: logical - .true. if x is considered missing, .false. otherwise
   ! ===========================================================
      integer, intent(in) :: x
      missing_int = (x == -999) .or. (x == -9999)
   end function missing_int
end module m_modmet_helpers
