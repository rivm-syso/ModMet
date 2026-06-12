!------------------------------------------------------------------------------
! Module:     m_modmet_obuk
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original OBUK routine)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module computes Obukhov length from friction velocity and
!   temperature scale.
!------------------------------------------------------------------------------
module m_modmet_obuk
    use modmet_constants, only: GRAVITY, VONK, TR, EPS
   implicit none (type, external)
   private
   public :: modmet_obuk
contains
pure function modmet_obuk(ust, tst) result(ol)
   ! ===========================================================
   ! Function: modmet_obuk
   ! Description: Computes Obukhov length and returns a large-magnitude
   !              fallback when temperature scale is near zero.
   ! input: ust - friction velocity [m/s]
   ! input: tst - temperature scale
   ! output: ol - Obukhov length [m]
   ! ===========================================================
      real, intent(in) :: ust, tst
      real :: ol


      if (abs(tst) < EPS) then
         ol = -1e5 ! effectively infinite Obukhov length
         return
      end if

      ol = (TR*ust**2) / (GRAVITY * tst * VONK)
   end function modmet_obuk
end module m_modmet_obuk
