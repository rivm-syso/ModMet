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
    use modmet_constants, only: RK, GRAVITY, VONK, TR, EPS
   implicit none (type, external)
   private
   public :: modmet_obuk
contains
   ! ===========================================================
   ! Function: modmet_obuk
   ! Description: Computes Obukhov length and returns a large-magnitude
   !              fallback when temperature scale is near zero.
   ! input: ust - friction velocity [m/s]
   ! input: tst - temperature scale
   ! output: ol - Obukhov length [m]
   ! ===========================================================
   !! Computes Obukhov length from friction velocity and temperature scale.
   !!   Reference: OBUK routine from KNMI legacy implementation.
pure function modmet_obuk(ust, tst) result(ol)
   real(RK), intent(in) :: ust
   !! friction velocity [m/s]
   real(RK), intent(in) :: tst
   !! temperature scale
      real(RK) :: ol


      if (abs(tst) < EPS) then
         ol = -1e5_RK ! effectively infinite Obukhov length
         return
      end if

      ol = (TR*ust**2) / (GRAVITY * tst * VONK)
   end function modmet_obuk
end module m_modmet_obuk
