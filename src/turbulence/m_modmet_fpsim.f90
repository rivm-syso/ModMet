!------------------------------------------------------------------------------
! Module:     m_modmet_fpsim
! Authors:    Marte Voorneveld, RIVM,
!             Anton Beljaars, KNMI
!             Franka Loeve, Cap Volmac (historical implementation)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module computes the momentum stability correction function
!   (psi_m) for unstable and stable atmospheric conditions.
!   References: Holtslag and De Bruin (1987); Hicks (1976).
!------------------------------------------------------------------------------
module m_modmet_fpsim
use modmet_constants, only: pid2

    implicit none (type, external)
    private
    public :: modmet_fpsim
contains
   ! ===========================================================
   ! Function: modmet_fpsim
   ! Description: Computes the momentum stability correction
   !              function using Beljaars and Holtslag (1991).
   ! input: eta - stability parameter z/L [-]
   ! output: fpsim_result - momentum stability correction [-]
   ! ===========================================================
      !! Computes the momentum stability correction function psi_m.
      !!   Reference: Beljaars and Holtslag (1991); Hicks (1976).
      pure function modmet_fpsim(eta) result(fpsim_result)
         real, intent(in) :: eta
         !! stability parameter z/L [-]
      real :: fpsim_result
      real :: x

      if (eta < 0.0) then
         x = sqrt(sqrt(1.0 - 16.0 * eta))
         fpsim_result = log((1.0 + x)**2 * (1.0 + x**2) / 8.0) - 2.0 * atan(x) + pid2
      else
         if (eta > 200.0) then
            fpsim_result = -0.7 * eta - 10.72
         else
            fpsim_result = -0.7 * eta - (0.75 * eta - 10.72) * exp(-0.35 * eta) - 10.72
         end if
      end if
   end function modmet_fpsim
end module m_modmet_fpsim
