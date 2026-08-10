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
use modmet_constants, only: RK, pid2, EPS, PI

    implicit none (type, external)
    private
    public :: modmet_fpsim, modmet_fpsim_holtslag
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
         real(RK), intent(in) :: eta
         !! stability parameter z/L [-]
      real(RK) :: fpsim_result
      real(RK) :: x

      if (eta < 0.0_RK) then
         x = sqrt(sqrt(1.0_RK - 16.0_RK * eta))
         fpsim_result = log((1.0_RK + x)**2 * (1.0_RK + x**2) / 8.0_RK) - 2.0_RK * atan(x) + pid2
      else
         if (eta > 200.0_RK) then
            fpsim_result = -0.7_RK * eta - 10.72_RK
         else
            fpsim_result = -0.7_RK * eta - (0.75_RK * eta - 10.72_RK) *&
                exp(-0.35_RK * eta) - 10.72_RK
         end if
      end if
   end function modmet_fpsim

   !! Computes the momentum stability correction function psi_m using the Holtslag 1984 formulation
   pure function modmet_fpsim_holtslag(z, ol) result(fpsim_result)
      real(RK), intent(in) :: z, ol
      real(RK) :: fpsim_result
      real(RK) :: eta, y

      eta = z / ol

      if (ol > (0.0_RK + EPS)) then
         fpsim_result = -17.0_RK * (1.0_RK - exp(-0.29_RK * eta))
      else
         ! v Ulden and Holtslag
         y = (1.0_RK - 15.0_RK * eta)**0.25_RK
         fpsim_result = 2.0_RK * log((1.0_RK + y) / 2.0_RK) + log((1.0_RK + y * y) / 2.0_RK) - &
            (2.0_RK * atan(y)) + (PI / 2.0_RK)  ! 2.4 OPS report
      end if

   end function modmet_fpsim_holtslag
end module m_modmet_fpsim
