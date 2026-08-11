!------------------------------------------------------------------------------
! Module:     m_modmet_flxln2
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original FLXLN2 routine)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module computes friction velocity and Obukhov length using
!   Monin-Obukhov similarity and an internal root-finding step.
!   References:
!   - Van Ulden and Holtslag (1985), JCAM 24, 1196-1207.
!   - Beljaars, Holtslag, and Van Westrhenen (1989), KNMI TR-112.
!------------------------------------------------------------------------------
module m_modmet_flxln2
   use modmet_constants, only: RK, VONK, TR, RO, CP_AIR, LAMBDA

   use m_modmet_radiat, only: modmet_radiat, modmet_radiat_result
   use m_modmet_find_zero, only: modmet_find_zero, modmet_solver_result
   use m_modmet_tst, only: modmet_tst, modmet_tst_result
   use m_modmet_obuk, only: modmet_obuk
   use m_modmet_fpsim, only: modmet_fpsim

   use m_modmet_helpers, only: modmet_missing
   implicit none (type, external)


   type :: modmet_flxln2_result
      real(RK) :: ust
      real(RK) :: ol
      real(RK) :: kin
      real(RK) :: tau
      real(RK) :: h
      real(RK) :: le
      real(RK) :: tst
      real(RK) :: qst
   end type modmet_flxln2_result



   private
   public :: modmet_flxln2, modmet_flxln2_result
contains
   ! ===========================================================
   ! Function: modmet_flxln2
   ! Description: Solves for friction velocity and returns associated
   !              Obukhov length and shortwave radiation used.
   ! input: u1             - wind speed at height zu1 [m/s] (0 when using roughness length as zu1)
   ! input: u2             - wind speed at height zu2 [m/s] (at height of wind measurements)
   ! input: zu1            - lower wind measurement height [m] (usually the roughness length)
   ! input: zu2            - upper wind measurement height [m] (usually z of wind measurements)
   ! input: T              - air temperature [C]
   ! input: cloud_fraction - cloud fraction [0..1]
   ! input: sinphi         - sine of solar elevation angle [-]
   ! input: kin            - incoming shortwave radiation [W/m^2]
   ! output: result%ust    - friction velocity [m/s]
   ! output: result%ol     - Obukhov length [m]
   ! output: result%kin    - incoming shortwave radiation used [W/m^2]
   ! ===========================================================
   !! Solves for friction velocity and returns associated Obukhov length and radiation terms.
   !!   Reference: Van Ulden and Holtslag (1985); Beljaars et al. (1989).
   pure function modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin) result(result)
      real(RK), intent(in) :: u1
      !! wind speed at height zu1 [m/s]
      real(RK), intent(in) :: u2
      !! wind speed at height zu2 [m/s]
      real(RK), intent(in) :: zu1
      !! lower wind measurement height [m]
      real(RK), intent(in) :: zu2
      !! upper wind measurement height [m]
      real(RK), intent(in) :: T
      !! air temperature [C]
      real(RK), intent(in) :: cloud_fraction
      !! cloud fraction [0..1]
      real(RK), intent(in) :: sinphi
      !! sine of solar elevation angle [-]
      real(RK), intent(in) :: kin
      !! incoming shortwave radiation [W/m^2]

      real(RK) :: ust_guess, max_ust
      type(modmet_radiat_result) :: rad_result
      type(modmet_solver_result) :: solver_result

      type(modmet_flxln2_result) :: result
      if (cloud_fraction < 0.0_RK) then
         result%ust = -9999.0_RK
         result%ol = -9999.0_RK
         result%kin = -9999.0_RK
         return
      end if

      if (cloud_fraction > 1.0_RK) then
         result%ust = -9999.0_RK
         result%ol = -9999.0_RK
         result%kin = -9999.0_RK
         return
      end if

      rad_result = modmet_radiat(sinphi, cloud_fraction, kin)


      ust_guess = (u2-u1) * VONK / log(zu2/zu1) !initial guess for ust based on log profile
      max_ust = 10.0_RK * ust_guess ! Set a reasonable upper limit for ust
      ust_guess = 0.01_RK*ust_guess ! Start with a smaller initial guess to ensure convergence


      solver_result = modmet_find_zero(momentum_f, ust_guess, max_ust, tol=1.0e-12_RK, max_iter=100)

      result%ust = solver_result%root

      if (result%ust < 0.0_RK .or. modmet_missing(result%ust)) then
         result%ust = -9999.0_RK
         result%tst = -9999.0_RK
         result%qst = -9999.0_RK
         result%ol = -9999.0_RK
         result%kin = -9999.0_RK
         result%tau = -9999.0_RK
         result%h = -9999.0_RK
         result%le = -9999.0_RK
         return
      end if
      result%tst = solver_result%payload(2)
      result%qst = solver_result%payload(3)
      result%ol = solver_result%payload(1) ! Obukhov length from the momentum function
      result%kin = rad_result%kin

      result%tau = RO * result%ust**2
      result%h = -RO * CP_AIR * result%tst * result%ust
      result%le = -RO * LAMBDA * result%qst  * result%ust

   contains
      ! ===========================================================
      ! Function: momentum_f
      ! Description: Residual function for momentum profile inversion.
      ! input: ust_g          - trial friction velocity [m/s]
      ! output: result%value  - residual for zero-finding
      ! output: result%payload(1) - Obukhov length [m]
      ! ===========================================================
      !! Residual function used by the internal root-finding step.
      !!   Reference: Monin-Obukhov momentum-profile inversion in FLXLN2.
      pure function momentum_f(ust_g) result(fx)
         real(RK), intent(in) :: ust_g
         !! trial friction velocity [m/s]
         type(modmet_tst_result) :: tst_out
         real(RK) :: tstv, ol
         type(modmet_solver_result) :: fx

         tst_out = modmet_tst(ust_g, T, rad_result%qsti)

         tstv = tst_out%tst + 0.00061_RK*TR* tst_out%qst

         ! updates the Obukhov length in the function
         ol = modmet_obuk(ust_g, tstv)

         fx%value = ust_g - (u2 - u1)*VONK/(log(zu2/zu1) - &
            modmet_fpsim(zu2/ol) + modmet_fpsim(zu1/ol))

         fx%payload(1) = ol
         fx%payload(2) = tst_out%tst
         fx%payload(3) = tst_out%qst

      end function momentum_f

   end function modmet_flxln2

! LCOV_EXCL_START
end module m_modmet_flxln2
! LCOV_EXCL_END
