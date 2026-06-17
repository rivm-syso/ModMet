!------------------------------------------------------------------------------
! Module:     m_modnet_z0corr
! Authors:    Marte Voorneveld, RIVM
!             Hans van Jaarsveld (original Z0 correction concept)
! Created:    June 17 2026
! Updated:    June 17 2026
! Description:
!   Corrects friction velocity and Monin-Obukhov length for a target
!   roughness length using a 50 m wind invariance assumption.
!   The method assumes wind speed at 50 m is unaffected by roughness
!   changes and does not include direct temperature-effect corrections.
!   A root-finding iteration is used to solve for consistent corrected
!   friction velocity and implied Obukhov length.
!------------------------------------------------------------------------------
module m_modnet_z0corr
   use modmet_constants, only: VONK, TR, RO, CP_AIR, LAMBDA, EPS
   use m_modmet_find_zero, only: modmet_find_zero, modmet_solver_result

   use m_modmet_fpsim, only: modmet_fpsim_holtslag

   implicit none (type, external)
   private
   public :: modmet_solve_z0_corr
contains
   ! ===========================================================
   ! Subroutine: modmet_solve_z0_corr
   ! Description: Corrects friction velocity and Obukhov length
   !              from an input roughness length to a target
   !              roughness length while preserving 50 m wind speed.
   ! input: z0_in   - source roughness length [m]
   ! input: z0_lu   - target roughness length [m]
   ! input: ol_old  - original Obukhov length [m]
   ! input: ust_old - original friction velocity [m/s]
   ! input, optional: max_iter - maximum iterations for root-finding (default 40)
   ! output: ol_new - corrected Obukhov length [m]
   ! output: ust_new- corrected friction velocity [m/s]
   ! ===========================================================
   !! Corrects ustar and Obukhov length to a target roughness length.
   !!   Assumptions: wind speed at 50 m is roughness-invariant; direct
   !!   temperature-effect corrections are not applied.
   subroutine modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new, &
       max_iter, tol, min_change)
      real, intent(in) :: z0_in
      !! source roughness length [m]
      real, intent(in) :: z0_lu
      !! target roughness length [m]
      real, intent(in) :: ol_old
      !! original Obukhov length [m]
      real, intent(in) :: ust_old
      !! original friction velocity [m/s]
      real, intent(out) :: ol_new
      !! corrected Obukhov length [m]
      real, intent(out) :: ust_new
      !! corrected friction velocity [m/s]
      integer, intent(in), optional :: max_iter
      !! maximum iterations for root-finding (default 40)
      real, intent(in), optional :: tol
      !! convergence tolerance for root-finding (default 0.015)
      real, intent(in), optional :: min_change
      !! minimum change for convergence (default 0.1)

      type(modmet_solver_result) :: solver_out

      ! Host variables shared with the internal objective function
      real :: u50, h0_init
      real :: x0, x1
      real :: phim_init
      ! Local parameters for root-finding
      integer :: max_iter_local
      real :: local_tol, local_min_change

      real, parameter :: kappa = 0.4
      real, parameter :: c1 = 93500.0
      real, parameter :: z_ref = 50.0


      max_iter_local = 40
      local_tol = 0.015
      local_min_change = 0.1

      if(present(max_iter)) max_iter_local = max_iter
      if(present(tol)) local_tol = tol
      if(present(min_change)) local_min_change = min_change

      ! 2. Enforce validity of optional parameters, with fallbacks to defaults
      if (max_iter_local <= 0)   max_iter_local = 40
      if (local_tol      <= 0.0) local_tol      = 0.015
      if (local_min_change     <= 0.0) local_min_change     = 0.1

      if (abs(z0_in - z0_lu) < (local_min_change*z0_in - EPS)) then
         ! If the roughness lengths are already close, return original values

         ust_new = ust_old
         ol_new = ol_old
         return
      end if



      ! 2. Compute reference 50 m wind speed and initial heat-flux scaling
      phim_init = modmet_fpsim_holtslag(z_ref, ol_old)

      u50 = ust_old / kappa * (log(z_ref / z0_in) - phim_init)

      h0_init = -ust_old**3 * c1 / ol_old

      ! Define search bounds for friction velocity
      if(ol_old < 0.0) then
         ! Unstable case: bigger bounds to allow for potential increase in ustar with roughness
         x0 = ust_old*0.01
         x1 = 40.0 * ust_old
      else
         ! Stable case: expect ustar to decrease with roughness, so search below old ustar
         x0 = 0.001 * ust_old
         x1 = ust_old*3.0
      end if

      ! 3. Solve for friction velocity that satisfies roughness correction
      solver_out = modmet_find_zero(z0_corr_objective, x0, x1, local_tol, max_iter_local)



      ! 4. Map solver output to corrected state or sentinel values
      if (solver_out%root == -9999.0 .or. solver_out%root == -999.0) then
         ust_new = -999.0
         ol_new  = -999.0
      else
         ust_new = solver_out%payload(1)
         ol_new  = solver_out%payload(2)
      end if

   contains

      ! ===========================================================
      ! Function: z0_corr_objective
      ! Description: Residual function for roughness correction
      !              using implied ustar and Obukhov length.
      ! input: ustar_guess   - trial friction velocity [m/s]
      ! output: fx%value     - residual for zero-finding
      ! output: fx%payload(1)- implied friction velocity [m/s]
      ! output: fx%payload(2)- implied Obukhov length [m]
      ! ===========================================================
      !! Internal residual function for roughness-length correction.
      pure function z0_corr_objective(ustar_guess) result(fx)
         real, intent(in) :: ustar_guess
         !! trial friction velocity [m/s]
         type(modmet_solver_result) :: fx

         real :: phim, ol_implied, ustar_implied, ur, h0_local

         ! Scale heat flux with ustar ratio (different exponents for stable/unstable)
         ur = ustar_guess / ust_old
         if (h0_init < 0.0) then
            h0_local = h0_init * (ur**0.8)
         else
            h0_local = h0_init * (ur**0.1)
         end if

         ol_implied = -ustar_guess**3 * c1 / h0_local


         phim = modmet_fpsim_holtslag(z_ref, ol_implied)

          if(log(z_ref / z0_lu) - phim <= 0.0) then
            ! Unphysical condition near free convection; force a large mismatch
             ustar_implied = 999.0
         else
             ustar_implied = kappa * u50 / (log(z_ref / z0_lu) - phim)
         end if

          ! Return residual and diagnostics through solver payload
         fx%value = ustar_guess - ustar_implied
         fx%root  = ustar_guess
         fx%payload(1) = ustar_implied
         fx%payload(2) = ol_implied
      end function z0_corr_objective

   end subroutine modmet_solve_z0_corr

end module m_modnet_z0corr

