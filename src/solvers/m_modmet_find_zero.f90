!------------------------------------------------------------------------------
! Module:     m_modmet_find_zero
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original ZEROAB routine)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module provides a root-finding solver using a safeguarded secant
!   method with Regula Falsi fallback for robustness inside a bracket.
!------------------------------------------------------------------------------
module m_modmet_find_zero

   implicit none (type, external)

   type :: modmet_solver_result
      real :: value  ! The function value at the given x
      real :: root  ! The estimated root value
      real :: payload(10)  ! Additional data for the solver to be passed back to the caller
   end type modmet_solver_result

   abstract interface

      pure function solver_function(x) result(fx)
         import :: modmet_solver_result
         implicit none (type, external)
         real, intent(in) :: x
         type(modmet_solver_result) :: fx
      end function solver_function
   end interface
   private
   public :: modmet_find_zero, modmet_solver_result
contains

   pure function modmet_find_zero(f, x0, x1, tol, max_iter) result(root)
   ! ===========================================================
   ! Function: modmet_find_zero
   ! Description: Finds a root of f(x) in [x0, x1] using a safeguarded
   !              hybrid algorithm: secant updates with Regula Falsi
   !              fallback when the secant step leaves the bracket.
   ! input: f        - solver callback returning value and payload
   ! input: x0       - left bracket bound
   ! input: x1       - right bracket bound
   ! input: tol      - convergence tolerance on x
   ! input: max_iter - maximum iterations
   ! output: root    - solver result (root, value, payload)
   ! ===========================================================
      procedure(solver_function) :: f  ! function for which we want to find a root
      real, intent(in) :: x0, x1, tol ! min, max, tolerance
      integer, intent(in) :: max_iter ! maximum number of iterations
      type(modmet_solver_result) :: root

      ! Local history tracking variables
      real :: xl, xr, xb   ! X positions: Left, Right, and Bracket bound
      type(modmet_solver_result) :: fl, fr, fb   ! Function values at those positions
      real :: x_next       ! The newly calculated guess
      type(modmet_solver_result) :: fx_next      ! Function value at the new guess
      integer :: iter

      real, parameter :: tiny = 1.0e-30 ! A small number to prevent division by zero


      ! --- Step 1: Initialize Boundaries ---
      xl = x0 ! left boundary
      fl = f(xl) ! function value at left boundary


      xr = x1
      fr = f(xr)




      if (abs(fl%value) < tiny) then
         root = fl
         root%root = xl
         return
      end if
      if (abs(fr%value) < tiny) then
         root = fr
         root%root = xr
         return
      end if

      ! Initialize the fallback safety bracket tracking
      xb = xl
      fb = fl

      fx_next = fl ! Just to initialize the variable, will be overwritten in the loop

      ! Verify that the root is actually bracketed (Intermediate Value Theorem)
      if (sign(1.0, fl%value) == sign(1.0, fr%value)) then
         root = fr ! Return best guess
         root%value = -9999.0 ! Indicate failure to find root
         root%root = -9999.0 ! Indicate failure to bracket
         return
      end if

      do iter = 1, max_iter
         ! Zero-division safety protection
         if (fl%value == fr%value) then
            root = fr
            root%root = -999.0 ! Indicate failure due to zero slope
            root%value = -999.0 ! Indicate failure due to zero slope
            return
         end if

         ! step 1: try a standard secant method step
         x_next = xl - (xr - xl) * fl%value / (fr%value - fl%value)


         ! 2. Safety Check: If the Secant step overshoots outside the bracket,
         !    fall back to a Regula Falsi (False Position) step instead.
         if (sign(1.0, x_next - xb) == sign(1.0, x_next - xr)) then
            x_next = xb - (xr - xb) * fb%value / (fr%value - fb%value)
         end if

         ! 3. Convergence test: Check if step size is smaller than the requested tolerance
         if (abs(x_next - xr) < tol) then
            root = fx_next
            root%root = x_next
            return ! Success! Exit early
         end if

         fx_next = f(x_next)

         ! 4. Update the fallback bracket bound if the signs align
         if (sign(1.0, fx_next%value) == sign(1.0, fb%value)) then
            xb = xr
            fb = fr
         end if

         ! 5. Shift history forward for the next iteration
         xl = xr
         fl = fr
         xr = x_next
         fr = fx_next

         ! Early check to see if we landed perfectly on the root
         if (abs(fr%value) < tiny) then
            root = fr
            root%root = xr
            return
         end if
      end do
      ! If the loop completes without returning early, max_iter was hit
      root = fr
      root%root = xr

   end function modmet_find_zero

! LCOV_EXCL_START
end module m_modmet_find_zero
! LCOV_EXCL_END
