!------------------------------------------------------------------------------
! Module:     test_modmet_find_zero
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for safeguarded root-finding behavior and failure modes.
!------------------------------------------------------------------------------
module test_modmet_find_zero
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use modmet_constants, only: RK
   use m_modmet_find_zero, only: modmet_find_zero, modmet_solver_result

   use m_modmet_helpers, only: modmet_missing
   implicit none (type, external)
   private
   public :: collect_modmet_find_zero_tests
contains

   subroutine collect_modmet_find_zero_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_find_zeroes", test_find_zeroes) &
         ]

   end subroutine collect_modmet_find_zero_tests

   subroutine test_find_zeroes(error)
      type(error_type), allocatable, intent(out) :: error
      type(modmet_solver_result) :: result


   ! test 1: positive root bracket [0, 3]
      result = modmet_find_zero(test_func, 0.0_RK, 3.0_RK, tol=1.0e-5_RK, max_iter=100)

      call check(error, result%root, 2.0_RK, &
         message="find_zero did not find the correct root of test_func", thr=1.0e-5_RK )
      call check(error, result%payload(1), 12.34_RK, &
         message="Payload value was not correctly set in the solver result", thr=1.0e-5_RK )
      call check(error, result%value, 0.0_RK, &
         message="Function value at the root should be zero", thr=1.0e-4_RK )


      ! test 2: negative root bracket [-10, 0]
      result = modmet_find_zero(test_func, -10._RK, 0.0_RK, tol=1.0e-5_RK, max_iter=100)

      call check(error, result%root, -2.0_RK, &
         message="find_zero did not find the correct root of test_func", thr=1.0e-5_RK )
      call check(error, result%payload(1), 12.34_RK, &
         message="Payload value was not correctly set in the solver result", thr=1.0e-5_RK )
      call check(error, result%value, 0.0_RK, &
         message="Function value at the root should be zero", thr=1.0e-4_RK )

      ! test 3: exact root at left endpoint

      result = modmet_find_zero(test_func, -2.0_RK, 0.0_RK, tol=1.0e-5_RK, max_iter=100)
      call check(error, result%root, -2.0_RK, &
         message="find_zero did not find the correct root of test_func when starting at a root",&
          thr=1.0e-5_RK )
      call check(error, result%value, 0.0_RK, &
         message="Function value at the root should be zero when starting at a root", thr=1.0e-4_RK )

      ! test 4: exact root at right endpoint
      result = modmet_find_zero(test_func, 0.0_RK, 2.0_RK, tol=1.0e-5_RK, max_iter=100)

      call check(error, result%root, 2.0_RK, &
         message="find_zero did not find the correct root of test_func when starting at a root",&
          thr=1.0e-5_RK )
      call check(error, result%value, 0.0_RK, &
         message="Function value at the root should be zero when starting at a root", thr=1.0e-4_RK )


      ! test 5: max_iter reached before convergence
      result = modmet_find_zero(test_func, 0.0_RK, 9.0_RK, tol=1.0e-5_RK, max_iter=2)

      call check(error, result%root, 0.847058_RK, &
         message="find_zero did not find the correct root of test_func when starting at a root", &
         thr=1.0e-5_RK )
      if (allocated(error)) return
      ! test 6: non-bracketed root should return missing/error sentinels

      result = modmet_find_zero(test_func, 3.0_RK, 4.0_RK, tol=1.0e-5_RK, max_iter=100)


      call check(error, modmet_missing(result%root), .true., &
         message="find_zero should return missing value for root when no root is bracketed" )
      call check(error, modmet_missing(result%value), .true., &
         message="find_zero should return missing value for function value when &
         no root is bracketed")

      call check(error, result%root, -9999.0_RK, &
         message="find_zero should return -9999.0 for root when no root is bracketed" )
      call check(error, result%value, -9999.0_RK, &
         message="find_zero should return -9999.0 for function value when no root is bracketed" )
      if (allocated(error)) return

      ! test 7: flat interior with opposite-signed boundaries triggers zero-slope sentinel
      result = modmet_find_zero(flat_plateau_f, -2.0_RK, 2.0_RK, tol=1.0e-5_RK, max_iter=100)

      call check(error, modmet_missing(result%root), .true., &
         message="find_zero should return missing value for root when function is flat between &
            boundaries" )
      call check(error, modmet_missing(result%value), .true., &
         message="find_zero should return missing value for function value when function is &
            flat between boundaries")
      call check(error, result%root, -999.0_RK, &
         message="find_zero should return -999.0 for root when function is flat between boundaries")
      call check(error, result%value, -999.0_RK, &
         message="find_zero should return -999.0 for function value when function is &
         flat between boundaries" )

         if (allocated(error)) return



   contains
      pure function test_func(x) result(fx)
         type(modmet_solver_result) :: fx
         real(RK), intent(in) :: x


         fx%payload(1) = 12.34_RK  ! Just an example of using the payload
         fx%value = x**2 - 4.0_RK
      end function test_func


      pure function flat_plateau_f(x) result(fx)
            real(RK), intent(in) :: x
            type(modmet_solver_result) :: fx

            fx%payload = 0.0_RK

            ! 1. Initial configuration step (Line 41): xl = -2.0
            if (abs(x - (-2.0_RK)) < 1.0e-5_RK) then
                fx%value = -1.0_RK

            ! 2. Initial configuration step (Line 45): xr = 2.0
            else if (abs(x - 2.0_RK) < 1.0e-5_RK) then
                fx%value = 1.0_RK

            else
                fx%value = 5.0_RK
            end if
        end function flat_plateau_f


   end subroutine test_find_zeroes



end module test_modmet_find_zero
