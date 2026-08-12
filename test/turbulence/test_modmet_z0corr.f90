!------------------------------------------------------------------------------
! Module:     test_modmet_z0corr
! Authors:    Marte Voorneveld, RIVM
! Created:    June 17 2026
! Updated:    June 17 2026
! Description:
!   Unit tests for the z0 correction solver, which iteratively adjusts friction
!   velocity and Obukhov length to achieve consistency between input and output roughness lengths.
!------------------------------------------------------------------------------

module test_modmet_z0corr
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use modmet_constants, only: RK
   use m_modnet_z0corr, only: modmet_solve_z0_corr

   use m_modmet_helpers, only: modmet_missing

    implicit none (type, external)
   private
   public :: collect_modmet_z0corr_tests
contains
   subroutine collect_modmet_z0corr_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_z0_corr", test_z0_corr) &
         ]

   end subroutine collect_modmet_z0corr_tests
   subroutine test_z0_corr(error)
      type(error_type), allocatable, intent(out) :: error
      real(RK) :: z0_in, z0_lu, ol_old, ust_old
      real(RK) :: ust_new, ol_new

      z0_in = 0.3_RK
      z0_lu = 1.0_RK
      ol_old = -1.0_RK
      ust_old = 0.1_RK

      ! Test 1: Extreme case low ustar and unstable conditions, with strict convergence criteria

      call modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new, &
         tol=0.00015_RK, max_iter=900, min_change=0.01_RK)
      call check(error, ust_new, 3.64195950e-2_RK, message="1: Unexpected ust value", thr=1e-3_RK)

      call check(error, ol_new, -1377.1210_RK, message="1: Unexpected ol value", thr=1e-4_RK)

      z0_in = 0.2_RK
      z0_lu = 0.5_RK
      ol_old = 100.0_RK
      ust_old = 0.3_RK

      ! Test 2: Stable case with moderate ustar, with default convergence criteria
      call modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new)



      call check(error, ust_new, 0.397354_RK, message="2: Unexpected ust value", thr=1e-3_RK)
      call check(error, ol_new, 182.8671532_RK, message="2: Unexpected ol value", thr=1e-4_RK)


      z0_in = 0.2_RK
      z0_lu = 0.5_RK
      ol_old = 0.0_RK
      ust_old = 0.3_RK

      ! Test 3: 0 obukhov length no solution case
      call modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new)

      call check(error, modmet_missing(ust_new), .true., message="3: Expected ust_new to be missing")
      call check(error, modmet_missing(ol_new), .true., message="3: Expected ol_new to be missing")

      ! Test 4: No correction case where z0_in and z0_lu are nearly identical,
      ! with default convergence criteria
      z0_in = 0.2_RK
      z0_lu = 0.209_RK
      ol_old = 100.0_RK
      ust_old = 0.3_RK



      call modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new)
      call check(error, ust_new, ust_old, &
         message="4: Expected ust_new to equal ust_old when z0 values are close")
      call check(error, ol_new, ol_old, &
         message="4: Expected ol_new to equal ol_old when z0 values are close")


      ! Test 5: wrong input for limits

      z0_in = 0.2_RK
      z0_lu = 0.5_RK
      ol_old = 100.0_RK
      ust_old = 0.3_RK

      call modmet_solve_z0_corr(z0_in, z0_lu, ol_old, ust_old, ol_new, ust_new, &
         max_iter=-10, tol=-0.01_RK, min_change=-0.05_RK)


      call check(error, ust_new, 0.397354_RK, message="6: Unexpected ust value", thr=1e-3_RK)
      call check(error, ol_new, 182.867157_RK, message="6: Unexpected ol value", thr=1e-4_RK)


   end subroutine test_z0_corr
end module test_modmet_z0corr
