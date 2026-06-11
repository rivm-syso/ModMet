!------------------------------------------------------------------------------
! Module:     test_modmet_fpsim
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for momentum stability correction function psi_m.
!------------------------------------------------------------------------------
module test_modmet_fpsim
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_modmet_fpsim, only: modmet_fpsim
   implicit none (type, external)
   private
   public :: collect_modmet_fpsim_tests
contains

   subroutine collect_modmet_fpsim_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_fpsim", test_fpsim) &
         ]

   end subroutine collect_modmet_fpsim_tests

   subroutine test_fpsim(error)
      type(error_type), allocatable, intent(out) :: error
      real :: fpsim

      ! test 1: unstable condition eta = -0.1
      fpsim = modmet_fpsim(-0.1)
      call check(error, fpsim, 0.283613801, &
         message="fpsim not as expected for eta=-0.1", thr=1.0e-3)
      if (allocated(error)) return

      ! test 2: neutral condition eta = 0.0
      fpsim = modmet_fpsim(0.0)
      call check(error, fpsim, 0.0, &
         message="fpsim not as expected for eta=0.0", thr=1.0e-3)
      if (allocated(error)) return

      ! test 3: stable condition eta = 10.0
      fpsim = modmet_fpsim(10.0)
      call check(error, fpsim, -17.6227646, &
         message="fpsim not as expected for eta=10.0", thr=1.0e-3)
      if (allocated(error)) return

      ! test 4: very stable condition eta = 230.0
      fpsim = modmet_fpsim(230.0)
      call check(error, fpsim, -171.720001, &
         message="fpsim not as expected for eta=230.0", thr=1.0e-3)
      if (allocated(error)) return

    end subroutine test_fpsim
end module test_modmet_fpsim
