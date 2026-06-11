!------------------------------------------------------------------------------
! Module:     test_modmet_sunhgh
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for solar elevation sine computation.
!------------------------------------------------------------------------------
module test_modmet_sunhgh
   use testdrive, only : new_unittest, unittest_type, error_type, check
    use m_modmet_sunhgh, only: modmet_sunhgh
   implicit none (type, external)
   private
   public :: collect_modmet_sunhgh_tests
contains

   subroutine collect_modmet_sunhgh_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_sunhgh", test_sunhgh) &
         ]

   end subroutine collect_modmet_sunhgh_tests

   subroutine test_sunhgh(error)
      type(error_type), allocatable, intent(out) :: error
      real :: sinphi

      ! test 1: reference case for March 15 at noon
      sinphi = modmet_sunhgh(52.0, 4.0, 3, 15, 12, 0) ! March 15, 12:00

      call check(error, sinphi, 0.593829870, &
         message="sinphi not as expected for March 15 at noon", thr=1.0e-5)
      if (allocated(error)) return

      ! test 2: June 21 at noon (summer solstice)
      sinphi = modmet_sunhgh(52.0, 4.0, 6, 21, 12, 0)

      call check(error, sinphi, 0.877172947, &
         message="sinphi not as expected for June 21 at noon", thr=1.0e-5)
      if (allocated(error)) return

      ! test 3: Sydney December 21 noon local time converted to GMT input
      sinphi = modmet_sunhgh(-33.9, 151.2, 12, 21, 2, 0)

      call check(error, sinphi, 0.983182073, &
         message="sinphi not as expected for Sydney on December 21 at noon", thr=1.0e-5)
      if (allocated(error)) return

   end subroutine test_sunhgh
end module test_modmet_sunhgh
