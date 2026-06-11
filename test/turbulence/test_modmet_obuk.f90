!------------------------------------------------------------------------------
! Module:     test_modmet_obuk
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for Obukhov length calculations and edge handling.
!------------------------------------------------------------------------------
module test_modmet_obuk
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_modmet_obuk, only: modmet_obuk
   implicit none (type, external)
   private
   public :: collect_modmet_obuk_tests
contains

   subroutine collect_modmet_obuk_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_obuk", test_obuk) &
         ]

   end subroutine collect_modmet_obuk_tests

   subroutine test_obuk(error)
      type(error_type), allocatable, intent(out) :: error
        real :: ust, tst, ol
            ! test 1: positive temperature scale gives positive Obukhov length
        ol = modmet_obuk(0.5, 0.01)

        call check(error, ol, 1804.84692, &
           message="modmet_obuk did not return expected Obukhov length", thr=1.0e-5)

      ! test 2: near-zero temperature scale returns fallback value
        ol = modmet_obuk(0.5, 0.0)

        call check(error, ol, -1e5, &
           message="modmet_obuk did not return expected Obukhov length for tst=0", thr=1.0e-5)

      ! test 3: negative temperature scale gives negative Obukhov length
        ol = modmet_obuk(0.5, -0.01)
        call check(error, ol, -1804.84692, &
           message="modmet_obuk did not return expected Obukhov length for negative tst", thr=1.0e-5)

    end subroutine test_obuk
end module test_modmet_obuk
