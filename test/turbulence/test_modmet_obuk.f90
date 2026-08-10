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

   use modmet_constants, only: RK
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
        real(RK) :: ust, tst, ol
            ! test 1: positive temperature scale gives positive Obukhov length
        ust = 0.5_RK
        tst = 0.01_RK
        ol = modmet_obuk(ust, tst)


        call check(error, ol, 1804.84693_RK, &
           message="modmet_obuk did not return expected Obukhov length", thr=1.0e-4_RK)

      ! test 2: near-zero temperature scale returns fallback value
        ol = modmet_obuk(0.5_RK, 0.0_RK)

        call check(error, ol, -1e5_RK, &
           message="modmet_obuk did not return expected Obukhov length for tst=0", thr=1.0e-4_RK)

      ! test 3: negative temperature scale gives negative Obukhov length
        ol = modmet_obuk(0.5_RK, -0.01_RK)
        call check(error, ol, -1804.84692_RK, &
           message="modmet_obuk did not return expected Obukhov length for negative tst", thr=1.0e-4_RK)

    end subroutine test_obuk
end module test_modmet_obuk
