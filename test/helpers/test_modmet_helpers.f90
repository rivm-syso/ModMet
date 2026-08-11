!------------------------------------------------------------------------------
! Module:     test_modmet_helpers
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for helper missing-value detection routines.
!------------------------------------------------------------------------------
module test_modmet_helpers
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use modmet_constants, only: RK
   use m_modmet_helpers, only: modmet_missing
   implicit none (type, external)
   private
   public :: collect_modmet_helpers_tests
contains

   subroutine collect_modmet_helpers_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_modmet_missing", test_modmet_missing) &
         ]

   end subroutine collect_modmet_helpers_tests

   subroutine test_modmet_missing(error)
      type(error_type), allocatable, intent(out) :: error
      logical :: is_missing

      ! test 1: real missing sentinel should be detected
      is_missing = modmet_missing(-9999.0_RK)

      call check(error, is_missing, .true., &
         message="modmet_missing did not return the expected missing value")

      ! test 2: valid real value should not be missing
      is_missing = modmet_missing(0.0_RK)

      call check(error, is_missing, .false., &
         message="modmet_missing incorrectly identified 0.0 as missing value")

      ! test 3: integer missing sentinel should be detected
      is_missing = modmet_missing(-9999)

      call check(error, is_missing, .true., &
         message="modmet_missing did not return the expected missing value for integer input")
      ! test 4: valid integer value should not be missing
      is_missing = modmet_missing(0)

   end subroutine test_modmet_missing

end module test_modmet_helpers
