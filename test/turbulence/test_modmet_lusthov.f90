!------------------------------------------------------------------------------
! Module:     test_modmet_lusthov
! Authors:    Marte Voorneveld, RIVM
! Created:    June 12 2026
! Updated:    June 12 2026
! Description:
!   Unit tests for integrated Lusthov surface-layer flux workflow.
!------------------------------------------------------------------------------
module test_modmet_lusthov
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use m_modmet_lusthov, only: modmet_lusthov, modmet_lusthov_result


   implicit none (type, external)
   private
   public :: collect_modmet_lusthov_tests
contains
   subroutine collect_modmet_lusthov_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_lusthov", test_lusthov) &
         ]

   end subroutine collect_modmet_lusthov_tests

   subroutine test_lusthov(error)
      type(error_type), allocatable, intent(out) :: error
      type(modmet_lusthov_result) :: result

      ! Input parameters for test scenarios
      integer :: mt, dy, hr, min
      real :: lat, lon
      real :: kin, z0
      real :: zra, u_zra
      real :: T, cloud_fraction

      mt = 6
      dy = 15
      hr = 12
      min = 0
      lat = 52.0
      lon = 5.0
      kin = -999.0
      z0 = 0.1
      zra = 10.0
      u_zra = 5.0
      T = 20.0
      cloud_fraction = 0.5

      ! test 1: missing kin value
      result = modmet_lusthov(mt, dy, hr, min, lat, lon, kin, z0, zra, u_zra, T, cloud_fraction)


      call check(error, result%ust, 0.472004652, message="Unexpected ust value", thr=1e-6)
      call check(error, result%ol, -68.2802200, message="Unexpected ol value", thr=1e-6)
      call check(error, result%kin, 777.212036, message="Unexpected kin value", thr=1e-4)
      call check(error, result%h, 116.371979, message="Unexpected h value", thr=1e-4)
      call check(error, result%evap, 0.454255164, message="Unexpected evap value", thr=1e-4)


      ! test 2: prescribed kin value

      mt = 6
      dy = 15
      hr = 12
      min = 0
      lat = 52.0
      lon = 5.0
      kin = 200.0
      z0 = 0.1
      zra = 10.0
      u_zra = 12.0
      T = 20.0
      cloud_fraction = 0.1
      result = modmet_lusthov(mt, dy, hr, min, lat, lon, kin, z0, zra, u_zra, T, cloud_fraction)

      call check(error, result%ust, 1.04003716, message="Unexpected ust value", thr=1e-6)
      call check(error, result%ol, 5122.79492, message="Unexpected ol value", thr=1e-4)
      call check(error, result%kin, 200.0, message="Unexpected kin value", thr=1e-6)
      call check(error, result%h, -25.3296242, message="Unexpected h value", thr=1e-4)
      call check(error, result%evap, 0.116497397, message="Unexpected evap value", thr=1e-6)

      ! component routines are tested separately; this test validates coupling output.
   end subroutine test_lusthov

end module test_modmet_lusthov
