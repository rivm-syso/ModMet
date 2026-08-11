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
   use modmet_constants, only: RK
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
      real(RK) :: lat, lon
      real(RK) :: kin, z0
      real(RK) :: zra, u_zra
      real(RK) :: T, cloud_fraction

      mt = 6
      dy = 15
      hr = 12
      min = 0
      lat = 52.0_RK
      lon = 5.0_RK
      kin = -999.0_RK
      z0 = 0.1_RK
      zra = 10.0_RK
      u_zra = 5.0_RK
      T = 20.0_RK
      cloud_fraction = 0.5_RK

      ! test 1: missing kin value
      result = modmet_lusthov(mt, dy, hr, min, lat, lon, kin, z0, zra, u_zra, T, cloud_fraction)

      call check(error, result%ust, 0.472004652_RK, message="Unexpected ust value", thr=1e-6_RK)
      call check(error, result%ol, -68.2802200_RK, message="Unexpected ol value", thr=1e-4_RK)
      call check(error, result%kin, 777.212036_RK, message="Unexpected kin value", thr=1e-4_RK)
      call check(error, result%h, 116.371979_RK, message="Unexpected h value", thr=1e-4_RK)
      call check(error, result%evap, 0.454255164_RK, message="Unexpected evap value", thr=1e-4_RK)


      ! test 2: prescribed kin value

      mt = 6
      dy = 15
      hr = 12
      min = 0
      lat = 52.0_RK
      lon = 5.0_RK
      kin = 200.0_RK
      z0 = 0.1_RK
      zra = 10.0_RK
      u_zra = 12.0_RK
      T = 20.0_RK
      cloud_fraction = 0.1_RK
      result = modmet_lusthov(mt, dy, hr, min, lat, lon, kin, z0, zra, u_zra, T, cloud_fraction)

      call check(error, result%ust, 1.04003716_RK, message="Unexpected ust value", thr=1e-6_RK)
      call check(error, result%ol, 5122.795083_RK, message="Unexpected ol value", thr=1e-4_RK)
      call check(error, result%kin, 200.0_RK, message="Unexpected kin value", thr=1e-6_RK)
      call check(error, result%h, -25.3296242_RK, message="Unexpected h value", thr=1e-4_RK)
      call check(error, result%evap, 0.116497397_RK, message="Unexpected evap value", thr=1e-6_RK)

      ! component routines are tested separately; this test validates coupling output.
   end subroutine test_lusthov

end module test_modmet_lusthov
