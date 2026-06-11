!------------------------------------------------------------------------------
! Module:     test_modmet_cloud_fraction
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for cloud fraction estimation from radiation inputs.
!------------------------------------------------------------------------------
module test_modmet_cloud_fraction
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_modmet_cloud_fraction, only: modmet_cloud_fraction
   implicit none (type, external)
   private
   public :: collect_modmet_cloud_fraction_tests
contains

   subroutine collect_modmet_cloud_fraction_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_cloud_fraction", test_cloud_fraction) &
         ]

   end subroutine collect_modmet_cloud_fraction_tests

   subroutine test_cloud_fraction(error)
      type(error_type), allocatable, intent(out) :: error
      real :: cloud_fraction
      integer :: mt, dy, hr
      real :: lat, lon
      real :: jcm2


      mt = 3 ! month
      dy = 15 ! day
      hr = 12 ! hr

      lat = 52.0
      lon = 4.0
      jcm2 = 125.0

      ! test 1: reference daytime case
      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr)

      call check(error, cloud_fraction, 0.809560895, &
         message="cloud fraction not as expected", thr=1.0e-5)
      if (allocated(error)) return

      ! test 2: negative global radiation should return 0.6
      jcm2 = -10.0
      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr)
      call check(error, cloud_fraction, 0.6, &
         message="cloud fraction should be 0.6 for negative global radiation", thr=1.0e-5)
      if (allocated(error)) return

      ! test 3: low June radiation should saturate at cloud fraction 1.0
      mt = 6
      hr = 12
      jcm2 = 10.0

      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr)
      call check(error, cloud_fraction, 1.0, &
         message="cloud fraction should be 1.0 for low global radiation in June", thr=1.0e-5)
      if (allocated(error)) return

      ! test 4: high June radiation should give cloud fraction 0.0
      jcm2 = 800.0
      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr)

      call check(error, cloud_fraction, 0.0, &
         message="cloud fraction should be 0.0 for high global radiation in June", thr=1.0e-5)
      if (allocated(error)) return

      ! test 5: globgem/globber >= 1.0 with globgem <= 100.0 returns default
      jcm2 = 35.0
      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, 12, 20, 10)

      call check(error, cloud_fraction, 0.6, &
         message="cloud fraction should be 0.6 when globgem/globber >= 1.0 but globgem <= 100.0", &
         thr=1.0e-5)
      if (allocated(error)) return



      ! test 6: invalid date/time inputs should return -999.0

      lat = 90.0             ! Aligns perfectly with internal decli calculation
      lon = 360.0             ! Calibrated to force hangle to 0.0 at hr=12
      jcm2 = 800.0              ! High midday radiation
      mt = -1                     ! June
      dy = -4                    ! Solstice day
      hr = -12                    ! Midday hour (t = 11.5)


      cloud_fraction = modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr)

      call check(error, cloud_fraction, -999.0, &
         message="cloud fraction should be -999.0 for invalid date/time inputs", thr=1.0e-5)
      if (allocated(error)) return

   end subroutine test_cloud_fraction
end module test_modmet_cloud_fraction
