!------------------------------------------------------------------------------
! Module:     test_rnf_radiat
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for isothermal net radiation and incoming shortwave logic.
!------------------------------------------------------------------------------
module test_rnf_radiat
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use modmet_constants, only: RK
   use m_modmet_radiat, only: modmet_radiat, modmet_radiat_result

   implicit none (type, external)
   private
   public :: collect_rnf_radiat_tests
contains

   subroutine collect_rnf_radiat_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_radiat", test_radiat) &
         ]

   end subroutine collect_rnf_radiat_tests

   subroutine test_radiat(error)
      type(error_type), allocatable, intent(out) :: error
      real(RK) :: sinphi, cloud_fraction, kin
      type(modmet_radiat_result) :: result


      ! test 1: small sinphi with missing kin
      kin = -999.0_RK

      result = modmet_radiat(0.02_RK, 0.5_RK, kin)

      call check(error, result%kin, 0.0_RK, &
         message="missing kin should become 0.0", thr=1.0e-5_RK )
      call check(error, result%qsti, -61.3466949_RK, &
         message="qsti value is incorrect for given inputs", thr=1.0e-5_RK )

      ! test 2: small sinphi with valid kin

      kin = 500.0_RK

      result = modmet_radiat(0.02_RK, 0.5_RK, kin)

      call check(error, result%kin, 500.0_RK, &
         message="kin should remain unchanged for small sinphi when valid kin is provided", &
         thr=1.0e-5_RK )
      call check(error, result%qsti, -61.3466949_RK, &
         message="qsti value is incorrect for given inputs", thr=1.0e-5_RK )


      ! test 3: high sinphi with missing kin
      kin = -9999.0_RK

      result = modmet_radiat(0.8_RK, 0.5_RK, kin)

      call check(error, result%kin, 707.860474_RK, &
         message="kin should be calculated based on the formula for valid sinphi", thr=1.0e-4_RK )
      call check(error, result%qsti, 483.705872_RK, &
         message="qsti value is incorrect for given inputs", thr=1.0e-4_RK )

      ! test 4: high sinphi with valid kin
      kin = 600.0_RK

      result = modmet_radiat(0.5_RK, 0.5_RK, kin)

      call check(error, result%kin, 600.0_RK, &
         message="kin should remain unchanged when valid input is provided", thr=1.0e-4_RK )
      call check(error, result%qsti, 400.653320_RK, &
         message="qsti value is incorrect for given inputs", thr=1.0e-4_RK )

   end subroutine test_radiat

end module test_rnf_radiat
