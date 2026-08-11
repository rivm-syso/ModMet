!------------------------------------------------------------------------------
! Module:     test_modmet_tst
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for temperature and humidity scale calculations.
!------------------------------------------------------------------------------
module test_modmet_tst
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use modmet_constants, only: RK
   use m_modmet_tst, only: modmet_tst, modmet_tst_result

   implicit none (type, external)
   private
   public :: collect_modmet_tst_tests
contains

   subroutine collect_modmet_tst_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_modmet_tst", test_tst) &
         ]

   end subroutine collect_modmet_tst_tests

   subroutine test_tst(error)
      type(error_type), allocatable, intent(out) :: error
      type(modmet_tst_result) :: result

      real(RK) :: ust, t, qsti
      real(RK) :: qst_old, tst_old


      ust = 0.5_RK
      t = 280.0_RK
      qsti = 400

      ! test 1: daytime high-radiation reference case
      result = modmet_tst(ust, t, qsti)




      call check(error, result%tst, 3.2999816E-02_RK, &
         message="modmet_tst did not return expected tst value", thr=1.0e-4_RK)
      call check(error, result%qst, -0.274415612_RK, &
         message="modmet_tst did not return expected qst value", thr=1.0e-4_RK)
      if (allocated(error)) return




      ust = 1.1_RK
      t = 290.0_RK
      qsti = 10.0_RK

   ! test 2: weak daytime forcing case
      result = modmet_tst(ust, t, qsti)



      call check(error, result%tst, 3.29999998E-02_RK, &
         message="modmet_tst did not return expected tst value for test 2", thr=1.0e-6_RK)
      call check(error, result%qst, -1.63559280E-02_RK, &
         message="modmet_tst did not return expected qst value for test 2", thr=1.0e-6_RK)
      if (allocated(error)) return


      qsti = -50.0_RK
   ! test 3: nighttime negative-radiation case
      result = modmet_tst(ust, t, qsti)
      ! TODO check this should be zero tst?
      call check(error, result%tst, 3.3E-02_RK, &
         message="modmet_tst did not return expected tst value for test 3", thr=1.0e-4_RK)
      call check(error, result%qst, -5.6469478E-05_RK, &
         message="modmet_tst did not return expected qst value for test 3", thr=1.0e-4_RK)
      if (allocated(error)) return

    end subroutine test_tst
end module test_modmet_tst
