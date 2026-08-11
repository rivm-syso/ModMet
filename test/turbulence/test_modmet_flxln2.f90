!------------------------------------------------------------------------------
! Module:     test_modmet_flxln2
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Unit tests for friction velocity and Obukhov length solver coupling.
!------------------------------------------------------------------------------
module test_modmet_flxln2
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use modmet_constants, only: RK
   use m_modmet_flxln2, only: modmet_flxln2, modmet_flxln2_result
   use m_modmet_helpers, only: modmet_missing

   implicit none (type, external)
   private
   public :: collect_modmet_flxln2_tests
contains
    subroutine collect_modmet_flxln2_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_flxln2", test_flxln2) &
            ]

    end subroutine collect_modmet_flxln2_tests

    subroutine test_flxln2(error)
        type(error_type), allocatable, intent(out) :: error
        type(modmet_flxln2_result) :: result
        real(RK) :: u1, u2, zu1, zu2, T, cloud_fraction
        real(RK) :: sinphi, kin



    ! test 1: daytime reference conditions
        u1 = 0.0_RK
        u2 = 5.0_RK
        zu1 = 0.10_RK ! roughness length
        zu2 = 10.0_RK ! height of the second level
        T = 15.0_RK ! temperature in C
        kin = 200.0_RK ! short wave radiation in W/m^2
        cloud_fraction = 0.1_RK
        sinphi = 0.8_RK ! sine of the solar elevation angle

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%ust, 0.436865836_RK, &
            message="modmet_flxln2 did not return expected ust value", thr=1.0e-4_RK)
        call check(error, result%ol, -1411.45898_RK, &
            message="modmet_flxln2 did not return expected ol value", thr=1.0e-4_RK)
        call check(error, result%kin, 200.0_RK, &
            message="modmet_flxln2 did not return expected kin value", thr=1.0e-4_RK)
        if (allocated(error)) return

        ! test 2: stable nighttime conditions
        u1 = 0.0_RK
        u2 = 2.0_RK                 ! Lower wind speed typical of nighttime
        zu1 = 0.10_RK
        zu2 = 10.0_RK
        T = 8.0_RK                  ! Cooler nighttime temperature
        kin = 0.0_RK                ! No incoming shortwave radiation
        cloud_fraction = 0.0_RK     ! Clear skies (enhances longwave cooling)
        sinphi = -0.5_RK            ! Sun below horizon

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%ust, 7.00441748e-2_RK, &
        message="Stable case: ust mismatch", thr=1.0e-5_RK)
        call check(error, result%ol, 5.64502287_RK, &
            message="Stable case: positive ol expected", thr=1.0e-4_RK)
        if (allocated(error)) return


        ! test 3: missing kin handling
        u1 = 0.0_RK
        u2 = 5.0_RK
        zu1 = 0.10_RK
        zu2 = 10.0_RK
        T = 15.0_RK
        kin = -9999.0_RK            ! Missing data trigger code
        cloud_fraction = 0.5_RK     ! Partly cloudy
        sinphi = 0.6_RK

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%kin >= 0.0_RK, .true., &
            message="modmet_flxln2 should handle missing kin by setting it to a non-negative value")

        call check(error, result%ol, -97.064895_RK, &
            message="ol not correct for missing kin case", thr=1.0e-4_RK)
        call check(error, result%ust, 0.463040978_RK, &
            message="ust not correct for missing kin case", thr=1.0e-4_RK)
        if (allocated(error)) return


        ! test 4: near-neutral high-wind overcast condition
        u1 = 0.0_RK
        u2 = 12.0_RK                ! Strong dynamic mixing
        zu1 = 0.10_RK
        zu2 = 10.0_RK
        T = 10.0_RK
        kin = 50.0_RK               ! Very weak solar radiation
        cloud_fraction = 1.0_RK     ! 100% overcast
        sinphi = 0.3_RK

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)
        call check(error, result%ust, 1.03801572_RK, &
            message="ust mismatch for near-neutral high wind case", thr=1.0e-5_RK)
        call check(error, result%ol, 2703.60466_RK, &
            message="ol mismatch for near-neutral high wind case", thr=1.0e-3_RK)
        if (allocated(error)) return

        ! test 5: invalid cloud fraction (below range)

        result = modmet_flxln2(u1, u2, zu1, zu2, T, -1.2_RK, sinphi, kin)

        call check(error, modmet_missing(result%ust), .true., &
            message="ust should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%ol), .true., &
            message="ol should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%kin), .true., &
            message="kin should be -9999.0 for invalid cloud fraction")
        if (allocated(error)) return
        ! test 6: invalid cloud fraction (above range)
        result = modmet_flxln2(u1, u2, zu1, zu2, T, 1.3_RK, sinphi, kin)

        call check(error, modmet_missing(result%ust), .true., &
            message="ust should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%ol), .true., &
            message="ol should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%kin), .true., &
            message="kin should be -9999.0 for invalid cloud fraction")
        if (allocated(error)) return
        ! test 7: strongly unstable free-convection limit
        u1 = 0.0_RK
        u2 = 4.0_RK                 ! Light breeze (minimizes dynamic shear)
        zu1 = 0.10_RK               ! Roughness length
        zu2 = 10.0_RK               ! Measurement height
        T = 30.0_RK                 ! Hot summer day
        kin = 800.0_RK              ! Intense, clear-sky midday solar radiation
        cloud_fraction = 0.0_RK     ! Completely clear skies
        sinphi = 0.95_RK            ! Sun almost directly overhead

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)



        call check(error, result%ust, 0.386140645_RK, &
            message="ust mismatch for strongly unstable case", thr=1.0e-5_RK)

        call check(error, result%ol, -48.6206894_RK, &
            message="ol mismatch for strongly unstable case", thr=1.0e-5_RK)
    end subroutine test_flxln2

end module test_modmet_flxln2
