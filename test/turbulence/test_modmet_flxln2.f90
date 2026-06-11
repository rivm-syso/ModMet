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
        real :: u1, u2, zu1, zu2, T, cloud_fraction
        real :: sinphi, kin



    ! test 1: daytime reference conditions
        u1 = 0.0
        u2 = 5.0
        zu1 = 0.10 ! roughness length
        zu2 = 10.0 ! height of the second level
        T = 15.0 ! temperature in C
        kin = 200.0 ! short wave radiation in W/m^2
        cloud_fraction = 0.1
        sinphi = 0.8 ! sine of the solar elevation angle

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%ust, 0.436865836, &
            message="modmet_flxln2 did not return expected ust value", thr=1.0e-6)
        call check(error, result%ol, -1411.45886, &
            message="modmet_flxln2 did not return expected ol value", thr=1.0e-6)
        call check(error, result%kin, 200.0, &
            message="modmet_flxln2 did not return expected kin value", thr=1.0e-6)
        if (allocated(error)) return

        ! test 2: stable nighttime conditions
        u1 = 0.0
        u2 = 2.0                 ! Lower wind speed typical of nighttime
        zu1 = 0.10
        zu2 = 10.0
        T = 8.0                  ! Cooler nighttime temperature
        kin = 0.0                ! No incoming shortwave radiation
        cloud_fraction = 0.0     ! Clear skies (enhances longwave cooling)
        sinphi = -0.5            ! Sun below horizon

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%ust, 7.00441748e-2, &
        message="Stable case: ust mismatch", thr=1.0e-5)
        call check(error, result%ol, 5.64502287, &
            message="Stable case: positive ol expected", thr=1.0e-4)
        if (allocated(error)) return


        ! test 3: missing kin handling
        u1 = 0.0
        u2 = 5.0
        zu1 = 0.10
        zu2 = 10.0
        T = 15.0
        kin = -9999.0            ! Missing data trigger code
        cloud_fraction = 0.5     ! Partly cloudy
        sinphi = 0.6

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%kin >= 0.0, .true., &
            message="modmet_flxln2 should handle missing kin by setting it to a non-negative value")

        call check(error, result%ol, -97.0649261, &
            message="ol not correct for missing kin case", thr=1.0e-6)
        call check(error, result%ust, 0.463040978, &
            message="ust not correct for missing kin case", thr=1.0e-6)
        if (allocated(error)) return


        ! test 4: near-neutral high-wind overcast condition
        u1 = 0.0
        u2 = 12.0                ! Strong dynamic mixing
        zu1 = 0.10
        zu2 = 10.0
        T = 10.0
        kin = 50.0               ! Very weak solar radiation
        cloud_fraction = 1.0     ! 100% overcast
        sinphi = 0.3

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

        call check(error, result%ust, 1.03801572, &
            message="ust mismatch for near-neutral high wind case", thr=1.0e-5)
        call check(error, result%ol, 2703.60327, &
            message="ol mismatch for near-neutral high wind case", thr=1.0e-5)
        if (allocated(error)) return

        ! test 5: invalid cloud fraction (below range)

        result = modmet_flxln2(u1, u2, zu1, zu2, T, -1.2, sinphi, kin)

        call check(error, modmet_missing(result%ust), .true., &
            message="ust should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%ol), .true., &
            message="ol should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%kin), .true., &
            message="kin should be -9999.0 for invalid cloud fraction")
        if (allocated(error)) return
        ! test 6: invalid cloud fraction (above range)
        result = modmet_flxln2(u1, u2, zu1, zu2, T, 1.3, sinphi, kin)

        call check(error, modmet_missing(result%ust), .true., &
            message="ust should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%ol), .true., &
            message="ol should be -9999.0 for invalid cloud fraction")
        call check(error, modmet_missing(result%kin), .true., &
            message="kin should be -9999.0 for invalid cloud fraction")
        if (allocated(error)) return
        ! test 7: strongly unstable free-convection limit
        u1 = 0.0
        u2 = 4.0                 ! Light breeze (minimizes dynamic shear)
        zu1 = 0.10               ! Roughness length
        zu2 = 10.0               ! Measurement height
        T = 30.0                 ! Hot summer day
        kin = 800.0              ! Intense, clear-sky midday solar radiation
        cloud_fraction = 0.0     ! Completely clear skies
        sinphi = 0.95            ! Sun almost directly overhead

        result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)



        call check(error, result%ust, 0.386140645, &
            message="ust mismatch for strongly unstable case", thr=1.0e-5)

        call check(error, result%ol, -48.6206894, &
            message="ol mismatch for strongly unstable case", thr=1.0e-5)
    end subroutine test_flxln2

end module test_modmet_flxln2
