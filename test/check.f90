!------------------------------------------------------------------------------
! Module:     tester
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   Test runner entry point that registers and executes all test suites.
!------------------------------------------------------------------------------
program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type

    use test_modmet_find_zero, only: collect_modmet_find_zero_tests
    use test_modmet_tst, only: collect_modmet_tst_tests
    use test_modmet_obuk, only: collect_modmet_obuk_tests
    use test_modmet_cloud_fraction, only: collect_modmet_cloud_fraction_tests
    use test_modmet_sunhgh, only: collect_modmet_sunhgh_tests
    use test_modmet_fpsim, only: collect_modmet_fpsim_tests
    use test_rnf_radiat, only: collect_rnf_radiat_tests
    use test_modmet_flxln2, only: collect_modmet_flxln2_tests

    use test_modmet_helpers, only: collect_modmet_helpers_tests

    use test_modmet_version, only: collect_modmet_version_tests

    use test_modmet_lusthov, only: collect_modmet_lusthov_tests

    implicit none (type, external)

    ! Initialize test suites
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    integer :: stat, is



    stat = 0

    ! test 1: solver test suite registration
    testsuites = [ &
        new_testsuite("modmet_find_zero_tests", collect_modmet_find_zero_tests), &
        ! test 2: turbulence tst test suite registration
        new_testsuite("modmet_tst_tests", collect_modmet_tst_tests), &
        ! test 3: turbulence obukhov test suite registration
        new_testsuite("modmet_obuk_tests", collect_modmet_obuk_tests), &
        ! test 4: radiation cloud fraction test suite registration
        new_testsuite("modmet_cloud_fraction_tests", collect_modmet_cloud_fraction_tests), &
        ! test 5: radiation sun height test suite registration
        new_testsuite("modmet_sunhgh_tests", collect_modmet_sunhgh_tests), &
        ! test 6: turbulence stability function test suite registration
        new_testsuite("modmet_fpsim_tests", collect_modmet_fpsim_tests), &
        ! test 7: radiation net radiation test suite registration
        new_testsuite("rnf_radiat_tests", collect_rnf_radiat_tests), &
        ! test 8: turbulence flux solver test suite registration
        new_testsuite("modmet_flxln2_tests", collect_modmet_flxln2_tests), &
        ! test 9: helper function test suite registration
        new_testsuite("modmet_helpers_tests", collect_modmet_helpers_tests), &
        ! test 10: lusthov test suite registration
        new_testsuite("modmet_lusthov_tests", collect_modmet_lusthov_tests), &
        ! test 11: version test suite registration
        new_testsuite("modmet_version_tests", collect_modmet_version_tests) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
        error stop
    end if



end program tester
