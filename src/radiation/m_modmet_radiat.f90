!------------------------------------------------------------------------------
! Module:     m_modmet_radiat
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original RADIAT routine)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module computes isothermal net radiation components from solar
!   geometry, cloudiness proxy, and incoming shortwave radiation.
!   Reference: Van Ulden and Holtslag (1985), JCAM 24, 1196-1207.
!------------------------------------------------------------------------------
module m_modmet_radiat
use modmet_constants, only: TR, RO, LAMBDA, GAMMA, AL_VEG
use m_modmet_helpers, only: modmet_missing
    implicit none (type, external)

    type :: modmet_radiat_result
        real :: kin
        real :: qsti
    end type modmet_radiat_result

    private
    public :: modmet_radiat, modmet_radiat_result

    ! Physical constants and parameters used in radiation calculations
    real, parameter :: A1 = 990.0
    real, parameter :: A2 = -30.0
    real, parameter :: B1 = 0.75
    real, parameter :: B2 = 3.4
    real, parameter :: C1 = 9.35e-6
    real, parameter :: C2 = 60.0
    real, parameter :: SIGMA = 5.67e-8
    real, parameter :: SPHI0 = 0.03

contains
    pure function modmet_radiat(sinphi, n, kin) result(result)
    ! ===========================================================
    ! Function: modmet_radiat
    ! Description: Computes incoming shortwave radiation and isothermal
    !              net radiation. If kin is missing, kin is estimated.
    ! input: sinphi - sine of solar elevation angle [-]
    ! input: n      - cloudiness factor [-]
    ! input: kin    - incoming shortwave radiation [W/m^2] or missing
    ! output: result%kin  - incoming shortwave radiation used [W/m^2]
    ! output: result%qsti - isothermal net radiation [W/m^2]
    ! ===========================================================
        real, intent(in) :: sinphi, n
        real, intent(in) :: kin
        real :: sphi, kst, lsti
        type(modmet_radiat_result) :: result

        ! NET SHORT WAVE RADIATION K*
        sphi = sinphi
        result%kin = kin  ! Initialize result%kin with the input kin value

        ! FOR PHI < 0.03 : NO SHORT WAVE RADIATION
        if (sphi < SPHI0) then
            kst = 0.0
            if (kin < 0.0) result%kin = 0.0
        else
            ! Check if kin is undefined or an error value
            ! Note: Using a sentinel value check may need revision based on actual error handling
            if (modmet_missing(kin)) then
                kst = (A1 * sphi + A2) * (1.0 - B1 * n**B2) * (1.0 - AL_VEG)
                result%kin = kst / (1.0 - AL_VEG)
            else
                kst = kin * (1.0 - AL_VEG)
                result%kin = kin
            end if
        end if

        ! ISOTHERMAL LONGWAVE RADIATION L*I
        lsti = -SIGMA * TR**4 * (1.0 - C1 * TR**2) + C2 * n

        ! ISOTHERMAL NET RADIATION Q*I
        result%qsti = kst + lsti

    end function modmet_radiat

! LCOV_EXCL_START
end module m_modmet_radiat
! LCOV_EXCL_END
