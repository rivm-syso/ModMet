!------------------------------------------------------------------------------
! Module:     m_modmet_lusthov
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original LUSTHOV routine basis)
! Created:    June 12 2026
! Updated:    June 12 2026
! Description:
!   Integrated surface-layer routine that computes friction velocity,
!   Monin-Obukhov length, sensible heat flux, and evaporation from
!   meteorological forcing and surface parameters.
!   Reference: Beljaars, Holtslag, and Van Westrhenen (1989), KNMI TR-112.
!------------------------------------------------------------------------------
module m_modmet_lusthov
   use modmet_constants, only: LAMBDA
   use m_modmet_sunhgh, only: modmet_sunhgh
   use m_modmet_flxln2, only: modmet_flxln2, modmet_flxln2_result
   use m_modmet_helpers, only: modmet_missing
   implicit none (type, external)

   type :: modmet_lusthov_result
      real :: ust
      real :: qst
      real :: ol
      real :: kin
      real :: h ! sensible heat flux
      real :: evap ! evaporation rate
      real :: tst ! temperature scale
   end type modmet_lusthov_result

   private
   public :: modmet_lusthov, modmet_lusthov_result
contains
   ! ===========================================================
   ! Function: modmet_lusthov
   ! Description: Computes near-surface turbulence and flux outputs
   !              from date/time, location, radiation, and wind inputs.
   !              This implementation supports the kin-based pathway
   !              only; the legacy qnet pathway is intentionally removed.
   ! input: mt             - month [1..12]
   ! input: dy             - day [1..31]
   ! input: hr             - hour (GMT) [0..23]
   ! input: min            - minute [0..59]
   ! input: lon            - longitude [degrees, east positive]
   ! input: lat            - latitude [degrees, north positive]
   ! input: kin            - incoming shortwave radiation [W/m^2]
   ! input: z0             - surface roughness length [m]
   ! input: zra            - wind measurement/evaluation height [m]
   ! input: u_zra          - wind speed at zra [m/s]
   ! input: t              - air temperature [degC]
   ! input: cloud_fraction - cloud fraction [-] (0..1)
   ! output: result%ol     - Monin-Obukhov length [m]
   ! output: result%ust    - friction velocity [m/s]
   ! output: result%kin    - shortwave radiation used [W/m^2]
   ! output: result%h      - sensible heat flux [W/m^2]
   ! output: result%evap   - evaporation rate [mm/hr]
   ! ===========================================================

   !! Computes near-surface turbulence and flux outputs from forcing and surface inputs.
   !!   Reference: Beljaars, Holtslag, and Van Westrhenen (1989), KNMI TR-112.
   pure function modmet_lusthov(mt, dy, hr, min,&
        lat, lon, &
        kin, z0, &
        zra, u_zra, &
        T, cloud_fraction &
    ) result(result)
         integer, intent(in) :: mt
         !! month [1..12]
         integer, intent(in) :: dy
         !! day [1..31]
         integer, intent(in) :: hr
         !! hour (GMT) [0..23]
         integer, intent(in) :: min
         !! minute [0..59]
         real, intent(in) :: lat
         !! latitude [degrees, north positive]
         real, intent(in) :: lon
         !! longitude [degrees, east positive]
         real, intent(in) :: kin
         !! incoming shortwave radiation [W/m^2]
         real, intent(in) :: z0
         !! surface roughness length [m]
         real, intent(in) :: zra
         !! wind measurement/evaluation height [m]
         real, intent(in) :: u_zra
         !! wind speed at zra [m/s]
         real, intent(in) :: T
         !! air temperature [degC]
         real, intent(in) :: cloud_fraction
         !! cloud fraction [-] (0..1)

        type(modmet_flxln2_result) :: flxln2_result
        type(modmet_lusthov_result) :: result
        real :: sinphi


        sinphi = modmet_sunhgh(lat, lon, mt, dy, hr, min)
        flxln2_result = modmet_flxln2(0.0, u_zra, z0, zra, T, cloud_fraction, sinphi, kin)

        if(modmet_missing(flxln2_result%ust)) then
            result%ust = -9999.0
            result%ol = -9999.0
            result%kin = -9999.0
            result%h = -9999.0
            result%evap = -9999.0
            result%tst = -9999.0
            result%qst = -9999.0
            return
        end if
        result%ust = flxln2_result%ust
        result%ol = flxln2_result%ol
        result%kin = flxln2_result%kin
        result%h = flxln2_result%h
        result%evap = 3.6 * flxln2_result%le/LAMBDA
        result%tst = flxln2_result%tst
        result%qst = flxln2_result%qst

   end function modmet_lusthov

! LCOV_EXCL_START
end module m_modmet_lusthov
! LCOV_EXCL_END
