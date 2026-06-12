!------------------------------------------------------------------------------
! Module:     m_modmet_tst
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original TST2 routine)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module computes temperature and humidity scales used in
!   Monin-Obukhov turbulence parameterization.
!   Reference: Van Ulden and Holtslag (1985), JCAM 24, 1196-1207.
!------------------------------------------------------------------------------

module m_modmet_tst
    use modmet_constants, only: KELVIN_OFFSET, RO, CP_AIR, LAMBDA, ALFA, AG, D1, TR
    implicit none (type, external)
   type :: modmet_tst_result
      real :: tst ! Temperature scale
      real :: qst ! Humidity scale
   end type modmet_tst_result
   private
   public :: modmet_tst, modmet_tst_result
contains
   pure function modmet_tst(ust, t, qsti) result(res)
   ! ===========================================================
   ! Function: modmet_tst
   ! Description: Computes temperature scale (tst) and humidity
   !              scale (qst) for day and night stability regimes.
   ! input: ust  - friction velocity [m/s]
   ! input: t    - air temperature [degC]
   ! input: qsti - isothermal net radiation [W/m^2]
   ! output: res%tst - temperature scale
   ! output: res%qst - humidity scale
   ! ===========================================================

      real, intent(in) :: ust, t, qsti
      type(modmet_tst_result) :: res

      real :: ta, s, vst, d2, d3, d4, tst, qmg, cg, ch
      real, parameter :: sigma = 5.67e-8, gz5 = 50.0, gammad = 0.01, zr = 50.0

      ta = t + KELVIN_OFFSET - 0.15 ! old temperature conversion was t + 273.
      ! actual Kelvin offset is 273.15
      s  = exp(0.055 * (ta - 279.0))

      if (qsti < 0.0) then
         ! Night scheme
         vst = ust / gz5

         d2  = 0.5 * (1.0 + s) * RO * CP_AIR * gz5 / (4.0 * sigma * TR**3 + AG)
         d3  = -qsti / (4.0 * sigma * TR**4 + AG * TR) + gammad * zr / TR
         d4  = d2 * 0.033 * 2.0 / TR

         tst = TR * (sqrt((D1 * vst**2 + d2 * vst**3)**2 + d3 * vst**2 + d4 * vst**3) -&
             D1 * vst**2 - d2 * vst**3)
         qmg = qsti + (4.0 * sigma * TR**3 + AG) * &
         (2.0 * tst * D1 + (tst / vst)**2 / TR - gammad * zr)
      else
         ! Day scheme
         ch  = 0.38 * (((1.0 - ALFA) * s + 1.0) / (s + 1.0))
         cg  = (AG / (4.0 * sigma * ta**3)) * ch
         qmg = (1.0 - cg) * qsti / (1.0 + ch)
         tst = (-((1.0 - ALFA) * s + 1.0) * qmg / (s + 1.0)) / (RO * CP_AIR * ust) + ALFA * 0.033
      end if
      res%tst = tst
      res%qst = (-qmg - RO * CP_AIR * ust * tst) / (RO * LAMBDA * ust)
   end function modmet_tst

! LCOV_EXCL_START
end module m_modmet_tst
! LCOV_EXCL_END
