!------------------------------------------------------------------------------
! Module:     m_modmet_cloud_fraction
! Authors:    Marte Voorneveld, RIVM
!             J.A. van Jaarsveld, RIVM (original CLOUDGMT routine)
! Created:    June 10 2026
! Updated:    June 11 2026
! Description:
!   This module computes cloud fraction from global radiation and
!   astronomical position for a given date, time, and location.
!   Based on KNMI WR 83-4 cloud-cover parameterization.
!------------------------------------------------------------------------------
module m_modmet_cloud_fraction
   use modmet_constants, only: RK
   implicit none (type, external)
   private
   public :: modmet_cloud_fraction


contains
   ! ===========================================================
   ! Function: modmet_cloud_fraction
   ! Description: Estimates cloud fraction from measured global radiation.
   !              Returns -999.0 for invalid month/day/hour input.
   ! input: lat   - latitude [degrees]
   ! input: lon   - longitude [degrees]
   ! input: jcm2  - global radiation [J/cm^2]
   ! input: mt    - month [1..12]
   ! input: dy    - day [1..31]
   ! input: hr    - hour [0..23]
   ! output: cloud_fraction - estimated cloud fraction [0..1] or -999.0
   ! ===========================================================
   !! Estimates cloud fraction from measured global radiation and solar geometry.
   !!   Reference: KNMI WR 83-4 cloud-cover parameterization.
   function modmet_cloud_fraction(lat, lon, jcm2, mt, dy, hr) result(cloud_fraction)

      real(RK), intent(in) :: lat
      !! latitude [degrees]
      real(RK), intent(in) :: lon
      !! longitude [degrees]
      real(RK), intent(in) :: jcm2
      !! global radiation [J/cm^2]
      integer, intent(in) :: mt
      !! month [1..12]
      integer, intent(in) :: dy
      !! day [1..31]
      integer, intent(in) :: hr
      !! hour [0..23]
      real(RK) :: cloud_fraction
      !! estimated cloud fraction [0..1] or -999.0

      ! local variables
      real(RK) :: wl, tlat, t, d,dd, ddd, sl, decli, hangle, sifi, el, globgem, globber
      real(RK) :: w1, w2
      ! coefficients for global radiation to cloud fraction conversion
      integer, parameter :: a1 = 1041, a2 = -69
      real(RK), parameter :: b1 = -.75_RK, b2 = 3.4_RK

      w2 = 0.6_RK
      w1 = w2


      globgem = jcm2 * 2.78_RK ! convert J/cm^2 to W/m^2 (assuming 1 hour integration)

      if (jcm2 <= 0) then
         cloud_fraction = w2
         return
      end if

      if (mt < 1 .or. mt > 12 .or. dy < 1 .or. dy > 31 .or. hr < 0 .or. hr > 23) then
         cloud_fraction = -999.0_RK
         return
      end if



      wl = (360.0_RK - lon) / 57.295_RK
      tlat = lat / 57.295_RK


      t = real(hr, kind=RK) - 0.5_RK
      d      = 30 * (mt - 1) + dy
      dd     = .01755_RK * d
      ddd    = .033_RK * sin(dd)
      sl     = 4.871_RK + dd + ddd
      decli  = asin(.398_RK * sin(sl))
      hangle = -wl + .043_RK * sin(2 * sl) - ddd + .262_RK * t - 3.1416_RK

      sifi   = sin(decli) *sin(tlat) + cos(decli) *cos(tlat) *cos(hangle)

      el   = asin(sifi)

      globber = a1 * sifi + a2

      if (globber < 0.0_RK) globber = 0.0_RK

      if ((globgem / globber) < 1.0_RK) then
         cloud_fraction = ( (globgem / globber - 1.0_RK) / b1 )**(1.0_RK / b2)
         if (cloud_fraction > 1.0_RK) cloud_fraction = 1.0_RK
      else
         if (globgem > 100.0_RK) then
            cloud_fraction = 0.0_RK
         else
            cloud_fraction = w1
         end if
      end if

   end function modmet_cloud_fraction

end module m_modmet_cloud_fraction
