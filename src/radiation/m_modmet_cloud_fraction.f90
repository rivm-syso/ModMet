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

      real, intent(in) :: lat
      !! latitude [degrees]
      real, intent(in) :: lon
      !! longitude [degrees]
      real, intent(in) :: jcm2
      !! global radiation [J/cm^2]
      integer, intent(in) :: mt
      !! month [1..12]
      integer, intent(in) :: dy
      !! day [1..31]
      integer, intent(in) :: hr
      !! hour [0..23]
      real :: cloud_fraction
      !! estimated cloud fraction [0..1] or -999.0

      ! local variables
      real :: wl, tlat, t, d,dd, ddd, sl, decli, hangle, sifi, el, globgem, globber
      real :: w1, w2
      ! coefficients for global radiation to cloud fraction conversion
      integer, parameter :: a1 = 1041, a2 = -69
      real, parameter :: b1 = -.75, b2 = 3.4

      w2 = 0.6
      w1 = w2


      globgem = jcm2 * 2.78 ! convert J/cm^2 to W/m^2 (assuming 1 hour integration)

      if (jcm2 <= 0) then
         cloud_fraction = w2
         return
      end if

      if (mt < 1 .or. mt > 12 .or. dy < 1 .or. dy > 31 .or. hr < 0 .or. hr > 23) then
         cloud_fraction = -999.0
         return
      end if



      wl = (360.0 - lon) / 57.295
      tlat = lat / 57.295


      t = real(hr) - 0.5
      d      = 30 * (mt - 1) + dy
      dd     = .01755 * d
      ddd    = .033 * sin(dd)
      sl     = 4.871 + dd + ddd
      decli  = asin(.398 * sin(sl))
      hangle = -wl + .043 * sin(2 * sl) - ddd + .262 * t - 3.1416

      sifi   = sin(decli) *sin(tlat) + cos(decli) *cos(tlat) *cos(hangle)

      el   = asin(sifi)

      globber = a1 * sifi + a2

      if (globber < 0.0) globber = 0.0

      if ((globgem / globber) < 1.0) then
         cloud_fraction = ( (globgem / globber - 1.0) / b1 )**(1.0 / b2)
         if (cloud_fraction > 1.0) cloud_fraction = 1.0
      else
         if (globgem > 100.0) then
            cloud_fraction = 0.0
         else
            cloud_fraction = w1
         end if
      end if

   end function modmet_cloud_fraction

end module m_modmet_cloud_fraction
