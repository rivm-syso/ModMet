!------------------------------------------------------------------------------
! Module:     m_modmet_sunhgh
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original SUNHGH routine)
! Created:    June 11 2026
! Updated:    June 11 2026
!------------------------------------------------------------------------------
module m_modmet_sunhgh
    use modmet_constants, only: PI, PI180
   implicit none (type, external)
   private
   public :: modmet_sunhgh
contains
   !! This module computes the sine of the solar elevation angle from
   !!   geographic position and date-time inputs.
   !!   Reference: Holtslag and Van Ulden (1983), JCAM 22, 517-529.
   pure function modmet_sunhgh(lat, lon, mt, dy, hr, min) result(sinphi)

   ! Arguments
      real, intent(in) :: lat
         !! latitude in degrees, positive northward
      real, intent(in) :: lon
         !! longitude in degrees, positive eastward
      integer, intent(in) :: mt
         !! month [1..12]
      integer, intent(in) :: dy
         !! day [1..31]
      integer, intent(in) :: hr
         !! hour [0..23]
      integer, intent(in) :: min
         !! minute [0..59]


      real :: sinphi
         !! Output: Sine of solar elevation angle [-]

      real :: lonr, latr, d, term, sl, sindel, cosdel, h

      d = 30.0 * real(mt - 1) + real(dy)
      lonr = lon / PI180
      latr = lat / PI180
      term = 0.033 * sin(0.0175 * d)
      sl = 4.871 + 0.0175 * d + term
      sindel = 0.398 * sin(sl)
      cosdel = sqrt(1.0 - sindel**2)
      h = lonr + 0.043 * sin(2.0 * sl) - term + 0.262 * (real(hr) + real(min) / 60.0) - PI
      sinphi = sindel * sin(latr) + cosdel * cos(latr) * cos(h)
   end function modmet_sunhgh

end module m_modmet_sunhgh
