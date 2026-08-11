!------------------------------------------------------------------------------
! Module:     m_modmet_sunhgh
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (original SUNHGH routine)
! Created:    June 11 2026
! Updated:    June 11 2026
!------------------------------------------------------------------------------
module m_modmet_sunhgh
    use modmet_constants, only: RK, PI, PI180
   implicit none (type, external)
   private
   public :: modmet_sunhgh
contains
   !! This module computes the sine of the solar elevation angle from
   !!   geographic position and date-time inputs.
   !!   Reference: Holtslag and Van Ulden (1983), JCAM 22, 517-529.
   pure function modmet_sunhgh(lat, lon, mt, dy, hr, min) result(sinphi)

   ! Arguments
      real(RK), intent(in) :: lat
         !! latitude in degrees, positive northward
      real(RK), intent(in) :: lon
         !! longitude in degrees, positive eastward
      integer, intent(in) :: mt
         !! month [1..12]
      integer, intent(in) :: dy
         !! day [1..31]
      integer, intent(in) :: hr
         !! hour [0..23]
      integer, intent(in) :: min
         !! minute [0..59]


      real(RK) :: sinphi
         !! Output: Sine of solar elevation angle [-]

      real(RK) :: lonr, latr, d, term, sl, sindel, cosdel, h

      d = 30.0_RK * real(mt - 1, kind=RK) + real(dy, kind=RK)
      lonr = lon / PI180
      latr = lat / PI180
      term = 0.033_RK * sin(0.0175_RK * d)
      sl = 4.871_RK + 0.0175_RK * d + term
      sindel = 0.398_RK * sin(sl)
      cosdel = sqrt(1.0_RK - sindel**2)
      h = lonr + 0.043_RK * sin(2.0_RK * sl) - term + 0.262_RK * &
         (real(hr, kind=RK) + real(min, kind=RK) / 60.0_RK) - PI
      sinphi = sindel * sin(latr) + cosdel * cos(latr) * cos(h)
   end function modmet_sunhgh

end module m_modmet_sunhgh
