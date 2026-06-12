!------------------------------------------------------------------------------
! Module:     modmet_constants
! Authors:    Marte Voorneveld, RIVM
!             Anton Beljaars, KNMI (source parameterization basis)
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This module defines physical and empirical constants used throughout
!   the ModMet library.
!   Constants and defaults follow the original KNMI routines that formed
!   the basis of this package.
!------------------------------------------------------------------------------
module modmet_constants
   implicit none (type, external)
   public

   ! Fundamental constants
   real, parameter :: VONK = 0.4                 ! von Karman constant [-]
   real, parameter :: GRAVITY = 9.8              ! gravitational acceleration [m/s^2]
   real, parameter :: KELVIN_OFFSET = 273.15     ! offset to convert Celsius to Kelvin [K]
   real, parameter :: CP_AIR = 1005.0

   ! Reference-state constants
   real, parameter :: TR = KELVIN_OFFSET + 9.85  ! reference temperature [K] (9.85 C)
   real, parameter :: PRESSURE_REF = 1005.0      ! reference pressure [hPa]
   real, parameter :: RO = PRESSURE_REF / (2.87 * TR) ! reference density [kg/m^3]

   ! Thermodynamic constants
   real, parameter :: LAMBDA = 2465.0 - 2.38 * (10.0 - 15.0)
   real, parameter :: GAMMA = CP_AIR / (LAMBDA * 0.622) ! psychrometric constant (K/hPa)

   ! Surface and scheme parameters
   real, parameter :: ALBEDO = 0.23              ! surface albedo [-]
   real, parameter :: D1 = 15.0                  ! empirical night-scheme constant [-]
   real, parameter :: ALFA = 1.0                 ! Priestley-Taylor day-scheme coefficient [-]
   real, parameter :: AG = 5.0                   ! soil heat transfer coefficient
   real, parameter :: AL_VEG = 0.23              ! typical albedo for vegetation [-]

   ! Numerical constants
   real, parameter :: EPS = 1.0e-6              ! small number for numerical stability
   real, parameter :: PI = 3.14159265358979323846
   real, parameter :: PI180 = 180.0 / PI        ! degree/radian conversion factor angle / PI180
   real, parameter :: PID2 = PI / 2.0

end module modmet_constants
