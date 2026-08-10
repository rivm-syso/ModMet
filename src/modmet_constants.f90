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
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none (type, external)
   public

   ! Working precision kind
   integer, parameter :: RK = real64          ! double precision real kind [-]

   ! Fundamental constants
   real(RK), parameter :: VONK = 0.4_RK                 ! von Karman constant [-]
   real(RK), parameter :: GRAVITY = 9.8_RK              ! gravitational acceleration [m/s^2]
   real(RK), parameter :: KELVIN_OFFSET = 273.15_RK     ! offset to convert Celsius to Kelvin [K]
   real(RK), parameter :: CP_AIR = 1005.0_RK

   ! Reference-state constants
   real(RK), parameter :: TR = KELVIN_OFFSET + 9.85_RK  ! reference temperature [K] (9.85 C)
   real(RK), parameter :: PRESSURE_REF = 1005.0_RK      ! reference pressure [hPa]
   real(RK), parameter :: RO = PRESSURE_REF / (2.87_RK * TR) ! reference density [kg/m^3]

   ! Thermodynamic constants
   real(RK), parameter :: LAMBDA = 2465.0_RK - 2.38_RK * (10.0_RK - 15.0_RK)
   real(RK), parameter :: GAMMA = CP_AIR / (LAMBDA * 0.622_RK) ! psychrometric constant (K/hPa)

   ! Surface and scheme parameters
   real(RK), parameter :: ALBEDO = 0.23_RK              ! surface albedo [-]
   real(RK), parameter :: D1 = 15.0_RK                  ! empirical night-scheme constant [-]
   real(RK), parameter :: ALFA = 1.0_RK                 ! Priestley-Taylor day-scheme coefficient [-]
   real(RK), parameter :: AG = 5.0_RK                   ! soil heat transfer coefficient
   real(RK), parameter :: AL_VEG = 0.23_RK              ! typical albedo for vegetation [-]

   ! Numerical constants
   real(RK), parameter :: EPS = 1.0e-6_RK              ! small number for numerical stability
   real(RK), parameter :: PI = 3.14159265358979323846_RK
   real(RK), parameter :: PI180 = 180.0_RK / PI        ! degree/radian conversion factor
   real(RK), parameter :: PID2 = PI / 2.0_RK

end module modmet_constants
