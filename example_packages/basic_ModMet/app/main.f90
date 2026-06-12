program basic_modmet
   use modmet, only: modmet_flxln2, modmet_flxln2_result
   implicit none (type, external)

   type(modmet_flxln2_result) :: result
   real :: u1, u2, zu1, zu2, T, cloud_fraction
   real :: sinphi, kin



   ! test 1: daytime reference conditions
   u1 = 0.0
   u2 = 5.0
   zu1 = 0.10 ! roughness length
   zu2 = 10.0 ! height of the second level
   T = 15.0 ! temperature in C
   kin = 200.0 ! short wave radiation in W/m^2
   cloud_fraction = 0.1
   sinphi = 0.8 ! sine of the solar elevation angle

   result = modmet_flxln2(u1, u2, zu1, zu2, T, cloud_fraction, sinphi, kin)

   print *, "Friction velocity (ust): ", result%ust
   print *, "Obukhov length (ol): ", result%ol

end program basic_modmet
