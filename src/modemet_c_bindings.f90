!------------------------------------------------------------------------------
! Module:     modmet_c_api
! Authors:    Marte Voorneveld, RIVM
! Created:    July 27 2026
! Updated:    July 27 2026
! Description:
!   Wrappers for ModMet Fortran functions to be called from C/C++.
!   This module provides C-compatible interfaces for various ModMet functions,
!   allowing them to be used in C/C++ applications.
!   It is also possible to use these with Python using ctypes, in the future
!------------------------------------------------------------------------------
module modmet_c_bindings
   use, intrinsic :: iso_c_binding, only: c_char, c_double, c_f_pointer, c_int, c_null_char, c_ptr
   use modmet, only: VERSION, modmet_cloud_fraction, modmet_obuk, modmet_sunhgh
   use m_modmet_fpsim, only: modmet_fpsim, modmet_fpsim_holtslag
   use m_modmet_tst, only: modmet_tst, modmet_tst_result
   use m_modmet_flxln2, only: modmet_flxln2, modmet_flxln2_result
   use m_modmet_lusthov, only: modmet_lusthov, modmet_lusthov_result
   use m_modnet_z0corr, only: modmet_solve_z0_corr

   implicit none (type, external)
   private

   public :: modmet_cloud_fraction_c
   public :: modmet_flxln2_c
   public :: modmet_fpsim_c
   public :: modmet_fpsim_holtslag_c
   public :: modmet_lusthov_c
   public :: modmet_obuk_c
   public :: modmet_solve_z0_corr_c
   public :: modmet_sunhgh_c
   public :: modmet_tst_c
   public :: modmet_version_c

contains

   subroutine modmet_version_c(buffer, buffer_len, out_len) bind(C, name="modmet_version_c")
      type(c_ptr), value, intent(in) :: buffer
      integer(c_int), value, intent(in) :: buffer_len
      integer(c_int), intent(out) :: out_len

      character(c_char), pointer :: buffer_f(:)
      integer :: i, n, writable

      n = len(VERSION)
      out_len = n

      if (buffer_len <= 0) return

      call c_f_pointer(buffer, buffer_f, [buffer_len])

      writable = min(n, buffer_len - 1)
      do i = 1, writable
         buffer_f(i) = VERSION(i:i)
      end do
      buffer_f(writable + 1) = c_null_char
   end subroutine modmet_version_c

   function modmet_obuk_c(ust, tst) result(ol) bind(C, name="modmet_obuk_c")
      real(c_double), value, intent(in) :: ust
      real(c_double), value, intent(in) :: tst
      real(c_double) :: ol

      ol = real(modmet_obuk(real(ust), real(tst)), kind=c_double)
   end function modmet_obuk_c

   function modmet_sunhgh_c(lat, lon, mt, dy, hr, mn) result(sinphi) bind(C, name="modmet_sunhgh_c")
      real(c_double), value, intent(in) :: lat
      real(c_double), value, intent(in) :: lon
      integer(c_int), value, intent(in) :: mt
      integer(c_int), value, intent(in) :: dy
      integer(c_int), value, intent(in) :: hr
      integer(c_int), value, intent(in) :: mn
      real(c_double) :: sinphi

      sinphi = real(modmet_sunhgh(real(lat), real(lon), int(mt), &
         int(dy), int(hr), int(mn)), kind=c_double)
   end function modmet_sunhgh_c

   function modmet_cloud_fraction_c(lat, lon, jcm2, mt, dy, hr)&
          result(cloud_fraction) bind(C, name="modmet_cloud_fraction_c")
      real(c_double), value, intent(in) :: lat
      real(c_double), value, intent(in) :: lon
      real(c_double), value, intent(in) :: jcm2
      integer(c_int), value, intent(in) :: mt
      integer(c_int), value, intent(in) :: dy
      integer(c_int), value, intent(in) :: hr
      real(c_double) :: cloud_fraction

      cloud_fraction = real(modmet_cloud_fraction(real(lat), real(lon), &
         real(jcm2), int(mt), int(dy), int(hr)), kind=c_double)
   end function modmet_cloud_fraction_c

   function modmet_fpsim_c(eta) result(fpsim_value) bind(C, name="modmet_fpsim_c")
      real(c_double), value, intent(in) :: eta
      real(c_double) :: fpsim_value

      fpsim_value = real(modmet_fpsim(real(eta)), kind=c_double)
   end function modmet_fpsim_c

   function modmet_fpsim_holtslag_c(z, ol)&
          result(fpsim_value) bind(C, name="modmet_fpsim_holtslag_c")
      real(c_double), value, intent(in) :: z
      real(c_double), value, intent(in) :: ol
      real(c_double) :: fpsim_value

      fpsim_value = real(modmet_fpsim_holtslag(real(z), real(ol)), kind=c_double)
   end function modmet_fpsim_holtslag_c

   subroutine modmet_tst_c(ust, t, qsti, tst, qst) bind(C, name="modmet_tst_c")
      real(c_double), value, intent(in) :: ust
      real(c_double), value, intent(in) :: t
      real(c_double), value, intent(in) :: qsti
      real(c_double), intent(out) :: tst
      real(c_double), intent(out) :: qst

      type(modmet_tst_result) :: res

      res = modmet_tst(real(ust), real(t), real(qsti))
      tst = real(res%tst, kind=c_double)
      qst = real(res%qst, kind=c_double)
   end subroutine modmet_tst_c

   subroutine modmet_flxln2_c(u1, u2, zu1, zu2, t, cloud_fraction,&
          sinphi, kin, ust, ol, kin_out, tau, h, le) bind(C, name="modmet_flxln2_c")
      real(c_double), value, intent(in) :: u1
      real(c_double), value, intent(in) :: u2
      real(c_double), value, intent(in) :: zu1
      real(c_double), value, intent(in) :: zu2
      real(c_double), value, intent(in) :: t
      real(c_double), value, intent(in) :: cloud_fraction
      real(c_double), value, intent(in) :: sinphi
      real(c_double), value, intent(in) :: kin
      real(c_double), intent(out) :: ust
      real(c_double), intent(out) :: ol
      real(c_double), intent(out) :: kin_out
      real(c_double), intent(out) :: tau
      real(c_double), intent(out) :: h
      real(c_double), intent(out) :: le

      type(modmet_flxln2_result) :: res

      res = modmet_flxln2(real(u1), real(u2), real(zu1), real(zu2), real(t), &
         real(cloud_fraction), real(sinphi), real(kin))
      ust = real(res%ust, kind=c_double)
      ol = real(res%ol, kind=c_double)
      kin_out = real(res%kin, kind=c_double)
      tau = real(res%tau, kind=c_double)
      h = real(res%h, kind=c_double)
      le = real(res%le, kind=c_double)
   end subroutine modmet_flxln2_c

   subroutine modmet_lusthov_c(mt, dy, hr, mn, lat, lon, kin, z0,&
            zra, u_zra, t, cloud_fraction, ust, ol,&
            kin_out, h, evap, tst, qst) bind(C, name="modmet_lusthov_c")
      integer(c_int), value, intent(in) :: mt
      integer(c_int), value, intent(in) :: dy
      integer(c_int), value, intent(in) :: hr
      integer(c_int), value, intent(in) :: mn
      real(c_double), value, intent(in) :: lat
      real(c_double), value, intent(in) :: lon
      real(c_double), value, intent(in) :: kin
      real(c_double), value, intent(in) :: z0
      real(c_double), value, intent(in) :: zra
      real(c_double), value, intent(in) :: u_zra
      real(c_double), value, intent(in) :: t
      real(c_double), value, intent(in) :: cloud_fraction
      real(c_double), intent(out) :: ust
      real(c_double), intent(out) :: ol
      real(c_double), intent(out) :: kin_out
      real(c_double), intent(out) :: h
      real(c_double), intent(out) :: evap
      real(c_double), intent(out) :: tst
      real(c_double), intent(out) :: qst

      type(modmet_lusthov_result) :: res

      res = modmet_lusthov(int(mt), int(dy), int(hr), int(mn), real(lat), &
         real(lon), real(kin), real(z0), real(zra), real(u_zra), real(t), &
         real(cloud_fraction))

      ust = real(res%ust, kind=c_double)
      ol = real(res%ol, kind=c_double)
      kin_out = real(res%kin, kind=c_double)
      h = real(res%h, kind=c_double)
      evap = real(res%evap, kind=c_double)
      tst = real(res%tst, kind=c_double)
      qst = real(res%qst, kind=c_double)

   end subroutine modmet_lusthov_c

   subroutine modmet_solve_z0_corr_c(z0_in, z0_lu, ol_old, ust_old, max_iter,&
          tol, min_change, ol_new, ust_new) bind(C, name="modmet_solve_z0_corr_c")
      real(c_double), value, intent(in) :: z0_in
      real(c_double), value, intent(in) :: z0_lu
      real(c_double), value, intent(in) :: ol_old
      real(c_double), value, intent(in) :: ust_old
      integer(c_int), value, intent(in) :: max_iter
      real(c_double), value, intent(in) :: tol
      real(c_double), value, intent(in) :: min_change
      real(c_double), intent(out) :: ol_new
      real(c_double), intent(out) :: ust_new

      real :: ol_new_f, ust_new_f

      call modmet_solve_z0_corr(real(z0_in), real(z0_lu), real(ol_old), real(ust_old),&
          ol_new_f, ust_new_f, int(max_iter), real(tol), real(min_change))

      ol_new = real(ol_new_f, kind=c_double)
      ust_new = real(ust_new_f, kind=c_double)
   end subroutine modmet_solve_z0_corr_c

end module modmet_c_bindings
