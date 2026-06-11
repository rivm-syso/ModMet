!------------------------------------------------------------------------------
! Module:     modmet
! Authors:    Marte Voorneveld, RIVM
! Created:    June 11 2026
! Updated:    June 11 2026
! Description:
!   This is the main public API module for the ModMet library.
!   It re-exports helper, radiation, turbulence, and solver routines
!   so users can import a single module for core meteorological
!   calculations and supporting numerical methods.
!------------------------------------------------------------------------------
module modmet
  use m_modmet_helpers, only: modmet_missing

  ! radiation
  use m_modmet_radiat, only: modmet_radiat, modmet_radiat_result
  use m_modmet_sunhgh, only: modmet_sunhgh
  use m_modmet_cloud_fraction, only: modmet_cloud_fraction

  ! turbulence
  use m_modmet_flxln2, only: modmet_flxln2, modmet_flxln2_result
  use m_modmet_fpsim, only: modmet_fpsim
  use m_modmet_obuk, only: modmet_obuk
  use m_modmet_tst, only: modmet_tst, modmet_tst_result

  ! solvers
  use m_modmet_find_zero, only: modmet_find_zero, modmet_solver_result

  implicit none (type, external)
  private


  public :: modmet_missing, &
  ! radiation
      modmet_radiat, modmet_radiat_result, &
      modmet_sunhgh, modmet_cloud_fraction, &
  ! turbulence
      modmet_flxln2, modmet_flxln2_result, &
      modmet_fpsim, &
      modmet_obuk, &
      modmet_tst, modmet_tst_result, &
  ! solvers
      modmet_find_zero, modmet_solver_result

end module modmet
