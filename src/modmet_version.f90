!------------------------------------------------------------------------------
! Module:     modmet_version
! Authors:    Marte Voorneveld, RIVM
! Created:    August 11 2026
! Updated:    August 11 2026
! Description:
!   Version metadata for the ModMet public API.
!   This module is updated by CI when creating automated releases.
!------------------------------------------------------------------------------
module modmet_version
   implicit none (type, external)
   private

   character(len=*), parameter, public :: VERSION = "1.2.1"
   character(len=*), parameter, public :: RELEASE_DATE = "2026-08-12"
   character(len=*), parameter, public :: BUILD_DATE = RELEASE_DATE

end module modmet_version
