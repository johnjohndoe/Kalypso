!     Last change:  WP   13 Mar 2006    9:27 am
!--------------------------------------------------------------------------
! This code, check_dialog.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - check_dialog
!
! Copyright (C) 2005  WOLF PLOEGER.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 19 Juli 2005
! Research Associate
!***********************************************************************



!-----------------------------------------------------------------------------
subroutine Check_Dialog(fehlnr, selected)
!
! Beschreibung:
! -------------
! Bei bestimmten Programmabruchen wird ein Fenster geoeffnet,
! das dem Benutzer die Fehlermeldung direkt zeigt.
!
! Der gezeigt Text in den Popup Fenstern muss in RESOURCE.RC
! eingetragen werden!
!
!                              Wolf Ploeger, 12.03.2006
! ----------------------------------------------------------------------------

!USE WINTERACTER
!
!IMPLICIT NONE
!
!INTEGER, INTENT(IN)  :: fehlnr
!INTEGER, INTENT(OUT) :: selected
!
!! Probleme bei Wehren
!INTEGER, PARAMETER :: IDD_WEHR1  =   101
!INTEGER, PARAMETER :: IDD_WEHR2  =   102
!
!! Probleme bei der Geometrie
!INTEGER, PARAMETER :: IDD_BV1    =   111
!
!TYPE(WIN_STYLE)   :: MAIN_WINDOW
!TYPE(WIN_MESSAGE) :: MESSAGE
!
!INTEGER           :: ITYPE
!
!!  Initialise WiSK and open the main window.
!!  This will also activate the main menu for the program.
!
!CALL WInitialise('')
!MAIN_WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton + HideRoot
!MAIN_WINDOW%X      = -1
!MAIN_WINDOW%Y      = -1
!MAIN_WINDOW%WIDTH  = 0
!MAIN_WINDOW%HEIGHT = 0
!MAIN_WINDOW%MENUID = 0
!MAIN_WINDOW%TITLE  = 'KALYPSO-1D Dialog Fenster'
!
!CALL WindowOpen(MAIN_WINDOW)
!
!select case (fehlnr)
!
!  case (1)
!
!    CALL WDialogLoad(IDD_WEHR1)
!    CALL WDialogSelect(IDD_WEHR1)
!    CALL WDialogShow(-1,-1,0,Modal)
!    IF (WInfoDialog(ExitButton) == IDOK) THEN
!      selected = 1
!      CALL WindowClose()
!      RETURN
!    ELSE
!      selected = 0
!      CALL WindowClose()
!      RETURN
!    END IF                                    
!
!  case (2)
!
!    CALL WDialogLoad(IDD_WEHR2)
!    CALL WDialogSelect(IDD_WEHR2)
!    CALL WDialogShow(-1,-1,0,Modal)
!    IF (WInfoDialog(ExitButton) == IDOK) THEN
!      selected = 1
!      CALL WindowClose()
!      RETURN
!    ELSE
!      selected = 0
!      CALL WindowClose()
!      RETURN
!    END IF                                    
!
!  case (11)
!
!    CALL WDialogLoad(IDD_BV1)
!    CALL WDialogSelect(IDD_BV1)
!    CALL WDialogShow(-1,-1,0,Modal)
!    IF (WInfoDialog(ExitButton) == IDOK) THEN
!      selected = 1
!      CALL WindowClose()
!      RETURN
!    ELSE
!      selected = 0
!      CALL WindowClose()
!      RETURN
!    END IF                                    
!
!  case default
!
!end select


end subroutine Check_Dialog





