!     Last change:  MD   23 Jul 2008   11:53 am
!--------------------------------------------------------------------------
! This code, wspow.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - wspow
!
! Copyright (C) 2004  ULF TESCHKE & WOLF PLOEGER.
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
! Ulf Teschke:      phone: +49 40 42878 3895 mail: teschke@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 August 2004
! Research Associate
!***********************************************************************

SUBROUTINE wspow (henow, strbr, q, q1, hrow, hv, rg, indmax, hvst,&
     & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz,    &
     & idr1, Q_Abfrage)

!***********************************************************************
!**                                                                     
!**   Subroutine wspow                                                  
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG DES WASSERSPIEGELS IM OBERWASSER         
!JK                 AUS DER ENERGIEHOEHE                                
!**                                                                     
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   df      --      Differenz                                         
!**   dif     --      Differenz                                         
!**   dx      --      Schrittweite                                      
!**   he1     --      Energiehöhe                                       
!**   hea     --      Energiehöhe                                       
!**   henow   --      Energiehöhe im Oberwasser                         
!**   hranf   --      Anfangswasserspiegelhöhe                          
!**   hrow    --      Wasserspiegelhöhe im Oberwasser                   
!**   hrowa   --      Wasserspiegelhöhe im Oberwasser                   
!**   hukmax  --      maximale Höhe der Unterkante                      
!**   hv      --      Geschwindigkeitsverlust                           
!**   iwehr   --      Abfrage nach Wehrprofil                           
!**   wsanf   --      Anfangswasserspiegelhöhe
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   wspanf(hrow,strbr,q,q1,hr,hv,rg,indmax,hvst,hrst,                 
!**          psieins,psiorts,nprof,hgrenz,ikenn,nblatt,nz)
!**                                                                     
!***********************************************************************
                                                                        
! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

implicit none

! Calling variables
REAL :: henow
REAL :: strbr
REAL :: q
REAL :: q1
REAL :: hrow, hvow
REAL :: hv
REAL :: rg
INTEGER :: indmax
REAL :: hvst
REAL :: hrst
REAL :: psieins, psiorts
INTEGER :: nprof
REAL :: hgrenz
INTEGER :: ikenn
INTEGER :: nblatt
INTEGER :: nz
CHARACTER(LEN=1) :: idr1
                                                                        

! COMMON-Block /BRUECK/ ------------------------------------------------------------
INTEGER 	:: iwl, iwr, nuk, nok
INTEGER 	:: iokl, iokr           ! Gelaende Oberkante Grenze
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk, hokmin
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, iokl, iokr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, hokmin, ibridge
! ----------------------------------------------------------------------------------


! COMMON-Block /WEHR/ ---------------------------------------------------------
REAL 		:: xokw (maxkla), hokw (maxkla)
INTEGER 	:: nokw, iwmin
REAL            :: hokwmin
CHARACTER(LEN=1):: iwehr
REAL 		:: xtrw (maxw), htrw (maxw)
INTEGER         :: nwfd
INTEGER 	:: iendw (maxw), ianfw (maxw)
COMMON / wehr / xokw, hokw, nokw, iwmin, hokwmin, iwehr, xtrw, htrw, nwfd, iendw, ianfw
! -----------------------------------------------------------------------------

! Local variables
INTEGER :: i
CHARACTER(LEN=2) :: wart
INTEGER :: itmax_ow

REAL :: dx
REAL :: hr, hra, hranf, hrowa
REAL :: he1, hea
REAL :: hva
REAL :: dif, df
REAL :: wsanf
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     ! Abfrage fuer Ende der Inneren Q-Schleife

! ------------------------------------------------------------------
! PROGRAMMBEGINN
! ------------------------------------------------------------------
                                                                        
!write (*,*) 'In WSPOW. Start Subroutine.'

!MD  IF (iwehr.eq.'w') hukmax = 0.0
!MD  IF (iwehr.eq.'w') hukmax = hokwmin

!MD  Nur fuer echte Wehre, keine Breucken
IF (iwehr.eq.'w') then
  hukmax = hokwmin
  hr = hrow
endif
                                                                        
dx = 0.05
itmax_ow = 20
hranf = henow - 0.05
! hranf = henow

hrow = hranf
                                                                        
!JK    SCHREIBEN IN KONTROLLFILE                                        
WRITE (UNIT_OUT_LOG, '(''bestimmung des wasserspiegels im oberwasser'',    &
   &     '' aus der energiehoehe im oberwasser'',/,''henow= '',       &
   &     f8.4)') henow


!JK    START ITERATIONSSCHLEIFE ZUR BERECHNUNG WSP IM OBERWASSER
!JK    ---------------------------------------------------------
DO 10 i = 1, itmax_ow

  !JK      SCHREIBEN IN KONTROLLFILE
  write (UNIT_OUT_LOG, '(''it= '',i2)') i


  CALL wspanf (hrow, strbr, q, q1, hr, hv, rg, indmax, hvst, hrst,&
   & psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1, Q_Abfrage)

  !MD !MD !MD  he1 = hr + hv
  !MD he1 = hrow + hv
  IF (iwehr.eq.'w') then  !MD: Nur fuer echte Wehre, keine Breucken
    he1 = hr + hv
  else !MD: Nur fuer Breucken
    he1 = hrow + hv
  Endif

  dif = henow - he1

  !JK      SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''he1= '',f8.4,'' dif= '',f8.4)') he1, dif

  IF (abs (dif) .lt.1.e-04) then

    IF (hrow.gt.hukmax) then
      !JK            SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(''abbruch der iteration im schritt'',i3,            &
        &'' wasserspiegel oberwasser:'',f8.4,/,''energielinie '',          &
        &''im oberwasser:'',f8.4)') i, hrow, he1

      GOTO 9999

    ELSE

      !JK            SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(''wasserspiegel oberwasser unter hukmax'',          &
       &     '' rechne hrow = henow weiter'')')

      wsanf = henow

      CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, &
      hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt,  &
      nz, idr1, Q_Abfrage)

      RETURN

    ENDIF

  ENDIF

  ! if (hrowa.gt.henow) then
  !   hrowa = henow - dx
  ! end if

  !MD!MD!MD  hrowa = hr + dx
  !MD hrowa = hrow - dx
  IF (iwehr.eq.'w') then  !MD: Nur fuer echte Wehre, keine Breucken
    hrowa = hr + dx
  else !MD: Nur fuer Breucken
    hrowa = hrow - dx
  Endif


  CALL wspanf (hrowa, strbr, q, q1, hra, hva, rg, indmax, hvst,   &
   & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1, Q_Abfrage)

  !MD!MD!MD hea = hra + hva
  !MD hea = hrowa + hva
  IF (iwehr.eq.'w') then  !MD: Nur fuer echte Wehre, keine Breucken
    hea = hra + hva
  else !MD: Nur fuer Breucken
    hea = hrowa + hva
  Endif

  !MD!MD!MD df = (hea - he1) / (hra - hr)
  !MD df = (hea - he1) / (hrowa - hrow)
  IF (iwehr.eq.'w') then  !MD: Nur fuer echte Wehre, keine Breucken
    df = (hea - he1) / (hra - hr)
  else !MD: Nur fuer Breucken
    df = (hea - he1) / (hrowa - hrow)
  Endif

  hrow = hr + (henow - he1) / df


 ! if (ABS(df).lt.0.0001) then
 !   df = (hea - he1) / (hva - hv)
 !   if (ABS(df).lt.0.0001) then
 !     hrow = (hr + hra)/2.
 !   else
 !    hvow = hv + (henow - he1) / df
 !     hrow = henow - hvow
 !   endif
 ! else
 !   hrow = hr + (henow - he1) / df
 !   !MD  hrow = hrow + (henow - he1) / df
 ! end if
 !
 ! !MD  Kontrolle der neuen Wassertiefe
 ! !------------------------------------

  if (hrow.le.0. .or. hrow.gt.henow) then
    hrow = (hr + hra)/2.
  end if

 !
 ! if (hrow .le. (2./3. * henow)) then
 !   hrow = (2./3. * henow) + 2.*dx
 ! end if


!MD  Deaktiviert am 24.04.2007
!------------------------------------
  IF (df.lt.1.e-04) then
    !JK      SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '(''keine loesung fuer oberwasser hrow = henow!'')')

    hrow = henow
    CALL wspanf (hrow, strbr, q, q1, hr, hv, rg, indmax, hvst, &
     & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1, Q_Abfrage)

    RETURN

  ENDIF


  !JK      SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, 9810) hea, df, hrow
  9810 FORMAT   ('hea= ',f8.4,' df= ',f8.4,' hrow= ',f8.4)


10 END DO

!JK    SCHREIBEN IN KONTROLLFILE
WRITE (UNIT_OUT_LOG, '(''keine konvergenz fuer oberwasser hrow = henow '')')

wsanf = henow
hrow = henow

CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, hrst, &
 & psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1, Q_Abfrage)

9999 RETURN

END SUBROUTINE wspow                                                                      
