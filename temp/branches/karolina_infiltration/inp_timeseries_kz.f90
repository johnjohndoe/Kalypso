!     Last change:  JN   17 JUN 2007    1:21 pm

!**************************************LICENSE**************************************************
!
! This code is part of the library 'Kalypso-NA'.
! KALYPSO-NA is a deterministic, non-linear, detailed Rainfall-Runoff-Model (RRM).
! The model permits a complete simulation of the land bound
! part of the global water balance as a reaction on observed precipitation events.
! Copyright (C) 2004  HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering (in co-operation with Bjoernsen Cunsulting Engineers)
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
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Dipl.-Ing. Jessica Hübsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Hübsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************


	subroutine inp_timeseries_kz(namfn,dt,faktn,naja,namo,natg,nast,namn,&
                         neja,nemo,netg,nest,nemn,p,idife,kzif)

!  Subroutine zum Einlesen von Niederschlags- und Pegelkurzzeitwerten

!  Einlesen der Werte erfolgt durch die Subroutine inp_nied_kz_dat
!  Unterstuetzt wird das 'grap' Format
!  Umrechnen auf aequidistante Zeitintervalle der Laenge dt in Subroutine n_approx

!17.06.2007
!JN Die Subroutinen inp_nied_kz und inp_peg_kz wurden in dieser Routine zusammengefasst,
!   da der einzige Unterschied die Wertebehandlung (kzif) ist.
!   Der Wert wird jetzt von aussen aus der aufrufenden Subroutine übertragen.

!***************************************EIN-/AUSGABE********************************************
!
!	Eingabe:
!		namfn	filename Niederschlags- bzw. Pegeldaten
!		naja,namo,natg,nast,namn	anfangsdatum, naja:xxxx
!		neja,nemo,netg,nest,nemn	enddatum, naja:xxxx
!	Ausgabe:
!		p	niederschlagswerte, in mm/h	(intensitaet), bzw. Pegelwerte
!		ianz	Anzahl der werte



!******************************************VARIABLEN********************************************
      USE Units
    IMPLICIT NONE
    include      'include/param.cmn'

	CHARACTER(LEN=120)		:: namfn
	INTEGER                 :: naja,namo,natg,nast,namn, &
                               neja,nemo,netg,nest,nemn,kzif,idife
    INTEGER					:: ianz_d,i
	REAL                    :: dt,faktn
	REAL					:: t(idim_n),p_inp(idim_n),p_m(idim_n)
	REAL                    :: p(*)
	REAL					:: tend,tstart,sum1,sum2

	ianz_d = 0

!******************************************ANWEISUNGEN******************************************
	call inp_timeseries_kz_dat(namfn,dt,kzif,naja,namo,natg,nast,namn,                 &
                         neja,nemo,netg,nest,nemn,tend,t,p_inp,ianz_d)

	tstart = 0.
	call n_approx(dt,tstart,tend,ianz_d,t,p_inp,0,idife,p_m,kzif)
    IF (kzif .eq. 2) THEN
       p_m=p_m*dt
    END IF
    
	sum1 = 0.
	sum2 = 0.
    summe1:DO i=1,ianz_d
		sum1=sum1+p_inp(i)
	ENDDO summe1

	summe2:DO i=1,idife
		p(i)=faktn*p_m(i)/dt
		sum2=sum2+p_m(i)
    ENDDO summe2

    IF (kzif .eq. 1) THEN
	   write(nres,9019)sum1,namfn
    END IF
    return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

9019 FORMAT( / ' Eingelesener Niederschlag: ',f8.1,' mm'/, &
		        & ' Datenfile:  ', a)
	end
