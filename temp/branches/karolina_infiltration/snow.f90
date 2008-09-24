
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

      subroutine snow(idif,dt,p,pns,tmeans,h,ws,pkorr,idif1,isim,     &
                       wwo,wwmax,snotem,snorad,h0,w0,              &
                       nanf,nanfstd,xanf,nanzanf,nanzanf1,nanff,nres,idatsa20,naja20)

!**************************************ÄNDERUNGEN***********************************************
!SK
!SK   -----------------------------------------------------------------------
!SK   Änderung am 28.02.03 bis 04.03.03 Stephan Kraessig (SK), Prof. Pasche (EP)
!SK   -----------------------------------------------------------------------
!SK   Neue Version ist Jahr 2000 faehig und kann bei Angabe von 1000001 als
!SK   Anfangsdatum in der *.konfig Datei für jeden gerechneten Zeitschritt
!SK   der Langzeitsimulation Anfangswerte für die Kurzzeitsimulation erzeugen.

!5.3.2002 Kraessig/v.Doemming:  Die Datei kann sehr groß werden, daher  kann der erzeugte lzsim Ordner
!SK  ausgelagert und in der *.konfig  Datei der entsprechende Pfad angegeben werden mit lzpath=.
!SK   Liegt der lzsim Ordner unter c:\daten\lzsim, ist lzpath=c:\daten anzugeben.
!     Last change:  CK   19 Apr 2006    3:17 pm

!c      WK: nur Umbenennung!

!JH   	 ------------------------------------------------------------------------------------------
!JH   	 Änderung  24. - 30.09.2004 Jessica Hübsch (JH)
!JH   	 ------------------------------------------------------------------------------------------
!JH      Anfangswerte können auch aus der Kurzzeitsimulation zu einer beliebigen Stunde
!JH	 geschrieben werden. Hierzu wurde das Format des *.lzs Files geaendert, da hier zusaetzlich
!JH      zum Datum der Anfangswerte jetzt auch die Stunden hinzugefügt wurden. Hinweis: die
!JH	 Änderungen beziehen sich nur auf das Schreiben der Anfangswerte zu bestimmten Daten und
!JH	 deckt für die Kurzzeitsimulation nicht die Möglichkeit der fortlaufenden Speicherung der
!JH  	 Anfangswerte ab.
!JH	 Im Zuge dieser Änderung mussten die Übergabeparameter dieser Routine durch die Variablen
!JH      nanfstd,nanzanf1,naja20 erweitert werden.

!***************************************BESCHREIBUNG********************************************

!***************************************EIN-/AUSGABE********************************************

!******************************************VARIABLEN********************************************
USE generic_LOG_FILE
USE generic_NAConstants
IMPLICIT NONE
INCLUDE 'include\param.cmn'
INCLUDE 'include\nad.cmn'

1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: snow_dt.f90                    ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (snow_dt.f90)             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

integer   nt,idif,idif1,isim
integer   ijahr, imona,itag,idatum,itemp,idatsa20
INTEGER   naja20
real      p(*),pkorr(*),h(*),ws(*),tmeans(*)
real      dt,w,ww,dw,dh,wakk
integer   nres,ianf,ndebug

real 	wwo,wwmax,snotem,snorad,h0,w0
real	snownied
CHARACTER (LEN=1) :: pns

!ccccccccccccccc
!c
!c	anfangswerte lzsim-datei
!c
real	xanf(idimanf)
integer	nanf(idimanf),nanzanf,nanff
INTEGER :: nanfstd(idimanf)
INTEGER	:: sjahr,smona,stag,sdatum,isstd
REAL 	:: sstd = 0.0
INTEGER :: nanzanf1

!******************************************ANWEISUNGEN******************************************
!cccccccccccccccccccccccccc
!c
!c   Log-Datei öffnen
ndebug = -1
IF(DEBUG) THEN
   CALL getLogFile( ndebug )
   WRITE(ndebug,1)
ENDIF


snownied=0.0125
IF ( wwo > wwmax) THEN
   snownied=wwo
   wwo=0.2
ENDIF

ianf=1
w=h0*w0
IF (w0 < wwo) w=h0*wwo

!-----------------------------------------------------------------------------------------------
!                            n a e c h s t e r  Z e i t p u n k t                              -
!-----------------------------------------------------------------------------------------------

time:DO nt=1,idim
! verlaengerung des zeitraums moeglich bei kurzzeitsimulation,
! falls noch nicht der gesamte schnee geschmolzen ist.
    IF ( nt > idif ) THEN
        IF ( isim /= 1 ) THEN
            idif1=nt-1
            IF (DEBUG) THEN
                WRITE (ndebug,*) 'nt > idif und isim /= 1 : idif1=',idif1
            END IF
            EXIT time
        ENDIF
        IF ( h(nt-1) < 10e-04 ) THEN
           idif1=nt-1
           IF (DEBUG) THEN
               WRITE (ndebug,*) 'nt > idif und h(nt-1) < 10e-04  : idif1=',idif1
           END IF
           EXIT time
        ENDIF
        IF ( nt > idim) THEN
            idif1=nt-1
            IF (DEBUG) THEN
                WRITE (ndebug,*) 'nt > idif und nt > idim : idif1=',idif1
            END IF
            EXIT time
        ENDIF
    ENDIF
    pkorr(nt)=p(nt)
    h(nt)=h0
    IF ( nt > 1) h(nt) = h(nt-1)
    IF( abs(w) <= 1.e-04 .AND. tmeans(nt) >= 0.0) goto 300

! Akkumulation  (Schneehoehe waechst)
!------------------------------------
    IF (  tmeans(nt) <= 0.) THEN
        w=w+p(nt)*dt
        h(nt)=h(nt)+p(nt)*dt/wwo
        IF ( h(nt) > 0 ) THEN
            ws(nt)=w/h(nt)
        ELSE
            ws(nt)=0.
        ENDIF
        pkorr(nt)=0.
    ELSE

! Schmelzrate Schneeverdunstung infolge Strahlung
!------------------------------------------------
        wakk=w+p(nt)*dt
        dw=snorad+(snotem+snownied*p(nt))*tmeans(nt)
        w=w-dw*dt

! Verdichtung  (Schneehoehe nimmt ab)
!------------------------------------
        IF ( w > 0.0 ) THEN
            dh=h(nt)*0.474*(wakk/w-1.)
            h(nt)=h(nt)-dh
            IF ( h(nt) > 0.0 ) THEN
                ww=wakk/h(nt)
                IF ( ww < wwmax ) THEN
                    w=wakk
                    pkorr(nt)=0.

! Ablation   (Tauen)
!-------------------
                ELSE
                    pkorr(nt)=(wakk-h(nt)*wwmax)/dt
                    w=wakk-pkorr(nt)*dt
                ENDIF
                ws(nt)=w/h(nt)
                goto 300
                !CONTINUE
            ENDIF
        ENDIF

! Null-Schneehoehe
!-----------------
        pkorr(nt)=wakk/dt
        w=0.0
        h(nt)=0.
        ws(nt)=0.

    ENDIF


!-----------------------------------------------------------------------------------------------
! Anfangswerte *.lzs                                                                           -
!                                                                                              -
! Schneedaten werden in *.lzs für vorgegebene Anfangszeitpunkte gespeichert.                   -
!                                                                                              -
!-----------------------------------------------------------------------------------------------

!cccccccccccccccccccc
!c SK/EP 28.02.03
!c Datenausgabe Anfangswerte LZSIM
!c Das Anfangsdatum des Zykluses 8 stellig wird in Jahr, Monat und Tag gesplittet, um die
!c Funktion cdatum anzuwenden.

!JH 28.09.2004 Anfangswerte werden auch aus einer Kurzzeitsimulation gespeichert.
!              Hierzu musste das Format des *.lzs-Files geaendert werden, um die Stunden hinzuzufügen.
!              Abfrage (If-Schliefe) um lz-kz erweitert.
    300 CONTINUE
    IF ( isim == 0 ) THEN                                                            ! Langzeitsim.
        IF ( nanf(1) == idatsa20 .AND. nanzanf == 0) THEN
            ijahr=(nanf(1)/10**4)
            imona=((nanf(1)-ijahr*10**4)/100)
            itag =(nanf(1)-ijahr*10**4-imona*100)
!c SK/EP 28.02.03
!c Die do Variable nt: do t=1, idif (Anzahl der Zeitschritte im Zyklus) muß neu initialisiert werden,
!c da keine do-Variable übergeben werden darf. Die Funktion cdatum setzt idatum für jeden Zeitschritt
!c neu, damit so in der lzsim alle Anfangswerte herausgeschrieben werden können.
            itemp=nt
            call cdatum (itag,imona,ijahr,itemp)                                        ! 1 Tag = 1 Simulationsschritt nt weiter
            idatum=ijahr*10**4+imona*10**2+itag
            write(nanff,'(i8,2x,a2,a2,2x,a4,1x,a4)') idatum,'00',' h','   1','snow'
            write(nanff,'(i4,2(f9.2))')1,h(nt),ws(nt)
        ELSEIF ( nanzanf > 0 .AND. nt >= xanf ( ianf ) ) THEN
            write(nanff,'(i8,2x,a2,a2,2x,a4,1x,a4)') nanf(ianf),'00',' h','   1','snow'
            write(nanff,'(i4,2(f9.2))')1,h(nt),ws(nt)
            ianf=ianf+1
            IF ( ianf > nanzanf ) nanzanf=0
        ENDIF
    ELSEIF ( pns /= 's' ) THEN                                          ! Kurzzeitsimulation natürlicher Niederschlag
        IF ( ianf <= nanzanf1 ) THEN
            IF ( nt == 1 ) THEN
                sjahr = naja20
                smona = namo
                stag = natg
                sstd = REAL(nast)
            ENDIF
            sstd = sstd + dt
            IF ( sstd >= 24.0) THEN
                sstd = sstd - 24.0
                call cdatum (stag,smona,sjahr,1)
            ENDIF
            sdatum = sjahr*10**4 + smona*10**2 + stag
            isstd=INT(sstd)
            IF ( nanf(ianf) == sdatum .and. nanfstd(ianf) == isstd) THEN
                write(nanff,'(i8,2x,i2,a2,2x,a4,1x,a4)') nanf(ianf),nanfstd(ianf),' h','   1','snow'
	            write(nanff,'(i4,2(f9.2))')1,h(nt),ws(nt)
                ianf = ianf + 1
            ENDIF
        ENDIF
    ENDIF

ENDDO time
IF (DEBUG) THEN
   WRITE (ndebug,2)
END IF
!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
end



