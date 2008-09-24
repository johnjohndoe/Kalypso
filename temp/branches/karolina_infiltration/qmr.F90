!     Last change:  CK   19 Apr 2006    3:15 pm
subroutine qmr(dtn,drs,drks,drd,bmr,areahyd,kf,fk,bof,bfmax,mr,art,tief)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Diese subroutine berechnet den Abfluss aus der Rigole pro Zeiteinheit
!c
!c      Variable        Einheit		Zweck
!c      --------        -------         -----
!c      dtn             [h]  	        Zeitschritt
!c      drs             [%]             Gefaelle des Draenrohres der MR
!c      drks            [mm]            eq. Sandrauheit des Draenrohres
!c      drd             [mm]            Nenndurchmesser des Draenrohres
!c      bmr             [m]             Systembreite der Mulden-Rigolen
!c      kf              [mm/d]          Durchlaessigkeit für gesaettigte Bodenschicht
!c      fk              [mm]            Feldkapzitaet der Kiespackung
!c      bof             [mm]            aktuelle Bodenfeuchte der Kiespackung
!c      bfmax           [mm]            maximale Bodenfeuchte der Kiespackung
!c      mr              [mm]            Abfluss aus MR
!c      tief	        [mm]            Schichtdicke der Rigolenpackung
!c      areahyd         [m²]            Gesamtflaeche der Mulden-Rigole
!c      qvoll           [m³/s]          Freispiegelabfluss im vollen Draenrohr
!c      alpha           [rad]           Zentriwinkel (zur Berechnung des
!c                                      hydraulischen Radius)
!c      qteil           [m³/s]          Freispiegelabfluss im teilgefüllten
!c                                      Draenrohr
!c      laenge          [m]             Laenge des Draenrohres
USE generic_LOG_FILE
USE Units
USE generic_NAConstants
USE generic_PHYSICAL_CONS
IMPLICIT NONE
INTEGER , INTENT(IN) :: art
REAL, INTENT(IN) :: drs,drks,drd,dtn,bmr,areahyd,tief,kf,fk,bof,bfmax
REAL, INTENT(OUT) :: mr
! Lokale Variablen
REAL :: qvoll,qteil,alpha,m,bconst,vol,bw,laenge, &
	Avoll,Ateil,luteil,rhydteil,ahyd,hw,hw_mr
INTEGER :: ndebug

! berechnet den Wasserstand in der der Rigole.
! Annahme:
! Lineare Funktion zwischen Feldkapazitaet und max. Bodenfeuchte mit hw(fk)=0 und hw(bfmax)=tief


1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: qmr.f90                        ****',/,10x,     &
      &'****     Swale-Trench-Type: ',I1,'                   ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (qmr.f90)                 ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

IF (DEBUG ) THEN
    CALL getLogFile( ndebug )
    !WRITE ( ndebug, 1)art
END IF

! Fläche in m² umgerechnet
ahyd = areahyd
laenge = ahyd /bmr
hw_mr = hw(tief,bfmax,fk,bof)

IF (DEBUG) THEN
!   WRITE (ndebug,*) '***********************************************'
!   WRITE (ndebug,*) '*         EINGANGS-PARAMETER 3. Schicht MR' 
!   WRITE (ndebug,*) '* Bodenfeuchte: ',bof
!   WRITE (ndebug,*) '* Fläche-mr [m**2]: ',ahyd
!   WRITE (ndebug,*) '* Maximale Bodenfeuchte: ',bfmax
!   WRITE (ndebug,*) '* Feldkapazität: ',fk
!   WRITE (ndebug,*) '* Wasserstand in der Schicht: ',hw_mr
!   WRITE (ndebug,*) '***********************************************'
END IF


!c Annahme: wasserstand im boden entspricht dem Wasserstand im Draenrohr der Mulden-Rigole
IF(art == 0)  THEN
        ! Formel nach Goodman et. al (1965) Steady-State Zufluss zu einem Tunnel
        vol = 2 * pi * kf*24*3600 * hw_mr /(2.3*LOG10(2*hw_mr/(drd/2)))*dtn*3600
        IF( vol > 0)  THEN
         	call getAlpha(vol/(ahyd/bmr),drd,alpha,100,1e-3)
        ELSE
         	vol = 0
         	alpha = 0
        END IF
        ! Kein Abfluss aus dem Rohr da kein Wasserstand
        IF (alpha == 0) THEN
        	mr = 0.
		RETURN
	END IF

	qvoll = 4*LOG10(3.71/(drks/drd))*SQRT(2*g*1000*drd/4*drs)*pi/4*drd**2

	qteil =  qvoll * (1 + LOG10( (alpha-SIN(alpha))/alpha)/	&
	&  LOG10(3.71/(drks/drd) ) ) * (alpha - SIN(alpha) )**1.5/	&
        &  (2 * pi * alpha**.5)

	mr = qteil * dtn * 3600 / ahyd / 1e6
ENDIF
        ! Formel Sieker Drosselablfluss mit Toricelli
IF( art == 1 ) THEN
        !Voller Rohrquerschnitt
        qvoll = 4*LOG10(3.71/(drks/drd))*SQRT(2*g*drd/4*drs)*pi/4*drd**2  ![mm³/s]
        Avoll = drd**2*pi/4 ![mm²]
        IF (hw_mr < drd .AND. hw_mr > 0 ) THEN
           luteil = drd*ACOS(1-2*hw_mr/drd) ![mm]
           bw = 2*drd*SQRT(hw_mr/drd*(1-hw_mr/drd)) ! [mm]
           Ateil = drd/4*(luteil- bw*(1-2*hw_mr/drd)) ![mm²]
           rhydteil = Ateil/luteil ![mm]
           qteil=qvoll*Ateil/Avoll*(rhydteil/drd*4)**0.625 ![mm³/s]
        ELSE
            qteil = qvoll
        END IF


      	IF( hw_mr > 10 ) THEN
!       		mr = qteil * SQRT((hw - 50)/(tief-100)) * 3600 * dtn / areahyd / 1e6 ![mm]
		mr = qteil * SQRT((hw_mr - 10)/(tief-100)) * 3600 * dtn / 1e6 ![mm]
      	ELSE
        	mr = 0.
        END IF
        IF (DEBUG) THEN
            WRITE(ndebug,*) 'qvoll=',qvoll,' ** qteil= ',qteil,' ** mr=',mr, ' ** hw=',hw_mr
        END IF
END IF
        ! Formel vom Hersteller Hegler GmbH (Untersuchung TU Braunschwieg)
IF (art == 2) THEN
      	IF (hw_mr < drd ) THEN
        	mr = 3.2 * kf * drd * laenge / ahyd*dtn*3600
      	ELSE
        	mr = 3.2 * kf * hw_mr/(tief-drd) * laenge * drd/ahyd*dtn*3600
      	END IF
        mr = mr * 3600**2*24/1000
END IF

!c Es kann keinen negativen Abfluss geben
IF ( mr < 0. ) THEN
    mr = 0.
END IF

IF (DEBUG) THEN
    !WRITE (ndebug,2)
END IF


end subroutine

REAL FUNCTION hw(tief,bfmax,fk,bof)
REAL :: m,bconst
REAL, INTENT(IN):: tief,bfmax,fk
m = tief/(bfmax-fk)
bconst = -tief*fk/(bfmax-fk)
hw = m * bof + bconst ![mm]
if(hw < 0 )hw = 0.
RETURN
END Function
