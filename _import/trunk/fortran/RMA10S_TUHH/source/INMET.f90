!IPK  LAST UPDATE SEP 6 2004 add error file
!IPK  LAST UPDATE AUG 5 2003 EXPAND TO ADD WIND DIRECTION
!IPK  LAST UPDATE SEP27 2002 ADD SNOW DEPTH
!IPK  LAST UPDATE JULY01 2001 ADD DEWPOINT OPTION
!IPK  LAST UPDATED Dec 17 1997
!ipk  last updated Nov 26 1997
!IPK  LAST UPDATED AUGUST 28 1995
      SUBROUTINE INMET(LOUT,NMETF,TET)
!
      USE BLKMETMOD
      USE BLK11MOD
!
      CHARACTER*8  ID
!IPK SEP02
      CHARACTER*88 DLIN10
!
!
!       Skip if no heat budget
!
      TOFDAY = TET
      IF(MMET == 0) RETURN
!
!-
!...... Initialize
!-
!      WRITE(*,*) 'IN GETMET IT,MMET',IT,MMET
      NMETF=0
      NSCRM=40
!-
!..... Read header from graph file
!-
!ipk nov97      READ(IIQ,'(A8,A72)') ID,DLIN
!IPK SEP02
      call ginpt10(iiq,id,dlin10)
      READ(DLIN10(1:72),'(A72)') METITL
!ipk nov97      READ(IIQ,'(A8,A72)') ID,DLIN
      call ginpt10(iiq,id,dlin10)
      READ(DLIN10,1026) ELEVAT,LAT,LONG,STANM,IYT,METRIC,EXTINC         &
     &,IHTMET,IWNDMET
!IPK AUG03 ADD SWITCHES
 1026 FORMAT(4F8.4,I8,8X,I8,F8.0,2I8)
      WRITE(LOUT,2036) ELEVAT,LAT,LONG,STANM,IYT,METRIC,EXTINC          &
     &,IHTMET,IWNDMET
 2036 FORMAT( /10x,'ELEVATION OF SITE',T40,F10.1/                       &
     &         10X,'LATITUDE OF SITE',T40,F10.1/                        &
     &         10X,'LONGITUDE OF SITE',T40,F10.1/                       &
     &         10X,'STANDARD TIME AT SITE',T40,F10.1/                   &
     &         10X,'STARTING YEAR FOR DATA',T40,I10/                    &
     &         10X,'SWITCH FOR ATMOS DATA 0=ENG,1=MET',T46,I4/          &
     &         10X,'SOLAR EXTINCTION COEFFICIENT',T44,F6.3,' (1/M)'/    &
     &         10X,'MET DATA DEACTIVATE SWITCH',T44,I6/                 &
     &         10X,'WIND STRESS SIMULATION SWITCH',T44,I6)
      IF(IHTMET == 1) MMET=0
!IPK AUG03 ADD HEAT AND WIND SWITCH AND TEST FOR ACTIVE HEAT
!ipk nov97      READ(IIQ,'(A8,A72)') ID,DLIN
!IPK SEP02
      call ginpt10(iiq,id,dlin10)
      IJULD = DAYOFY
  200 CONTINUE
      IYS(NMETF+1)=IYT
!IPK SEP02
      READ(DLIN10(1:72),'(A72)') METITL
!ipk nov97      READ(IIQ,'(A8,A72)') ID,DLIN
      call ginpt10(iiq,id,dlin10)
      IF(ID(1:2) == 'ET') THEN
        NSCRM=NSCRM+1
        OPEN(NSCRM,FORM='UNFORMATTED',STATUS='SCRATCH')
!IPK SEP02
        READ(DLIN10,'(9I8)') (METID(NMETF+1,J),J=1,9)
        WRITE(*,*) NMETF+1
!            WRITE(*,*) (METID(NMETF+1,J),J=1,9)
!ipk nov97        READ(IIQ,'(A8,A72)') ID,DLIN
!IPK SEP02
        call ginpt10(iiq,id,dlin10)
        IF(ID(1:2) == 'EV') THEN
!ipk dec97 add switch for solar radiation
          READ(DLIN10,'(2F8.6,2I8)') AETMP(NMETF+1),BETMP(NMETF+1)      &
     &                        ,ISOL(NMETF+1),IDPT(NMETF+1)
!ipk jul01 add idpt switch to data
!
        ELSE
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR --- NO EV DATA FOUND'
          WRITE(75,*) 'ERROR --- NO EV DATA FOUND'
          STOP
        ENDIF
  210   CONTINUE
!ipk nov97        READ(IIQ,'(A8,A72)') ID,DLIN
!IPK SEP02
        call ginpt10(iiq,id,dlin10)
        IF(ID(1:2) == 'TI') GO TO 220
        IF(ID(1:7) == 'ENDDATA') THEN
          wdt1(1,1)=1.e20
!IPK DEC97 CHANGE LIMIT OT 9 TO ALLOW DIRECT SOLAR INPUT
!IPK AUG03 EXPAND TO ADD WIND DIRECTION
          WRITE(NSCRM) (WDT1(J,1),J=1,11)
          REWIND NSCRM
          NMETF=NMETF+1
          GO TO 230
        ENDIF
        IF(ID(1:3) == 'MET') THEN
          READ(ID(5:8),'(F4.0)') WDT1(1,1)
!IPK DEC97 CHANGE LIMIT OT 9 TO ALLOW DIRECT SOLAR INPUT
!IPK SEP02
!IPK AUG03 EXPAND TO ADD WIND DIRECTION
          READ(DLIN10,'(11F8.4)') (WDT1(J,1),J=2,11)
!          WRITE(*,*) 'WDT1',WDT1(1,1),NSCRM,WDT1(2,1)
!IPK DEC97 CHANGE LIMIT OT 9 TO ALLOW DIRECT SOLAR INPUT
!IPK AUG03 EXPAND TO ADD WIND DIRECTION
          WRITE(NSCRM) (WDT1(J,1),J=1,11)
          GO TO 210
        ELSE
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'ERROR --- NO MET DATA FOUND'
          WRITE(*,*) 'ERROR --- NO MET DATA FOUND'
          STOP
        ENDIF
  220   CONTINUE
        wdt1(1,1)=1.e20
!IPK DEC97 CHANGE LIMIT OT 9 TO ALLOW DIRECT SOLAR INPUT
!IPK SEP02
!IPK AUG03 EXPAND TO ADD WIND DIRECTION
        WRITE(NSCRM) (WDT1(J,1),J=1,1)
        REWIND NSCRM
        NMETF=NMETF+1
        GO TO 200
      ELSE
        WRITE(*,*) 'ERROR --- NO ET DATA FOUND' 
      ENDIF
  230 CONTINUE
!
!
      RETURN
      END
