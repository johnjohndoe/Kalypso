CIPK DEC02 FIX TO TRAP NEGATIVE WAVEDR1 COMING FROM ERROR IN SWAN

      SUBROUTINE GETDRSST
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKSANMOD
      SAVE
C-
C...... This routine extracts surface stress data from a succession of files
C-
C-

      INTEGER DAYWV

      CHARACTER*8 IDWV
      CHARACTER*72 DLINWV
      CHARACTER*1002 HEDR
      CHARACTER*64 FNAMEWV,FNAMEWVNX
c
      data hrinc/0./,IENT/0/,TFORMER/0/
C-
C...... Initialize
C-
      IF(IENT == 0) THEN
cipk jan98
        iyrwv=iyrr
        iyrd=iyrr
c
        REWIND IWVFC
        CALL GINPT(IWVFC,IDWV,DLINWV)
        IF(IDWV(1:3) == 'WVD') THEN
          READ(DLINWV,5000) TITL
 5000     FORMAT(A72)
        ELSE
          WRITE(*,*) 'NO WAVE DIRECTORY DATA FOUND'
          WRITE(75,*) 'NO WAVE DIRECTORY DATA FOUND'
          STOP
        ENDIF
        CALL GINPT(IWVFC,IDWV,DLINWV)
        IF(IDWV(1:3) == 'YEA') THEN
            READ(DLINWV,5001) IYRWV
 5001     FORMAT(I8)
        ELSE
          WRITE(*,*) 'NO YEAR DATA FOUND ON DIRECTORY DATA FILE'
          WRITE(75,*) 'NO YEAR DATA FOUND ON DIRECTORY DATA FILE'
          STOP
        ENDIF
        CALL GINPT(IWVFC,IDWV,DLINWV)
        IF(IDWV(1:2) == 'WV') THEN
          READ(IDWV,5002) DAYWV
            READ(DLINWV,5003) HRWV,FNAMEWV
 5002     FORMAT(4X,I4)
 5003     FORMAT(F8.0,A64)
          TNEXT=(DAYWV-1.)*24.+HRWV
        ELSE
          WRITE(*,*) 'NO WAVE DATA FOUND ON DIRECTORY DATA FILE'
          WRITE(75,*) 'NO WAVE DATA FOUND ON DIRECTORY DATA FILE'
          STOP
        ENDIF
        IENT=1
        OPEN(105,FILE=FNAMEWV,STATUS='OLD',FORM='BINARY')
        READ(105) HEDR
C
C               Reading Binary Data
C
        READ(105)INODESIN
        DO J = 1, INODESIN
          READ(105) I, STR11(I),STR21(I),
     +               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)
        ENDDO
        CLOSE(105)
        GO TO 200
      ENDIF
C-

c     logic to pass end of year

      GO TO 250
  200 CONTINUE

C-
C...... First pass or TNEXT now not large enough read another and use that
C-
      TCUR=TNEXT
      CALL GINPT(IWVFC,IDWV,DLINWV)
      IF(IDWV(1:2) == 'WV') THEN
        READ(IDWV,5002) DAYWV
          READ(DLINWV,5003) HRWV,FNAMEWVNX
        TNEXT=(DAYWV-1.)*24.+HRWV
      ELSE
        WRITE(*,*) 'NO WAVE DATA FOUND ON DIRECTORY DATA FILE'
        WRITE(75,*) 'NO WAVE DATA FOUND ON DIRECTORY DATA FILE'
        STOP
      ENDIF
      IF(IYRWV > IYRR) THEN
        IF(MOD(IYRR,4) == 0) THEN 
          HRINC=HRINC+366.*24.
        ELSE
          HRINC=HRINC+365.*24.
        ENDIF
      ENDIF

      WRITE(75,*) 'Simulation time,file time,year correction'
      WRITE(75,*) TET+(DAYOFY-1.)*24.,TNEXT,HRINC,IYRWV,IYRR

      WRITE(75,*) 'READING A NEW FILE', FNAMEWVNX
      DO N=1,NP
        STR10(N)=STR11(N)
        STR20(N)=STR21(N)
        WAVEHT0(N)=WAVEHT1(N)
        WAVEDR0(N)=WAVEDR1(N)
        PEAKPRD0(N)=PEAKPRD1(N)
      ENDDO
      OPEN(105,FILE=FNAMEWVNX,STATUS='OLD',FORM='BINARY')
      READ(105) HEDR
C
C               Reading Binary Data
C
      READ(105)INODESIN
      DO J = 1, INODESIN
        READ(105) I, STR11(I),STR21(I),
     +               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)

CIPK DEC02 FIX TO TRAP NEGATIVE WAVEDR1 COMING FROM ERROR IN SWAN

        IF(WAVEDR1(I) < 0.) THEN
          WAVEHT1(I)=0.
          PEAKPRD1(I)=0.
          WAVEDR1(I)=0.
          STR11(I)=0.
          STR21(I)=0.
        ENDIF

      ENDDO
      CLOSE(105)

C    Test for data year less than current

      IF(IYRWV < IYRR) THEN
        GO TO 200
      ENDIF

  250 CONTINUE
      TMODEL=TET+(DAYOFY-1.)*24.
      IF(TMODEL > TNEXT+HRINC) THEN
C
C     Cannot use this file get another file          
C
        go to 200
      ENDIF

      FCTI=(TMODEL-TCUR)/(TNEXT-TCUR)

      write(75,*) 'fcti',fcti,tmodel,tcur,tnext
      DO K=1,NP
        STRESS(K,1)=STR10(K)+FCTI*(STR11(K)-STR10(K))
        STRESS(K,2)=STR20(K)+FCTI*(STR21(K)-STR20(K))
        WAVEHT(K)=WAVEHT0(K)+FCTI*(WAVEHT1(K)-WAVEHT0(K))
        PEAKPRD(K)=PEAKPRD0(K)+FCTI*(PEAKPRD1(K)-PEAKPRD0(K))
        WAVEDR(K)=WAVEDR0(K)+FCTI*(WAVEDR1(K)-WAVEDR0(K))

c        write(75,6566) k,waveht(k),peakprd(k),wavedr(k),
c     +   wAVEDR1(K),wAVEDR0(K)
c 6566 format('wavedr',i5,5f11.4)
      ENDDO

      RETURN

      END
C
