      SUBROUTINE SWANDT
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKSANMOD

      INCLUDE 'SWANF.COM'
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05	INCLUDE 'BLKSST.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'

      CHARACTER*8 IDSW
      CHARACTER*80 DLINSW,SWANDIR

      DATA ITIME/0/

c      WRITE(143,*) 'ENTERING SWANDT',ISWANR
      IF(ISWANR .LT. 1) RETURN

C     routine to execute swan and get the forces

      IF(ITIME .EQ. 0) THEN

C     First time open the SWAN defaults file

        ITIME=1
        OPEN(106,FILE=SWANFL,STATUS='OLD',FORM='FORMATTED')

        DO N=1,100
          CALL GINPT(106,IDSW,DLINSW)
          IF(IDSW(1:6) .EQ. 'WORDIR') THEN
	      SWANDIR=DLINSW
          ELSEIF(IDSW(1:6) .EQ. 'SWANEX') THEN
	      SWANEX=DLINSW
          ELSEIF(IDSW(1:7) .EQ. 'ENDDATA') THEN
	      GO TO 200
          ENDIF
        ENDDO 
  200   CONTINUE
        REWIND 106
        DO N=1,100
          CALL GINPT(106,IDSW,DLINSW)
	    IF(IDSW(1:6) .EQ. 'EDTFL1') THEN
	      SWANFL1=TRIM(SWANDIR)//TRIM(DLINSW)
          ELSEIF(IDSW(1:6) .EQ. 'EDTFL2') THEN
	      SWANFL2=TRIM(SWANDIR)//TRIM(DLINSW)
          ELSEIF(IDSW(1:7) .EQ. 'FWAVOUT') THEN
	      SWANOUT=TRIM(SWANDIR)//TRIM(DLINSW)
 	    ELSEIF(IDSW(1:7) .EQ. 'SWANCTL') THEN
	      SWANCTL=TRIM(SWANDIR)//TRIM(DLINSW)

cipk jan03 add RMAPTS file
 	    ELSEIF(IDSW(1:7) .EQ. 'FRMPTS') THEN
	      SWANPTS=TRIM(SWANDIR)//TRIM(DLINSW)


          ELSEIF(IDSW(1:7) .EQ. 'ENDDATA') THEN
	      GO TO 210
          ENDIF
        ENDDO 
  210   CLOSE(106)
  	  				
        OPEN(106,FILE=SWANFL1,STATUS='OLD',FORM='FORMATTED')

        DO N=1,100
          READ(106,'(A120)') SWANDAT(N,1)
          IF(SWANDAT(N,1)(1:4) .EQ. 'STOP') THEN
            NLINSW1=N
            GO TO 220
          ENDIF
        ENDDO
  220   CONTINUE
	  CLOSE(106)

        OPEN(106,FILE=SWANFL2,STATUS='OLD',FORM='FORMATTED')

        DO N=1,100
          READ(106,'(A120)') SWANDAT(N,2)
          IF(SWANDAT(N,2)(1:4) .EQ. 'STOP') THEN
            NLINSW2=N
            GO TO 250
          ENDIF
        ENDDO
  250   CONTINUE

	  CLOSE(106)
C    Now get the first line of the Swan values

        OPEN(107,FILE=SWANCTL,STATUS='OLD',FORM='FORMATTED')
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:3) .NE. 'WVD') THEN
          WRITE(*,*) 'NO WAVE SOURCE DATA FOUND'
          WRITE(75,*) 'NO WAVE SOURCE DATA FOUND'
          STOP
        ENDIF
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:3) .EQ. 'YEA') THEN
          READ(DLINSW,'(I8)') IYRSW
        ELSE
          WRITE(*,*) 'NO YEAR DATA FOUND ON WAVE DATA FILE'
          WRITE(75,*) 'NO YEAR DATA FOUND ON WAVE DATA FILE'
          STOP
        ENDIF
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:2) .EQ. 'WV') THEN
          READ(IDSW,'(4X,I4)') IDAYSW
          READ(DLINSW,'(6F8.0)') HOURSW,HSSW,TPSW,DIRSW,SPRSW,TIDSW
        ELSE
          WRITE(*,*) 'NO WAVE VALUES FOUND ON WAVE DATA FILE'
          WRITE(75,*) 'NO WAVE VALUES FOUND ON WAVE DATA FILE'
          STOP
        ENDIF
C     Look ahead in case we need to use the next step 
            
  300   CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:2) .EQ. 'WV') THEN
          READ(IDSW,'(4X,I4)') IDAYSW1
          READ(DLINSW,'(6F8.0)') HOURSW1,
     +          HSSW1,TPSW1,DIRSW1,SPRSW1,TIDSW1
        ELSE
          STOP
        ENDIF
        IF(IYRR .EQ. IYRSW) THEN
          IF(TET+(DAYOFY-1)*24. .LT. HOURSW1+(IDAYSW1-1)*24.) THEN

C       Run SWAN

CJAN03     Setup the rmaplts file containing the depths

  	      OPEN(109,FILE=SWANPTS,FORM='FORMATTED',STATUS='UNKNOWN')
	      WRITE(109,'(I10)') NP
	      DO N=1,NP
	       WRITE(109,'(I10,F10.4)') N,AO(N)
	      ENDDO
            CLOSE(109)

c  	     WRITE(143,*) 'SWANDT-0',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
c           WRITE(143,*) 'SWNDT-0',HOURSW,IDAYSW,TPSW,TPSW1
            CALL EXSWAN
c  	     WRITE(143,*) 'SWANDT-1',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
c           WRITE(143,*) 'SWNDT-1',HOURSW,IDAYSW,TPSW,TPSW1

C        These are initial values so move them over
            DO N=1,NP
 	        STR10(N)=STR11(N)
	        STR20(N)=STR21(N)
              WAVEHT0(N)=WAVEHT1(N)
              WAVEDR0(N)=WAVEDR1(N)
              PEAKPRD0(N)=PEAKPRD1(N)
	      ENDDO 
            TCUR=(IDAYSW-1.)*24.+HOURSW

C         Get values ready for next call to SWAN

            IDAYSW=IDAYSW1
            HOURSW=HOURSW1
            HSSW=HSSW1
            TPSW=TPSW1
            DIRSW=DIRSW1
            SPRSW=SPRSW1
            TIDSW=TIDSW1
            TNEXT=(IDAYSW-1.)*24.+HOURSW


CIPK JAN03            RETURN          
          ELSE

C       Move values over and read a new file

            IDAYSW=IDAYSW1
            HOURSW=HOURSW1
            HSSW=HSSW1
		  TPSW=TPSW1
            DIRSW=DIRSW1
            SPRSW=SPRSW1
            TIDSW=TIDSW1
            GO TO 300
          ENDIF
        ENDIF

      ENDIF

C     Check for time match for processing a SWAN run

c	WRITE(143,*) 'IN SWANDT',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
c      WRITE(143,*) 'IN1 SWNDT',HOURSW,IDAYSW,TPSW,TPSW1

      IF(IYRR .EQ. IYRSW  .OR. ITIME .EQ. 1) THEN
        IF(TET+(DAYOFY-1)*24. .GE. HOURSW1+(IDAYSW1-1)*24.
     +    .OR. ITIME .EQ. 1) THEN

C       First read a new file if not first time

          IF(ITIME .GT. 1) THEN

C        values out of date so move them over
            DO N=1,NP
 	        STR10(N)=STR11(N)
	        STR20(N)=STR21(N)
              WAVEHT0(N)=WAVEHT1(N)
              WAVEDR0(N)=WAVEDR1(N)
              PEAKPRD0(N)=PEAKPRD1(N)
	      ENDDO 

            CALL GINPT(107,IDSW,DLINSW)
            IF(IDSW(1:2) .EQ. 'WV') THEN
              READ(IDSW,'(4X,I4)') IDAYSW1
              READ(DLINSW,'(6F8.0)') HOURSW1,
     +          HSSW,TPSW,DIRSW,SPRSW,TIDSW
	        TCUR=TNEXT
              TNEXT=(IDAYSW1-1.)*24.+HOURSW1
            ELSE
              STOP
            ENDIF
	    ENDIF
	    ITIME=2

CIPK JAN03     Setup the rmaplts file containing the depths

	    OPEN(109,FILE=SWANPTS,FORM='FORMATTED',STATUS='UNKNOWN')
	    WRITE(109,'(I10)') NP
	    DO N=1,NP
	      WRITE(109,'(I10,F10.4)') N,AO(N)
	    ENDDO
          CLOSE(109)

c       Then execute old set

          CALL EXSWAN

        ENDIF
      ENDIF         
c       Now interpolate

      TMODEL=TET+(DAYOFY-1.)*24.
      FCTI=(TMODEL-TCUR)/(TNEXT-TCUR)

      write(75,*) 'fcti',fcti,tmodel,tcur,tnext
      DO K=1,NP
        STRESS(K,1)=STR10(K)+FCTI*(STR11(K)-STR10(K))
        STRESS(K,2)=STR20(K)+FCTI*(STR21(K)-STR20(K))
	  WAVEHT(K)=WAVEHT0(K)+FCTI*(WAVEHT1(K)-WAVEHT0(K))
	  PEAKPRD(K)=PEAKPRD0(K)+FCTI*(PEAKPRD1(K)-PEAKPRD0(K))
	  WAVEDR(K)=WAVEDR0(K)+FCTI*(WAVEDR1(K)-WAVEDR0(K))
      ENDDO
      RETURN
      END

