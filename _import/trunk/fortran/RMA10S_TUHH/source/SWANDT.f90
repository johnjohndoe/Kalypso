      SUBROUTINE SWANDT
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKSANMOD
!
      use BLKSWANF
!
      CHARACTER*8 IDSW
! DJW 28/10/05 Changed from 80 to 72.  This managed to get it working!
      CHARACTER*72 DLINSW,SWANDIR 
!
!
      DATA ITIME/0/
!
!skip execution of following code, if there is no SWAN control file       
      IF(unitSwan == 0) RETURN
!
!     routine to execute swan and get the forces
!
!itime shows, that it's called the first time (itime == 0) or a further time (itime == 1)      
      IF(ITIME == 0) THEN
!
!     First time open the SWAN defaults file
!
        ITIME=1
!
!unit 106 is the swan control file; the opening of the file was moved to file.sub        
!OPEN(106,FILE=SWANFL,STATUS='OLD',FORM='FORMATTED')        
!
        DO N=1,100
          CALL GINPT(unitSwan,IDSW,DLINSW)
          IF(IDSW(1:6) == 'WORDIR') THEN
            SWANDIR=DLINSW
          ELSEIF(IDSW(1:6) == 'SWANEX') THEN
            SWANEX=DLINSW
          ELSEIF(IDSW(1:7) == 'ENDDATA') THEN
            GO TO 200
          ENDIF
        ENDDO 
  200   CONTINUE
        REWIND unitSwan
        DO N=1,100
          CALL GINPT(unitSwan,IDSW,DLINSW)
          IF(IDSW(1:6) == 'EDTFL1') THEN
            SWANFL1=TRIM(SWANDIR)//TRIM(DLINSW)
          ELSEIF(IDSW(1:6) == 'EDTFL2') THEN
            SWANFL2=TRIM(SWANDIR)//TRIM(DLINSW)
!Added Capability to handle EDTFL3 here : Note Extras can be added if required djw 24/01/05
          ELSEIF(IDSW(1:6) == 'EDTFL3') THEN
            SWANFL3=TRIM(SWANDIR)//TRIM(DLINSW)
!
!
!
          ELSEIF(IDSW(1:7) == 'FWAVOUT') THEN
            SWANOUT=TRIM(SWANDIR)//TRIM(DLINSW)
           ELSEIF(IDSW(1:7) == 'SWANCTL') THEN
            SWANCTL=TRIM(SWANDIR)//TRIM(DLINSW)
!
!ipk jan03 add RMAPTS file
           ELSEIF(IDSW(1:7) == 'FRMPTS') THEN
            SWANPTS=TRIM(SWANDIR)//TRIM(DLINSW)
!
! DJW 10/06/04  Add Check for Number of Meshes
          ELSEIF(IDSW(1:5) == 'IMESH') THEN
            READ(DLINSW,*) ISWMESH 
!
! DJW 10/06/04 End of Changes
!
          ELSEIF(IDSW(1:7) == 'ENDDATA') THEN
            GO TO 210
          ENDIF
        ENDDO 
  210   CLOSE(unitSwan)
!
        OPEN(106,FILE=SWANFL1,STATUS='OLD',FORM='FORMATTED')
!
        DO N=1,100
          READ(106,'(A120)') SWANDAT(N,1)
          IF(SWANDAT(N,1)(1:4) == 'STOP') THEN
            NLINSW1=N
            GO TO 220
          ENDIF
        ENDDO
  220   CONTINUE
        CLOSE(106)
!     DJW 10/06/04 Surrounding If block added to trap if there is a single mesh.
      IF (ISWMESH > 1) THEN
        OPEN(106,FILE=SWANFL2,STATUS='OLD',FORM='FORMATTED')
!
!
        DO N=1,100
          READ(106,'(A120)') SWANDAT(N,2)
          IF(SWANDAT(N,2)(1:4) == 'STOP') THEN
            NLINSW2=N
            GO TO 250
          ENDIF
        ENDDO
      END IF
  250 CONTINUE
!     DJW 24/01/04 Added Capability for a third mesh
      IF (ISWMESH > 2) THEN
        OPEN(106,FILE=SWANFL3,STATUS='OLD',FORM='FORMATTED')
        DO N=1,100
          READ(106,'(A120)') SWANDAT(N,3)
          IF(SWANDAT(N,3)(1:4) == 'STOP') THEN
            NLINSW3=N
            GO TO 260
          ENDIF
        ENDDO
      END IF
  260 CONTINUE
!
!
!
!
        CLOSE(106)
!    Now get the first line of the Swan values
!
        OPEN(107,FILE=SWANCTL,STATUS='OLD',FORM='FORMATTED')
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:3) /= 'WVD') THEN
          WRITE(*,*) 'NO WAVE SOURCE DATA FOUND'
          WRITE(75,*) 'NO WAVE SOURCE DATA FOUND'
          STOP
        ENDIF
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:3) == 'YEA') THEN
          READ(DLINSW,'(I8)') IYRSW
        ELSE
          WRITE(*,*) 'NO YEAR DATA FOUND ON WAVE DATA FILE'
          WRITE(75,*) 'NO YEAR DATA FOUND ON WAVE DATA FILE'
          STOP
        ENDIF
        CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:2) == 'WV') THEN
          READ(IDSW,'(4X,I4)') IDAYSW
          READ(DLINSW,'(6F8.0)') HOURSW,HSSW,TPSW,DIRSW,SPRSW,TIDSW
        ELSE
          WRITE(*,*) 'NO WAVE VALUES FOUND ON WAVE DATA FILE'
          WRITE(75,*) 'NO WAVE VALUES FOUND ON WAVE DATA FILE'
          STOP
        ENDIF
!     Look ahead in case we need to use the next step 
!
  300   CALL GINPT(107,IDSW,DLINSW)
        IF(IDSW(1:2) == 'WV') THEN
          READ(IDSW,'(4X,I4)') IDAYSW1
          READ(DLINSW,'(6F8.0)') HOURSW1,                               &
     &          HSSW1,TPSW1,DIRSW1,SPRSW1,TIDSW1
        ELSE
          STOP
        ENDIF
        IF(IYRR == IYRSW) THEN
          IF(TET+(DAYOFY-1)*24. < HOURSW1+(IDAYSW1-1)*24.) THEN
!
!       Run SWAN
!
!JAN03     Setup the rmaplts file containing the depths
!
              OPEN(109,FILE=SWANPTS,FORM='FORMATTED',STATUS='UNKNOWN')
            WRITE(109,'(I10)') NP
            DO N=1,NP
             WRITE(109,'(I10,F10.4)') N,AO(N)
            ENDDO
            CLOSE(109)
!
!             WRITE(143,*) 'SWANDT-0',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
!           WRITE(143,*) 'SWNDT-0',HOURSW,IDAYSW,TPSW,TPSW1
            CALL EXSWAN
!             WRITE(143,*) 'SWANDT-1',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
!           WRITE(143,*) 'SWNDT-1',HOURSW,IDAYSW,TPSW,TPSW1
!
!        These are initial values so move them over
            DO N=1,NP
               STR10(N)=STR11(N)
              STR20(N)=STR21(N)
              WAVEHT0(N)=WAVEHT1(N)
              WAVEDR0(N)=WAVEDR1(N)
              PEAKPRD0(N)=PEAKPRD1(N)
            ENDDO 
            TCUR=(IDAYSW-1.)*24.+HOURSW
!
!         Get values ready for next call to SWAN
!
            IDAYSW=IDAYSW1
            HOURSW=HOURSW1
            HSSW=HSSW1
            TPSW=TPSW1
            DIRSW=DIRSW1
            SPRSW=SPRSW1
            TIDSW=TIDSW1
            TNEXT=(IDAYSW-1.)*24.+HOURSW
!
!
!IPK JAN03            RETURN          
          ELSE
!
!       Move values over and read a new file
!
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
!
      ENDIF
!
!     Check for time match for processing a SWAN run
!
!      WRITE(143,*) 'IN SWANDT',IYRR,TET,DAYOFY,IYRSW,HOURSW1,IDAYSW1
!      WRITE(143,*) 'IN1 SWNDT',HOURSW,IDAYSW,TPSW,TPSW1
!
      IF(IYRR == IYRSW .OR. ITIME == 1) THEN
        IF(TET+(DAYOFY-1)*24. >= HOURSW1+(IDAYSW1-1)*24.                &
     &   .OR. ITIME == 1) THEN
!
!       First read a new file if not first time
!
          IF(ITIME > 1) THEN
!
!        values out of date so move them over
            DO N=1,NP
               STR10(N)=STR11(N)
              STR20(N)=STR21(N)
              WAVEHT0(N)=WAVEHT1(N)
              WAVEDR0(N)=WAVEDR1(N)
              PEAKPRD0(N)=PEAKPRD1(N)
            ENDDO 
!
            CALL GINPT(107,IDSW,DLINSW)
            IF(IDSW(1:2) == 'WV') THEN
              READ(IDSW,'(4X,I4)') IDAYSW1
              READ(DLINSW,'(6F8.0)') HOURSW1,                           &
     &          HSSW,TPSW,DIRSW,SPRSW,TIDSW
              TCUR=TNEXT
              TNEXT=(IDAYSW1-1.)*24.+HOURSW1
            ELSE
              STOP
            ENDIF
          ENDIF
          ITIME=2
!
!IPK JAN03     Setup the rmaplts file containing the depths
!
          OPEN(109,FILE=SWANPTS,FORM='FORMATTED',STATUS='UNKNOWN')
          WRITE(109,'(I10)') NP
          DO N=1,NP
            WRITE(109,'(I10,F10.4)') N,AO(N)
          ENDDO
          CLOSE(109)
!
!       Then execute old set
!
          CALL EXSWAN
!
        ENDIF
      ENDIF         
!       Now interpolate
!
      TMODEL=TET+(DAYOFY-1.)*24.
      FCTI=(TMODEL-TCUR)/(TNEXT-TCUR)
!
      write(75,*) 'fcti',fcti,tmodel,tcur,tnext
      DO K=1,NP
        STRESS(K,1)=STR10(K)+FCTI*(STR11(K)-STR10(K))
        STRESS(K,2)=STR20(K)+FCTI*(STR21(K)-STR20(K))
        WAVEHT(K)=WAVEHT0(K)+FCTI*(WAVEHT1(K)-WAVEHT0(K))
        PEAKPRD(K)=PEAKPRD0(K)+FCTI*(PEAKPRD1(K)-PEAKPRD0(K))
!        
!  djw 18/04/05        
!        
!  Altered to consider the case when a direction of zero is calculated        
!  Due to the wave model being dry nearshore at low tide.  In this case        
!  a value equal to the other end of the interpolation is adopted        
!        
!WAVEDR(K)=WAVEDR0(K)+FCTI*(WAVEDR1(K)-WAVEDR0(K))        
        If (WAVEHT0(K) == 0.0) Then
           WAVEDR(K) = WAVEDR1(K)
        Else If (WAVEHT1(K) == 0.0) Then
           WAVEDR(K) = WAVEDR0(K)
        Else
           WAVEDR(K)=WAVEDR0(K)+FCTI*(WAVEDR1(K)-WAVEDR0(K))
        End If
      ENDDO
      RETURN
      END
!
