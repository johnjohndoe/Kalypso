      SUBROUTINE GETWAVE
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSANMOD
      SAVE
C-
C...... This routine interpolates wave data
C-
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'

      CHARACTER*1000 HEDR
c
      data hrinc/0./
C-
C...... Initialize
C-
      IF(IT .EQ. 1) THEN
cipk jan98
        iydd=iyrr
        iyrd=iyrr
        IUT=0
c
        IFRST=0
        IFSV=0
        TTT=0.
CIPK MAY96 DEFINE TETADD
CIPK MAY02        TETADD=TTT-TCORR
        TETADD=TTT
        REWIND IWVIN
        READ(IWVIN) HEDR
        IF(HEDR(101:105) .EQ. '    1') THEN
          MBND=1
        ELSE
          MBND=0
        ENDIF
        NSCR3=102
      ENDIF
C-
  161 CONTINUE
C-
C..... MBND = 1 is the case of only 1 wave data file
C-

      WRITE(75,*) 'GETWAVE-54',MBND,HEDR(101:105)
      IF(MBND .EQ. 1) THEN
        IF(IT .EQ. 1) THEN
C
C      WAVE DATA READ
C

          DO K=1,NP
            WAVEHT(K)=0.
            PEAKPRD(K)=0.
            WAVEDR(K)=0.
          ENDDO

          READ(IWVIN,ERR=165)
     +        TTT,NDM,NV,IYDD,(WAVEHT(K),K=1,NV),(PEAKPRD(K),K=1,NV),
     +        (WAVEDR(K),K=1,NV)
          GO TO 166
  165     CONTINUE
          STOP 'ERROR IN WAVE DATA'
  166     CONTINUE 
        ENDIF
cipk dec97 add check for zero nodes on input file
        IF(NV .EQ. 0) THEN
          WRITE(75,*) 'Error!!  total nodes = 0 on solution file'
          WRITE(75,*) 'Header contains time',ttt,' Year',iydd
          WRITE(*,*) 'Error!!  total nodes = 0 on solution file'
          WRITE(*,*) 'Header contains time',ttt,' Year',iydd
          STOP
        ENDIF
        WRITE(75,*) 'Time ONLY file =',ttt,'current time =',TET
        GO TO 600
C-
C..... MBND = 0 is the case of multiple files to be interpolated
C-
      ELSE
  250   CONTINUE

        IYDD=IYRD

c     logic to pass end of year

        IF(IYDD .GT. IYRR) THEN
          IF(MOD(IYRR,4) .EQ. 0) THEN 
            HRINC=HRINC+366.*24.
          ELSE
            HRINC=HRINC+365.*24.
          ENDIF
        ENDIF

C    Test for data year less than current

        IF(IYDD .LT. IYRR) THEN
          IF(MOD(IYDD,4) .EQ. 0) THEN 
            HRINC=HRINC-366.*24.
          ELSE
            HRINC=HRINC-365.*24.
          ENDIF
          IYDD=IYDD+1   
        ENDIF

        WRITE(75,*) 'Simulation time,file time,year correction'
        WRITE(75,*) TET+(DAYOFY-1.)*24.,TTT,HRINC,IYDD,IYRR
        WRITE(75,*) 'tet,dayofy',tet,dayofy

        IF(TET+(DAYOFY-1.)*24. .GT. TTT+HRINC) THEN
cipk  reset hrinc
          HRINC=0.

C-
C...... TTT not yet large enough  move 1 to 0 and read another
C-
          DO K=1,NP
            WVH0(K) = WVH1(K)
            WVT0(K) = WVT1(K)
            WVA0(K) = WVA1(k)
            WVH1(K) = 0.
            WVT1(K) = 0.
            WVA1(K) = 0.
          ENDDO
          T0=TTT
C
C      WAVE DATA  read
C
          READ(IWVIN,ERR=286) TTTV,NDM,NV,IYDD
     +                            ,(WVH1(K),K=1,NV),(WVT1(K),K=1,NV),
     +                             (WVA1(K),K=1,NV)

          WRITE(75,*) 'TIME FROM WAVE DATA FILE',TTTV,NV,IYDD
          GO TO 287
  286     CONTINUE
          STOP 'ERROR IN WAVE DATA'

  287     CONTINUE 
c            write(75,*) 'tttv,nv,iydd,irma2,tetadd'
c            write(75,*)  tttv,nv,iydd,irma2,tetadd
cipk jan98 add to preserve iydd
          IYRD=IYDD
c     check for zero nodes on input file

          if(nv .eq. 0) then
            write(75,*) 'Error!!  total nodes = 0 on solution file'
            Write(75,*) 'Header contains time',tttv,' Year',iydd
            write(*,*) 'Error!!  total nodes = 0 on solution file'
            Write(*,*) 'Header contains time',tttv,' Year',iydd
            stop
          endif
          TTT=TTTV+TETADD

  288     CONTINUE
          IF(MOD(IYDD,4) .EQ. 0) THEN
            HYR=366.*24.
          ELSE
            HYR=365.*24.
          ENDIF

          IF(TTT .GT. HYR) THEN
            TTT=TTT-HYR
            IYDD=IYDD+1
          ELSE
            GO TO 289
          ENDIF
          GO TO 288
  289     CONTINUE
          IYRD=IYDD

          WRITE(75,6225) TTTV,TTT,iydd,iyrr
 6225     FORMAT( /5X, 'WAVE DATA FILE READ AT TIME =', F10.2,
     +    '  CORRECTED TIME = ',F10.2/'YEARS',2I8 )
cipk            write(75,*) 'Time from file =',ttt,'current time =',TET
          GO TO 250
        ELSE
C-
C...... We have now spanned across the time for interpolation
C-
          write(75,*) 'time,t0,ttt',tET,t0,ttt
          FCTI=(TET+(DAYOFY-1)*24.-T0)/(TTT+HRINC-T0)
          TTTC=TTT+HRINC
          HRINC=0.
          WRITE(75,*) 'GETWAVE-188  FCTI',FCTI

          DO K=1,NP
            WAVEHT(K)=wvh0(K)+FCTI*(wvh1(K)-wvh0(K))
            WAVEDR(K)=wva0(K)+FCTI*(wva1(K)-wva0(K))
            PEAKPRD(K)=wvt0(K)+FCTI*(wvt1(K)-wvt0(K))
          ENDDO

        ENDIF
      ENDIF
  600 CONTINUE
C-
C

      DO K=1,NP
        WRITE(75,*)'WAVEDATA',K,WAVEHT(K),PEAKPRD(K),WAVEDR(K)
      ENDDO

      RETURN

      END
C
