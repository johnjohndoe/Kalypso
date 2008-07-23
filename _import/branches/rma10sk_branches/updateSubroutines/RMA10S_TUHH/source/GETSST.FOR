      SUBROUTINE GETSST
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      SAVE
C-
C...... This routine interpolates surface stress data
C-
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSST.COM'

      CHARACTER*1000 HEDR
c
      data hrinc/0./,IENT/0/
C-
C...... Initialize
C-
      IF(IENT .EQ. 1) THEN
cipk jan98
        iydd=iyrr
        iyrd=iyrr
c
        TTT=0.
CIPK MAY96 DEFINE TETADD
CIPK MAY02        TETADD=TTT-TCORR
        TETADD=TTT
        REWIND IWVFC
        READ(IWVFC) HEDR
        IF(HEDR(101:105) .EQ. '    1') THEN
          MBND=1
        ELSE
          MBND=0
        ENDIF
      ENDIF
C-
  161 CONTINUE
C-
C..... MBND = 1 is the case of only 1 wind force data file
C-

      WRITE(75,*) 'GETSST-44',HEDR(101:105),mbnd
      IF(MBND .EQ. 1) THEN
        IF(IT .EQ. 1) THEN
C
C      SURFACE STRESS DATA READ
C

          DO K=1,NP
            STRESS(K,1)=0.
            STRESS(K,2)=0.
          ENDDO

          READ(IWVFC,ERR=165)
     +        TTT,NV,IYDD,(K,(STRESS(K,J),J=1,2),L=1,NV)
          GO TO 166
  165     CONTINUE
          STOP 'ERROR IN SURFACE STRESS DATA'
  166     CONTINUE 
        ENDIF
cipk dec97 add check for zero nodes on input file
        IF(NV .EQ. 0) THEN
          WRITE(75,*) 'Error!! total nodes = 0 on surface stress file'
          WRITE(75,*) 'Header contains time',ttt,' Year',iydd
          WRITE(*,*) 'Error!!  total nodes = 0 on surface stress file'
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
            STR10(K) = STR11(K)
            STR20(K) = STR21(K)
            STR11(K)=0.
            STR21(K)=0.
          ENDDO
          T0=TTT
C
C      SURFACE STRESS DATA  read
C

          READ(IWVFC,ERR=286) TTTV,NV,IYDD
     +                            ,(K,STR11(K),STR21(K),L=1,NV)

          WRITE(75,*) 'TIME FROM SURFACE STRESS DATA FILE',TTTV,NV,IYDD
          GO TO 287
  286     CONTINUE
          STOP 'ERROR IN SURFACE STRESS DATA'

  287     CONTINUE 
c            write(75,*) 'tttv,nv,iydd,irma2,tetadd'
c            write(75,*)  tttv,nv,iydd,irma2,tetadd
cipk jan98 add to preserve iydd
          IYRD=IYDD
c     check for zero nodes on input file

          if(nv .eq. 0) then
            write(75,*) 'Error!! total nodes = 0 on surface stress file'
            Write(75,*) 'Header contains time',tttv,' Year',iydd
            write(*,*) 'Error!!  total nodes = 0 on surface stress file'
            Write(*,*) 'Header contains time',tttv,' Year',iydd
            stop
          endif
cipk jun02          TTT=TTTV+TETADD

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
 6225     FORMAT( /5X, 'SURFACE STRESS DATA FILE READ AT TIME =', F10.2,
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
          WRITE(75,*) 'GETSST-169  FCTI',FCTI

          DO K=1,NP
            STRESS(K,1)=STR10(K)+FCTI*(STR11(K)-STR10(K))
            STRESS(K,2)=STR20(K)+FCTI*(STR21(K)-STR20(K))
          ENDDO

        ENDIF
      ENDIF
  600 CONTINUE
C-
C

      DO K=1,NP
        WRITE(75,*)'WAVEDATA',K,STRESS(K,1),STRESS(K,2)
      ENDDO

      RETURN

      END
C
