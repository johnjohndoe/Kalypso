CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
      SUBROUTINE GETSTRESS
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKABMOD
      SAVE

cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05	INCLUDE 'BLKSST.COM'
C
      DATA ITIME/0/,HRINC/0/
      CHARACTER*1000 HEADWT,HEDR
C      COMMON /STR/
C     +STRESS(MNP,2),STR11(MNP),STR21(MNP),STR10(MNP),STR20(MNP)

      IF(ITIME .EQ. 0) THEN
        IF(INWGT .NE. 0) THEN
          READ(INWGT,'(A1000)') HEADWT
          DO N=1,NP
            READ(INWGT,5000) M,(NODWT(M,J),J=1,3),(WAT(M,J),J=1,3)
 5000     FORMAT(8X,I8,3I8,3F8.5)
          ENDDO
          CLOSE(INWGT)
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove reading from obsolete binary file inbnwgt
!inbnwgt is obsolete
!-
        ELSEIF(ICORDIN .EQ. 0) THEN
          RETURN
        !EFa aug08, added if-clause for external coordinates for wave data
        ELSEIF(INSTR .EQ. 0.and.INBNSTR.eq.0) THEN
          RETURN
        !-
        ENDIF
        iydd=iyrr
        iyrd=iyrr
        TTT=0.
        TETADD=TTT
        IF(INSTR .GT. 0) THEN
          REWIND INSTR
          READ(INSTR,'(A100)') HEDR(1:100)
          IF(HEDR(1:8) .EQ. 'STRESS-D') THEN
            MBND=0
            READ(INSTR,5001,ERR=145,END=145) IDYY,TTTV,NV,IYDD
            go to 150
 145        CONTINUE
cipk sep04
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
            write(75,*) 'ERROR IN SURFACE STRESS DATA'
            STOP 'ERROR IN SURFACE STRESS DATA 111'
            !EFa jul08, changed do-loop of the read-statement
  150       do l=1,nv
              READ(INSTR,'(8X,I8,2F16.0,4F8.0)')K, (str2(k,j),j=1,6)             
            end do
           ! READ(INSTR,'(8X,I8,2F16.0,4F8.0)')
      !+      (K,(STR2(K,J),J=1,6),L=1,NV)
           !-
          ELSE
            MBND=1
          ENDIF
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove reading from obsolete binary file inbnstr
!inbnstr is obsolete
!-
        ELSEIF(ICORDIN .EQ. 0) THEN
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR   No binary weighting file defined'
          WRITE(75,*) 'ERROR   No binary weighting file defined'
          STOP
        ENDIF
      ENDIF
      ITIME=1
      IF(MBND .EQ. 1) THEN
        IF(ITIME .EQ. 1) THEN
	    ITIME=2
C
C      SURFACE STRESS DATA READ
C

            READ(INSTR,5001,ERR=165) IDYY,TTT,NV,IYDD
 5001       FORMAT(8X,I8,F8.0,I8,I8)
            !EFa jul08, changed do-loop of the read-statement
            do l=1,nv
              READ(INSTR,'(8X,I8,2F16.0,4F8.0)')K, (str2(k,j),j=1,6)              
            end do
            !READ(INSTR,'(8X,I8,2F16.0,4F8.0)')
      !+      ((K,STR2(K,J),J=1,6),L=1,NV)
            !-
            GO TO 166
  165     CONTINUE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          STOP 'ERROR IN SURFACE STRESS DATA'
  166     CONTINUE 
        ENDIF

c       Check for zero values on input file

        IF(NV .EQ. 0) THEN
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'Error!! total values = 0 on surface stress file'
          WRITE(75,*) 'Header contains time',ttt,' Year',iydd
          WRITE(*,*) 'Error!!  total values = 0 on surface stress file'
          WRITE(*,*) 'Header contains time',ttt,' Year',iydd
          STOP
        ENDIF
        WRITE(75,*) 'Time ONLY file =',ttt,'current time =',TET

c       Now interpolate to get stress values

        DO N=1,NP
            STRESS(N,1)=0.
            STRESS(N,2)=0.
            SPWP(N)=0
            AWL(N)=0
            SWH(N)=0
            WVDR(N)=0
            DO M=1,3
              K=NODWT(N,M)
              IF(K .GT. 0) THEN
                STRESS(N,1)=STRESS(N,1)+WAT(N,M)*STR2(K,1)
                STRESS(N,2)=STRESS(N,2)+WAT(N,M)*STR2(K,2)
                SPWP(N)=SPWP(N)+WAT(N,M)*STR2(K,3)
                AWL(N)=AWL(N)+WAT(N,M)*STR2(K,4)
                SWH(N)=SWH(N)+WAT(N,M)*STR2(K,5)
                WVDR(N)=WVDR(N)+WAT(N,M)*STR2(K,6)
              ENDIF
            ENDDO
          ENDDO
        GO TO 600
        ELSE
C-
C..... MBND = 0 is the case of multiple files to be interpolated
C-
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
          DO K=1,NV
	      DO L=1,6
              STR1(K,L) = STR2(K,L)
              STR2(K,L)=0.
            ENDDO
	    ENDDO
          T0=TTT
C
C      SURFACE STRESS DATA  read
C

           READ(INSTR,5001,ERR=286,END=285) IDYY,TTTV,NV,IYDD
            !EFa jul08, changed do-loop of the read-statement
            do l=1,nv
              READ(INSTR,'(8X,I8,2F16.0,4F8.0)')K, (str2(k,j),j=1,6)
            end do
              !READ(INSTR,'(8X,I8,2F16.0,4F8.0)')
      !+      (K,(STR2(K,J),J=1,6),L=1,NV)
            !-
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove reading from obsolete binary file inbnstr
!inbnstr is obsolete
!-
          NVK=NV
          TTTV=TTTV+(IDYY-1)*24.
            TTT=TTTV
          WRITE(75,*) 'TIME FROM SURFACE STRESS DATA FILE',TTTV,NV,IYDD
          GO TO 287
  285     NV=NVK
          DO L=1,NV
            DO J=1,6
              STR2(L,J)=STR1(L,J)
            ENDDO
          ENDDO
          IYDD=IYRD+1
          GO TO 287
  286     CONTINUE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          write(75,*) 'ERROR IN SURFACE STRESS DATA'
          STOP 'ERROR IN SURFACE STRESS DATA'

  287     CONTINUE 
c            write(75,*) 'tttv,nv,iydd,irma2,tetadd'
c            write(75,*)  tttv,nv,iydd,irma2,tetadd
cipk jan98 add to preserve iydd
          IYRD=IYDD
c     check for zero nodes on input file

          if(nv .eq. 0) then
cipk sep04
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
           write(75,*) 'Error!! total values = 0 on surface stress file'
           Write(75,*) 'Header contains time',tttv,' Year',iydd
           write(*,*) 'Error!!  total values = 0 on surface stress file'
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

            if(idebug .eq. 1) then
            !EFa jul08, testing
            OPEN(234,FILE='STRESSFUL.TXT')
            !-
            DO K=1,100
             WRITE(234,'(I8,6F15.3)')K,XUSR(K),YUSR(K),(STR2(K,J),J=3,6)
            ENDDO
            endif
          DO N=1,NP
              STRESS(N,1)=0.
              STRESS(N,2)=0.
              SPWP(N)=0
              AWL(N)=0
              SWH(N)=0
              WVDR(N)=0
              DO M=1,3
                K=NODWT(N,M)
                IF(K .GT. 0) THEN
                STRESS(N,1)=STRESS(N,1)
     +              +(STR1(K,1)+FCTI*(STR2(K,1)-STR1(K,1)))*WAT(N,M)
                STRESS(N,2)=STRESS(N,2)
     +              +(STR1(K,2)+FCTI*(STR2(K,2)-STR1(K,2)))*WAT(N,M)
                SPWP(N)=SPWP(N)
     +              +(STR1(K,3)+FCTI*(STR2(K,3)-STR1(K,3)))*WAT(N,M)
                AWL(N)=AWL(N)
     +              +(STR1(K,4)+FCTI*(STR2(K,4)-STR1(K,4)))*WAT(N,M)
                SWH(N)=SWH(N)
     +              +(STR1(K,5)+FCTI*(STR2(K,5)-STR1(K,5)))*WAT(N,M)
                WVDR(N)=WVDR(N)
     +              +(STR1(K,6)+FCTI*(STR2(K,6)-STR1(K,6)))*WAT(N,M)
                ENDIF 
              ENDDO
              !EFa jul08, testing
              WRITE(234,*)n,'awl(n): ',awl(n),wat(n,1),wat(n,2),wat(n,3) !(Achtung: interne Knoten!)
              !-
          ENDDO

        ENDIF

      ENDIF
  600 CONTINUE
C      DO N=1,NP
C        WRITE(135,'(I5,7F9.3,3I6)') N,SPWP(N),AWL(N),SWH(N),WVDR(N),
C     +  WAT(N,1),WAT(N,2),WAT(N,3),NODWT(N,1),NODWT(N,2),NODWT(N,3)
C	ENDDO
C      DO N=1,NV
C        WRITE(136,*) N,STR1(N,4),STR2(N,4)
C	ENDDO
      RETURN
      END