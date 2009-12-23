!IPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
      SUBROUTINE GETSTRESS
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKABMOD
      SAVE
!
      DATA ITIME/0/,HRINC/0/
      CHARACTER*1000 HEADWT,HEDR
!      COMMON /STR/
!     +STRESS(MNP,2),STR11(MNP),STR21(MNP),STR10(MNP),STR20(MNP)
!
      IF(ITIME == 0) THEN
        IF(INWGT /= 0) THEN
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
        ELSEIF(ICORDIN == 0) THEN
          RETURN
!EFa aug08, added if-clause for external coordinates for wave data        
        ELSEIF(INSTR == 0 .AND. INBNSTR == 0) THEN
          RETURN
!-        
        ENDIF
        iydd=iyrr
        iyrd=iyrr
        TTT=0.
        TETADD=TTT
        IF(INSTR > 0) THEN
          REWIND INSTR
          READ(INSTR,'(A100)') HEDR(1:100)
          IF(HEDR(1:8) == 'STRESS-D') THEN
            MBND=0
            READ(INSTR,5001,ERR=145,END=145) IDYY,TTTV,NV,IYDD
            go to 150
 145        CONTINUE
!ipk sep04
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
        ELSEIF(ICORDIN == 0) THEN
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR   No binary weighting file defined'
          WRITE(75,*) 'ERROR   No binary weighting file defined'
          STOP
        ENDIF
      ENDIF
      ITIME=1
      IF(MBND == 1) THEN
        IF(ITIME == 1) THEN
          ITIME=2
!
!      SURFACE STRESS DATA READ
!
!
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
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          STOP 'ERROR IN SURFACE STRESS DATA'
  166     CONTINUE 
        ENDIF
!
!       Check for zero values on input file
!
        IF(NV == 0) THEN
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'Error!! total values = 0 on surface stress file'
          WRITE(75,*) 'Header contains time',ttt,' Year',iydd
          WRITE(*,*) 'Error!!  total values = 0 on surface stress file'
          WRITE(*,*) 'Header contains time',ttt,' Year',iydd
          STOP
        ENDIF
        WRITE(75,*) 'Time ONLY file =',ttt,'current time =',TET
!
!       Now interpolate to get stress values
!
        DO N=1,NP
            STRESS(N,1)=0.
            STRESS(N,2)=0.
            SPWP(N)=0
            AWL(N)=0
            SWH(N)=0
            WVDR(N)=0
            DO M=1,3
              K=NODWT(N,M)
              IF(K > 0) THEN
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
!-
!..... MBND = 0 is the case of multiple files to be interpolated
!-
  250   CONTINUE
!
        IYDD=IYRD
!
!     logic to pass end of year
!
        IF(IYDD > IYRR) THEN
          IF(MOD(IYRR,4) == 0) THEN 
            HRINC=HRINC+366.*24.
          ELSE
            HRINC=HRINC+365.*24.
          ENDIF
        ENDIF
!
!    Test for data year less than current
!
        IF(IYDD < IYRR) THEN
          IF(MOD(IYDD,4) == 0) THEN 
            HRINC=HRINC-366.*24.
          ELSE
            HRINC=HRINC-365.*24.
          ENDIF
          IYDD=IYDD+1   
        ENDIF
!
        WRITE(75,*) 'Simulation time,file time,year correction'
        WRITE(75,*) TET+(DAYOFY-1.)*24.,TTT,HRINC,IYDD,IYRR
        WRITE(75,*) 'tet,dayofy',tet,dayofy
!
        IF(TET+(DAYOFY-1.)*24. > TTT+HRINC) THEN
!ipk  reset hrinc
          HRINC=0.
!
!-
!...... TTT not yet large enough  move 1 to 0 and read another
!-
          DO K=1,NV
            DO L=1,6
              STR1(K,L) = STR2(K,L)
              STR2(K,L)=0.
            ENDDO
          ENDDO
          T0=TTT
!
!      SURFACE STRESS DATA  read
!
!
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
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          write(75,*) 'ERROR IN SURFACE STRESS DATA'
          STOP 'ERROR IN SURFACE STRESS DATA'
!
  287     CONTINUE 
!            write(75,*) 'tttv,nv,iydd,irma2,tetadd'
!            write(75,*)  tttv,nv,iydd,irma2,tetadd
!ipk jan98 add to preserve iydd
          IYRD=IYDD
!     check for zero nodes on input file
!
          if(nv == 0) then
!ipk sep04
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
           write(75,*) 'Error!! total values = 0 on surface stress file'
           Write(75,*) 'Header contains time',tttv,' Year',iydd
           write(*,*) 'Error!!  total values = 0 on surface stress file'
           Write(*,*) 'Header contains time',tttv,' Year',iydd
           stop
          endif
!ipk jun02          TTT=TTTV+TETADD
!
  288     CONTINUE
          IF(MOD(IYDD,4) == 0) THEN
            HYR=366.*24.
          ELSE
            HYR=365.*24.
          ENDIF
!
          IF(TTT > HYR) THEN
            TTT=TTT-HYR
            IYDD=IYDD+1
          ELSE
            GO TO 289
          ENDIF
          GO TO 288
  289     CONTINUE
          IYRD=IYDD
!
          WRITE(75,6225) TTTV,TTT,iydd,iyrr
 6225     FORMAT( /5X, 'SURFACE STRESS DATA FILE READ AT TIME =', F10.2,&
     &    '  CORRECTED TIME = ',F10.2/'YEARS',2I8 )
!ipk            write(75,*) 'Time from file =',ttt,'current time =',TET
          GO TO 250
        ELSE
!-
!...... We have now spanned across the time for interpolation
!-
          write(75,*) 'time,t0,ttt',tET,t0,ttt
          FCTI=(TET+(DAYOFY-1)*24.-T0)/(TTT+HRINC-T0)
          TTTC=TTT+HRINC
          HRINC=0.
          WRITE(75,*) 'GETSST-169  FCTI',FCTI
!
            if(idebug == 1) then
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
                IF(K > 0) THEN
                STRESS(N,1)=STRESS(N,1)                                 &
     &              +(STR1(K,1)+FCTI*(STR2(K,1)-STR1(K,1)))*WAT(N,M)
                STRESS(N,2)=STRESS(N,2)                                 &
     &              +(STR1(K,2)+FCTI*(STR2(K,2)-STR1(K,2)))*WAT(N,M)
                SPWP(N)=SPWP(N)                                         &
     &              +(STR1(K,3)+FCTI*(STR2(K,3)-STR1(K,3)))*WAT(N,M)
                AWL(N)=AWL(N)                                           &
     &              +(STR1(K,4)+FCTI*(STR2(K,4)-STR1(K,4)))*WAT(N,M)
                SWH(N)=SWH(N)                                           &
     &              +(STR1(K,5)+FCTI*(STR2(K,5)-STR1(K,5)))*WAT(N,M)
                WVDR(N)=WVDR(N)                                         &
     &              +(STR1(K,6)+FCTI*(STR2(K,6)-STR1(K,6)))*WAT(N,M)
                ENDIF 
              ENDDO
!EFa jul08, testing              
!(Achtung: interne Knoten!)
              WRITE(234,*)n,'awl(n): ',awl(n),wat(n,1),wat(n,2),wat(n,3) 
!-              
          ENDDO
!
        ENDIF
!
      ENDIF
  600 CONTINUE
!      DO N=1,NP
!        WRITE(135,'(I5,7F9.3,3I6)') N,SPWP(N),AWL(N),SWH(N),WVDR(N),
!     +  WAT(N,1),WAT(N,2),WAT(N,3),NODWT(N,1),NODWT(N,2),NODWT(N,3)
!      ENDDO
!      DO N=1,NV
!        WRITE(136,*) N,STR1(N,4),STR2(N,4)
!      ENDDO
      RETURN
      END