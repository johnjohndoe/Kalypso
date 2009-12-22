C     Last change:  NIS  15 May 2008    8:35 pm
cipk  last update FEB 26 2007 REFINE TEST FOR SUBMERGENCE
cipk  last update nov 28 2006 allow for 1-d control structures
cipk  last update JUNE 22 2005 ADD TO rma10
cipk  last update JULY 30 2004 fix bug in 2-d/1-d test
CIPK  LAST UPDATE JUL 7 2004 ALLOW FOR 1-d ELEMENTS RESTORE TRANSELV 
cipk  last update Dec 13 2000 correct for types other 10
      SUBROUTINE SUBSET
C-

      USE BLK10MOD
      USE BLKSUBMOD
*-
*...... Test for and set submerged weir elements
*-
      if(maxn == 1) then
        DO N=1,NP
          IF (NFIX (N) / 100 == 12) THEN
            ISUBM(N)=0
          ELSEIF (NFIX (N) / 100 == 102) THEN
            ISUBM(N)=0
          ELSEIF (NFIX (N) / 100 > 100) THEN
C
C     This indicates a boundary node
C
            ISUBM (N) = 0
          ELSE
            ISUBM(N)=-1
          ENDIF
        ENDDO
      endif
   
CIPK JUL04 Use TRC to allow for units
      IF(GRAV > 30.) THEN
        TRC=0.25
      ELSE
        TRC=0.1
      ENDIF

      DO  NN=1,NE
        IF(IMAT(NN) /= 0) THEN

          !check, whether element is really a control structure element
          IF(IMAT(NN) > 903  .AND. IMAT(NN) < 990) THEN
CIPK JUN05
cipk outnov06           IF(NCTREF(IMAT(NN)) == 0) THEN
cipk dec00

            !tabular data control structure element
            if (njt (imat (nn) - 900) == 10) then
cipk outnov06              if(maxn /= 1) go to 332
cipk outnov06              ISUBMEL(nn)=0
cipk jul04
              !number of corner nodes
              ncn = ncorn (nn)
              !only for corner nodes
                   DO K = 1, NCN, 2
                IF (TRANSEL (NOP (NN, K)) < -9000.) THEN
                  TRANSEL (NOP (NN, K)) = 10000.
                  WHGT (NOP (NN, K)) = 10000.
                ENDIF
              ENDDO
CIPK nov06
              !NCTREF :: nctref is weir number in definition file
              IF (NCTREF (IMAT (NN)) == 0) THEN
                !do this only in the first iteration
                if (maxn /= 1) go to 332
                ISUBMEL (nn) = 0
cipk jul04
                ncn = ncorn (nn)

                !for 2D control structure elements with tabular data
                !***************************************************
                IF (NCN == 8) THEN
                  WRITE(75,6001) 
     +            NN,N,NOP(NN,1),WSLL(NOP(NN,1)),WSLL(NOP(NN,7)),
     +            WHGT(NOP(NN,1)),WHGT(NOP(NN,7)),TRANSEL(NOP(NN,1))
 6001           FORMAT(' SUBSET1',3I6,5F12.3)

CIPK JUL04            IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))+.35) THEN
                  IF(WSLL(NOP(NN,1)) < TRANSEL(NOP(NN,1))) THEN
                    IF(ABS(WSLL(NOP(NN,1))-WSLL(NOP(NN,7))) < TRC)
     +                THEN
     +      
                      IF(WSLL(NOP(NN,7)) < WHGT(NOP(NN,7))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF

                  WRITE(75,6001) 
     +            NN,N,NOP(NN,3),WSLL(NOP(NN,3)),WSLL(NOP(NN,5)),
     +            WHGT(NOP(NN,3)),WHGT(NOP(NN,5)),TRANSEL(NOP(NN,3))

CIPK JUL04            IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))+.35) THEN
                  IF(WSLL(NOP(NN,3)) < TRANSEL(NOP(NN,3))) THEN
                    IF(ABS(WSLL(NOP(NN,3))-WSLL(NOP(NN,5))) < TRC)
     +                THEN
            
                      IF(WSLL(NOP(NN,5)) < WHGT(NOP(NN,5))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF

                  WRITE(75,6001) 
     +            NN,N,NOP(NN,5),WSLL(NOP(NN,5)),WSLL(NOP(NN,3)),
     +            WHGT(NOP(NN,5)),WHGT(NOP(NN,3)),TRANSEL(NOP(NN,5))

CIPK JUL04            IF(WSLL(NOP(NN,5)) < WHGT(NOP(NN,5))+.35) THEN
                  IF(WSLL(NOP(NN,5)) < TRANSEL(NOP(NN,5))) THEN
                    IF(ABS(WSLL(NOP(NN,3))-WSLL(NOP(NN,5))) < TRC)
     +                THEN
             
                      IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,5)) < WHGT(NOP(NN,5))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF

                  WRITE(75,6001) 
     +            NN,N,NOP(NN,7),WSLL(NOP(NN,7)),WSLL(NOP(NN,1)),
     +            WHGT(NOP(NN,7)),WHGT(NOP(NN,1)),TRANSEL(NOP(NN,7))

CIPK JUL04            IF(WSLL(NOP(NN,7)) < WHGT(NOP(NN,7))+.35) THEN
                  IF(WSLL(NOP(NN,7)) < TRANSEL(NOP(NN,7))) THEN
                    IF(ABS(WSLL(NOP(NN,1))-WSLL(NOP(NN,7))) < TRC)
     +                           THEN
             
                      IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,7)) < WHGT(NOP(NN,7))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF

c          set isubm for all nodes around a weir element
                  DO L=1,NCORN(NN)
cipk oct00 
                    if(isubm(nop(nn,l)) == -1) then
                      ISUBM(NOP(NN,L))=1
                     elseif(isubm(nop(nn,l)) == 0) then
                      ISUBM(NOP(NN,L))=2
                    else
                      ISUBM(NOP(NN,L))=1
                    endif
                  ENDDO
                  ISUBMEL(nn)=1
CIPK NOV06 ISUBMEL =1 MEANS SUBMERGED
                  GO TO 332

                !for 1D control structure elements with tabular data
                !***************************************************
                ELSE

                  WRITE(75,6001) 
     +            NN,N,NOP(NN,1),WSLL(NOP(NN,1)),WSLL(NOP(NN,3)),
     +            WHGT(NOP(NN,1)),WHGT(NOP(NN,3)),TRANSEL(NOP(NN,1))
                  call FLUSH (75)

CIPK JUL04            IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))+.35) THEN
                  IF(WSLL(NOP(NN,1)) < TRANSEL(NOP(NN,1))) THEN
                    IF(ABS(WSLL(NOP(NN,1))-WSLL(NOP(NN,3))) < TRC)
     +                THEN
     +       
                      IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF

                  WRITE(75,6001) 
     +            NN,N,NOP(NN,3),WSLL(NOP(NN,3)),WSLL(NOP(NN,1)),
     +             WHGT(NOP(NN,3)),WHGT(NOP(NN,1)),TRANSEL(NOP(NN,3))

CIPK JUL04            IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))+.35) THEN
                  IF(WSLL(NOP(NN,3)) < TRANSEL(NOP(NN,3))) THEN
                    IF(ABS(WSLL(NOP(NN,1))-WSLL(NOP(NN,3))) < TRC)
     +                THEN
            
                      IF(WSLL(NOP(NN,1)) < WHGT(NOP(NN,1))) THEN
                        GO TO 331
                      ENDIF
                      IF(WSLL(NOP(NN,3)) < WHGT(NOP(NN,3))) THEN
                        GO TO 331
                      ENDIF
                    ELSE
                      GO TO 331
                    ENDIF
                  ENDIF


c          set isubm for all nodes around a weir element

                  DO L=1,3
cipk oct00 
                    if(isubm(nop(nn,l)) == -1) then
                      ISUBM(NOP(NN,L))=1
                    elseif(isubm(nop(nn,l)) == 0) then
                      ISUBM(NOP(NN,L))=2
                    else
                      ISUBM(NOP(NN,L))=1
                    endif
                  ENDDO
                  ISUBMEL(nn)=1
CIPK NOV06 ISUBMEL =1 MEANS SUBMERGED
                  GO TO 332
                ENDIF




  331           CONTINUE
                DO L=1,NCORN(NN)
                  if(isubm(nop(nn,l)) == -1) then
                    ISUBM(NOP(NN,L))=0
                  elseif(isubm(nop(nn,l)) == 1) then
                    ISUBM(NOP(NN,L))=2
                  else
                    ISUBM(NOP(NN,L))=0
 
                  endif
                ENDDO

  332           CONTINUE
c             WRITE(75,*) 'SUBSET2',NN,ISUBMEL(NN),ISUBM(24)

                if(isubmel(nn) == 1) then
                  write(75,*) 'At step',icyc,' element ',nn,' submerged'
                elseif (WSLL (NOP (NN, 1)) > WHGT(NOP(NN,1)) .OR. 
     +            WSLL(NOP(NN,3)) > WHGT(NOP(NN,3)) .OR. 
     +            WSLL(NOP(NN,5)) > WHGT(NOP(NN,5)) .OR. 
     +            WSLL(NOP(NN,7)) > WHGT(NOP(NN,7)))  THEN
                  write(75,*) 'At step',icyc,' element ',nn,' flowing'
                  WRITE(75,6001) 
     +            NN,N,NOP(NN,1),WSLL(NOP(NN,1)),WSLL(NOP(NN,3)),
     +            WHGT(NOP(NN,1)),WHGT(NOP(NN,3)),TRANSEL(NOP(NN,1))
                  WRITE(75,6001) 
     +            NN,N,NOP(NN,5),WSLL(NOP(NN,5)),WSLL(NOP(NN,7)),
     +            WHGT(NOP(NN,5)),WHGT(NOP(NN,7)),TRANSEL(NOP(NN,5))
                endif

              else
                DO L=1,NCORN(NN)
                  ISUBM(NOP(NN,L))=0
                ENDDO
              endif
            ELSE

              ISUBMEL(nn)=0

              DO L=1,NCORN(NN)
                if(isubm(nop(nn,l)) == -1) then
                  ISUBM(NOP(NN,L))=0
                elseif(isubm(nop(nn,l)) == 1) then
                  ISUBM(NOP(NN,L))=2
                else
                  ISUBM(NOP(NN,L))=0
 
                endif
              ENDDO

            ENDIF
          ELSEIF(IMAT(NN) > 900) THEN
              DO L=1,NCORN(NN)
                if(nop(nn,l) > 0) ISUBM(NOP(NN,L))=0
              ENDDO
          ENDIF
        ENDIF

      ENDDO

cipk oct00  reset to for submerged elements

      do n=1,ne
        IF(IMAT(N) > 903  .AND. IMAT(N) < 990) THEN
          if(ncorn(n) == 8) then
            if(isubmel(n) == 1) then
              do k=2,ncorn(n),2
                n2=nop(n,k)
                if(isubm(n2) == 2) then
                  if(ibn(n2) == 20) then
                    ibn(n2)=21
                  elseif(ibn(n2) == 21) then
                    ibn(n2)=20
                  endif
                elseif(isubm(n2) == 1) then
                  if(ibn(n2) == 11) then
                    ibn(n2)=20
                  endif
                endif
              enddo
            endif
          endif
        ENDIF
      enddo 
c       IF(MAXN == 1) THEN
C         WRITE(145,*) 'IBN AND ISUBM'
C         DO N=1,NP
C           WRITE(145,'(3I8,F12.3)') N,IBN(N),ISUBM(N),ALFA(N)   
C         ENDDO
c       ENDIF

      RETURN
      END
