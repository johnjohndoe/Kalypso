C     Last change:  MD   30 Jul 2009    2:04 pm
cipk  last update apr 05 2006   make a first call to get sed initial conditions
crrr  Last update Aug 23 1997
      SUBROUTINE BED
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD
      Implicit none
!      DATA NLMTP/100/
      Integer :: NLMTP = 100
      Integer :: NN, N, K, I, L, N9, N1, JJ, J, I1, K1
      Integer :: IBED, IELEV, NLA, NTYPS, ETEMP, NL
      Real (KIND = 8) :: TEMP1
C**********************************************************************
C
C       THIS SUBROUTINE READS THE INITIAL BED PROFILE AND ELEVATION DATA 
C
C
C*******************************************************************
C-
      DO N=1,NE
        IF(IMAT(N) > 0) THEN
          NCN=NCORN(N)
          DO K=1,NCN
            !MD: Fuer alle Mittseitenknoten (lokal)
            !MD: Teilen der Knotenummer / 2 --> kein Rest
            IF(MOD(K,2) == 0) THEN
              IMID(NOP(N,K),1)=NOP(N,K-1)
              IF(K == NCN) THEN
                IMID(NOP(N,K),2)=NOP(N,1)
              ELSE
                IMID(NOP(N,K),2)=NOP(N,K+1)
              ENDIF
              !MD: Bsp. fuer Mittseiten-Kn 2:
              !MD:    IMID(2,1) = 1  =Eck-Knoten 1
              !MD:    IMID(2,2) = 3  =Eck-Knoten 2

            !MD: Fuer alle Eckknoten: IMID(2,1) = 0
            ELSE
              IMID(NOP(N,K),1)=0
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      ALLOCATE (SSTOM(MXSEDLAY),SMVALM(MXSEDLAY)
     +         ,GBOM(MXSEDLAY),THICKOM(MXSEDLAY))

      SSTOM=0.
      SMVALM=0.
      GBOM=0.


      IF(NB == 0) GO TO 10
      
cipk aug05 update to allow redefinition of bed

      if(inewbed == 1) go to 10

      DO N=1,NPM
        TM(N)=0.
        !MD: TM = Total Mass [kg/m²] ueber alle Layer
      ENDDO
cipk apr06   make a first call to get sed initial conditions

!MD  deactivated 14.08.2008
!MD  Data for restart with suspenend sediment
!MD  causes problems in Kalypso
!----------------------------------
!MD      CALL GETINIT(IBIN,0)
!MD      CALL FORMBED(1)
!MD      RETURN
!MD  End of deactivation 14.08.2008

   10 CONTINUE
CIPK AUG05      IF(NB == 0) THEN
CIPK SEP05        CALL GINPT(LIN,ID,DLIN)
CIPK AUG05      ENDIF
        IF(ID(1:3) == 'CB ') THEN
          READ(DLIN,'(2I8)') IBED,IELEV
        ELSE
          WRITE(LOUT,*) ' ERROR -- EXPECTED LINE TYPE CB'
          STOP
        ENDIF
C
C     READ INITIAL BED PROFILE DATA

      WRITE(LOUT,*) ' IBED = ', IBED

!MD:  Fuer Bed-Layer Parameter an allen Knoten gleich
!MD:  Kennung IBED = 1
      IF(IBED == 1) THEN
      CALL GINPT(LIN,ID,DLIN)
        IF(ID(1:3) == 'CBD') THEN
          READ(DLIN,'(2I8,F8.0)') N,NLA,TEMP1
          WRITE(75,*) 'MD: CBD mit IBED=1 ist gelesen'
          if (NLA > mxsedlay)  then
            write(*,*) '  lt > mlayer ', N,NLA,TEMP1
            write(75,*) '  lt > mlayer ', N,NLA,TEMP1
            stop 
          endif
          CALL GINPT(LIN,ID,DLIN)
        ELSE
          WRITE(LOUT,*) ' ERROR -- EXPECTED LINE TYPE CBD'
          STOP
        ENDIF
C-
        NLAYO(1)=NLA
        NL=NLAYO(1)
        IF(NL > 0) THEN
          WRITE(LOUT,6014)
          WRITE(LOUT,6015)
          DO I=1,NL
crrr aug97 update test to 3 characters
            IF(ID(1:3) /= 'CBL') THEN
              WRITE(LOUT,*) ' ERROR -- EXPECTING LINE TYPE CBL'
              STOP
            ENDIF
            READ(DLIN,5003) L,SSTO(1,L),SMVAL(1,L),GBO(1,L),
     +                      THICKOND(1,L)
            WRITE(75,*) 'MD: CBL mit IBED=1 ist gelesen'
            CALL GINPT(LIN,ID,DLIN)
            IF(I <= 2) THEN
              WRITE(LOUT,6016) N,L,THICKOND(1,L),GBO(1,L),SSTO(1,L)
     +                         ,SMVAL(1,L)
            ELSE
              WRITE(LOUT,6017) L,THICKOND(1,L),GBO(1,L),SSTO(1,L)
     +                         ,SMVAL(1,L)
            ENDIF
          ENDDO
        ENDIF
        IF (TEMP1 >= 0.0) TM(1)=TEMP1
        DO N=2,NPM
          NLAYO(N)=NLAYO(1)
          IF(NL > 0) THEN
            DO L=1,NL
              SSTO(N,L)=SSTO(1,L)
              SMVAL(N,L)=SMVAL(1,L)
              GBO(N,L)=GBO(1,L)
              THICKOND(N,L)=THICKOND(1,L)
              THICKO(N,L)=THICKOND(1,L)
            ENDDO
          ENDIF
          IF(TEMP1 >= 0.) TM(N)=TEMP1
        ENDDO
cc        CALL GINPT(LIN,ID,DLIN)

        CALL FORMBED(1)

!MD:  Fuer Bed-Layer Parameter am Knoten N
!MD:  Kennung IBED = 2
      ELSEIF(IBED == 2) THEN
        CALL GINPT(LIN,ID,DLIN)
        IF(ID(1:3) /= 'CBD') THEN
          WRITE(LOUT,*) ' ERROR -- EXPECTING LINE TYPE CBD'
          STOP
        ENDIF
        READ(DLIN,'(2I8,F8.0)') N,NLA,TEMP1
        WRITE(75,*) 'MD: CBD mit IBED=2 ist gelesen'
        if (NLA > mxsedlay)  then
          write(*,*) '  lt > mlayer ', N,NLA,TEMP1
          write(75,*) '  lt > mlayer ', N,NLA,TEMP1
          stop 
        endif

        WRITE(LOUT,6014)
        WRITE(LOUT,6015)
C-
 12     CONTINUE
        NLAYO(N)=NLA
        NL=NLAYO(N)

        DO I=1,NL
            CALL GINPT(LIN,ID,DLIN)
          IF(ID(1:2) /= 'CBL') THEN
           WRITE(LOUT,*) ' ERROR -- EXPECTING LINE TYPE CBL'
           STOP
          ENDIF
          READ(DLIN,5003) L,SSTO(N,L),SMVAL(N,L),GBO(N,L),THICKOND(N,L)
          WRITE(75,*) 'MD: CBL mit IBED=2 ist gelesen'
          IF(I <= 2) THEN
            WRITE(LOUT,6016) N,I,THICKOND(N,I),GBO(N,I),SSTO(N,I),
     +                       SMVAL(N,I)
          ELSE
            WRITE(LOUT,6017) I,THICKOND(N,I),GBO(N,I),SSTO(N,I),
     +                       SMVAL(N,I)
          ENDIF
        ENDDO
        IF (TEMP1 >= 0.0) TM(N)=TEMP1

C..... READ NEXT RECORD
        CALL GINPT(LIN,ID,DLIN)
        IF(ID(1:3) /= 'CBD') THEN
          GO TO 400
        ENDIF
        READ(DLIN,'(2I8,F8.0)') N,NLA,TEMP1
        if (NLA > mxsedlay)  then
          write(*,*) '  lt > mlayer ', N,NLA,TEMP1
          write(75,*) '  lt > mlayer ', N,NLA,TEMP1
          stop 
        endif

        GO TO 12
  400   CONTINUE

        CALL FORMBED(1)

!MD:  Fuer Bed-Layer Parameter je Material-Klasse CBMT ....
!MD:  Kennung IBED = 3
      ELSEIF(IBED == 3) THEN
C-
      CALL GINPT(LIN,ID,DLIN)
      WRITE(LOUT,6014)
        
  440   N1=1
        !MD: Einlese-Schleife 450 fuer jeden CBMT bis CBL Block
        !MD: -----------------------------------------------
  450   CONTINUE
        IF(ID(1:4) == 'CBMT') THEN
          N9=N1+8
          READ(DLIN,'(9I8)') (IMTREF(I),I=N1,N9)
          WRITE(75,*) 'MD: CBMT mit IBED=3 ist gelesen'
          N1=N1+9
          CALL GINPT(LIN,ID,DLIN)
          GO TO 450
          ! Weitere Materialklassen einlesen
        ELSEIF(ID(1:3) /= 'CBD') THEN
          WRITE(LOUT,*) ' ERROR -- EXPECTING LINE TYPE CBD'
          STOP
        ELSEIF(ID(1:3) == 'CBD') THEN
          READ(DLIN,'(2I8,F8.0)') N,NLA,TEMP1
          WRITE(75,*) 'MD: CBD mit IBED=3 ist gelesen'
          if (NLA > mxsedlay)  then
            write(*,*) '  lt > mlayer ', N,NLA,TEMP1
            write(75,*) '  lt > mlayer ', N,NLA,TEMP1
            stop
          endif
          DO K=1,NLMTP
            NTYPS=K-1
            IF(IMTREF(K) == 0) GO TO 460
          ENDDO
        ENDIF

  460   CONTINUE      
        IF(NTYPS > 0) THEN
          WRITE(LOUT,6010) (IMTREF(K),K=1,NTYPS)
          WRITE(LOUT,6015)
        ENDIF
      
C-
 475    CONTINUE
        NL=NLA

        DO I=1,NL
          CALL GINPT(LIN,ID,DLIN)
          IF(ID(1:3) /= 'CBL') THEN
            WRITE(LOUT,*) ' ERROR -- EXPECTING LINE TYPE CBL'
            STOP
          ENDIF
          READ(DLIN,5003) L,SSTOM(L),SMVALM(L),GBOM(L),THICKOM(L)
          WRITE(75,*) 'MD: CBL mit IBED=3 ist gelesen'
          WRITE(LOUT,6017) L,THICKOM(L),GBOM(L),SSTOM(L),SMVALM(L)
        ENDDO
        
C..... READ NEXT RECORD

        DO N=1,NE
          !MD:  IF(IMAT(N) > 0) THEN
          !MD:  Layer auch fuer momentan trockene Elemente und Knoten!!
          IF(IMAT(N) /= 0) THEN
            DO I=1,NLMTP
              IF(IMTREF(I) == 0 .AND. I > 1) GO TO 480
              IF(IMTREF(I) == ABS(IMAT(N)) .OR. IMTREF(I) == 0) THEN
                DO JJ=1,NCORN(N)
                  J=NOP(N,JJ)
                  NLAYO(J)=NLA
                  TM(J)=TEMP1
                  !MD: wrong for variing Layers: DO K=1,NLA
                  DO K=1, NLAYO(J)
                    THICKOND(J,K)=THICKOM(K)
                    THICKO(J,K)=THICKOM(K)
                    GBO(J,K)=GBOM(K)
                    SSTO(J,K)=SSTOM(K)
                    SMVAL(J,K)=SMVALM(K)
                  ENDDO
                ENDDO
              ENDIF
            ENDDO
  480       CONTINUE          
          ENDIF
        ENDDO

      CALL GINPT(LIN,ID,DLIN)
        IF(ID(1:4) /= 'CBMT') THEN
          GO TO 500
          !Wenn Ende erreicht
        Else
          GO TO 440
          !Weiterlesen neuer Materialklassen
        ENDIF

  500   CONTINUE


!MD: neu fuer alle Mittseiten-Knoten
!MD--------------------------------------
      DO N=1,NPM
        IF(IMID(N,1) > 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
          NLAYO(NN)=MIN(NLAYO(IMID(NN,1)),NLAYO(IMID(NN,2)))
          TM(NN)=(TM(IMID(NN,1))+TM(IMID(NN,2)))/2.
         DO K=1,NLAYO(NN)
           THICKOND(NN,K)=(THICKOND(IMID(NN,1),K)+
     +                     THICKOND(IMID(NN,2),K))/2.
           THICKO(NN,K)=(THICKO(IMID(NN,1),K)+THICKO(IMID(NN,2),K))/2.
           GBO(NN,K)=(GBO(IMID(NN,1),K)+GBO(IMID(NN,2),K))/2.
           SSTO(NN,K)=(SSTO(IMID(NN,1),K)+SSTO(IMID(NN,2),K))/2.
           SMVAL(NN,K)=(SMVAL(IMID(NN,1),K)+SMVAL(IMID(NN,2),K))/2.
         ENDDO
        ENDIF
      ENDDO
!MD: neu fuer alle Mittseiten-Knoten
!MD--------------------------------------


        CALL FORMBED(1)

      ENDIF

C   -------------------------------------------------
C     READ INITIAL BED ELEVATION DATA
C   -------------------------------------------------
      IF (IELEV == 1) THEN
         IF(IBED == 1 .OR. IBED == 2 .OR. IBED == 3) GO TO 401
          CALL GINPT(LIN,ID,DLIN)
  401    CONTINUE

         K1=0
  405    CONTINUE
         IF(ID(1:3) /= 'CBE') THEN
           GO TO 407
         ENDIF
         READ (DLIN,'(I8,F8.0)') I1,ETEMP
         CALL GINPT(LIN,ID,DLIN)
         K1=K1+1
         ELEVB(I1)=ETEMP
         GO TO 405
      ENDIF
  407 CONTINUE
      RETURN

 5003 FORMAT(I8,7F8.5)
 6010 FORMAT(/9X,'ELEMENT TYPES',/(I16,9I8))
 6014 FORMAT(32X,'INITIAL   BED   PROFILE')
 6015 FORMAT(
     */8X,'NODE',4X,'LAYER',5X,'THICKNESS',4X,
     *'BULK DENSITY  CRITICAL SHEAR  ER. RATE CONST.'/8X,'NO.',
     *7X,'NO.',8X,'METERS',7X,'KG./CU.M',8X,'N/SQ.M',6X,
     *'KG/SQ.M./SEC.')
 6016 FORMAT(2I10,F15.3,F15.1,F15.3,1PE15.2)
 6017 FORMAT(10X,I10,F15.3,F15.1,F15.3,1PE15.2)

                                                                                                                                    
      END
