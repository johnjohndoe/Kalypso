C     Last change:  MD   30 Jul 2009    2:04 pm
CIPK  LAST UPDATE APR 20 2006 ADD INPUT OPTION FOR TLAYM
CIPK  LAST UPDATE NOV 19 1997
crrr  last update Aug 30 1997 test for max number of layers
cipk  last update Aug 6 1997 change format
CIPK  LAST UPDATE JUNE 2 1997
CIPK  LAST UPDATE APR 7 1997 FIX LABEL AND FORMAT 
cipk  last update June 4 1996
      SUBROUTINE SPROP
C-
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKFTMOD
c      USE PARAMMOD
      Implicit none

      !MD: Zaehler fuer Materialklassen
      INTEGER :: VSETMAX
      INTEGER :: NI_MAT(85), NIMAT

      Integer :: NN, N1, N9, I, J, K, L, N, JJ, NSEDTYP, NTYPS
      REAL (KIND = 8) :: TTLAYM, WEGTM

cipk nov97 add alternate reading logic
      CHARACTER (LEN = 8) ::  IDA
      CHARACTER (LEN = 72) :: DLINA
      Integer :: NLMTP = 100
C-
C******************************************************************
C
C         THIS SUBROUTINE READS THE PROPERTIES OF COHESIVE SEDIMENTS THAT
C      EXIST IN THE SYSTEM AND TRANSFERS.
C      THE INITIAL BED PROFILE AND BED PROPERTIES ARE READ IN BY
C      SUBROUTINE BED.
C
C******************************************************************
C
cipk  set IBNK =0 for the moment until bank shear is implemented

!MD: Basis-Variable: Wasserdichte
      !MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
      !     Je nach Vorgabe der SI-Einheiten unter "IGRV"
      ROAVG=1.935
      IF (GRAV.LT.32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte

      N1 = 0
      VSETMAX = 0

  200 CONTINUE
      CALL GINPT(LIN,ID,DLIN)
      IF(ID(1:4) .EQ. 'CSS1') THEN
        N1 = N1 + 1
        !MD: READ(DLIN,'(I8,5F8.5,I8)') ISVL,VSST,CRCON1,VSS1,CRCON2,EXP2,NM
        READ(DLIN,'(I8)') ISVL


        IF(ISVL.EQ.2  .and. N1.eq.1) THEN
          READ(DLIN,'(I8,5F8.3)') ISVL,VSST,CRCON1(1),VSS1(1),
     +                   CRCON2(1),EXP2(1)
          VSK(1)=VSS1(1)/CRCON1(1)**EXP2(1)
          VSS2(1)=VSK(1)*CRCON2(1)**EXP2(1)
          WRITE(LOUT,6033)
          WRITE(LOUT,6034) CRCON1(1),VSS1(1)
          WRITE(LOUT,6035) CRCON1(1),CRCON2(1),VSK(1),EXP2(1)
          WRITE(LOUT,6036) CRCON2(1),VSS2(1)
        
        !MD: constant Velocity
        ELSEIF(ISVL .EQ. 1 .and. N1.eq.1) THEN
          READ(DLIN,'(I8,F8.3)') ISVL,VSST
          WRITE(LOUT,6037) VSST*1000.

        !MD: NEW for Roughness-Classes (IMAT)
        ElseIF(ISVL .EQ. 3 .or. N1.ge.1) THEN
          READ(DLIN,'(I8,I8,4F8.3)') ISVL,NIMAT,
     &           CRC_1(NIMAT),VS_1(NIMAT),CRC_2(NIMAT),EX_2(NIMAT)

          WRITE(75,*) 'EXPECTED EX_2: ', EX_2(NIMAT)
          VS_K(NIMAT)=VS_1(NIMAT)/CRC_1(NIMAT)**EX_2(NIMAT)
          VS_2(NIMAT)=VS_K(NIMAT)*CRC_2(NIMAT)**EX_2(NIMAT)

          NI_MAT(N1)=NIMAT
          IF (N1 == 1) THEN
            WRITE(LOUT,6033)
          END IF
          VSETMAX = N1
          WRITE(LOUT,6041) NIMAT
          WRITE(LOUT,6034) CRC_1(NIMAT),VS_1(NIMAT)
          WRITE(LOUT,6035) CRC_1(NIMAT),CRC_2(NIMAT),
     +                     VS_K(NIMAT),EX_2(NIMAT)
          WRITE(LOUT,6036) CRC_2(NIMAT),VS_2(NIMAT)
          GO TO 200
        ENDIF

      ELSEIF (N1==0) Then
        WRITE(75,*) 'EXPECTED CSS1 DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED CSS1 DATA - NONE FOUND'
        STOP
      ENDIF

      IF (VSETMAX.gt.85) Then
        WRITE(75,*) 'Number of available Mat-Classes is exceeded'
        WRITE(75,*) 'Mat-Classes must be < 85 --> STOP'
        WRITE(*,*) 'Number of available Mat-Classes is exceeded'
        WRITE(*,*) 'Mat-Classes must be < 85 --> STOP'
        STOP
      ENDIF


      DO N=1,NE
        !MD: Parameter auch fuer momentan trockene Elemente und Knoten!!
        IF(IMAT(N).NE.0 .and. ISVL.EQ.3) THEN
          DO I=1,VSETMAX
            IF(NI_MAT(I).EQ.ABS(IMAT(N)) .OR. NI_MAT(I).EQ.0) THEN
              NIMAT = NI_MAT(I)
              DO JJ=1,NCORN(N)
                J=NOP(N,JJ)
                CRCON1(J)=CRC_1(NIMAT)
                CRCON2(J)=CRC_2(NIMAT)
                VSS2(J)  =VS_2(NIMAT)
                VSS1(J)  =VS_1(NIMAT)
                VSK(J)   =VS_K(NIMAT)
                EXP2(J)  =EX_2(NIMAT)
              EndDo
            Endif
          EndDo
        ElseIF(IMAT(N).NE.0 .and. ISVL.EQ.2) THEN
          DO JJ=1,NCORN(N)
             J=NOP(N,JJ)
             CRCON1(J)=CRCON1(1)
             CRCON2(J)=CRCON2(1)
             VSS2(J)  =VSS2(1)
             VSS1(J)  =VSS1(1)
             VSK(J)   =VSK(1)
             EXP2(J)  =EXP2(1)
          EndDo
        Endif
      Enddo


      IF (N1 .eq.1 ) THEN
        CALL GINPT(LIN,ID,DLIN)
      END IF
      IF(ID(1:4) .EQ. 'CSS2') THEN
        READ (DLIN,'(3F8.5)') VK,RKS,D90
      ELSE
        WRITE(75,*) 'EXPECTED CSS2 DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED CSS2 DATA - NONE FOUND'
        STOP
      ENDIF

      IF (VK .EQ. 0)  VK=0.4
      WRITE (LOUT,6038) VK, RKS,D90

C..................................................................
C...      NEW BED PROPERTIES. SAME DRY MASS IN EACH LAYER.
C.................................................................
      CALL GINPT(LIN,ID,DLIN)
      NN=0

      !MD: Einlese-Schleife 250 fuer alle CSMT bis CSS4 Bloecke je Materialklassen
      !MD: Blockweise untereinander müssen:
      !MD:    CSMT  .... Anfang erste Materialklassen
      !MD:    CSS3  ....
      !MD:    CSS4  .... Ende erste Materialklassen
      !MD:    CSMT  .... Anfang zweite Materialklassen
      !MD;    ....  ....
      !MD:    CSS3  .... Ende xxx
      !MD: Einlese-Schleife 250 fuer alle CSMT bis CSS4 Bloecke je Materialklassen
      !MD: ------------------------------------------------------------------
  250 CONTINUE
      !MD  CALL GINPT(LIN,ID,DLIN)
CIPK SEP05
      NN=NN+1	      
      N1=1

      !MD: Einlese-Schleife 275 fuer jeden CSMT bis CSS4 Block
      !MD: -----------------------------------------------
  275 CONTINUE
        
      IF(ID(1:4) .EQ. 'CSMT') THEN
        N9=N1+8
        READ(DLIN,'(9I8)') (IMTREF(I),I=N1,N9)
        N1=N1+9
        CALL GINPT(LIN,ID,DLIN)
        GO TO 275
      ELSEIF(ID(1:4) .EQ. 'CSS3') THEN
        READ(DLIN,5001)NLAYTM,TAUCDM,GAWM,GABM
     +     ,TTLAYM ,GACM ,ERCM ,UNM

      !MD wenn Ende aller Daten erreicht
      ELSEIF(ID(1:6) .EQ. 'ENDSED' .OR. ID(1:3) .EQ. 'CB ') THEN
	NSEDTYP=NN
	GO TO 300
      ELSE	  
        WRITE(75,*) 'EXPECTED MORE SED DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED MORE DATA - NONE FOUND'
        STOP
      ENDIF

crrr aug97
      if (NLAYTM  .GT. MXSEDLAY) THEN
        WRITE(LOUT,'(/A,I3)')
     &      ' Too many cohesive sediment layers, NLAYT =', NLAYTM 
        WRITE(LOUT,'(/A)')
     &      ' Increase dimension MLAYER '
        STOP ' Too many cohesive sediment layers'
      ENDIF
   
      DO K=1,NLMTP
        NTYPS=K-1
        IF(IMTREF(K) .EQ. 0) GO TO 280
      ENDDO
      
  280 CONTINUE      
      IF(NTYPS .GT. 0) THEN
        WRITE(LOUT,6010) (IMTREF(K),K=1,NTYPS)
      ENDIF
      
      WRITE(LOUT,6001)TAUCDM
      WRITE(LOUT,6002)

      !MD:  NEUE Trocken-Rohdichte [kg/m3] fuer Gewicht des
      !     Sediments pro Fläche WEGTM
      WEGTM =TTLAYM *(GABM -ROAVG)*GACM/ (GACM -ROAVG)
      !MD GAWM wurde mit Wasserdichte ersetzt

      !MD: ALte Berechnung Deaktiviert: 13.01.2009
      !MD  WEGTM =TTLAYM *(GABM -GAWM )*GACM / (GACM -GAWM )
      !MD  Achtung: Annahme dass GAWM = Suspensionsdichte
      !MD  gleich Wasserdichte ist (nicht richtig!)

      WRITE(LOUT,6020)NLAYTM ,GAWM ,UNM ,GABM ,TTLAYM 
     +,GACM ,WEGTM ,ERCM 
      CALL GINPT(LIN,ID,DLIN)
      
      WRITE(LOUT,6021)

      !MD: Daten ueber alle Boden-Layer L
      DO L=1,NLAYTM 
	IF(ID(1:4) .EQ. 'CSS4') THEN
CIPK APR06 ADD INPUT OPTION FOR TLAYM
CIPK MAY06   ADD EROSTM
          READ(DLIN,5002) I,SSM(I),GBM(I),TLAYM(I),EROSTM(I)
          CALL GINPT(LIN,ID,DLIN)
 	ELSE
          WRITE(75,*) 'EXPECTED CSS4 DATA - NONE FOUND'
          WRITE(*,*) 'EXPECTED CSS4 DATA - NONE FOUND'
          STOP
	ENDIF
      ENDDO

      DO I=1,NLAYTM
        !MD:  NEUE Berechnung der Trocken-Rohdichte [kg/m3]
        GADM(I)=(GBM(I)-ROAVG)*GACM /(GACM -ROAVG)
        !MD GAWM wurde mit Wasserdichte ersetzt

        !MD:  ALte Berechnung Deaktiviert: 13.01.2009
        !MD  GADM(I)=(GBM(I)-GAWM )*GACM /(GACM -GAWM )
        !MD  Achtung: Annahme dass GAWM = Suspensionsdichte
        !MD  gleich Wasserdichte ist (nicht richtig!)

CIPK APR06 ADD INPUT OPTION FOR TLAYM
        IF(TLAYM(I) .EQ. 0.) THEN
          TLAYM(I)=WEGTM /GADM(I)
        ENDIF
CIPK MAY06   ADD EROSTM        
        WRITE(LOUT,6025)I,SSM(I),GBM(I),TLAYM(I),GADM(I),EROSTM(I)
      ENDDO

      DO N=1,NE
        !MD:  IF(IMAT(N) .GT. 0) THEN
        !MD:  Layer auch fuer momentan trockene Elemente und Knoten!!
        IF(IMAT(N) .NE. 0) THEN
          DO I=1,NLMTP
            IF(IMTREF(I).EQ.0  .AND.  I.GT.1) GO TO 350
            IF(IMTREF(I).EQ.ABS(IMAT(N)) .OR. IMTREF(I).EQ.0) THEN
              DO JJ=1,NCORN(N)
                J=NOP(N,JJ)
                NLAYTND(J)=NLAYTM
                TAUCDND(J)=TAUCDM
                UNND(J)=UNM
                GABND(J)=GABM
                TTLAYND(J)=TTLAYM 
                GACND(J)=GACM
                WEGTND(J)=WEGTM
                ERCND(J)=ERCM
                !MD: NEU Berechnung der Suspensionsdichte
                IF (VEL(6,J).ge.0.) THEN
                GAWND(J)=(VEL(6,J)/1000.)*(1.-(ROAVG/GACND(J))) + ROAVG
                Else
                  GAWND(J)= ROAVG
                END IF
                !MD: alte Vorgabe deaktiviert: 13.01.2009
                !MD  GAWND(J)=GAWM
                !MD: wrong for variing Layers: DO K=1,NLAYTM
                DO K=1, NLAYTND(J)
                  SSND(J,K)=SSM(K)
                  GBND(J,K)=GBM(K)
                  TLAYND(J,K)=TLAYM(K)
                  GADND(J,K)=GADM(K)
CIPK MAY06   ADD EROSTM                  
                  EROST(J,K)=EROSTM(K)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
  350     CONTINUE          
        ENDIF
      ENDDO

      GO TO 250
      !MD weiterlesen der CSMT-Daten!
      
  300 CONTINUE  


!MD: neu fuer alle Mittseiten-Knoten
!MD--------------------------------------
      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          NLAYTND(NN)=MIN(NLAYTND(IMID(NN,1)),NLAYTND(IMID(NN,2)))
          TAUCDND(NN)=(TAUCDND(IMID(NN,1))+TAUCDND(IMID(NN,2)))/2.
          UNND(NN)=(UNND(IMID(NN,1))+UNND(IMID(NN,2)))/2.
          GABND(NN)=(GABND(IMID(NN,1))+GABND(IMID(NN,2)))/2.
          TTLAYND(NN)=MIN(TTLAYND(IMID(NN,1)),TTLAYND(IMID(NN,2)))
          GACND(NN)=(GACND(IMID(NN,1))+GACND(IMID(NN,2)))/2.
          WEGTND(NN)=(WEGTND(IMID(NN,1))+WEGTND(IMID(NN,2)))/2.
          ERCND(NN)=(ERCND(IMID(NN,1))+ERCND(IMID(NN,2)))/2.
          GAWND(NN)=(GAWND(IMID(NN,1))+GAWND(IMID(NN,2)))/2.
          DO K=1,NLAYTND(NN)
            SSND(NN,K)  =  (SSND(IMID(NN,1),K) + SSND(IMID(NN,2),K))/2.
            GBND(NN,K)  =  (GBND(IMID(NN,1),K) + GBND(IMID(NN,2),K))/2.
            TLAYND(NN,K)=(TLAYND(IMID(NN,1),K)+TLAYND(IMID(NN,2),K))/2.
            GADND(NN,K) = (GADND(IMID(NN,1),K) +GADND(IMID(NN,2),K))/2.
            EROST(NN,K) = (EROST(IMID(NN,1),K) +EROST(IMID(NN,2),K))/2.
          ENDDO
        ENDIF
      ENDDO
!MD: neu fuer alle Mittseiten-Knoten
!MD--------------------------------------


      IF(ID(1:3) .EQ. 'CB ') THEN
        CALL BED
      Endif

cipk June 1996      IF(IBNK .NE. 0) THEN
cipk June 1996        READ(IIQ,5010) BNKCD,BNKGAB,BNKERC,BRKS
cipk June 1996        WRITE(LOUT,6030) BNKCD,BNKGAB,BNKERC,BRKS
cipk June 1996      ENDIF

      IF(ID(1:6) .NE. 'ENDSED') THEN
	WRITE(75,*) 'NO ENDSED DATA LINE FOUND'
	WRITE(*,*) 'NO ENDSED DATA LINE FOUND'
	STOP
      ENDIF


!MD: READ BED RESTART-File if available (from SUbR Filehandling)
      IF(IREBED == 50) THEN
        WRITE(75,*) 'RestartBed is readed.'
        WRITE(*,*) 'RestartBed is readed.'
        CALL Read_KALYP_BED
        !MD: neue Formation von DELBED, BEDORIG(NN) usw. erforderlich
        CALL FORMBED(1)
      ELSEIF(IREBED == 0) THEN
        WRITE(75,*) 'No RestartBed available: Start with full B.Layers.'
        WRITE(*,*) 'No RestartBed available: Start with full B.Layers.'
      ENDIF
!MD: READ BED RESTART-File if available (from SUbR Filehandling)


C
      RETURN

 5001 FORMAT(I8,7F8.5)
 5002 FORMAT(I8,8F8.0)
 5010 FORMAT(8X,4F8.0)
 6010 FORMAT(/10X,'PROPERTIES FOR ELEMENT TYPES',/(I16,9I8))
 6001 FORMAT(/10X,'CRIT. SHEAR FOR DEPOSITION =',T50,F10.3,
     *' (N/SQ.M =.1*DYNES/SQ.CM)')
 6002 FORMAT(/10X,'LAYERED BED PROPERTIES...')
 6020 FORMAT(/14X,'NUMBER OF LAYERS               =',I3/
     '14X,'DENSITY OF SUSPENDING WATER    =',F7.1,' KG/CU.M'/
     '14X,'KIN. VISCOSITY OF SUSP. WATER  =',E8.2,' SQ.M/SEC'/
     '14X,'BULK DENSITY OF TOP LAYER      =',F7.1,' KG/CU.M'/
     '14X,'THICKNESS OF TOP LAYER         =',F7.3,' METERS'/
     '14X,'DENSITY OF SEDIMENT MINERAL    =',F7.1,' KG/CU. M'/
     '14X,'DRY WT. OF SED. IN TOP LAYER   =',F7.1,' KG/SQ.M'/
     '14X,'ERO. RATE CONS. FOR BOT. LAYER =',F7.5,'KG./SQ.M/SEC')
 6021 FORMAT(/10X,'LAYER',10X,'CRIT. SHEAR',10X,'BULK DENSITY',
     '10X,'THICKNESS',10X,'DRY DENSITY'/
     '26X,'NEWT./SQ.M',13X,'KG./CU.M',13X,'METERS',13X,'KG./CU.M'/)
cipk aug97 change format 6025 FORMAT(I13,F19.2,F24.2,F20.3,F20.2)
 6025 FORMAT(I13,F19.2,F24.2,F20.5,F20.2,F15.5)
 6030 FORMAT(5X,'BANK CRITICAL SHEAR STRESS',1PE15.4/
     +          'BULK DENSISTY OF BANK MAT.',1PE15.4/
     +          'BANK EROSION RATE CONSTANT',1PE15.4/
     +          'BANK ROUGHNESS HEIGHT     ',1PE15.4)
C 6032 FORMAT(1H1,30X,'SEDIMENT PROPERTIES'/)

 6033 FORMAT(//10X,'SETTLING VELOCITIES...'///20X,
CIPK APR 97 FIX LABEL AND FORMAT
     * 'CONC. RANGE(GM/CU.M)',15X,'SET. VEL(M/SEC)')
 6041 FORMAT(10X,'.... for Roughness-Class: ',I8, '. ')
 6034 FORMAT(20X,' 0.',4X,'TO',F10.3,16X,1PE12.3)
 6035 FORMAT(13X,F10.3,4X,'TO',F10.3,13X,'VS=',
     + 1PE12.5,'*CONC^',1PE12.5,'.')
 6036 FORMAT(23X,'ABOVE',F10.3,21X,1PE12.3)
 6037 FORMAT(10X,'SETTLING VELOCITY',T50,F8.4,'  (MM/SEC)')
 6038 FORMAT(//,
     *14X,'VON KARMANNS CONSTANT          =',F7.2,' CONSTANT'/
     *14X,'BED ROUGHNESS HEIGHT           =',F7.4,' METERS  '/
     *14X,'D90                            =',F7.4,' METERS  '/)
CIPK JUN97 ADD FORMATS
 6039 FORMAT('  WIND/WAVE DATA'//
     +    '  MAT TYP   WIND SPEED     FETCH    MANN')
 6040 FORMAT(I9,F13.2,F10.1,F8.3)
      END
C_______________________________________________________________________
