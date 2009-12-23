!     Last change:  MD   30 Jul 2009    2:11 pm
!IPK LAST UPDATE MAY 30 2006 ADD GAD DEF.
!IPK LAST UPDATE APRIL 22 2006 REMOVE TEST ON NLAYER
!IPK LAST UPDATE APRIL 05 2006 TEST TO MAKE SURE LOOPS ARE ONLY ENTERED ONCE
!IPK LAST UPDATE MARCH 30 2006 CORRECT A SUBSCRIPT ERROR
!IPK LAST UPDATE JAN 23 2005 ALLOW FOR RECORD OF BED CHANGES
!IPK LAST UPDATE APRIL 18 1999 ADD CHECK FOR MUD FLATS TO CUT OFF SDEP/ERS
!IPK LAST UPDATE fEB 18 1999 FIX ERROR IN PRINTING ORDER AND BEDELEV
!IPK LAST UPDATE JAN 9 1999 FIX ERROR IN UNITS FOR PRINTING
!IPK                        CORRECT REFERENCE TO BED TO NN
!RRR  Last update Aug 30 1997  Bed numbering changed:
!  1 = top surface layer, NLAYT = bottom new bed layer
!IPK  LAST UPDATE APR 4 1997 
      SUBROUTINE FORMBED(II)
!
!----------------------------------------------------------------------
!       THIS ROUTINE FORMS THE NEW BED THAT IS A RESULT OF DEPOSITION
!     IN THE LAST TIME INTERVAL
!----------------------------------------------------------------------
!
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
!
!rrr aug97 add character lines
      CHARACTER*256 LINE256
      CHARACTER*80  FMT
!-
!NiS,jul06: Consistent data types for variable passing
      REAL(KIND=8):: HTP
      REAL(KIND=8):: SUMTHICK   
      integer (kind = 4) :: iprtf
!-
!-.....ASSIGN PROPER COEFS.....
!-
!
!MD: Basis-Variable: Wasserdichte
!MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx      
!     Je nach Vorgabe der SI-Einheiten unter "IGRV"      
      ROAVG=1.935
      IF (GRAV < 32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte
!
      DO 350 NN=1,NPM
        N=NN
        IF(NDEP(NN) > 1) N=NREF(NN)+NDEP(NN)-1
!
!IPKSEP05
        NLAYT=NLAYTND(N)
        DO J=1,NLAYT
          SS(J)=SSND(N,J)
          GAD(J)=GADND(N,J)
          TLAY(J)=TLAYND(N,J)
        ENDDO
        GAC=GACND(N)
!MD: GAB=GABND(N) use per Layer        
!MD: NEU mit berechneter Suspensionsdichte aus SPROP        
        GAW=GAWND(N)
        TAUCD=TAUCDND(N)
        WEGT=WEGTND(N)
        TH1=0.
!cc     AT(NN)=0.
!
!********************************************************************
!
!      FORM NEW BED PROFILE.
!*********************************************************************
!
!         WHEN II=1 ONLY NEWBED IS FORMED FROM INPUT VALUES OF
!    MASS READ IN AT THE BOTTOM OF EACH NODE.
!
  150   CONTINUE
!rrr add test for II = 1
        IF(IABS(NB) > 0 .AND. INEWBED == 0) GO TO 221
!ipk feb95        IF(TM(NN) <= 0.)GO TO 350
!.......................................................................
!       NUMBER OF suspended LAYERS ON BOTTOM
!.......................................................................
        BEDEL(NN)=AO(NN)
        NLAY(NN)=1
!
!   ..   CALCULATE THE NUMBER OF LAYERS THAT CAN BE FORMED BY THIS MASS
!
!ipk apr06        NL=TM(NN)/WEGT
!ipk apr06        if (nl > mxsedlay+10)  then
!ipk apr06           write(*,*) '  lt > mlayer ', lt, nn,tm(nn)
!ipk apr06           stop 
!ipk apr06        endif
!
!
!rrr aug97 add largely new logic to statment "350 CONTINUE"
!  Add the mass of top layer to mass to be deposited.
!     *** However if all layers except the bottom are
!         full add mass directly into bottom layer  ****
!
        DO L=1,NLAYT-1
!MD:    For all suspended Layer, without the last one
!  Layer not full            
          IF (THICK(NN,L) < TLAY(L))  THEN
            ATHICK = TLAY(L) - THICK(NN,L)
            THICKNW = TM(NN)/GAD(L)
            IF (ATHICK >= THICKNW)  THEN
              THICK(NN,L) = THICK(NN,L) + THICKNW
              TM(NN) = 0.
              GO TO 200
            ELSE
              THICK(NN,L) = TLAY(L)
              TM(NN) = TM(NN) - ATHICK*GAD(L)
            ENDIF
          ENDIF
        ENDDO
!
!  Put All remaining total mass TM in bottom layer
        THICKNW = TM(NN)/GAD(NLAYT)
        THICK(NN,NLAYT) = THICK(NN,NLAYT) + THICKNW
        TM(NN) = 0.
  200   CONTINUE
!
!
!MD: BEDORIG Sohldicke in [m] ohne GOK!!        
        BEDORIG(NN) = 0.
        DO 220 L=1,NLAYT
          BEDORIG(NN) = BEDORIG(NN) + THICK(NN,L)
          SST(NN,L) = SS(L)
          IF (THICK(NN,L) > 0.0) THEN
            NLAY(NN) = L
          ENDIF
  220   CONTINUE
!
!IPK JAN05
        IF(NLAYO(NN) > 0) THEN
          DO L=1,NLAYO(NN)
            BEDORIG(NN) = BEDORIG(NN) + THICKO(NN,L)
          ENDDO
        ENDIF
!
!     AORIG is the solid bed level
        GO TO 350
!
!       ENTER HERE ONLY FOR RESTART CASE
!------------------------------------------
  221   LT = NLAY(NN)
        TH2=0.
        bedel(nn)=aorig(nn)
        DO I=1,LT
          TH2 = TH2 + THICK(NN,I)
        ENDDO
        DELTHK = TH2-TH1
!MD: DELTHK = SUMME(THICK)        
        BEDEL(NN) = BEDEL(NN) + TH2
!MD: BEDEL Sohllage in [mNN] mit GOK!!        
!IPK JAN05
        IF(NLAYO(NN) > 0) THEN
          DO L=1,NLAYO(NN)
            BEDEL(NN) = BEDEL(NN) + THICKO(NN,L)
          ENDDO
        ENDIF
!
  350 CONTINUE
!
!
!   BEDEL IS NOW LEVEL OF BED
! ---------------------------------------
      DO N=1,NPM
        IF(IMID(N,1) > 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
!MD: must be MIN not MAX          
          NLAYTND(NN)= MIN (NLAYTND(IMID(NN,1)),NLAYTND(IMID(NN,2)))
!IPK MAY06 SET DENSITY
          do l=1,nlaytnd(nn)
            gadnd(nn,l)= (gadnd(IMID(NN,1),l) + gadnd(IMID(NN,2),l)) /2.
          enddo
          BEDORIG(NN)=(BEDORIG(IMID(NN,1))+BEDORIG(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          TM(NN)=(TM(IMID(NN,1))+TM(IMID(NN,2)))/2.
          NLAYT=NLAYTND(NN)
          DO J=1,NLAYT
            THICK(NN,J)=(THICK(IMID(NN,1),J)                            &
     &                    +THICK(IMID(NN,2),J))/2.
!IPK MAR06
            SST(NN,J)=(SST(IMID(NN,1),J)+SST(IMID(NN,2),J))/2.
          ENDDO
!MD: must be MIN not MAX!!          
          NLAYTO=Min(NLAYO(IMID(NN,1)),NLAYO(IMID(NN,2)))
          IF(NLAYTO > 0) THEN
            DO L=1,NLAYTO
              THICKO(NN,L)=(THICKO(IMID(NN,1),L)                        &
     &                     +THICKO(IMID(NN,2),L))/2.
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
!
!     BEDEL is the total thickness of mud (OUT OF DATE)
!********************************************************************
!    SET old Elevations and SHEARS-Stress TO NEW VALUES.
!*********************************************************************
      DO 400 N=1,NPM
!IPK JAN05
!        IF(II == 1 .AND. IABS(NB) >= 100 .AND. 
!     +     IDEPCH /= 2) THEN
!          BEDORIG(N)=BEDEL(N)
!        ENDIF
        IF(INEWBED == 0) THEN
          AORIG(N)=AORIG(N)-BEDORIG(N)
!         AORIG is the solid bed level
          bedel(n)=bedel(n)-bedorig(n)        
          DEPINCR(N)=BEDEL(N)-aorig(n)-BEDORIG(N)
!         DEPINCR IS INCREASE IN BED THICKNESS FROM INPUT VALUE
!
        ELSE
          DEPINCR(N)=0.
          AORIG(N)=AORIG(N)-BEDORIG(N)
!         AORIG is the solid bed level
!
!MDMD: UNSINN - Pruefen          
          DELBED(N)=-DEPINCR(N)
!MD: UNSINN Betthöhe = -0.0 ???          
!
        ENDIF
        I=N
        IF(NDEP(N) > 1) I=NREF(N)+NDEP(N)-1
        ESRO(I)=BSHEAR(I)
        ESRO(N)=BSHEAR(N)
!       CONCO(I)=CONC(I)
        BEDELO(N)=BEDEL(N)-AORIG(N)
        BEDEL(N)=BEDELO(N)
  400 CONTINUE
!
!MD:      WRITE(LOUT,77)
!
!*********************************************************************
!     PRINT THE BED SHEAR ,ER/DEP RATES AND ELEVATIONS.
!*********************************************************************
      IF(II /= 1) THEN
!ipk apr97 add option
!IPK SKIP THIS TEST        IF(MOD(IT,NPRTF) /= 0 .AND. TET /= 0.) RETURN
!        WRITE(181,80)
        DO 480 I=1,NPM
          N=I
          IF(NDEP(I) > 1) N=NREF(I)+NDEP(I)-1
!
!ipk feb99  BC=BEDEL(I)-AO(I)
!MD: cleaning up things which are never used
!MD:    BC=BEDEL(I)+AORIG(I)      
!rrr aug97 remove scling by depth
!MD: cleaning up things which are never used
!MD:    DMER=EDOT(N)      
!MD:    SER=SERAT(N)      
!MD:    DR=TMD(I)/DELT      
!ipk jan99    EDM=(DMER+SER-DR)*DELT
!
!MD:      Used original Parameters
!MD:    EDM=((DMER+SER)/1000.-DR)*DELT      
          EDM=((EDOT(N)+SERAT(N))/1000.-(TMD(I)/DELT))*DELT
!
!ipk jan99 reverse prionting order
!          WRITE(181,82)I,BSHEAR(I),BC,BEDEL(I),DMER,SER,DR,EDM
  480   CONTINUE
      ENDIF
!
!IPK APR06 INIIALIZE IPASST
      DO N=1,NPM
        IPASST(N)=0
      ENDDO
!
! update bed elevation for flow calculation
!
      DO M=1,NEM
!
       IF(IMAT(M) > 0) THEN
        DO K=1,NCORN(M),2
          N=NOP(M,K)
!IPK APR06 TEST TO SEE IF THIS NODE HAS BEEN PROCESSED 
          IF(IPASST(N) == 0) THEN
            IPASST(N)=1
              DEP=VEL(3,N)
!MD       AO(N)=AO(N)+DEPINCR(N)       
!MD       ADO(N)=ADO(N)+DEPINCR(N)       
!MD       ADB(N)=ADB(N)+DEPINCR(N)       
!MD       ADT(N)=ADT(N)+DEPINCR(N)       
!MD       HEL(N)=WSLL(N)-ADO(N)       
!MD       CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)       
!MD       VEL(3,N)=HTP   !MD HTP=HSIG       
            RATIO=vel(3,n)/DEP
            VEL(1,N)=VEL(1,N)/RATIO
            VEL(2,N)=VEL(2,N)/RATIO
              VEL(6,N)=VEL(6,N)/RATIO
            VDOT(1,N)=VDOT(1,N)/RATIO
            VDOT(2,N)=VDOT(2,N)/RATIO
                   VDOT(6,N)=VDOT(6,N)/RATIO
          ENDIF 
          ENDDO
          DO K=2,NCORN(M),2
            N1=NOP(M,K-1)
          N=NOP(M,K)
            IF(K < NCORN(M)) THEN
            N2=NOP(M,K+1)
          ELSE
              N2=NOP(M,1)
          ENDIF
!IPK APR06 TEST TO SEE IF THIS NODE HAS BEEN PROCESSED 
          IF(IPASST(N) == 0) THEN
            IPASST(N)=1
             DEP=VEL(3,N)
            VEL(3,N)=(VEL(3,N1)+VEL(3,N2))/2.
            AO(N)=(AO(N1)+AO(N2))/2.
            ADO(N)=(ADO(N1)+ADO(N2))/2.
            ADB(N)=(ADB(N1)+ADB(N2))/2.
            ADT(N)=(ADT(N1)+ADT(N2))/2.
!MD: new new              
              ESRO(N)=(BSHEAR(N1)+BSHEAR(N2))/2.
              DEPINCR(N)=(DEPINCR(N)+DEPINCR(N))/2.
            BEDELO(N)=(BEDELO(N1)+BEDELO(N2))/2.
!MD: new new              
              BEDEL(N)=BEDELO(N)
            RATIO=vel(3,n)/DEP
            VEL(1,N)=VEL(1,N)/RATIO
            VEL(2,N)=VEL(2,N)/RATIO
              VEL(6,N)=VEL(6,N)/RATIO
            VDOT(1,N)=VDOT(1,N)/RATIO
            VDOT(2,N)=VDOT(2,N)/RATIO
              VDOT(6,N)=VDOT(6,N)/RATIO
          ENDIF
          ENDDO
      ENDIF
      ENDDO
!
!... PRINT OUT LAYER PROPERTIES.
!
!ipk apr97 add option
!nis,dec08: Introduce local copy, because nprtf shouldn't be changed here and zero-entry means one-entry      
      iprtf = nprtf
      if (iprtf == 0) iprtf = 1
!
!MD: Output is deactivated, because results are already written into BED-File
!MD      !MD: Abfrage zur Ausgabe:
!MD      !MD: Geaendert, da anschließend noch Berechnungen!!
!MD      !MD:   IF (MOD(IT,IPRTF) /= 0 .AND. TET /= 0.) RETURN
!MD      IF (MOD(IT,IPRTF) /= 0 .AND. TET /= 0.) THEN
!MD        WRITE(LINE256,'(A)')
!MD     &  '  Node Bed Shear Bed-elev   SedMass  SumLayer
!MD     + SusLayer-Thickness (mm)'
!MD        IDX = MAX(65,47+NLAYT*10) + 3
!MD
!MD        IF (NLAYO(1) > 0) THEN
!MD          WRITE(LINE256(IDX:IDX+26),'(A)') 'BedLayer-Thickness (mm) '
!MD        ENDIF
!MD        WRITE(LOUT,'(/A)') LINE256(1:IDX+26)
!MD
!MD        WRITE(LINE256,'(A,12I10)')
!MD     & '          (N/m2)    (m)       (Kg/m2)   (mm) ',
!MD     &                        (L,L=1,NLAYT),(L,L=1,NLAYO(1))
!MD        WRITE(LOUT,'(A)') LINE256
!MD
!MD      ENDIF
!MD      !MD: Geaendert mit Ausgabe
!MD: Output is deactivated, because results are already written into BED-File
!
      INT = (NP+5)/6
      MLAYRS = MIN(NLAYT,5)
      DO 75 NN=1,NPM
         N = NN
         IF(NDEP(NN) > 1) N=NREF(NN)+NDEP(NN)-1
!
!IPKSEP05
         NLAYT=NLAYTND(N)
         DO J=1,NLAYT
           SS(J)=SSND(N,J)
!MD: NEUE Berechnung der Trocken-Rohdichte JE Sus.[kg/m3]           
!MD: use bulk densitiy per Layer           
!MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)           
           GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
           GAD(J)=GADND(N,J)
!MD: NEUE Berechnung der Trocken-Rohdichte Sus.[kg/m3]           
           TLAY(J)=TLAYND(N,J)
         ENDDO
         GAC=GACND(N)
!MD: GAB=GABND(N) use per Layer         
!
!MD NEU Berechnung der Suspensionsdichte         
         IF (VEL(6,N) >= 0.) THEN
           GAWND(N)=(VEL(6,N)/1000.)*(1.-(ROAVG/GACND(N))) + ROAVG
         Else
           GAWND(N)= ROAVG
         END IF
         GAW=GAWND(N)
!MD NEU Berechnung der Suspensionsdichte         
!
         TAUCD=TAUCDND(N)
         WEGT=WEGTND(N)
! Calculate total dry mass of new bed
         TVN = 0.
         SUMTHICK = 0.
         DO L=1,NLAY(NN)
            TVN = TVN + THICK(NN,L)*GAD(L)
            SUMTHICK = SUMTHICK + THICK(NN,L)
         ENDDO
!ipk jan05
         IF(NLAYO(NN) > 0) THEN
           DO L=1,NLAYO(NN)
!MD: GADLN = (GBO(NN,L)-GAW)*GAC/(GAC-GAW)             
             GADLN=(GBO(NN,L)-ROAVG)*GAC/(GAC-ROAVG)
!MD:  NEUE Berechnung der Trocken-Rohdichte Bed [kg/m3]             
             TVN = TVN + THICKO(NN,L)*GADLN
             SUMTHICK = SUMTHICK + THICKO(NN,L)
           ENDDO
       ENDIF
!
!
! Print layer info
!ipk jan99 fix reference to N when it should be NN
!ipk jan99         IF (NLAY(N) > 0 .OR. NLAYO(N) > 0) THEN
!             SUMTHICK = BEDEL(N) - AO(N)
!ipk jan99             MLAYRS = NLAY(N)
!ipk jan99             WRITE(LOUT,'(I5,F10.4, F10.1, 20F10.3)')
!ipk jan99     &          N, BSHEAR(N), TVN, SUMTHICK*1000.,
!ipk jan99     &             (1000. * THICK(NN,L),L=1,NLAYT),
!ipk jan99     &             (1000. * THICKO(NN,L),L=1,NLAYO(N))
!ipk jan99         ENDIF
         IF (NLAYT > 0 .OR. NLAYO(NN) > 0) THEN
!             SUMTHICK = BEDEL(N) - AO(N)
             MLAYRS = NLAY(NN)
!MD             WRITE(LOUT,'(I5,F10.4,F10.6, F10.2, 20F10.3)')
!MD     &          NN, BSHEAR(NN), AO(NN),TVN, SUMTHICK*1000.,
!MD     &             (1000. * THICK(NN,L),L=1,NLAYT),
!MD     &             (1000. * THICKO(NN,L),L=1,NLAYO(NN))
        ENDIF
   75 CONTINUE
!
!
!
!      WRITE(LOUT,83)
!      IF(NLAYO(1) > 0) THEN
!        WRITE(LOUT,85)
!        WRITE(LOUT,84)
!      ENDIF
!      DO  I=1,NPM
!        LL=NLAYO(I)
!        WRITE(LOUT,88) (I,L,THICKO(I,L),SSTO(I,L),L=1,LL)
!        IF(LL > 0) THEN
!rr          WRITE(LOUT,86) (L,THICKO(I,L),SSTO(I,L),L=1,LL)
!          WRITE(LOUT,89) (L,THICKO(I,L),SSTO(I,L),L=1,LL)
!        ENDIF
!      ENDDO
!      WRITE(LOUT,87)
!      WRITE(LOUT,84)
!      DO I=1,NPM
!        LL=NLAY(I)
!        L=1
!        N1=LL-L+1
!        WRITE(LOUT,88) I,L,THICK(I,L),SST(I,L)
!        IF(LL > 1) THEN
!          DO L=2,LL
!            N1=LL-L+1
!            WRITE(LOUT,89) L,THICK(I,L),SST(I,L)
!          ENDDO
!        ENDIF
!      ENDDO
!rrr aug97 end changes to output
!
!MD   77 FORMAT(////11X,40HBED SHEARS AND BED PROFILE .............//)
!MD   80 FORMAT(1X,'NODE',5X,'BED SHEAR',5X,'BOT.ELEV',5X,'BED CHANGE',4X
!MD     1,'MASS ER. RATE',3X,'SURF. ER. ',3X,'DEP. RATE',4X,'ER/DEP MASS')
!
!MD:   82 FORMAT(I5,1P7E14.3)
!MD:   83 FORMAT(1H1,///' BED LAYER PROPERTIES....'//)
!MD:   84 FORMAT(5X,'NODE',3X,'LAYER',4X,'THICKNESS',7X,'SHEAR ST.')
!rrr aug97   85 FORMAT(/1X,I7,25X,' OLD DEPOSIT')
!MD:   85 FORMAT(/1X, 25X,' OLD DEPOSIT')
!rrr aug97   86 FORMAT(13X,I4,1P3E15.3)
!MD:   86 FORMAT(13X,I4,1P2E15.3)
!rrr aug97   87 FORMAT(1X,I7,25X,' NEW DEPOSIT')
!MD:   87 FORMAT(1X, 25X,' NEW DEPOSIT')
!MD:   88 FORMAT(5X,2I5,1P2E15.3)
!MD:   89 FORMAT(10X,I5,1P2E15.3)
      RETURN
      END
