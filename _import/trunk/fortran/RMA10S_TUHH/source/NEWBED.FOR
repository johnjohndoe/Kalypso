C     Last change:  MD    5 Oct 2009    6:39 pm
cipk last update jan 11 2007 allow for 5 node transition elements
CIPK LAST UPDATE SEP 05 2006 FINALIZE UPDATES
CIPK LAST UPDATE SEP 05 2006 REVISE TESTS
CIPK LAST UPDATE may 30 2006 add mass output and refine compuatation of mass laid down
CIPK LAST UPDATE APRIL 22 2006 REMOVE TEST ON NLAYER
CIPK LAST UPDATE APRIL 05 2006 TEST TO MAKE SURE LOOPS ARE ONLY ENTERED ONCE
CIPK LAST UPDATE JAN 23 2005 ALLOW FOR RECORD OF BED CHANGES
CIPK LAST UPDATE APRIL 18 1999 ADD CHECK FOR MUD FLATS TO CUT OFF SDEP/ERS
CIPK LAST UPDATE fEB 18 1999 FIX ERROR IN PRINTING ORDER AND BEDELEV
CIPK LAST UPDATE JAN 9 1999 FIX ERROR IN UNITS FOR PRINTING
CIPK                        CORRECT REFERENCE TO BED TO NN
CRRR  Last update Aug 30 1997  Bed numbering changed:
C  1 = top surface layer, NLAYT = bottom new bed layer
CIPK  LAST UPDATE APR 4 1997 
      SUBROUTINE NEWBED(II)

C----------------------------------------------------------------------
C     THIS ROUTINE FORMS THE NEW BED THAT IS A RESULT OF DEPOSITION
C     IN THE LAST TIME INTERVAL
C----------------------------------------------------------------------
C
C
C     FACT_SPEED_UP = Speed-up factor for bed evolution
C
C----------------------------------------------------------------------
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      
crrr aug97 add character lines
      CHARACTER*256 LINE256
      CHARACTER*80  FMT
!NiS,jul06: Consistent data types for passing paramters
      REAL(KIND=8) :: HTP
CIPK SEP06
      REAL(KIND=8):: TVN
      REAL(KIND=8):: SUMTHICK
C-
C-.....ASSIGN PROPER COEFS.....
!MD: Basis-Variable: Wasserdichte
      !MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
      !     Je nach Vorgabe der SI-Einheiten unter "IGRV"
      ROAVG=1.935
      IF (GRAV.LT.32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte

      DO 350 NN=1,NPM
        N=NN
        IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

CIPKSEP05
        NLAYT=NLAYTND(N)
        DO J=1,NLAYT
          SS(J)=SSND(N,J)

          !MD: NEUE Berechnung der Trocken-Rohdichte Sus.[kg/m3]
          !MD: use bulk densitiy per Layer
          !MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
          GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
          GAD(J)=GADND(N,J)
          TLAY(J)=TLAYND(N,J)
        ENDDO
        GAC=GACND(N)
        !MD: GAB=GABND(N) use per Layer

        !MD NEU Berechnung der Suspensionsdichte
        IF (VEL(6,N).ge.0.) THEN
          GAWND(N)=(VEL(6,N)/1000.)*(1.-(ROAVG/GACND(N))) + ROAVG
        Else
          GAWND(N)= ROAVG
        END IF
        GAW=GAWND(N)
        !MD NEU Berechnung der Suspensionsdichte

        TAUCD=TAUCDND(N)
        WEGT=WEGTND(N)

        TH1=0.
ccc      AT(NN)=0.
        IF(II .EQ. 1) GO TO 150
        L1=NLAY(NN)
        DO  K=1,L1
          TH1=TH1+THICK(NN,K)
        ENDDO
        TM(NN)=0.
        TMD(NN)=0.
cipk apr99
CIPK AUG06   SKIP TEST

!MDMD: reactivaed  in order to avoid maximum dep-rates
        if(wsll(nn) .lt. ao(nn)) go to 200
!MDMD: reactivaed  in order to avoid maximum dep-rates

        IF(ESRO(NN).GT.TAUCD .AND. BSHEAR(NN).GT.TAUCD) GO TO 200
C-
      !MD: cleaning up things which are never used
      !MD:   TAU=BSHEAR(NN)
      !MD:   TAUO=ESRO(NN)
      !MD:   CS=VEL(6,N)
      !MD:   CSO=VOLD(6,N)
      !MD:   VST=VS(N)
      !MD: cleaning up things which are never used & Unsinn
      !MD:  IF(TAU .GT. TAUCD) TAU=TAUCD
      !MD:  IF(TAUO .GT. TAUCD) TAUO=TAUCD
        
cipk may06 refine computation of added mass
c        TEMP=((3.*TAUCD-2.*TAUO-TAU)*CSO+
c     *         (3.*TAUCD-TAUO-2.*TAU)*CS)/(6.*TAUCD)
c        TEMP1=3.*TAUCD-TAUO-2.*TAU/(3.*TAUCD)
c        TM(NN)=ABS(VST)*DELT*((3.*TAUCD-2.*TAUO-TAU)*CSO+
c     *         (3.*TAUCD-TAUO-2.*TAU)*CS)/(6.*TAUCD)
c        TM(NN)=ABS(VST)*DELT*((3.*TAUCD-2.*TAUO-TAU)*CSO+
c     *         (3.*TAUCD-TAUO-2.*TAU)*CS)/(6.*TAUCD)

!MD: Use orginale Parameters
        TMTMP1=deprat(nn)* (DELT*FACT_SPEED_UP) *VEL(6,N)

CIPK REVISE TO USE OLD DEPRATO
!MD: Use orginale Parameters
        TMTMP2=DEPRATO(NN)* (DELT*FACT_SPEED_UP) *VOLD(6,N)
c        TMTMP2A=TEMP1*ABS(VST)*DELT*CS
C        write(215,*) nn,temp,vst,delt
C        write(215,*) TM(NN),TMTMP1,TMTMP2,deprat(nn)

      !MDMD: deactivaed and corrected 07-05-2009
      !MDMD  TM(NN)=((ALPHA-1.)*TMTMP2+TMTMP1)/ALPHA
        TM(NN)=(TMTMP2+TMTMP1)/2.00
      !MDMD: deactivaed and corrected 07-05-2009

cipk may06 end updates       
        TM(NN)=TM(NN)/1000.
        DELBED(NN)=BEDEL(NN)

        IF(TM(NN).LE.0.)GO TO 350
        TMD(NN)=TM(NN)
!MD: TMD=TM goto verschoben

C    MASS DEPOSITED PER UNIT AREA.
C..............................................................


C********************************************************************
C
C      FORM NEW BED PROFILE.
C*********************************************************************
C
C
  150   CONTINUE
C.......................................................................
C       NUMBER OF LAYERS ON BOTTOM
C.......................................................................

C..   CALCULATE THE NUMBER OF LAYERS THAT CAN BE FORMED BY THIS MASS
c
cipk apr06        NL=TM(NN)/WEGT
cipk apr99        if (lt .gt. mlayer+10)  then
cipk aug05        if (nl .gt. mlayer+10)  then
cipk apr06        if (nl .gt. mxsedlay+10)  then
cipk apr06           write(*,*) '  lt > mlayer ', lt, nn,tm(nn)
cipk apr06           stop 
cipk apr06        endif


crrr aug97 add largely new logic to statment "350 CONTINUE"
C  Add the mass of top layer to mass to be deposited.
C     *** However if all layers except the bottom are
C         full add mass directly into bottom layer  ****
C
         DO 100 L=1,NLAYT
C  Layer not full            
           IF (THICK(NN,L) .LT. TLAY(L))  THEN
             ATHICK = TLAY(L) - THICK(NN,L)
             THICKNW = TM(NN)/GAD(L)
             IF (ATHICK .GE. THICKNW)  THEN
               THICK(NN,L) = THICK(NN,L) + THICKNW
               TM(NN) = 0.
               GO TO 200
             ELSE
               THICK(NN,L) = TLAY(L)
               TM(NN) = TM(NN) - ATHICK*GAD(L)
             ENDIF
           ENDIF
  100    CONTINUE
C
!MD: OLD  C  Put All remaining mass in bottom layer
!MD:         THICKNW = TM(NN)/GAD(NLAYT)
!MD:         THICK(NN,NLAYT) = THICK(NN,NLAYT) + THICKNW
!MD:         TM(NN) = 0.

         !MD: New New New:
         !MD: Also fill up Bed-Layer if Suspended Layers are full
         DO 101 L=1, NLAYO(NN)
           !MD:  Bed-Layer not full
           IF (THICKO(NN,L).LT.THICKOND(NN,L) .and. L.ne.NLAYO(NN)) THEN
             ATHICK = THICKOND(NN,L) - THICKO(NN,L)
             !MD: NEUE Berechnung der Trocken-Rohdichte BETT[kg/m3]
             GADLN =(GBO(NN,L)-ROAVG)*GAC/(GAC-ROAVG)
             THICKNW = TM(NN)/GADLN

             IF (ATHICK .GE. THICKNW)  THEN
               THICKO(NN,L) = THICKO(NN,L) + THICKNW
               TM(NN) = 0.
               GO TO 200
             ELSE
               THICKO(NN,L) = THICKOND(NN,L)
               TM(NN) = TM(NN) - ATHICK*GADLN
             ENDIF
           ElseIF (L.eq.NLAYO(NN)) THEN
             !MD: Put All remaining mass in bottom bed layer!!
             GADLN =(GBO(NN,L)-ROAVG)*GAC/(GAC-ROAVG)
             THICKNW = TM(NN)/GADLN
             THICKO(NN,L) = THICKO(NN,L) + THICKNW
             TM(NN) = 0.
           ENDIF
  101    CONTINUE

  200    CONTINUE
C
         DELBED(NN) = 0.
         DO 220 L=1,NLAYT
            DELBED(NN) = DELBED(NN) + THICK(NN,L)
            SST(NN,L) = SS(L)
            !MD  SST wird niemals weiter genutzt
            !MD  alle ero u depo routinen arbeiten mit SSND!
            IF (THICK(NN,L) .GT. 0.0) THEN
              NLAY(NN) = L
            ENDIF
  220    CONTINUE

CIPK JAN05
         IF(NLAYO(NN) .GT. 0) THEN
           DO L=1,NLAYO(NN)
             DELBED(NN) = DELBED(NN) + THICKO(NN,L)
           ENDDO
         ENDIF

  350 CONTINUE

      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          DELBED(NN)=(DELBED(IMID(NN,1))+DELBED(IMID(NN,2)))/2.
          TMD(NN)=(TMD(IMID(NN,1))+TMD(IMID(NN,2)))/2.
          NLAYT=NLAYTND(NN)
          DO J=1,NLAYT
            THICK(NN,J)=(THICK(IMID(NN,1),J)
     +                    +THICK(IMID(NN,2),J))/2.
            SST(NN,J)=(SST(IMID(NN,1),J)+SST(IMID(NN,2),J))/2.
          ENDDO
        ENDIF
      ENDDO        

C********************************************************************
C    SET OLD CONCENTRATIONS AND SHEARS TO NEW VALUES.
C*********************************************************************
      DO 400 N=1,NPM

        DEPINCR(N)=DELBED(N)-BEDELO(N)
        I=N
        IF(NDEP(N) .GT. 1) I=NREF(N)+NDEP(N)-1
        ESRO(I)=BSHEAR(I)
        ESRO(N)=BSHEAR(N)
C       CONCO(I)=CONC(I)
  400 CONTINUE
      INEWBED=0
      
      WRITE(LOUT,77)

C*********************************************************************
C     PRINT THE BED SHEAR ,ER/DEP RATES AND ELEVATIONS.
C*********************************************************************
!MD: Hinweis: II IMMER gleich 2
      IF(II .NE. 1) THEN
cipk apr97 add option
CIPK APR06        IF(MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) RETURN
C        WRITE(181,80)
        DO 480 I=1,NPM
          N=I
          IF(NDEP(I) .GT. 1) N=NREF(I)+NDEP(I)-1
cipk feb99  BC=BEDEL(I)-AO(I)
!MD: cleaning up things which are never used
      !MD:    BC=BEDEL(I)+AORIG(I)
crrr aug97    remove scling by depth
!MD: cleaning up things which are never used
      !MD:    DMER=EDOT(N)
      !MD:    SER=SERAT(N)
      !MD:    DR=TMD(I)/DELT
cipk jan99    EDM=(DMER+SER-DR)*DELT

!MD:      Used original Parameters
      !MD:    EDM=((DMER+SER)/1000.-DR)*DELT
          EDM = ((EDOT(N)+SERAT(N))/1000. -
     +           (TMD(I)/(DELT*FACT_SPEED_UP)))* (DELT*FACT_SPEED_UP)


!MD: Test ob sohlevultion richtig: !MD: Test ob sohlevultion richtig:
!MD          IF (N.eq.1) THEN
!MD             WRITE(75,*) 'CHECK-Bed:        EDM,             DEPINCR,
!MD      +           DELBED,           BEDELO,       DELBED(EDM)'
!MD           END IF
!MD           WRITE(75,*) EDM, DEPINCR(N), DELBED(N), BEDELO(N),
!MD      + BEDELO(N)-EDM
          !MD: Test ob sohlevultion richtig:
          !MD:  DEPINCR(N)= EDM
          !MD:  DELBED(N) = BEDELO(N)+DEPINCR(N)
!MD: Test ob sohlevultion richtig: !MD: Test ob sohlevultion richtig:



cipk jan99 reverse prionting order
C          WRITE(181,82)I,BSHEAR(I),BC,BEDEL(I),DMER,SER,DR,EDM
  480   CONTINUE
      ENDIF

CIPK APR06 INIIALIZE IPASST
      DO N=1,NPM
        IPASST(N)=0
      ENDDO

c update bed elevation for flow calculation
cipk may06 skip of morphology off

      IF(FACTMORPH .NE. 1.) GO TO 7777
      DO M=1,NEM
        IF(IMAT(M) .GT. 0) THEN
          DO K=1,ncorn(m),2
	    N=NOP(M,K)
cipk jan07	     
            if(ncorn(m) .eq. 5 .and. k .eq. 3) then
              n=nop(m,k+1)
            endif
CIPK APR06 TEST TO SEE IF THIS NODE HAS BEEN PROCESSED 
	    IF(IPASST(N) .EQ. 0) THEN
       	      IPASST(N)=1
       	      DEP=VEL(3,N)
       	      AO(N)=AO(N)+DEPINCR(N)
              ADO(N)=ADO(N)+DEPINCR(N)
              ADB(N)=ADB(N)+DEPINCR(N)
              ADT(N)=ADT(N)+DEPINCR(N)
              HEL(N)=WSLL(N)-ADO(N)
              CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)
              VEL(3,N)=HTP
       	 !MD   RATIO=vel(3,n)/DEP
              RATIO = 1.0   !MD: test
       	      VEL(1,N)=VEL(1,N)/RATIO
       	      VEL(2,N)=VEL(2,N)/RATIO
       	      VDOT(1,N)=VDOT(1,N)/RATIO
       	      VDOT(2,N)=VDOT(2,N)/RATIO
cipk update suspended sed
       	      VEL(6,N)=VEL(6,N)/RATIO
       	      VDOT(6,N)=VDOT(6,N)/RATIO
              BEDEL(N)=DELBED(N)
              BEDELO(N)=BEDEL(N)
              DEPINCR(N)=0.
            ENDIF
          ENDDO
	ENDIF
      ENDDO


cipk may06 separate loops to assure correct updating
      DO M=1,NEM
        IF(IMAT(M) .GT. 0) THEN
cipk jan07
          ncn=ncorn(m)
          nlg=2
          nhg=ncn
          ninc=2
          if(ncn .eq. 5) then
            nlg=4
            nhg=2
            ninc=-2
          endif
          DO K=nlg,nhg,ninc
           N1=NOP(M,K-1)
	    N=NOP(M,K)
           IF(K .LT. NCORN(M)) THEN
	      N2=NOP(M,K+1)
	    ELSE
             N2=NOP(M,1)
	    ENDIF
cipk jan07
            if(ncn .eq. 5) then
              if(k  .eq. 4) then
                n=nop(k,3)
                n1=nop(k,5)
                n2=nop(k,4)
              else
                n=nop(k,2)
                n1=nop(k,1)
                n2=nop(k,3)
              endif
            endif
CIPK APR06 TEST TO SEE IF THIS NODE HAS BEEN PROCESSED
  	    IF(IPASST(N) .EQ. 0) THEN
  	      IPASST(N)=1
  	      DEP=VEL(3,N)
  	      VEL(3,N)=(VEL(3,N1)+VEL(3,N2))/2.
  	      AO(N)=(AO(N1)+AO(N2))/2.
  	      ADO(N)=(ADO(N1)+ADO(N2))/2.
  	      ADB(N)=(ADB(N1)+ADB(N2))/2.
  	      ADT(N)=(ADT(N1)+ADT(N2))/2.
              !MD: new new
              DEPINCR(N)=0.
              !MD: new new
              BEDEL(N)=(BEDEL(N1)+BEDEL(N2))/2.
              BEDELO(N)=BEDEL(N)
              RATIO = 1.0    !MD: TEst
  	  !MD:   RATIO=vel(3,n)/DEP
	      VEL(1,N)=VEL(1,N)/RATIO
	      VEL(2,N)=VEL(2,N)/RATIO
	      VDOT(1,N)=VDOT(1,N)/RATIO
	      VDOT(2,N)=VDOT(2,N)/RATIO
cipk may06 update susp sed
	      VEL(6,N)=VEL(6,N)/RATIO
	      VDOT(6,N)=VDOT(6,N)/RATIO
	    ENDIF
          ENDDO
	ENDIF
       ENDDO


 7777  CONTINUE

C  ----------------------------------------
C... PRINT OUT LAYER PROPERTIES.
C
cipk apr97 add option
cipk  modify to allow mass computation at all steps
      IF (MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) go to 60

crrr aug97 revise print format and structure

!MD      WRITE(LINE256,'(A)')
!MD     &  '  Node Bed Shear Bed-elev   SedMass  SumLayer
!MD     + SusLayer-Thickness (mm)'
!MD      IDX = MAX(65,47+NLAYT*10) + 3
!MD      IF (NLAYO(1) .GT. 0) THEN
!MD         WRITE(LINE256(IDX:IDX+26),'(A)') 'BedLayer-Thickness (mm) '
!MD      ENDIF
!MD      WRITE(LOUT,'(/A)') LINE256(1:IDX+26)

!MD      WRITE(LINE256,'(A,12I10)')
!MD     & '          (N/m2)    (m)       (Kg/m2)   (mm) ',
!MD     &                        (L,L=1,NLAYT),(L,L=1,NLAYO(1))
!MD      WRITE(LOUT,'(A)') LINE256
      
   60 continue
   
      INT = (NP+5)/6
      MLAYRS = MIN(NLAYT,5)
      DO 75 NN=1,NPM
         N = NN
         IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

CIPKSEP05
         NLAYT=NLAYTND(N)
         DO J=1,NLAYT
           SS(J)=SSND(N,J)

           !MD: use bulk densitiy per Layer
           !MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
           GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
           GAD(J)=GADND(N,J)
           TLAY(J)=TLAYND(N,J)
         ENDDO
         GAC=GACND(N)
         !MD: GAB=GABND(N) use per Layer

         !MD NEU Berechnung der Suspensionsdichte
         IF (VEL(6,N).ge.0.) THEN
           GAWND(N)=(VEL(6,N)/1000.)*(1.-(ROAVG/GACND(N))) + ROAVG
         Else
           GAWND(N)= ROAVG
         END IF
         GAW=GAWND(N)
         !MD NEU Berechnung der Suspensionsdichte

         TAUCD=TAUCDND(N)
         WEGT=WEGTND(N)
C Calculate total dry mass of new bed
         TVN = 0.
         SUMTHICK = 0.
         DO L=1,NLAY(NN)
           TVN = TVN + THICK(NN,L)*GAD(L)
           SUMTHICK = SUMTHICK + THICK(NN,L)
         ENDDO
cipk jan05
         IF(NLAYO(NN) .GT. 0) THEN
           DO L=1,NLAYO(NN)
             !MD: GADLN = (GBO(NN,L)-GAW)*GAC/(GAC-GAW)
             GADLN=(GBO(NN,L)-ROAVG)*GAC/(GAC-ROAVG)
             !MD:  NEUE Berechnung der Trocken-Rohdichte Bed [kg/m3]
             TVN = TVN + THICKO(NN,L)*GADLN
             SUMTHICK = SUMTHICK + THICKO(NN,L)
           ENDDO
	 ENDIF
cipk may06 get nodal masses for susp sed and bed 	   
CIPK AUG06 REVISE MASS COMPUTATION
	 TMSED(NN) =TVN

	 TMSSED(NN)= VEL(3,NN)*VEL(6,NN)/1000. + (EDOT(nn)+ SERAT(nn))
     +             * (delt*FACT_SPEED_UP)/1000.*(ALPHA-1.)/ALPHA

	 TMSADD(NN)= (EDOT(nn)+SERAT(nn)-DEPRAT(NN)*VEL(6,NN))
     +             * (delt*FACT_SPEED_UP)/(1000.*ALPHA)+TMASSLEFT(NN)
         !MD:  TMLF =TMASSLEFT(NN)    :never used

	 TMASSLEFT(NN)= (EDOT(nn)+SERAT(nn)-DEPRAT(NN)*VEL(6,NN))
     +                * (delt*FACT_SPEED_UP)/1000.*(ALPHA-1.)/ALPHA

	 TMSDEP(NN) =(DEPRAT(NN)*VEL(6,NN))* (delt*FACT_SPEED_UP)/1000.

C Print layer info
cipk jan99 fix reference to N when it should be NN
cipk jan99         IF (NLAY(N) .GT. 0 .OR. NLAYO(N) .GT. 0) THEN
c             SUMTHICK = BEDEL(N) - AO(N)
cipk jan99             MLAYRS = NLAY(N)
cipk jan99             WRITE(LOUT,'(I5,F10.4, F10.1, 20F10.3)')
cipk jan99     &          N, BSHEAR(N), TVN, SUMTHICK*1000.,
cipk jan99     &             (1000. * THICK(NN,L),L=1,NLAYT),
cipk jan99     &             (1000. * THICKO(NN,L),L=1,NLAYO(N))
cipk jan99         ENDIF

      IF (MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) go to 75
!MD         IF (NLAYT .GT. 0 .OR. NLAYO(NN) .GT. 0) THEN
c             SUMTHICK = BEDEL(N) - AO(N)
!MD             MLAYRS = NLAY(NN)
!MD             WRITE(LOUT,'(I5,F10.4,F10.6, F10.2, 20F10.3)')
!MD     &          NN, BSHEAR(NN), AO(NN),TVN, SUMTHICK*1000.,
!MD     &             (1000. * THICK(NN,L),L=1,NLAYT),
!MD     &             (1000. * THICKO(NN,L),L=1,NLAYO(NN))
!MD         ENDIF
   75 CONTINUE
cipk may06 put total mass in subscript '0'   
      TMSED(0)=0
      TMSSED(0)=0
CIPK AUG06
      TMSADD(0)=0.
      TMD(0)=0
      TMSDEP(0)=0

      DO N=1,NE
        NCN=NCORN(N)
        IF(NCN .EQ. 6) THEN

CIPK AUG06 REVISE TO FULL ELEMENT INTEGRATION

        TMSED(0)=TMSED(0)-
     +  (TMSED(NOP(N,1))+TMSED(NOP(N,3))+TMSED(NOP(N,5)))*
     +           (AREA(N)/6.)+        
     +  (TMSED(NOP(N,2))+TMSED(NOP(N,4))+TMSED(NOP(N,6)))*
     +           (AREA(N)/2.)        
        TMSSED(0)=TMSSED(0)-
     +  (TMSSED(NOP(N,1))+TMSSED(NOP(N,3))+TMSSED(NOP(N,5)))*
     +           (AREA(N)/6.)+        
     +  (TMSSED(NOP(N,2))+TMSSED(NOP(N,4))+TMSSED(NOP(N,6)))*
     +           (AREA(N)/2.)        
        TMSADD(0)=TMSADD(0)-
     +  (TMSADD(NOP(N,1))+TMSADD(NOP(N,3))+TMSADD(NOP(N,5)))*
     +           (AREA(N)/6.)+        
     +  (TMSADD(NOP(N,2))+TMSADD(NOP(N,4))+TMSADD(NOP(N,6)))*
     +           (AREA(N)/2.)        

        TMSDEP(0)=TMSDEP(0)-
     +  (TMSDEP(NOP(N,1))+TMSDEP(NOP(N,3))+TMSDEP(NOP(N,5)))*
     +           (AREA(N)/6.)+        
     +  (TMSDEP(NOP(N,2))+TMSDEP(NOP(N,4))+TMSDEP(NOP(N,6)))*
     +           (AREA(N)/2.)        

         TMD(0)= TMD(0)-
     +  (serat(NOP(N,1))+serat(NOP(N,3))+serat(NOP(N,5)))*
     +           (AREA(N)*(delt*FACT_SPEED_UP)/6000.)+
     +  (serat(NOP(N,2))+serat(NOP(N,4))+serat(NOP(N,6)))*
     +           (AREA(N)*(delt*FACT_SPEED_UP)/2000.)
     + -(edot(NOP(N,1))+edot(NOP(N,3))+edot(NOP(N,5)))*
     +           (AREA(N)*(delt*FACT_SPEED_UP)/6000.)+
     +  (edot(NOP(N,2))+edot(NOP(N,4))+edot(NOP(N,6)))*
     +           (AREA(N)*(delt*FACT_SPEED_UP)/2000.)
         
        ELSEIF(NCN .EQ. 8) THEN
        TMSED(0)=TMSED(0)-
     +  (TMSED(NOP(N,1))+TMSED(NOP(N,3))+TMSED(NOP(N,5))+
     +            TMSED(NOP(N,7)))*(AREA(N)/12.)+ 
     +  (TMSED(NOP(N,2))+TMSED(NOP(N,4))+TMSED(NOP(N,6))+
     +            TMSED(NOP(N,8)))*(AREA(N)/3.)  
        TMSSED(0)=TMSSED(0)-
     +  (TMSSED(NOP(N,1))+TMSSED(NOP(N,3))+TMSSED(NOP(N,5))+
     +            TMSSED(NOP(N,7)))*(AREA(N)/12.)+ 
     +  (TMSSED(NOP(N,2))+TMSSED(NOP(N,4))+TMSSED(NOP(N,6))+
     +            TMSSED(NOP(N,8)))*(AREA(N)/3.)
        TMSADD(0)=TMSADD(0)-
     +  (TMSADD(NOP(N,1))+TMSADD(NOP(N,3))+TMSADD(NOP(N,5))+
     +            TMSADD(NOP(N,7)))*(AREA(N)/12.)+ 
     +  (TMSADD(NOP(N,2))+TMSADD(NOP(N,4))+TMSADD(NOP(N,6))+
     +            TMSADD(NOP(N,8)))*(AREA(N)/3.)
        TMSDEP(0)=TMSDEP(0)-
     +  (TMSDEP(NOP(N,1))+TMSDEP(NOP(N,3))+TMSDEP(NOP(N,5))+
     +            TMSDEP(NOP(N,7)))*(AREA(N)/12.)+ 
     +  (TMSDEP(NOP(N,2))+TMSDEP(NOP(N,4))+TMSDEP(NOP(N,6))+
     +            TMSDEP(NOP(N,8)))*(AREA(N)/3.)

         TMD(0)= TMD(0)-
     + (serat(NOP(N,1))+serat(NOP(N,3))+serat(NOP(N,5))+serat(NOP(N,7)))
     +          *(AREA(N)*(delt*FACT_SPEED_UP)/12000.)+
     + (serat(NOP(N,2))+serat(NOP(N,4))+serat(NOP(N,6))+serat(NOP(N,8)))
     +          *(AREA(N)*(delt*FACT_SPEED_UP)/3000.)
     +-(edot(NOP(N,1))+edot(NOP(N,3))+edot(NOP(N,5))+edot(NOP(N,7)))
     +          *(AREA(N)*(delt*FACT_SPEED_UP)/12000.)+
     + (edot(NOP(N,2))+edot(NOP(N,4))+edot(NOP(N,6))+edot(NOP(N,8)))
     +          *(AREA(N)*(delt*FACT_SPEED_UP)/3000.)
c     +  (TMD(NOP(N,1))+TMD(NOP(N,3))+TMD(NOP(N,5))+TMD(NOP(N,7)))*
c     +           (AREA(N)/6.)+        
c     +  (TMD(NOP(N,2))+TMD(NOP(N,4))+TMD(NOP(N,6))+TMD(NOP(N,8)))*
c     +           (AREA(N)/2.)        
        ENDIF      
      ENDDO
      
      IF (MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) go to 76

CIPK SEP06 ADD TO OUTPUT DATA
      WRITE(LOUT,'(A)')
     & 'TMSED         TMSSED(0)           TMSED+TMSSED       TMSADD
     +               TMD                 TMSDEP'
      WRITE(LOUT,9999) TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0),TMSADD(0)
     +  ,TMD(0),TMSDEP(0)
   76 continue      
      if(IMASSOUT .gt. 0) then      
        WRITE(24,9999) TET,TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0)
     +  ,TMSADD(0),TMD(0),TMSDEP(0)

      endif
 9999 FORMAT(1P2E20.10,1PE20.10,1P4E20.10)
      

C
c
c      WRITE(LOUT,83)
c      IF(NLAYO(1) .GT. 0) THEN
c        WRITE(LOUT,85)
c        WRITE(LOUT,84)
c      ENDIF
c      DO  I=1,NPM
c        LL=NLAYO(I)
c        WRITE(LOUT,88) (I,L,THICKO(I,L),SSTO(I,L),L=1,LL)
c        IF(LL .GT. 0) THEN
crr          WRITE(LOUT,86) (L,THICKO(I,L),SSTO(I,L),L=1,LL)
c          WRITE(LOUT,89) (L,THICKO(I,L),SSTO(I,L),L=1,LL)
c        ENDIF
c      ENDDO
c      WRITE(LOUT,87)
c      WRITE(LOUT,84)
c      DO I=1,NPM
c        LL=NLAY(I)
c        L=1
c        N1=LL-L+1
c        WRITE(LOUT,88) I,L,THICK(I,L),SST(I,L)
c        IF(LL .GT. 1) THEN
c          DO L=2,LL
c            N1=LL-L+1
c            WRITE(LOUT,89) L,THICK(I,L),SST(I,L)
c          ENDDO
c        ENDIF
c      ENDDO
crrr aug97 end changes to output

   77 FORMAT(////11X,40HBED SHEARS AND BED PROFILE .............//)
   80 FORMAT(1X,'NODE',5X,'BED SHEAR',5X,'BOT.ELEV',5X,'BED CHANGE',4X
     1,'MASS ER. RATE',3X,'SURF. ER. ',3X,'DEP. RATE',4X,'ER/DEP MASS')
   82 FORMAT(I5,1P7E14.3)
!MD:   83 FORMAT(1H1,///' BED LAYER PROPERTIES....'//)
!MD:   84 FORMAT(5X,'NODE',3X,'LAYER',4X,'THICKNESS',7X,'SHEAR ST.')
crrr aug97   85 FORMAT(/1X,I7,25X,' OLD DEPOSIT')
!MD:   85 FORMAT(/1X, 25X,' OLD DEPOSIT')
crrr aug97   86 FORMAT(13X,I4,1P3E15.3)
!MD:   86 FORMAT(13X,I4,1P2E15.3)
crrr aug97   87 FORMAT(1X,I7,25X,' NEW DEPOSIT')
!MD:   87 FORMAT(1X, 25X,' NEW DEPOSIT')
!MD:   88 FORMAT(5X,2I5,1P2E15.3)
!MD:   89 FORMAT(10X,I5,1P2E15.3)
      RETURN
      END

