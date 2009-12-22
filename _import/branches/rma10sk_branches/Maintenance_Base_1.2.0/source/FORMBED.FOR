C     Last change:  MD   30 Jul 2009    2:11 pm
CIPK LAST UPDATE MAY 30 2006 ADD GAD DEF.
CIPK LAST UPDATE APRIL 22 2006 REMOVE TEST ON NLAYER
CIPK LAST UPDATE APRIL 05 2006 TEST TO MAKE SURE LOOPS ARE ONLY ENTERED ONCE
CIPK LAST UPDATE MARCH 30 2006 CORRECT A SUBSCRIPT ERROR
CIPK LAST UPDATE JAN 23 2005 ALLOW FOR RECORD OF BED CHANGES
CIPK LAST UPDATE APRIL 18 1999 ADD CHECK FOR MUD FLATS TO CUT OFF SDEP/ERS
CIPK LAST UPDATE fEB 18 1999 FIX ERROR IN PRINTING ORDER AND BEDELEV
CIPK LAST UPDATE JAN 9 1999 FIX ERROR IN UNITS FOR PRINTING
CIPK                        CORRECT REFERENCE TO BED TO NN
CRRR  Last update Aug 30 1997  Bed numbering changed:
C  1 = top surface layer, NLAYT = bottom new bed layer
CIPK  LAST UPDATE APR 4 1997 
      SUBROUTINE FORMBED(II)

C----------------------------------------------------------------------
C       THIS ROUTINE FORMS THE NEW BED THAT IS A RESULT OF DEPOSITION
C     IN THE LAST TIME INTERVAL
C----------------------------------------------------------------------
C
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD

cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05	INCLUDE 'BLKDR.COM'

crrr aug97 add character lines
      CHARACTER*256 LINE256
      CHARACTER*80  FMT
C-
!NiS,jul06: Consistent data types for variable passing
      REAL(KIND=8):: HTP
      REAL(KIND=8):: SUMTHICK   
      integer (kind = 4) :: iprtf
!-
C-.....ASSIGN PROPER COEFS.....
C-

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
ccc     AT(NN)=0.
C
C********************************************************************
C
C      FORM NEW BED PROFILE.
C*********************************************************************
C
C         WHEN II=1 ONLY NEWBED IS FORMED FROM INPUT VALUES OF
C    MASS READ IN AT THE BOTTOM OF EACH NODE.
C
  150   CONTINUE
crrr add test for II = 1
        IF(IABS(NB) .GT. 0 .AND.  INEWBED .EQ. 0) GO TO 221
cipk feb95        IF(TM(NN).LE.0.)GO TO 350
C.......................................................................
C       NUMBER OF suspended LAYERS ON BOTTOM
C.......................................................................
        BEDEL(NN)=AO(NN)
        NLAY(NN)=1

C   ..   CALCULATE THE NUMBER OF LAYERS THAT CAN BE FORMED BY THIS MASS
c
cipk apr06        NL=TM(NN)/WEGT
cipk apr06        if (nl .gt. mxsedlay+10)  then
cipk apr06           write(*,*) '  lt > mlayer ', lt, nn,tm(nn)
cipk apr06           stop 
cipk apr06        endif


crrr aug97 add largely new logic to statment "350 CONTINUE"
C  Add the mass of top layer to mass to be deposited.
C     *** However if all layers except the bottom are
C         full add mass directly into bottom layer  ****
C
        DO L=1,NLAYT-1
!MD:    For all suspended Layer, without the last one
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
        ENDDO
C
C  Put All remaining total mass TM in bottom layer
        THICKNW = TM(NN)/GAD(NLAYT)
        THICK(NN,NLAYT) = THICK(NN,NLAYT) + THICKNW
        TM(NN) = 0.
  200   CONTINUE
C
C
        !MD: BEDORIG Sohldicke in [m] ohne GOK!!
        BEDORIG(NN) = 0.
        DO 220 L=1,NLAYT
          BEDORIG(NN) = BEDORIG(NN) + THICK(NN,L)
          SST(NN,L) = SS(L)
          IF (THICK(NN,L) .GT. 0.0) THEN
            NLAY(NN) = L
          ENDIF
  220   CONTINUE

CIPK JAN05
        IF(NLAYO(NN) .GT. 0) THEN
          DO L=1,NLAYO(NN)
            BEDORIG(NN) = BEDORIG(NN) + THICKO(NN,L)
          ENDDO
        ENDIF
         
C     AORIG is the solid bed level
        GO TO 350
C
C       ENTER HERE ONLY FOR RESTART CASE
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
CIPK JAN05
        IF(NLAYO(NN) .GT. 0) THEN
          DO L=1,NLAYO(NN)
            BEDEL(NN) = BEDEL(NN) + THICKO(NN,L)
          ENDDO
        ENDIF

  350 CONTINUE


C   BEDEL IS NOW LEVEL OF BED
! ---------------------------------------
      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          !MD: must be MIN not MAX
          NLAYTND(NN)= MIN (NLAYTND(IMID(NN,1)),NLAYTND(IMID(NN,2)))
CIPK MAY06 SET DENSITY
          do l=1,nlaytnd(nn)
            gadnd(nn,l)= (gadnd(IMID(NN,1),l) + gadnd(IMID(NN,2),l)) /2.
          enddo
          BEDORIG(NN)=(BEDORIG(IMID(NN,1))+BEDORIG(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          TM(NN)=(TM(IMID(NN,1))+TM(IMID(NN,2)))/2.
          NLAYT=NLAYTND(NN)
          DO J=1,NLAYT
            THICK(NN,J)=(THICK(IMID(NN,1),J)
     +                    +THICK(IMID(NN,2),J))/2.
CIPK MAR06
            SST(NN,J)=(SST(IMID(NN,1),J)+SST(IMID(NN,2),J))/2.
          ENDDO
          !MD: must be MIN not MAX!!
          NLAYTO=Min(NLAYO(IMID(NN,1)),NLAYO(IMID(NN,2)))
          IF(NLAYTO .GT. 0) THEN
            DO L=1,NLAYTO
              THICKO(NN,L)=(THICKO(IMID(NN,1),L)
     +                     +THICKO(IMID(NN,2),L))/2.
            ENDDO
          ENDIF
        ENDIF
      ENDDO


c     BEDEL is the total thickness of mud (OUT OF DATE)
C********************************************************************
C    SET old Elevations and SHEARS-Stress TO NEW VALUES.
C*********************************************************************
      DO 400 N=1,NPM
CIPK JAN05
c        IF(II .EQ. 1  .AND. IABS(NB) .GE. 100  .AND.
c     +     IDEPCH .NE. 2) THEN
c	    BEDORIG(N)=BEDEL(N)
c	  ENDIF
        IF(INEWBED .EQ. 0) THEN
          AORIG(N)=AORIG(N)-BEDORIG(N)
C         AORIG is the solid bed level
          bedel(n)=bedel(n)-bedorig(n)        
          DEPINCR(N)=BEDEL(N)-aorig(n)-BEDORIG(N)
C         DEPINCR IS INCREASE IN BED THICKNESS FROM INPUT VALUE

        ELSE
          DEPINCR(N)=0.
          AORIG(N)=AORIG(N)-BEDORIG(N)
C         AORIG is the solid bed level

          !MDMD: UNSINN - Pruefen
          DELBED(N)=-DEPINCR(N)
          !MD: UNSINN Betthöhe = -0.0 ???
        
        ENDIF
        I=N
        IF(NDEP(N) .GT. 1) I=NREF(N)+NDEP(N)-1
        ESRO(I)=BSHEAR(I)
        ESRO(N)=BSHEAR(N)
C       CONCO(I)=CONC(I)
        BEDELO(N)=BEDEL(N)-AORIG(N)
        BEDEL(N)=BEDELO(N)
  400 CONTINUE
      
!MD:      WRITE(LOUT,77)

C*********************************************************************
C     PRINT THE BED SHEAR ,ER/DEP RATES AND ELEVATIONS.
C*********************************************************************
      IF(II .NE. 1) THEN
cipk apr97 add option
CIPK SKIP THIS TEST        IF(MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) RETURN
C        WRITE(181,80)
        DO 480 I=1,NPM
          N=I
          IF(NDEP(I) .GT. 1) N=NREF(I)+NDEP(I)-1

cipk feb99  BC=BEDEL(I)-AO(I)
!MD: cleaning up things which are never used
      !MD:    BC=BEDEL(I)+AORIG(I)
crrr aug97 remove scling by depth
!MD: cleaning up things which are never used
      !MD:    DMER=EDOT(N)
      !MD:    SER=SERAT(N)
      !MD:    DR=TMD(I)/DELT
cipk jan99    EDM=(DMER+SER-DR)*DELT

!MD:      Used original Parameters
      !MD:    EDM=((DMER+SER)/1000.-DR)*DELT
          EDM=((EDOT(N)+SERAT(N))/1000.-(TMD(I)/DELT))*DELT

cipk jan99 reverse prionting order
C          WRITE(181,82)I,BSHEAR(I),BC,BEDEL(I),DMER,SER,DR,EDM
  480   CONTINUE
      ENDIF

CIPK APR06 INIIALIZE IPASST
      DO N=1,NPM
        IPASST(N)=0
      ENDDO

c update bed elevation for flow calculation
      
      DO M=1,NEM

       IF(IMAT(M) .GT. 0) THEN
        DO K=1,NCORN(M),2
	    N=NOP(M,K)
CIPK APR06 TEST TO SEE IF THIS NODE HAS BEEN PROCESSED 
	    IF(IPASST(N) .EQ. 0) THEN
	      IPASST(N)=1
  	      DEP=VEL(3,N)
       !MD	 AO(N)=AO(N)+DEPINCR(N)
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
            IF(K .LT. NCORN(M)) THEN
	      N2=NOP(M,K+1)
	    ELSE
              N2=NOP(M,1)
	    ENDIF
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
C
C... PRINT OUT LAYER PROPERTIES.
C
cipk apr97 add option
      !nis,dec08: Introduce local copy, because nprtf shouldn't be changed here and zero-entry means one-entry
      iprtf = nprtf
      if (iprtf == 0) iprtf = 1

!MD: Output is deactivated, because results are already written into BED-File
!MD      !MD: Abfrage zur Ausgabe:
!MD      !MD: Geaendert, da anschließend noch Berechnungen!!
!MD      !MD:   IF (MOD(IT,IPRTF).NE.0  .AND. TET.NE.0.) RETURN
!MD      IF (MOD(IT,IPRTF).NE.0  .AND. TET.NE.0.) THEN
!MD        WRITE(LINE256,'(A)')
!MD     &  '  Node Bed Shear Bed-elev   SedMass  SumLayer
!MD     + SusLayer-Thickness (mm)'
!MD        IDX = MAX(65,47+NLAYT*10) + 3
!MD
!MD        IF (NLAYO(1) .GT. 0) THEN
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

      INT = (NP+5)/6
      MLAYRS = MIN(NLAYT,5)
      DO 75 NN=1,NPM
         N = NN
         IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

CIPKSEP05
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

C
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
         IF (NLAYT .GT. 0 .OR. NLAYO(NN) .GT. 0) THEN
c             SUMTHICK = BEDEL(N) - AO(N)
             MLAYRS = NLAY(NN)
!MD             WRITE(LOUT,'(I5,F10.4,F10.6, F10.2, 20F10.3)')
!MD     &          NN, BSHEAR(NN), AO(NN),TVN, SUMTHICK*1000.,
!MD     &             (1000. * THICK(NN,L),L=1,NLAYT),
!MD     &             (1000. * THICKO(NN,L),L=1,NLAYO(NN))
        ENDIF
   75 CONTINUE

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

!MD   77 FORMAT(////11X,40HBED SHEARS AND BED PROFILE .............//)
!MD   80 FORMAT(1X,'NODE',5X,'BED SHEAR',5X,'BOT.ELEV',5X,'BED CHANGE',4X
!MD     1,'MASS ER. RATE',3X,'SURF. ER. ',3X,'DEP. RATE',4X,'ER/DEP MASS')

!MD:   82 FORMAT(I5,1P7E14.3)
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
