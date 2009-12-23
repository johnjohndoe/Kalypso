C     Last change:  MD    5 Oct 2009    6:24 pm
cipk last update JUNE 5 2006  add test marshing
cipk last update MAY 30 2006  add test for zero thickness and correct NL
cipk last update dec 21 2004 fix bug getting node at bed for averaging
cipk last update dec 16 2004 fix 3-D bug 
cipk last update apr 7 2000 correct interpolation function for 3-d
cipk  last update Mar 14 2000 make erosion linear
CRRR  last update Aug 30 1997  Bed numbering changed:
C  1 = top surface layer, NLAYT = bottom new bed layer
C----------------------------------------------------------------------
      SUBROUTINE MEROSN(II)
      USE BLK10MOD
      USE BLKSEDMOD

C**********************************************************************
C
C     THIS SUBROUTINE COMPUTES THE EROSION RATE FOR MASS EROSION
C     WHICH OCCURS FOR THE TOP NLAYT-1 sus.LAYERS OF DEPOSIT. ALL
C     COMPUTATIONS ARE MADE ON A NODAL BASIS.
C         THE EROSION RATE IS RETURNED IN ARRAY EDOT(N).
C         BEDEL(N)      = KEEPS TRACK OF THE BED LEVEL
C         THICK(N,J)    = RECORDS THE THICKNESS OF EACH LAYER
C         ESRO(N)       = OLD VERSION OF BSHEAR(N)
C         SS(1)         = CRITICAL SHEAR STRENGTH (NEWTONS/M2
C         SST(N,J)      = LAYER SHEAR STRENGTH
C         DELT          = TIME STEP
C         VEL(3,N)      = WATER DEPTH
C         FACT_SPEED_UP = Speed-up factor for bed evolution
C
!MD:  Neu Kennung des Aufrufs II:
!MD     II = 1 in der Iterations-Schleife eines Zeitschrittes
!MD            (Keine Anpassung der Sohle und Layerdicke)
!MD     II = 2 ausserhalb der Iterations-Schleife eines Zeitschrittes
!MD             Anpassung der Sohle und Layerdicke
!MD:  Neu Kennung des Aufrufs II:
C
C********************************************************************

!MD:  --------------------------------------------------------
!MD:  Erosion in den suspendierten Layern: als Massenerosion
!MD:   --> Ergebnis Erosionsrate EDOT(NN) in [mg/l x m/s]
!MD:  --------------------------------------------------------

C    LAYERS ARE NUMBERED FROM BOTTOM TO TOP.
C    CHECK IF TOP LAYER WILL ERODE.
C

!MD: Basis-Variable: Wasserdichte
      !MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
      !     Je nach Vorgabe der SI-Einheiten unter "IGRV"
      ROAVG=1.935
      IF (GRAV < 32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte

      DO NN=1,NPM
        N=NN
        IF(NDEP(NN) > 1) N=NREF(NN)+NDEP(NN)-1
CIPKSEP05
        NLAYT=NLAYTND(N)
        DO J=1,NLAYT
          SS(J)=SSND(N,J)

          !MD:NEUE Berechnung der Trocken-Rohdichte Sus [kg/m3]
          !MD: use bulk densitiy per Layer
          !MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
          GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
          GAD(J)=GADND(N,J)
          EROSTM(J)=EROST(N,J)
        ENDDO
                
        EDOT(N)=0.
        edot(nn)=0.
cipk jun06  test for marshing
        if(wsll(n) < ao(n)) go to 100

!MD:  Entfernen einer zeitlichen Wichtung an dieser
!MD:   Stelle! Konzept durchgängig gleich mit einwirkender
!MD:   Schub = aktueller Schub!
!MD
!MD:  ES=BSHEAR(N)/ALPHA+(1.-1./ALPHA)*ESRO(N)
!MD
!MD:  --> "ES" wird durch "BSHEAR(N)" ersetzt!!!


! ---------------------------------------------
C     CHECK IF ANY EROSION WILL OCCUR.
C     **** TOP LAYER ERODES    ******
! ---------------------------------------------
       IF(BSHEAR(N) > SS(1)) THEN
crrr aug97  rearrang to loop through layers
CIPK MAY06  CORRECT TO SETTING OF NL
        NL = NLAYTND(NN)
        IF (NL == NLAYT) NL = NLAYT - 1
crrr aug97  loop through all layers
        DO 20 J=1,NL
        
CIPK MAY06  TEST FOR ZERO THICKNESS
          IF(THICK(NN,J) == 0.) GO TO20
C
crrr aug97  test for shear stress level to skip out
           IF (BSHEAR(N) < SS(J)) GO TO 30
CIPK MAY06
           IF(EROSTM(J) == 0.) THEN
             EDOT(N) = EDOT(N) + THICK(NN,J)*GAD(J)
             edot(nn)=edot(n)
             IF(II == 2) THEN
               BEDEL(NN) = BEDEL(NN)-THICK(NN,J)
               THICK(NN,J)=0.
               SST(NN,J)=0.
               !MD: ??? wenn, dann SS nicht SST!
             ENDIF

C   IF IT IS A FULLY CONSOLIDATED BOTTOM LAYER IT WILL UNDERGO SURFACE
C  EROSION AND IS CONSIDERED IN SUBROUTINE SEROSN,
C
c        IF(SST(NN,J) > SS(NLAYT)-0.00001 .AND. 
c     .   SST(NN,J) < SS(NLAYT)+0.00001) GO TO 11
c        IF(SST(NN,J) > ES) GO TO 11

           ELSE
             seratnewl= EROSTM(J)*(BSHEAR(N)/SS(J)-1.)
             !MD:  INDC = 1  :never used!!
             TMERODE = SERATNEWL*DELT*FACT_SPEED_UP
             DTHK = TMERODE/GAD(J)
CIPK MAY06   EDOT(NN)=SERATNEWL*DELT

C-   SEE IF MORE THAN THICKNESS OF LAYER IS ERODED
!    ---------------------------------------------
             BALAN = THICK(NN,J) - DTHK
             IF(BALAN <= 0) THEN
               AMASER   =THICK(NN,J)*GAD(J)
               SERATNEWL=AMASER/ (DELT*FACT_SPEED_UP)
               DELTREM  =DELT*FACT_SPEED_UP*(1.-THICK(NN,J)/DTHK)
               EDOT(NN) =EDOT(NN)+ SERATNEWL*DELT*FACT_SPEED_UP
               IF(II == 2) THEN
                 BEDEL(NN) = BEDEL(NN) - THICK(NN,J)
                 THICK(NN,J) = 0.
               Endif
             ELSE
               EDOT(NN) =EDOT(NN)+ SERATNEWL*DELT*FACT_SPEED_UP
               IF(II == 2) THEN
                 BEDEL(NN) = BEDEL(NN) - DTHK
                 THICK(NN,J) = THICK(NN,J) - DTHK
               ENdif
               GO TO 30
             ENDIF            
             
           ENDIF
   20   CONTINUE

   30   CONTINUE

c   11   NLAY(NN)=J+1
c        IF(NLAY(NN) > NLAYT) NLAY(NN)=NLAYT
c        SST(NN,J+1)=SS(1)
C        EDOT(N)=EDOT(N)/(DELT*WD(N))
crr aug97 end changes
        EDOT(N)=EDOT(N)/ (DELT*FACT_SPEED_UP)
        EDOT(N)=EDOT(N)*1000.
cipk mar00
        edot(nn)=edot(n)
       ENDIF
cipk jun06       
  100  continue       
      ENDDO

      DO N=1,NPM
        IF(IMID(N,1) > 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
          EDOT(NN)=(EDOT(IMID(NN,1))+EDOT(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          NLAY(NN)=MIN(NLAY(IMID(NN,1)),NLAY(IMID(NN,2)))
          NL = NLAY(NN)
          IF (NL == NLAYTND(NN)) NL = NLAYTND(NN) - 1
          DO J=1,NL
            THICK(NN,J)=(THICK(IMID(NN,1),J)
     +                    +THICK(IMID(NN,2),J))/2.
          ENDDO
        ENDIF
      ENDDO

cipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
cipk dec04  add ncn=0
        ncn=0
        if(imat(n) > 0  ) then
          if(imat(n) < 1000 .AND. ncorn(n) < 9) then
            ncn=ncorn(n)
cipk dec04          elseif(imat(n)/1000 == 1 .OR. imat(n)/1000 == 2) then
          elseif(imat(n)/1000 == 1 .OR. imat(n)/1000 == 1) then
            ncn=ncorn(n)
          endif
          if(ncn > 0) then
            if(ncn == 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              nod=nop(n,j)
              k1=nod
cipk dec04              if(ndep(n) > 1) k1=nref(nod)+ndep(nod)-1
              if(ndep(nod) > 1) k1=nref(nod)+ndep(nod)-1
              edot(nod)=(edot(nop(n,j1))+edot(nop(n,j2)))/2.
              bedel(nod)=(bedel(nop(n,j1))+bedel(nop(n,j2)))/2.
              if(k1 /= nod) then
                edot(k1)=edot(nod)
              endif
              if(nlayt > 0) then
                do k=1,nlayt
                 thick(nod,k)=(thick(nop(n,j1),k)+thick(nop(n,j2),k))/2.
                enddo
              endif
            enddo
          endif
        endif
      enddo 


      RETURN
      END
