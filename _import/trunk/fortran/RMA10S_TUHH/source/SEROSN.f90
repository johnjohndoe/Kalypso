!     Last change:  MD    5 Oct 2009    6:31 pm
!ipk last update JUNE 5 2006  add test marshing
!IPK LAST UPDATE MARCH 30 2006 UPDATE FOR MORE REALISTIC TRANSITION OPTION 
!IPK LAST UPDATE MARCH 25 2006 ADD TESTMODE
!IPK LAST UPDATE MARCH 25 MAJOR REWRITE 
!ipk last update dec 21 2004 fix bug getting node at bed for averaging
!ipk last update dec 16 2004 fix 3-D bug 
!ipk last update apr 7 2000 correct interpolation function for 3-d
!ipk  last update Mar 14 2000 make erosion linear
!IPK  LAST UPDATE JAN 9 1999 FIX REFERENCE TO THICK (N FOR NN)
!RRR  last update Aug 30 1997
!     Major rewrite to reverse logic order.
!     No attemp to compare with last version
!     Bed numbering changed:
!     1 = top surface layer, NLAYT = bottom new bed layer
!----------------------------------------------------------------------
      SUBROUTINE SEROSN(II)
      USE BLK10MOD
      USE BLKSEDMOD
!----------------------------------------------------------------------
!       THIS SUBROUTINE COMPUTES THE RATE OF SURFACE EROSION AS SERAT(N)
!   FOR EACH NODE. SURFACE EROSION CAN OCCUR FROM THE BOTTOM LAYER OF
!   A NEW DEPOSIT WHICH IS FILLED UP OR FROM ANY LAYER OF THE ORIGINAL
!   BED IF SUCH EXISTS.
!
!
!MD:  Erosion in den Layern an der Sohle: als Sohlerosion
!MD:   --> Ergebnis Erosionsrate SERAT(NN) in [mg/l x m/s]
!
!     FACT_SPEED_UP = Speed-up factor for bed evolution
!
!MD:  Neu Kennung des Aufrufs II:
!MD     II = 1 in der Iterations-Schleife eines Zeitschrittes
!MD            (Keine Anpassung der Sohle und Layerdicke)
!MD     II = 2 ausserhalb der Iterations-Schleife eines Zeitschrittes
!MD             Anpassung der Sohle und Layerdicke
!MD:  Neu Kennung des Aufrufs II:
!----------------------------------------------------------------------
!
!...LOOP FOR EACH NODE
!
!
!......................................................
!rrr aug97 new looping
!MD: Schleife ueber alle Layer NLAYT, in der Wassersaule
!MD:  Sortierung der Daten "Suspendiertes Material"
!......................................................
!
!MD: Basis-Variable: Wasserdichte
!MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx      
!     Je nach Vorgabe der SI-Einheiten unter "IGRV"      
      ROAVG=1.935
      IF (GRAV < 32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte
!
      DO 500 N=1,NPM
        IF(IMID(N,1) == 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
!IPKSEP05
          NLAYT=NLAYTND(N)
          DO J=1,NLAYT
            SS(J)=SSND(N,J)
!
!MD: NEUE Berechnung der Trocken-Rohdichte JE Sus.[kg/m3]            
!MD: use bulk densitiy per Layer            
!MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)            
            GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
            GAD(J)=GADND(N,J)
          ENDDO
!MD: ERC=ERCND(N) use per Layer          
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
          NL = NLAYT
!rr aug97 new units for temal2
!MD:      TEMAL2 = SERAT(NN)/1000.  :never used!!
!ipk mar00
          serat(n)=0.0
          SERAT(NN) = 0.0
!ipk jun06  test for marshing
          if(wsll(n) < ao(n)) go to 500
!
!ipk mar06         
          seratnewl=0.0
!MD:  INDC = 0   :never used!!          
!MD:  BALM = 0.  :never used!!          
          DELTREM=DELT
!
!
!......................................................
!...CHECK IF BOTTOM LAYER OF NEW, FULL DEPOSIT EXISTS.
!MD:  Material, aus suspeniertem und abgelagertem Material
!MD:   im obersten Sus.Layer erodiert
!   check no other layers exist
!......................................................
         DO L=1,NLAYT-1
!ipk jan99 change thick from nn to n
            IF (THICK(N,L) > 0.0)  GO TO 500
         ENDDO
!
!  Errode bottom layer of new deposit
!
!ipk jan99 change thick from nn to n
         IF (THICK(N,NLAYT) > 0.) THEN
            IF (BSHEAR(NN) < SS(NLAYT)) GO TO 500
!ipk mar06            
!MD: seratnewl= ERC*(BSHEAR(NN)/SS(NLAYT)-1.)            
!MD: erosionrate per Layer            
            Seratnewl= EROST(NN,NLAYT) *(BSHEAR(NN)/SS(NLAYT)-1.)
!
!MD   INDC = 1  :never used!!            
            TMERODE = SERATNEWL*DELT*FACT_SPEED_UP
            DTHK = TMERODE/GAD(NLAYT)
!
!-   SEE IF MORE THAN THICKNESS OF LAYER IS ERODED.
            BALAN = THICK(N,NLAYT) - DTHK
            IF(BALAN <= 0) THEN
              AMASER    =THICK(N,NLAYT)*GAD(NLAYT)
              SERATNEWL =AMASER/ (DELT*FACT_SPEED_UP)
              DELTREM   =DELT*FACT_SPEED_UP * (1.-THICK(N,NLAYT)/DTHK)
              IF(II == 2) THEN
                BEDEL(N) = BEDEL(N) - THICK(N,NLAYT)
                THICK(N,NLAYT) = 0.
              Endif
            ELSE
              IF(II == 2) THEN
                BEDEL(N) = BEDEL(N) - DTHK
                THICK(N,NLAYT) = THICK(N,NLAYT) - DTHK
              Endif
              GO TO 1
            ENDIF            
         ENDIF
!
!
!......................................................
!     EROSION OF ORIGINAL BED LAYERS
!...
!   MD: Gleiche Erosionsberechnungen für das INITIAL BED
!......................................................
!   CHECK IF ANY MATERIAL ON BED
         IF (NLAYO(N) == 0)  GO TO 1
         SERATOLD=0.
         SERATOLD1=0.
         DO 30 L=1,NLAYO(N)
            IF (THICKO(N,L) <= 0.)  GO TO 30
            IF (BSHEAR(NN) <= SSTO(N,L)) THEN
!  shear stres too small skip to end of loop            
              SERATNEWL=SERATOLD+SERATNEWL
!             newval=mass from full layers+new layer                
              GO TO 1
            ELSE
              seratold1 = SMVAL(N,L)*(BSHEAR(NN)/SSTO(N,L)-1.)
!             seratold1 is rate for this layer assuming full erosion              
              OLDEROD=SERATOLD1*DELTREM*FACT_SPEED_UP
!
!MD: NEUE Berechnung der Trocken-Rohdichte BETT[kg/m3]              
              GADLN =(GBO(N,L)-ROAVG)*GAC/(GAC-ROAVG)
!MD: GADLN =(GBO(N,L)-GAW)*GAC/(GAC-GAW)              
!MD: NEUE Berechnung der Trocken-Rohdichte BETT [kg/m3]              
!
              DTHKOLD = OLDEROD/GADLN
              BALOLD  = THICKO(N,L) - DTHKOLD
!  rates for this layer computed  balold positive means all taken so finalize
!-    ONLY THIS LAYER ERODED
              IF (BALOLD > 0.0) THEN
!               new thickness computed add to accumulate more erosion
                SERATNEWL=SERATOLD + SERATOLD1*DELTREM/DELT + SERATNEWL
!               new value = mass from full layer+current+new layer
                IF(II == 2) THEN
                  BEDEL(N) = BEDEL(N) - DTHKOLD
                  THICKO(N,L) = THICKO(N,L) - DTHKOLD
                Endif
                GO TO 1
              ELSE
!-            MORE THAN THIS LAYER MAY BE ERODED.
                AMASER  =THICKO(N,L)*GADLN
                DELTREM =DELTREM*FACT_SPEED_UP*(1.-THICKO(N,L)/DTHKOLD)
                SERATOLD=SERATOLD + AMASER/(DELT*FACT_SPEED_UP)
                IF(II == 2) THEN
                  BEDEL(N) = BEDEL(N) - THICKO(N,L)
                  THICKO(N,L) = 0.0
                Endif
              ENDIF
            ENDIF
!
   30     CONTINUE
          SERATNEWL=SERATNEWL+SERATOLD
!
    1     CONTINUE
!MD: Umrechnung [kg/m³ x m/s] * 1000          
!MD:  --> in [g/m³ x m/s]          
          SERAT(NN) = SERATNEWL*1000.
          serat(n)=serat(nn)
        ENDIF
  500 CONTINUE
!
!...................................................
!MD:   Mittelung der Ergebnisse an den Eckknoten ueber
!MD:    fuer die Ergebnisse an alle Mittseiten-Knoten
!......................................................
      DO N=1,NPM
        IF(IMID(N,1) > 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
          SERAT(NN)=(SERAT(IMID(NN,1))+SERAT(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          NLAYT=NLAYTND(NN)
          THICK(NN,NLAYT)=(THICK(IMID(NN,1),NLAYT)                      &
     &                    +THICK(IMID(NN,2),NLAYT))/2.
          NLAYOT=MIN(NLAYO(IMID(NN,1)),NLAYO(IMID(NN,2)))
          IF(NLAYOT > 0) THEN
            DO L=1,NLAYO(NN)
              THICKO(NN,L)=(THICKO(IMID(NN,1),L)                        &
     &                     +THICKO(IMID(NN,2),L))/2.
            ENDDO
          ENDIF
        ELSE
!          WRITE(239,*) N,SERAT(N)
        ENDIF 
      ENDDO
!
!
!
!......................................................
!IPK AUG06 LOOK FOR OVERLOADING OF ELEMENTS AND ADD DIFFUSION
!ipkexp      IOVLDN=0
!ipkexp      DO N=1,NPM
!ipkexp        PROJCONC=(EDOT(N)+SERAT(N))*DELT/VEL(3,N)
!ipkexp        IF(PROJCONC > 100)THEN
!ipkexp          IOVLDN(N)=1
!ipkexp          WRITE(*,*) IT,N,PROJCONC
!ipkexp          WRITE(215,*) IT,N,PROJCONC
!ipkexp        ENDIF
!ipkexp      ENDDO
!ipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
!ipk dec04
        ncn=0
        if(imat(n) > 0  ) then
          if(imat(n) < 1000 .AND. ncorn(n) < 9) then
            ncn=ncorn(n)
!ipk dec04          elseif(imat(n)/1000 == 1 .OR. imat(n)/1000 == 2) then
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
!ipk dec04              if(ndep(n) > 1) k1=nref(nod)+ndep(nod)-1
              if(ndep(nod) > 1) k1=nref(nod)+ndep(nod)-1
              serat(nod)=(serat(nop(n,j1))+serat(nop(n,j2)))/2.
              bedel(nod)=(bedel(nop(n,j1))+bedel(nop(n,j2)))/2.
              if(k1 /= nod) then
                serat(k1)=serat(nod)
              endif
              if(nlayt > 0) then
               do k=1,nlayt
                 thick(nod,k)=(thick(nop(n,j1),k)+thick(nop(n,j2),k))/2.
               enddo
              endif
              if(nlayo(nod) > 0) then
              do k=1,nlayo(nod)
              thicko(nod,k)=(thicko(nop(n,j1),k)+thicko(nop(n,j2),k))/2.
              enddo
              endif
            enddo
          endif
        endif
      enddo 
!
!      DO N=1,NPM
!        WRITE(233,'(I5,3G15.5)') N,SERAT(N),THICK(N,1),THICKO(N,1)
!      ENDDO
!
      RETURN
      END
