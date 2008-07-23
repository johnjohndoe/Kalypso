cipk last update JUNE 5 2006  add test marshing
cipk last update MAY 30 2006  add test for zero thickness and correct NL
cipk last update dec 21 2004 fix bug getting node at bed for averaging
cipk last update dec 16 2004 fix 3-D bug 
cipk last update apr 7 2000 correct interpolation function for 3-d
cipk  last update Mar 14 2000 make erosion linear
CRRR  last update Aug 30 1997  Bed numbering changed:
C  1 = top surface layer, NLAYT = bottom new bed layer
C_______________________________________________________________________
      SUBROUTINE MEROSN
      USE BLK10MOD
      USE BLKSEDMOD
C-
cipk aug05      INCLUDE 'BLK10.COM'

CIPK AUG05	INCLUDE 'BLKSED.COM'
C-
C**********************************************************************
C
C        THIS SUBROUTINE COMPUTES THE EROSION RATE FOR MASS EROSION
C     WHICH OCCURS FOR  THE  TOP NLAYT-1 LAYERS OF DEPOSIT.  ALL
C     COMPUTATIONS ARE MADE ON A NODAL BASIS.
C         THE EROSION RATE IS RETURNED IN ARRAY EDOT(N).
C         BEDEL(N) KEEPS TRACK OF THE BED LEVEL
C         THICK(N,J) RECORDS THE THICKNESS OF EACH LAYER
C         ESRO(N) = OLD VERSION OF BSHEAR(N)
C         SS(1) = CRITICAL SHEAR STRENGTH (NEWTONS/M2
C         SST(N,J) = LAYER SHEAR STRENGTH
C         DELT = TIME STEP
C         VEL(3,N) = WATER DEPTH
C
C********************************************************************
C
C
C.....  LAYERS ARE NUMBERED FROM BOTTOM TO TOP.
C
C
C       CHECK IF TOP LAYER WILL ERODE.
C
      DO NN=1,NPM
        N=NN
        IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1
CIPKSEP05
        NLAYT=NLAYTND(N)
        DO J=1,NLAYT
          SS(J)=SSND(N,J)
          GAD(J)=GADND(N,J)
CIPK MAY06          
          EROSTM(J)=EROST(N,J)
        ENDDO
                
       EDOT(N)=0.
cipk mar00
       edot(nn)=0.
cipk jun06  test for marshing
       if(wsll(n) .lt. ao(n)) go to 100
       
       ES=BSHEAR(N)/ALPHA+(1.-1./ALPHA)*ESRO(N)

C
C     CHECK IF ANY EROSION WILL OCCUR.
C
       IF(ES .GT. SS(1)) THEN
C
C     **** TOP LAYER ERODES    ******
C
crrr aug97  rearrang to loop through layers

CIPK MAY06 CORRECT TO SETTING OF NL
        NL = NLAYTND(NN)

        IF (NL .EQ. NLAYT) NL = NLAYT - 1
crrr aug97 loop through all layers
        DO 20 J=1,NL
        
CIPK MAY06  TEST FOR ZERO THICKNESS
         
          IF(THICK(NN,J) .EQ. 0.) GO TO20
C
crrr aug97 test for shear stress level to skip out
           IF (ES .LT. SS(J)) GO TO 30
CIPK MAY06
           IF(EROSTM(J) .EQ. 0.) THEN
             EDOT(N) = EDOT(N) + THICK(NN,J)*GAD(J)
cipk mar00
             edot(nn)=edot(n)
             BEDEL(NN) = BEDEL(NN)-THICK(NN,J)
             THICK(NN,J)=0.
C
C   IF IT IS A FULLY CONSOLIDATED BOTTOM LAYER IT WILL UNDERGO SURFACE
C  EROSION AND IS CONSIDERED IN SUBROUTINE SEROSN,
C
c        IF(SST(NN,J) .GT. SS(NLAYT)-0.00001.AND.
c     .   SST(NN,J) .LT. SS(NLAYT)+0.00001) GO TO 11
c        IF(SST(NN,J) .GT. ES) GO TO 11
             SST(NN,J)=0.
           ELSE
             seratnewl= EROSTM(J)*(ES/SS(J)-1.)
             INDC = 1           
             TMERODE = SERATNEWL*DELT
             DTHK = TMERODE/GAD(J)
CIPK MAY06             EDOT(NN)=SERATNEWL*DELT

C-   SEE IF MORE THAN THICKNESS OF LAYER IS ERODED.

             BALAN = THICK(NN,J) - DTHK
             IF(BALAN .LE. 0) THEN
               BEDEL(NN) = BEDEL(NN) - THICK(NN,J)
               AMASER=THICK(NN,J)*GAD(J)
               SERATNEWL=AMASER/DELT
               DELTREM=DELT*(1.-THICK(NN,J)/DTHK)
               THICK(NN,J) = 0.
               EDOT(NN)=EDOT(NN)+SERATNEWL*DELT
             ELSE
               BEDEL(NN) = BEDEL(NN) - DTHK
               THICK(NN,J) = THICK(NN,J) - DTHK
               EDOT(NN)=EDOT(NN)+SERATNEWL*DELT
               GO TO 30
             ENDIF            
             
           ENDIF
   20   CONTINUE

   30   CONTINUE

c   11   NLAY(NN)=J+1
c        IF(NLAY(NN) .GT. NLAYT) NLAY(NN)=NLAYT
c        SST(NN,J+1)=SS(1)
C        EDOT(N)=EDOT(N)/(DELT*WD(N))
crr aug97 end changes
        EDOT(N)=EDOT(N)/DELT
        EDOT(N)=EDOT(N)*1000.
cipk mar00
        edot(nn)=edot(n)
       ENDIF
cipk jun06       
  100  continue       
      ENDDO

      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          EDOT(NN)=(EDOT(IMID(NN,1))+EDOT(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          NLAY(NN)=MAX(NLAY(IMID(NN,1)),NLAY(IMID(NN,2)))
          NL = NLAY(NN)
          IF (NL .EQ. NLAYTND(NN)) NL = NLAYTND(NN) - 1
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
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
cipk dec04          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 1) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              nod=nop(n,j)
              k1=nod
cipk dec04              if(ndep(n) .gt. 1) k1=nref(nod)+ndep(nod)-1
              if(ndep(nod) .gt. 1) k1=nref(nod)+ndep(nod)-1
              edot(nod)=(edot(nop(n,j1))+edot(nop(n,j2)))/2.
              bedel(nod)=(bedel(nop(n,j1))+bedel(nop(n,j2)))/2.
              if(k1 .ne. nod) then
                edot(k1)=edot(nod)
              endif
              if(nlayt .gt. 0) then
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
