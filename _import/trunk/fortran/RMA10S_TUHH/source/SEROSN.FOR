CIPK LAST UPDATE MARCH 30 2006 UPDATE FOR MORE REALISTIC TRANSITION OPTION 
CIPK LAST UPDATE MARCH 25 2006 ADD TESTMODE
CIPK LAST UPDATE MARCH 25 MAJOR REWRITE 
cipk last update dec 21 2004 fix bug getting node at bed for averaging
cipk last update dec 16 2004 fix 3-D bug 
cipk last update apr 7 2000 correct interpolation function for 3-d
cipk  last update Mar 14 2000 make erosion linear
CIPK  LAST UPDATE JAN 9 1999 FIX REFERENCE TO THICK (N FOR NN)
CRRR  last update Aug 30 1997
C     Major rewrite to reverse logic order.
C     No attemp to compare with last version
C     Bed numbering changed:
C     1 = top surface layer, NLAYT = bottom new bed layer
C_________________________________________________________________________
      SUBROUTINE SEROSN
      USE BLK10MOD
      USE BLKSEDMOD
C.......
C       THIS SUBROUTINE COMPUTES THE RATE OF SURFACE EROSION AS SERAT(N)
C   FOR EACH NODE. SURFACE EROSION CAN OCCUR FROM THE BOTTOM LAYER OF
C   A NEW DEPOSIT WHICH IS FILLED UP OR FROM ANY LAYER OF THE ORIGINAL
C   BED IF SUCH EXISTS.
C.......
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPKAUG05	INCLUDE 'BLKSED.COM'
C-
C
C...LOOP FOR EACH NODE
C
C
crrr aug97 new looping
      DO 500 N=1,NPM
       IF(IMID(N,1) .EQ. 0) THEN
         NN = N
         IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
CIPKSEP05
        NLAYT=NLAYTND(N)
        DO J=1,NLAYT
          SS(J)=SSND(N,J)
          GAD(J)=GADND(N,J)
        ENDDO
        ERC=ERCND(N)
        GAC=GACND(N)
        GAB=GABND(N)
        GAW=GAWND(N)
                         
         NL = NLAYT
crr aug97 new units for temal2
         TEMAL2 = SERAT(NN)/1000.
cipk mar00
         serat(n)=0.
         SERAT(NN) = 0.

cipk mar06         
         seratnewl=0
         
         INDC = 0
         BALM = 0.
         DELTREM=DELT
C
C...CHECK IF BOTTOM LAYER OF NEW, FULL DEPOSIT EXISTS.
C
C   check no other layers exist
         DO L=1,NLAYT-1
cipk jan99 change thick from nn to n
            IF ( THICK(N,L) .GT. 0.0)  GO TO 500
         ENDDO

C  Errode bottom layer of new deposit

cipk jan99 change thick from nn to n
         IF (THICK(N,NLAYT) .GT. 0.) THEN
            IF (BSHEAR(NN) .LT. SS(NLAYT)) GO TO 500
cipk mar06            
            seratnewl= ERC*(BSHEAR(NN)/SS(NLAYT)-1.)
            INDC = 1           
            TMERODE = SERATNEWL*DELT
            DTHK = TMERODE/GAD(NLAYT)

C-   SEE IF MORE THAN THICKNESS OF LAYER IS ERODED.

            BALAN = THICK(N,NLAYT) - DTHK
            IF(BALAN .LE. 0) THEN
              BEDEL(N) = BEDEL(N) - THICK(N,NLAYT)
              AMASER=THICK(N,NLAYT)*GAD(NLAYT)
              SERATNEWL=AMASER/DELT
              DELTREM=DELT*(1.-THICK(N,NLAYT)/DTHK)
              THICK(N,NLAYT) = 0.
            ELSE
               BEDEL(N) = BEDEL(N) - DTHK
               THICK(N,NLAYT) = THICK(N,NLAYT) - DTHK
               GO TO 1
            ENDIF            
         ENDIF
C...
C     EROSION OF ORIGINAL BED LAYERS
C...

C   CHECK IF ANY MATERIAL ON BED

         IF (NLAYO(N) .EQ. 0)  GO TO 1
         SERATOLD=0.
         SERATOLD1=0.
         DO 30 L=1,NLAYO(N)
            IF (THICKO(N,L) .LE. 0.)  GO TO 30
            IF (BSHEAR(NN) .LE. SSTO(N,L)) THEN
              SERATNEWL=SERATOLD+SERATNEWL
              GO TO 1
            ELSE
              seratold1 = SMVAL(N,L)*(BSHEAR(NN)/SSTO(N,L)-1.)
              OLDEROD=SERATOLD1*DELTREM
              GADLN = (GBO(N,L)-GAW)*GAC/(GAC-GAW)
              DTHKOLD = OLDEROD/GADLN
              BALOLD  = THICKO(N,L) - DTHKOLD
C-    ONLY THIS LAYER ERODED
              IF (BALOLD .GT. 0.0) THEN
                BEDEL(N) = BEDEL(N) - DTHKOLD
                THICKO(N,L) = THICKO(N,L) - DTHKOLD
                SERATNEWL=SERATOLD+SERATOLD1*DELTREM/DELT+SERATNEWL
                GO TO 1
              ELSE
C-    MORE THAN THIS LAYER MAY BE ERODED.

                BEDEL(N) = BEDEL(N) - THICKO(N,L)
                AMASER=THICKO(N,L)*GADLN
                DELTREM=DELTREM*(1.-THICKO(N,L)/DTHKOLD)
                THICKO(N,L) = 0.0
                SERATOLD=SERATOLD+AMASER/DELT
              ENDIF
            ENDIF

   30    CONTINUE            
         SERATNEWL=SERATNEWL+SERATOLD
C
C......
C......................................................
C
    1   CONTINUE

        SERAT(NN) = SERATNEWL*1000.
        serat(n)=serat(nn)
       ENDIF
  500 CONTINUE
  
  
      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          SERAT(NN)=(SERAT(IMID(NN,1))+SERAT(IMID(NN,2)))/2.
          BEDEL(NN)=(BEDEL(IMID(NN,1))+BEDEL(IMID(NN,2)))/2.
          NLAYT=NLAYTND(NN)
          THICK(NN,NLAYT)=(THICK(IMID(NN,1),NLAYT)
     +                    +THICK(IMID(NN,2),NLAYT))/2.
          NLAYOT=MAX(NLAYO(IMID(NN,1)),NLAYO(IMID(NN,2)))
          IF(NLAYOT .GT. 0) THEN
            DO L=1,NLAYO(NN)
              THICKO(NN,L)=(THICKO(IMID(NN,1),L)
     +                     +THICKO(IMID(NN,2),L))/2.
            ENDDO
          ENDIF
        ELSE
c          WRITE(239,*) N,SERAT(N)
        ENDIF 
      ENDDO

cipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
cipk dec04
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
              serat(nod)=(serat(nop(n,j1))+serat(nop(n,j2)))/2.
              bedel(nod)=(bedel(nop(n,j1))+bedel(nop(n,j2)))/2.
              if(k1 .ne. nod) then
                serat(k1)=serat(nod)
              endif
              if(nlayt .gt. 0) then
               do k=1,nlayt
                 thick(nod,k)=(thick(nop(n,j1),k)+thick(nop(n,j2),k))/2.
               enddo
              endif
              if(nlayo(nod) .gt. 0) then
              do k=1,nlayo(nod)
              thicko(nod,k)=(thicko(nop(n,j1),k)+thicko(nop(n,j2),k))/2.
              enddo
              endif
            enddo
          endif
        endif
      enddo 

C      DO N=1,NPM
      
C        WRITE(233,'(I5,3G15.5)') N,SERAT(N),THICK(N,1),THICKO(N,1)
C      ENDDO
      RETURN
      END
