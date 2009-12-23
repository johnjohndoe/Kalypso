C     Last change:  IPK   5 Oct 98    3:43 pm

      SUBROUTINE MELLII
      USE IHILMOD
      USE BLK10MOD
      USE BLK11MOD
C 
c      This routine has been modified by WLP for use on the EMP.
c      I was unable to understand the methodology behind the original
c      version of mellii.  The vertical eddy viscosity for unstratified
c      conditions is assumed to be parabolic and the co-efficients of 
c      Henderson-Sellers have been imposed.
C 
      real*8 GRI,DENGR,DV2,DUDZ,DVDZ,RHOC,DZPDZ,ZPUP,ZPDN,UUP,VUP,
     &       VDN,UDN,GRI0

C 
C 
C      LOOP THROUGH ALL THE SURFACE NODES
C 
      DO 110 N = 1, NPM
cipk oct96
        if(nsurf(n) < 0) go to 110
        
C           K IS THE GLOBAL NODE NUM. JUST BELOW SUR, N IS SUR
C           GL. NODE NUM., L IS THE BOTTOM GL. NODE NUM.
        K = NREF(N) + 1
C         IF K=1 THEN THIS IS 2-D DEPTH INTEGRATED SO SKIP
        IF (K == 1) GO TO 110
        L = K + NDEP(N) - 2
CIPK OCT98        L1 = L - 1
c       write(*,*)'n,k,l,l1',n,k,l,l1
C 
C           LOOP THROUGH ALL THE NODES BETWEEN THE SURFACE AND
C           BOTTOM CALCULATING VISCOSITIES
C
        HTEMP=VEL(3,N)
        AOTEMP=AO(N)
        CONVER=(ELEV-AOTEMP)/HTEMP
CIPK OCT98        DEPEL=1.10*HTEMP
CIPK OCT98        DELH=0.05*HTEMP
        NRF=NREF(N)
c        WRITE(*,*) 'AT DO 100',CONVER,AOTEMP,ELEV,HTEMP
        DO 100 J = NRF, L
          JC=J
          IF(J == NRF)THEN
             JUP=N
             JDN=K
             JC=N
          ELSEIF(J == K)THEN
             JUP=N
             JDN=J+1
             IF(K == L)JDN=J
          ELSEIF(J == L)THEN
             JUP=J-1
             IF(K == L)JUP=N
             JDN=L
          ELSE
             JUP=J-1
             JDN=J+1
          ENDIF
          ZPUP=CORD(JUP,3)
          UUP=VEL(1,JUP)
          VUP=VEL(2,JUP)
          RHOUP=DEN(JUP)
          ZPC = CORD(JC,3)
CIPK OCT98          UC = VEL(1,JC)
CIPK OCT98          VC = VEL(2,JC)
          RHOC = DEN(JC)
          ZPDN=CORD(JDN,3)
          UDN=VEL(1,JDN)
          VDN=VEL(2,JDN)
          RHODN=DEN(JDN)



C                CALCULATE RICHARDSON NUMBER RI, AND THE FLUX
C                RICHARDSON NUMBER RF
C                DERIVATIVES ARE CALCULATED HERE AS CENTERED
C                DIFFERENCES
          DZPDZ = ZPUP - ZPDN
          DUDZ = ((UUP-UDN) / DZPDZ) * CONVER
          DVDZ = ((VUP-VDN) / DZPDZ) * CONVER
          DRHODZ = ((RHOUP-RHODN) / DZPDZ) * CONVER
          DV2 = (DUDZ * DUDZ + DVDZ * DVDZ) + 1.E-6
          DENGR =  - GRAV * DRHODZ /( RHOC + 1.E-5)
         GRI = DENGR / DV2
         GRI0 = GRI



C        RICHARDSON NUMBER ADJUSTMENT OF HENDERSON-SELLORS.
C        1982. A SIMPLE FORMULA FOR VERTICAL DIFFUSION COEFFICIENTS UNDER
C        CONDITIONS OF NONNEUTRAL STABILITY. J. GEOPHY. RES 87 (C8) 5860-5864
c         write(76,*) n,gri,dengr,dv2,dudz,drhodz,rhoup,rhodn,conver
          IF (GRI < 0.) GRI = 0.



C        CALCULATE EDDY VISCOSITY AND VERTICAL DIFFUSION
C   **** HENDERSON-SELLERS ADJUSTMENT
       xht=elev-aotemp
         ZIN=(Zpc-Aotemp)/XHT
cipk oct98 update to f90
         IMMT=IMAT(1)
       nr=mod(IMMT,100)
       EPSXZ=ORT(nr,6)*(EDD1(nr)+ZIN*(EDD2(nr)+ZIN*EDD3(nr)))*XHT
       DIFZ=ORT(nr,10)*(EDD1(nr)+ZIN*(EDD2(nr)+ZIN*EDD3(nr)))*XHT
         IF(GRAV < 10.) THEN
           EVISYZ(JC) =  epsxz / (1. + 0.74 * GRI)
         ELSE
           EVISYZ(JC) = 1.94 * epsxz / (1. + 0.74 * GRI)
         ENDIF
         EVISXZ(JC) = EVISYZ(JC)
         if(GRI0 < 0.0) THEN
            DVISZ(JC) = difz * 200.0
         else
            DVISZ(JC) = difz / (1. + 37. * GRI ** 2)
         endif
       if (n == 705) then
           write(*,*) 'jc=',jc,evisxz(jc),DVISZ(JC)
       endif
c      write(*,*) edd1,edd2,edd3,ort(nr,6),nr
c      WRITE(*,*) jc,GRI,epsxz,zin,xht,EVISXZ(jc)
c       if ((jc <= 21) .AND. (jc >= 19)) 
c     + WRITE(*,*) 'evisxz dvisz ',jc,EVISXZ(jc),dvisz(jc),
c     + xht,zin,aotemp,zpc,edd1,edd2,edd3
  100   CONTINUE
  110 CONTINUE
cipk oct96
      do n=1,ne
        if(imat(n) < 900) then
          if(ncorn(n) > 8) then
            ncn=ncorn(n)
            ILK=1
            IF(NCN == 15) ILK=2
            IF(NCN == 10) ILK=3
            IF(NCN == 13) ILK=4
            DO K=1,NCN
              IF(IL(K,ILK) /= 0) THEN
               N1=IL(K,ILK)
               N2=IH(K,ILK)
               DVISZ(NOP(N,K))=(DVISZ(NOP(N,N1))+DVISZ(NOP(N,N2)))/2.
               EVISYZ(NOP(N,K))=(EVISYZ(NOP(N,N1))+EVISYZ(NOP(N,N2)))/2.
               EVISXZ(NOP(N,K)) = EVISYZ(NOP(N,K))
              ENDIF
            ENDDO  
          endif
        elseif(imat(n) > 5000) then
          ncn=ncorn(n)
          DO K=2,NCN,2
            N1=K-1
            N2=K+1
            IF(N2 > NCN) N2=1
            DVISZ(NOP(N,K))=(DVISZ(NOP(N,N1))+DVISZ(NOP(N,N2)))/2.
            EVISYZ(NOP(N,K))=(EVISYZ(NOP(N,N1))+EVISYZ(NOP(N,N2)))/2.
            EVISXZ(NOP(N,K)) = EVISYZ(NOP(N,K))
          ENDDO
        endif
      enddo
      RETURN 
      END
