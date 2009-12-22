CIPK  NEW ROUTINE SEP27 2002
      SUBROUTINE ICETHICK(DELTH)
      USE BLK10MOD
      USE BLK11MOD

      SAVE

      REAL NEWTHK
      ALLOCATABLE IBNTHER(:)
      DATA ITIMEH/0/
      
      IF(ITIMEH == 0) THEN
        ITIMEH=1
        ALLOCATE (IBNTHER(MAXP))
        IBNTHER=0
      ENDIF
      
      IF(ICESW == 0) THEN
        DO N=1,NPM
          ICETHK(N)=0.0
        ENDDO
      ELSE
        DO N=1,NPM
          IBNTHER(N)=0
        ENDDO
C
C     Loop on the surface nodes, using the elements
C       
        DO J=1,NEM
          if(imat(j) > 0) then
            nmt=mod(imat(j),100)
          endif
          DO MM=1,7,2
            IF(NOPS(J,MM) == 0) GO TO 250
            NCN=MM+1
            N=NOPS(J,MM)
            IF(IBNTHER(N) == 1) GO TO 250
            IBNTHER(N)=1
C
c     First get a simplified average velocity and water temperature
C       
            VMN=SQRT(VEL(1,N)**2+VEL(2,N)**2)
            TMN=VEL(5,N)
            K=NREF(N)+1
            IF(K > 1) THEN
              L=NREF(N)+NDEP(N)-1
              IF(K <= L) THEN
                DO M=K,L-1,2
                  IF(M == K) THEN
                      DEP=CORD(N,3)-CORD(M+1,3)
                    TMN=DEP*((VEL(5,N)+VEL(5,M+1))/6.+0.6666*VEL(5,M))
                    V2=SQRT(VEL(1,M)**2+VEL(2,M)**2)
                    V3=SQRT(VEL(1,M+1)**2+VEL(2,M+1)**2)
                    VMN=DEP*((VMN+V3)/6.+V2*0.6666)
                  ELSE
                    DEP=CORD(M-1,3)-CORD(M+1,3)
                    TMN=TMN+DEP*(VEL(5,M)*0.66666
     +                +(VEL(5,M+1)+VEL(5,M-1))/6.)
                    V1=SQRT(VEL(1,M-1)**2+VEL(2,M-1)**2)
                    V2=SQRT(VEL(1,M)**2+VEL(2,M)**2)
                    V3=SQRT(VEL(1,M+1)**2+VEL(2,M+1)**2)
                    VMN=VMN+DEP*((V1+V3)/6.+V2*0.6666)
                        ENDIF     
                ENDDO
                FDIV=CORD(N,3)-CORD(L,3)
              ELSE
                FDIV=1.0
              ENDIF
            ELSE
              FDIV=1.
            ENDIF
            VMN=VMN/FDIV
            TMN=TMN/FDIV
c
c         test for appropiate equation and solve for QW
c
C            write(172,'(i5,f8.2,2i5,4f15.6)') 
C     +          dayofy,tet,maxn,n,icethk(n),vel(5,n),tmed,tmn
            if((ICETHK(N) > 0. .OR. vel(5,n) <= tmed)
     +         .AND. ndep(n) > 1 .AND. icesw == 2) then
                nmid=nref(n)+1
               IF( VEL(5,N) > TMED) THEN
                tslop=(-3.*VEL(5,N)+4.*vel(5,nmid)-vel(5,nmid+1))
     +          /(cord(n,3)-cord(nmid+1,3))
c                tslop=(-VEL(5,N)+vel(5,nmid+1))
c     +          /(cord(n,3)-cord(nmid+1,3))
c                nbot=ndep(n)+nref(n)-1
c                tslop=(-VEL(5,N)+vel(5,nbot))
c     +          /(cord(n,3)-cord(nbot,3))
              ELSE
                tslop=(-3.*tmed+4.*vel(5,nmid)-vel(5,nmid+1))
     +          /(cord(n,3)-cord(nmid+1,3))
c                tslop=(-tmed+vel(5,nmid+1))
c     +          /(cord(n,3)-cord(nmid+1,3))
c                nbot=ndep(n)+nref(n)-1
c                tslop=(-tmed+vel(5,nbot))
c     +          /(cord(n,3)-cord(nbot,3))
              ENDIF
              qw=ort(nmt,10)*vel(3,n)*4.1865*ROW*tslop
              qice(n)=-qw/1000.
            else
              IF(VMN < VTR) THEN
                QW=CAL1*(TMN-TMED)/VEL(3,N)**0.2
                    QICE(N)=-QW/1000.
              ELSE
                QW=CAL2*(TMN-TMED)/VEL(3,N)**0.2*VMN**0.8
                QICE(N)=-QW/1000.
C
C           Note that qice is in units of KJ/m2/sec
C
              ENDIF
            endif
C
c         thermal conductivity of ice
c
            IF(TICE <= 0.) THEN
              TTICE=TICE
            ELSE
              TTICE=0.5*(AIRTMP(N)+TMED)
            ENDIF
            XKI=2.21-0.11*TTICE
C
C         thermal conductivity of snow
C
            XKS=2.84E-6*ROSN**2
C
C         form dhdt
C
            IF(HTR == 0.) THEN
              CDHDT=CAL3*(TMED-AIRTMP(N))/
     +          (ICETHK(N)/XKI + CAL4*SNOWD(N)/XKS) - QW
            ELSE
              CDHDT=CAL3*(TMED-AIRTMP(N))/
     +        (ICETHK(N)/XKI + CAL4*SNOWD(N)/XKS + 1./HTR) - QW
            ENDIF
            DHDT=CDHDT/(ROW*XLAT*1000.)
            NEWTHK=ICETHK(N)+DHDT*DELTH*3600.

C            write(174,'(i5,f8.2,2i5,4f15.8)') 
C     +          dayofy,tet,maxn,n,dhdt*1000000.,newthk,cdhdt,tslop

            IF(HTR == 0.) THEN
              CDHDT0=CAL3*(TMED-AIRTMP(N))/
     +        ((ICETHK(N)+NEWTHK)/2.*XKI + CAL4*SNOWD(N)/XKS) - QW
            ELSE
              CDHDT0=CAL3*(TMED-AIRTMP(N))/
     +        ((ICETHK(N)+NEWTHK)/2.*XKI + CAL4*SNOWD(N)/XKS + 1./HTR)
     +        - QW
            ENDIF
            DHDT=CDHDT0/(ROW*XLAT*1000.)
            NEWTHK=ICETHKOL(N)+DHDT*DELTH*3600.

C            write(174,'(i5,f8.2,2i5,3f15.6)') 
C     +          dayofy,tet,maxn,n,dhdt*1000000.,newthk,cdhdt0

            IF(NEWTHK > 0.) THEN
              ICETHK(N)=NEWTHK
            ELSE
              ICETHK(N)=0.
              qice(n)=0.
            ENDIF
  250       CONTINUE
          ENDDO
          if(ncn == 4) ncn=2
          DO i=2,ncn,2
            IF(I < NCN .OR. ncn == 2) THEN
              ICETHK(NOPS(J,I))=
     +         (ICETHK(NOPS(J,I-1))+ICETHK(NOPS(J,I+1)))/2.
            ELSE
              ICETHK(NOPS(J,I))=
     +         (ICETHK(NOPS(J,I-1))+ICETHK(NOPS(J,1)))/2.
            ENDIF
          ENDDO

cc      n1=nops(j,1)
cc      n2=nops(j,2)
cc      n3=nops(j,3)
cc      WRITE(175,'(''ICETHK'',i5,f7.1,6i5)') 
cc     +   DAYOFY,TET,maxn,j,n1,n2,n3,itpas
cc      WRITE(175,'(3f15.8,f8.3,3f15.8,3f8.3)') 
cc     +ICETHK(n1),ICETHK(n2),icethk(n3),TMN,Qice(n1),Qice(n2),Qice(n3)
cc     + ,vel(5,n1),vel(5,n2),vel(5,n3)
        ENDDO
      ENDIF
      RETURN
      END
