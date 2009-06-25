C     Last change:  MD   11 Aug 2008    2:58 pm
CIPK  LAST UPDATE MAR 5 2006 RESET TO ALLOW NODAL VALUES OF PROPS
CIPK  NEW ROUTINE BASED ON BEDSUR MAY 30 2002
       SUBROUTINE BEDLBED
C
C***********************************************************************
C
C     ROUTINE COMPUTES EROSION AND DEPOSITION BED CHANGES FOR SAND BEDS
C     DUE TO BED LOAD

C
C***********************************************************************
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
CIPK AUG05	INCLUDE 'BLKDR.COM'
	REAL*8 DYBED

!NiS,jul06: Declaring HTP with consistent data type
      REAL(KIND=8) :: HTP
!-

C
C      WRITE (LOUT,1040)
C 1040 FORMAT(' ****SUBROUTINE BEDSUR...')
      IF(MOD(IT,NPRTF) .EQ. 0) THEN
       ! WRITE (LOUT, 6000)
        WRITE (LOUT, 6004) TITLE
       ! WRITE (LOUT, 6003) DAYOFY,TET
 6000 FORMAT ( '1'  / 10X,
     +  'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRAM RMA-11'/ 10X,
     +  ' SAND TRANSPORT '/, 10X,'VERSION 3.5d MAy 2006 '///)
 6003 FORMAT ( / 5X,'...RESULTS AT DAY', I7,'  HOUR', F9.2,'...')
 6004 FORMAT(5X,A80)
        WRITE(LOUT,1060)
 1060   FORMAT('    N        GAN     ',
     * 'SET MASS      DELBED     ELEVB       THICK      USTAR')
      ENDIF
c 1080 CTETA = 1.0 - ALPHA
cipk mar95 1080 CTETA = 1.0 - ALPHA
 1080 CTETA = (alpha-1.0)/alpha
      CONP=1.-POSA
CIPK MAR06 MOVED      RHOBS=1000.*CONP*SGSA
*
      BEDMSG = 0
*
      DO 3000 NN=1,NPM
       IF(IBNA(NN) .GT. 0  .AND.  WSLL(NN) .GT. AO(NN)) THEN

        N=NN
        IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

        RHOBS=1000.*CONP*SGSAND(N)


C        DYBED=DYBE(N)
cipk jan98 test for missing node number
        if(VEL(3,N) .eq. 0.) go to 3000

cipk apr99
        if(wsll(n) .lt. ao(n)) then
          edot(n)=0.
          go to 3000
        endif
        
C
C***********************************************************************
C
C     SAND BED COMPUTATIONS
C
C***********************************************************************
C
C        VBS=DELT*((GAN0(N)+GAN(N)*(ALPHASN-1.))/(ALPHASN))
        VBS=DELT*(GAN0(N)+GAN(N))/2.0
C     +      +DGN(N)*DELT*(ALPHASN-1.)/(2.*(ALPHASN+1.)))
C        srate(n)=(CBS/ALPHA+EDOT(N)*CTETA)*D/RHOBS
C        NOTE.  THE SOURCE TERM, ALPHA2+ALPHA1*CONC, IS POSITIVE FOR
C               EROSION SINCE SEDIMENT IS BEING ADDED TO THE FLOW FIELD.
C               A SIGN CHANGE IS REQUIRED WHEN PASSING TO THE BED
C               CONTROL VOLUME SO EROSION BECOMES (-).
        DYBED=-VBS/(RHOBS*1000.)
 2191   CONTINUE
!
!  DJW dec 2004 - Enables use of factor entered on 'SMO' line
!
        DELBED(N)=DELBED(N)+DYBED*FACTMORPH
        ELEVB(N)=ELEVB(N)+DYBED*FACTMORPH
        TTHICK(N)=TTHICK(N)+DYBED*FACTMORPH
!        DELBED(N)=DELBED(N)+DYBED
!        ELEVB(N)=ELEVB(N)+DYBED
!        TTHICK(N)=TTHICK(N)+DYBED
!
!  END djw 2004
!

C
CIPK APR 2000  COPY TO TOP LAYER
C
        IF(N .NE. NN) THEN
          DELBED(NN)=DELBED(N)
        ENDIF

        IF(MOD(IT,NPRTF) .EQ. 0) THEN
          WRITE(LOUT,1130) N,GAN(N),VBS,DELBED(N),ELEVB(N),
     +      TTHICK(N),UST(N)
        ENDIF
 1120   CONTINUE
        DGN(N)=(GAN(N)-GAN0(N))*ALPHASN/DELT+(1.-ALPHASN)*DGN(N)
	  GAN0(N)=GAN(N)
 1130   FORMAT(I5,6(1X,G11.5))
       ENDIF
 3000 CONTINUE


c     set distribution linear
      ncn=0
      do n=1,ne
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              delbed(nop(n,j))=(delbed(nop(n,j1))+delbed(nop(n,j2)))/2.
              elevb(nop(n,j))=(elevb(nop(n,j1))+elevb(nop(n,j2)))/2.
              tthick(nop(n,j))=(tthick(nop(n,j1))+tthick(nop(n,j2)))/2.
              dgn(nop(n,j))=(dgn(nop(n,j1))+dgn(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 

CIPK MAY02 UPDATE BED ELEVATION
	DO N=1,NP
	  DEP=VEL(3,N)
	  DIFF=ELEVB(N)-AO(N)
	  AO(N)=ELEVB(N)
          ADO(N)=ADO(N)+DIFF
          HEL(N)=WSLL(N)-ADO(N)
          CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)

          VEL(3,N)=HTP
	  RATIO=VEL(3,N)/DEP
	  VEL(1,N)=VEL(1,N)/RATIO
	  VEL(2,N)=VEL(2,N)/RATIO
	  VDOT(1,N)=VDOT(1,N)/RATIO
	  VDOT(2,N)=VDOT(2,N)/RATIO
	ENDDO

      RETURN
      END
