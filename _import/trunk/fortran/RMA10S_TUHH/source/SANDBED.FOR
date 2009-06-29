C     Last change:  MD    8 Jun 2009    1:14 pm
CIPK LAST UPDATE June 06 2006 add mass output and refine compuatation of mass laid down
CIPK LAST UPDATE MAR 05 2006 UPDATE TO ALLOW FOR NODAL VALUES
CIPK  last update MAY 02 2003 turn off bed activity for selected element types
cipk last update apr 7 2000 correct interpolation function for 3-d
cipk last update Mar 14 2000 extend linear function for 3-D elements
cipk  last update Apr 18 1999 make sand settling linear
cipk  last update Apr 14 1999 add test for bed > wsel
cipk  last update Jan 14 add test for missing node number
CIPK  LAST UPDATE NOV 26 1995 UPDATE VERSION, CHANGE TO ADD DAY FOR OUTPUT
CIPK  LAST UPDATE SEP 11 1995
       SUBROUTINE BEDSUR
C      SUBROUTINE BEDSUR(IVEL)
C
C***********************************************************************
C
C     ROUTINE COMPUTES EROSION AND DEPOSITION BED CHANGES FOR SAND BEDS

C
C***********************************************************************
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
CIPK AUG05	INCLUDE 'BLKDR.COM'
      REAL*8 DYBED
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: HTP
!-
C
C      WRITE (LOUT,1040)
C      1040 FORMAT(' ****SUBROUTINE BEDSUR...')
      IF(MOD(IT,NPRTF) .EQ. 0) THEN
        ! WRITE(LOUT,6000)
        WRITE(LOUT,6004) TITLE
        ! WRITE(LOUT,6003) DAYOFY,TET
 6000   FORMAT( '1'  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...
     +        PROGRAM_RMA-10S'/ 10X, 'SAND TRANSPORT '
     +        /,10X,'VERSION 3.5D MAY 2006 '///)
C 6003  FORMAT( / 5X,'...RESULTS AFTER',F9.2,' HOURS OF DYNAMIC SIMULATION
C       1...' /)
 6003   FORMAT( / 5X,'...RESULTS AT DAY',F7.0,'  HOUR', F9.2,'...')
 6004   FORMAT(5X,A80)
        WRITE(LOUT,1060)
 1060   FORMAT('    N    E/D(OLD)    ',
     * 'E/D(NEW)      DELBED     ELEVB       THICK      USTAR')
      ENDIF

cipk mar95 1080 CTETA = 1.0 - ALPHA
 1080 CTETA = (alpha-1.0)/alpha
      CONP=1.-POSA
CIPK MAR06 MOVED      RHOBS=1000.*CONP*SGSA
      BEDMSG = 0
*
CIPK MAY03 turn off bed activity for selected element types
      DO NN=1,NE
        NCN=NCORN(NN)
        DO M=1,NCN
          MR=NOP(NN,M)
          !MD:  do ned=1,9 : New: more than 9 Mat-Types
          do ned=1,DROPMAX
cipk may03  reduce nodal rates to zero when IEDROP active

            IF(IMAT(NN) .EQ. iedrop(ned)) THEN
  	      alpha1(mr)=0.
	      alpha2(mr)=0.
	    ENDIF
	  enddo
	enddo
      ENDDO


      DO 3000 NN=1,NPM
CIPK MAY03 test for low depth
        if(WSLL(NN) -ao(NN) .lt. zstdep) then
	  alpha1(NN)=0.
	  alpha2(NN)=0
        ENDIF

        !MD Unsinn/ Controllabfrage raus
        !MD IF(NN .EQ. 12386) THEN
        !MD   A65=0
        !MD ENDIF

        if(ibna(nn) .gt. 0) then
          N=NN
          IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1
C         DYBED=DYBE(N)
cipk jan98 test for missing node number
          if(VEL(3,N) .eq. 0.) go to 3000

cipk apr99
          if(wsll(n) .lt. ao(n)) then
            edot(n)=0.
            GAN(N)=edot(n)
            go to 3000
          endif
        
          CBS=(ALPHA2(N)+ALPHA1(N)*VEL(6,N))/(VEL(3,N)*1000.)
          D=VEL(3,N)

!MD 11.08.2008
          !MD Neu fuer Koeffizienten-Matrix ist GAN und GAN0 zu belegen
          !MD Vergleiche SubR BEDLBED: CBS = GAN0 und GAN = EDOT
          GAN0(N)=CBS

      !MD    IF (N.lt.2) THEN
      !MD      !testoutput into output.out
      !MD      WRITE (LOUT,*) 'GAN0(N), VEL(6,n), ALPHA1(N), ALPHA2(n)'
      !MD      WRITE (LOUT,*)  GAN0(N), VEL(6,N), ALPHA1(N), ALPHA2(N)
      !MD    END IF
C
C***********************************************************************
C
C     SAND BED COMPUTATIONS
C
C***********************************************************************
C
          RTO=1.0

cipk mar95 VBS=(CBS*ALPHA+EDOT(N)*CTETA)*DELT
          VBS=(CBS/ALPHA+EDOT(N)*CTETA)*DELT
CIPK MAR06
          RHOBS=1000.*CONP*SGSAND(N)
          srate(n)=(CBS/ALPHA+EDOT(N)*CTETA)*D/RHOBS
C        NOTE.  THE SOURCE TERM, ALPHA2+ALPHA1*CONC, IS POSITIVE FOR
C              EROSION SINCE SEDIMENT IS BEING ADDED TO THE FLOW FIELD.
C              A SIGN CHANGE IS REQUIRED WHEN PASSING TO THE BED
C              CONTROL VOLUME SO EROSION BECOMES (-).
          IF(VBS.LT.0.) RTO=0.
          DYBED=-D*VBS/(RHOBS-RTO*VBS)
          DNEW=D-DYBED
          IF(DNEW.GT.0.) GOTO 2192
          DNEW=0.1*D

          WRITE(LOUT,2193)N,DYBED
 2193   FORMAT(1X,'***WARNING. AT NODE NO. ',I5,' DEPOSITION OF ',G10.4,
     *  ' IS GREATER THAN DEPTH.  TO PREVENT, REDUCE TIME',/1X,
     * 'STEP OR DECREASE DEPOSITION RATE BY REDUCING SETTLING VELOCITY')
 2192     CONTINUE
C
C         IF(IVEL .LE. 0) THEN
C           VELR=D/DNEW
C           XVEL(N,1)=VELR*XVEL(N,1)
C           XVEL(N,2)=VELR*XVEL(N,2)
C         ENDIF
C         WD(N)=DNEW

 2191     CONTINUE
!
!  DJW dec 2004 - Enables use of factor entered on 'SMO' line
!
          DELBED(N)=DELBED(N)+DYBED*FACTMORPH
          ELEVB(N)=ELEVB(N)+DYBED*FACTMORPH
          TTHICK(N)=TTHICK(N)+DYBED*FACTMORPH

!         DELBED(N)=DELBED(N)+DYBED
!         ELEVB(N)=ELEVB(N)+DYBED
C         IF(MTC.NE.0)TTHICK(N)=TTHICK(N)+DYBED
!         TTHICK(N)=TTHICK(N)+DYBED
!
!  End DJW dec 2004
!
C
CIPK APR 2000  COPY TO TOP LAYER
C
          IF(N .NE. NN) THEN
            DELBED(NN)=DELBED(N)
            SRATE(NN)=SRATE(N)
          ENDIF

          IF(MOD(IT,NPRTF) .EQ. 0) THEN
            WRITE(LOUT,1130) N,EDOT(N),CBS,DELBED(N),ELEVB(N),
     +        TTHICK(N),UST(N)
          ENDIF

 1120     CONTINUE
          EDOT(N)=(ALPHA2(N)+ALPHA1(N)*VEL(6,N))/(VEL(3,N)*1000.)
          !MD 11.08.2008
          !MD Neu fuer Koeffizienten-Matrix ist GAN und GAN0 zu belegen
          !MD Vergleiche SubR BEDLBED: CBS = GAN0 und GAN = EDOT
          GAN(N)=EDOT(N)

          IF (N.eq.5) THEN
            !testoutput into output.out
            WRITE (75,*) 'Erosionsrate N=5 GAN =', GAN(N)
          ELSeIF (N.eq.50) THEN
            !testoutput into output.out
            WRITE (75,*) 'Erosionsrate N=50 GAN =', GAN(N)
          END IF

 1130     FORMAT(I5,6(1X,G11.5))
        else
	  edot(nn)=0.
          !MD 11.08.2008
          !MD Neu fuer Koeffizienten-Matrix ist GAN und GAN0 zu belegen
          !MD Vergleiche SubR BEDLBED: CBS = GAN0 und GAN = EDOT
          GAN(NN)=edot(nn)         
	endif

 3000 CONTINUE


cipk april 1999 set distribution linear
cipk Mar 2000 Adjustment to make interpolation work for 3-D elements
cipk apr00 corect for 3-d using top and bottom element only
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
              edot(nop(n,j))=(edot(nop(n,j1))+edot(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 


CIPK MAY02 UPDATE BED ELEVATION
	!
	!  DJW 11/06/04 Include ability to overwrite bed level update (i.e. only provide transport potentials in 
	! results file, removing potential instabilities due to bed update.  Value of 0 means bed will not be updated.
	!  Added conditional if block
	!
!      Updatebed = 0.0
!	If (Updatebed /= 0.0) Then
      
      DO N=1,NP
        DEP=VEL(3,N)
        DIFF=ELEVB(N)-AO(N)
        AO(N)=ELEVB(N)
        ADO(N)=ADO(N)+DIFF
        HEL(N)=WSLL(N)-ADO(N)
        !TODO: Differ between Marsh and not-Marsh as well as dimensions; it shouldn't be applied for 1D Polynomial
        CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)
        VEL(3,N)=HTP
	if ( abs( vel(3,N) ) <= 0.0009 ) cycle 
	RATIO=vel(3,n)/(vel(3,n)+diff)
	VEL(1,N)=VEL(1,N)/RATIO
	VEL(2,N)=VEL(2,N)/RATIO
        VDOT(1,N)=VDOT(1,N)/RATIO
	VDOT(2,N)=VDOT(2,N)/RATIO
cipk jun06 test	  VEL(6,N)=VEL(6,N)/RATIO
cipk jun06 test	  VDOT(6,N)=VDOT(6,N)/RATIO
cipk jun06 get total masses by node for susp sed and bed 	   
        RHOBS=1000.*CONP*SGSAND(N)
	TMSAND(N)=tthick(n)*rhobs
     +   +(alpha2(n))*delt/1000.*(ALPHA-1.)/ALPHA
	TMSSAND(N)=DEP*VEL(6,N)/1000.
	VEL(6,N)=VEL(6,N)/RATIO
	VDOT(6,N)=VDOT(6,N)/RATIO

      ENDDO
      
 !     Else

!	Write (*,*) "No Bed Update"

!	End If
	! End DJW Changes 11/06/04

cipk may06 put total mass in subscript '0'   
      TMSAND(0)=0
      TMSSAND(0)=0
      DO N=1,NE
        NCN=NCORN(N)
        IF(NCN .EQ. 6) THEN
        TMSAND(0)=TMSAND(0)+
     +  (TMSAND(NOP(N,1))+TMSAND(NOP(N,3))+TMSAND(NOP(N,5)))*
     +           (AREA(N)/3.)        
        TMSSAND(0)=TMSSAND(0)+
     +  (TMSSAND(NOP(N,1))+TMSSAND(NOP(N,3))+TMSSAND(NOP(N,5)))*
     +           (AREA(N)/3.)        
        ELSEIF(NCN .EQ. 8) THEN
        TMSAND(0)=TMSAND(0)+
     +  (TMSAND(NOP(N,1))+TMSAND(NOP(N,3))+TMSAND(NOP(N,5))+
     +            TMSAND(NOP(N,7)))*(AREA(N)/4.)  
        TMSSAND(0)=TMSSAND(0)+
     +  (TMSSAND(NOP(N,1))+TMSSAND(NOP(N,3))+TMSSAND(NOP(N,5))+
     +            TMSSAND(NOP(N,7)))*(AREA(N)/4.)  
        ENDIF      
      ENDDO
      
      IF (MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) go to 76

      WRITE(LOUT,9999) TMSAND(0),TMSSAND(0),TMSAND(0)+TMSSAND(0)
   76 continue      
      if(IMASSOUT .gt. 0) then      
        WRITE(24,9999) TET,TMSAND(0),TMSSAND(0),TMSAND(0)+TMSSAND(0)
      endif
 9999 FORMAT(F10.3,1PE20.8,1PE20.8,1PE20.8) 
      

      RETURN
      END
