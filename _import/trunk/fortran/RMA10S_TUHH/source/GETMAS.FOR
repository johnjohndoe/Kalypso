C     Last change:  MD   30 Jul 2009    1:59 pm
CIPK LAST UPDATE SEP 05 2006 REVISE MASS COMPUTATION
CIPK LAST UPDATE MAY 30 2006 ADD GAD DEF. AND INITIAL MASS 
      SUBROUTINE GETMAS
C-
C       THIS ROUTINE FORMS THE NEW BED THAT IS A RESULT OF DEPOSITION
C     IN THE LAST TIME INTERVAL
C-
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
      
      !local nprtf
      integer (kind = 4) :: iprtf
C-
C-.....ASSIGN PROPER COEFS.....
C-

      !init iprtf
      iprtf = 1

!MD: Basis-Variable: Wasserdichte
      !MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
      !     Je nach Vorgabe der SI-Einheiten unter "IGRV"
      ROAVG=1.935
      IF (GRAV.LT.32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte

C
      DO 75 NN=1,NPM
         N = NN
         IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1
         NLAYT=NLAYTND(N)
         DO J=1,NLAYT
           !MD: NEUE Berechnung der Trocken-Rohdichte JE Sus.[kg/m3]
           !MD: use bulk densitiy per Layer
           !MD: GADND(N,J)=(GABND(N)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
           GADND(N,J)=(GBND(N,J)-ROAVG)*GACND(N) /(GACND(N)-ROAVG)
           GAD(J)=GADND(N,J)
           !MD: NEUE Berechnung der Trocken-Rohdichte Sus.[kg/m3]
         ENDDO
         GAC=GACND(N)
         !MD: GAB=GABND(N) use per Layer
         !MD NEU Berechnung der Suspensionsdichte
         IF (VEL(6,N).ge.0.) THEN
           GAWND(N)=(VEL(6,N)/1000.)*(1.-ROAVG/GACND(N)) + ROAVG
         Else
           GAWND(N)= ROAVG
         END IF
         GAW=GAWND(N)
         !MD NEU Berechnung der Suspensionsdichte

C Calculate total dry mass of new bed
         TVN = 0.
         DO L=1,NLAY(NN)
            TVN = TVN + THICK(NN,L)*GAD(L)
         ENDDO
cipk jan05
         IF(NLAYO(NN) .GT. 0) THEN
           DO L=1,NLAYO(NN)
             !MD: GADLN = (GBO(NN,L)-GAW)*GAC/(GAC-GAW)
             GADLN=(GBO(NN,L)-ROAVG)*GAC/(GAC-ROAVG)
             !MD:  NEUE Berechnung der Trocken-Rohdichte Bed [kg/m3]
             TVN = TVN + THICKO(NN,L)*GADLN
           ENDDO
	   ENDIF
cipk may06 get nodal masses for susp sed and bed 
CIPK SEP06 UPDATE COMPUTATION	   
	   TMSED(NN)=TVN
C	   +(EDOT(nn)+SERAT(nn))*delt/1000.*(ALPHA-1.)/ALPHA
	   TMSSED(NN)=VEL(3,NN)*VEL(6,NN)/1000.

   75 CONTINUE
cipk may06 put total mass in subscript '0'   
      TMSED(0)=0
      TMSSED(0)=0
      DO N=1,NE
        NCN=NCORN(N)
        IF(NCN .EQ. 6) THEN
CIPK AUG06 UPDATE TO ADD CORNERS
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


        ENDIF      
      ENDDO
      
      
      if (nprtf == 0) then
        iprtf =1
      else
        iprtf = nprtf
      endif 

      IF (MOD(IT,iPRTF) .NE. 0  .AND. TET .NE. 0.) go to 76

      WRITE(LOUT,9999) TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0)
   76 continue
      if(IMASSOUT .gt. 0) then      
        WRITE(24,9999) TET,TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0)
      endif
 9999 FORMAT('      TIME    MASS IN SEDIMENT           SUSPENDED',
     + '               TOTAL'/F10.3,1PE20.10,1PE20.10,1PE20.10) 
      

      RETURN
      END
