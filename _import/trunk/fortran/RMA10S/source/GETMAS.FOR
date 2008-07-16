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
C-
C-.....ASSIGN PROPER COEFS.....
C-

C
      DO 75 NN=1,NPM
         N = NN
         IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

CIPKSEP05
 
         NLAYT=NLAYTND(N)
         DO J=1,NLAYT
           GAD(J)=GADND(N,J)
         ENDDO
         GAC=GACND(N)
         GAB=GABND(N)
         GAW=GAWND(N)
C Calculate total dry mass of new bed
         TVN = 0.
         DO L=1,NLAY(NN)
            TVN = TVN + THICK(NN,L)*GAD(L)
         ENDDO
cipk jan05
         IF(NLAYO(NN) .GT. 0) THEN
           DO L=1,NLAYO(NN)
             GADLN = (GBO(NN,L)-GAW)*GAC/(GAC-GAW)
             TVN = TVN + THICKO(NN,L)*GADLN
           ENDDO
	   ENDIF
cipk may06 get nodal masses for susp sed and bed 	   
	   TMSED(NN)=TVN+(EDOT(nn)+SERAT(nn))*delt/1000.*(ALPHA-1.)/ALPHA
	   TMSSED(NN)=VEL(3,NN)*VEL(6,NN)/1000.

   75 CONTINUE
cipk may06 put total mass in subscript '0'   
      TMSED(0)=0
      TMSSED(0)=0
      DO N=1,NE
        NCN=NCORN(N)
        IF(NCN .EQ. 6) THEN
        TMSED(0)=TMSED(0)+
     +  (TMSED(NOP(N,1))+TMSED(NOP(N,3))+TMSED(NOP(N,5)))*
     +           (AREA(N)/3.)        
        TMSSED(0)=TMSSED(0)+
     +  (TMSSED(NOP(N,1))+TMSSED(NOP(N,3))+TMSSED(NOP(N,5)))*
     +           (AREA(N)/3.)        
        ELSEIF(NCN .EQ. 8) THEN
        TMSED(0)=TMSED(0)+
     +  (TMSED(NOP(N,1))+TMSED(NOP(N,3))+TMSED(NOP(N,5))+
     +            TMSED(NOP(N,7)))*(AREA(N)/4.)  
        TMSSED(0)=TMSSED(0)+
     +  (TMSSED(NOP(N,1))+TMSSED(NOP(N,3))+TMSSED(NOP(N,5))+
     +            TMSSED(NOP(N,7)))*(AREA(N)/4.)  
        ENDIF      
      ENDDO
      
      IF (MOD(IT,NPRTF) .NE. 0  .AND. TET .NE. 0.) go to 76

      WRITE(LOUT,9999) TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0)
   76 continue
      if(IMASSOUT .gt. 0) then      
        WRITE(24,9999) TET,TMSED(0),TMSSED(0),TMSED(0)+TMSSED(0)
      endif
 9999 FORMAT('      TIME    MASS IN SEDIMENT           SUSPENDED',
     + '               TOTAL'/F10.3,1PE20.8,1PE20.8,1PE20.8) 
      

      RETURN
      END
