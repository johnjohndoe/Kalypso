C     Last change:  AF   14 Jul 2006   11:03 am
CIPK  LAST UPDATE APR 20 2006 ADD INPUT OPTION FOR TLAYM
CIPK  LAST UPDATE NOV 19 1997
crrr  last update Aug 30 1997 test for max number of layers
cipk  last update Aug 6 1997 change format
CIPK  LAST UPDATE JUNE 2 1997
CIPK  LAST UPDATE APR 7 1997 FIX LABEL AND FORMAT 
cipk  last update June 4 1996
      SUBROUTINE SPROP
C-
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKFTMOD
c      USE PARAMMOD

cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05	INCLUDE 'BLKSED.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'
CIPK JUN97
cipk nov97 add alternate reading logic
      CHARACTER*8  IDA
      CHARACTER*72 DLINA
      DATA NLMTP/100/
C      
C-
C******************************************************************
C
C         THIS SUBROUTINE READS THE PROPERTIES OF COHESIVE SEDIMENTS THAT
C      EXIST IN THE SYSTEM AND TRANSFERS.
C      THE INITIAL BED PROFILE AND BED PROPERTIES ARE READ IN BY
C      SUBROUTINE BED.
C
C******************************************************************
C
cipk  set IBNK =0 for the moment until bank shear is implemented
      CALL GINPT(LIN,ID,DLIN)

	IF(ID(1:4) .EQ. 'CSS1') THEN
        READ(DLIN,'(I8,5F8.5)') ISVL,VSST,CRCON1,VSS1,CRCON2,EXP2
	ELSE
        WRITE(75,*) 'EXPECTED CSS1 DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED CSS1 DATA - NONE FOUND'
        STOP
	ENDIF
C      WRITE(LOUT,6032)
      IF(ISVL .EQ. 2) THEN
        VSK=VSS1/CRCON1**EXP2
        VSS2=VSK*CRCON2**EXP2
C
        WRITE(LOUT,6033)
        WRITE(LOUT,6034) CRCON1,VSS1
        WRITE(LOUT,6035) CRCON1,CRCON2,VSK,EXP2
        WRITE(LOUT,6036) CRCON2,VSS2
C
      ELSE

        WRITE(LOUT,6037) VSST*1000.

      ENDIF

      CALL GINPT(LIN,ID,DLIN)
	IF(ID(1:4) .EQ. 'CSS2') THEN
        READ (DLIN,'(3F8.0)') VK,RKS,D90
	ELSE
        WRITE(75,*) 'EXPECTED CSS2 DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED CSS2 DATA - NONE FOUND'
        STOP
	ENDIF
      IF (VK .EQ. 0)  VK = 0.4
      WRITE (LOUT,6038) VK, RKS,D90

C..................................................................
C...      NEW BED PROPERTIES. SAME DRY MASS IN EACH LAYER.
C.................................................................
      CALL GINPT(LIN,ID,DLIN)
      NN=0
  250 CONTINUE

CIPK SEP05
      NN=NN+1	      
      N1=1
  275 CONTINUE      
      IF(ID(1:4) .EQ. 'CSMT') THEN
        N9=N1+8
        READ(DLIN,'(9I8)') (IMTREF(I),I=N1,N9)
        N1=N1+9
        CALL GINPT(LIN,ID,DLIN)
        GO TO 275
	ELSEIF(ID(1:4) .EQ. 'CSS3') THEN
        READ(DLIN,5001)NLAYTM,TAUCDM,GAWM,GABM
     +     ,TTLAYM ,GACM ,ERCM ,UNM 
	ELSEIF(ID(1:6) .EQ. 'ENDSED'  .OR. ID(1:3) .EQ. 'CB ') THEN

	  NSEDTYP=NN
	  GO TO 300
      ELSE	  
        WRITE(75,*) 'EXPECTED MORE SED DATA - NONE FOUND'
        WRITE(*,*) 'EXPECTED MORE DATA - NONE FOUND'
        STOP
	ENDIF
crrr aug97
      if (NLAYTM  .GT. MXSEDLAY ) THEN
         WRITE(LOUT,'(/A,I3)')  
     &      ' Too many cohesive sediment layers, NLAYT =', NLAYTM 
         WRITE(LOUT,'(/A)')  
     &      ' Increase dimension MLAYER '
         STOP ' Too many cohesive sediment layers'
      ENDIF
   
      DO K=1,NLMTP
        NTYPS=K-1
        IF(IMTREF(K) .EQ. 0) GO TO 280
      ENDDO
      
  280 CONTINUE      
      IF(NTYPS .GT. 0) THEN
        WRITE(LOUT,6010) (IMTREF(K),K=1,NTYPS)
      ENDIF
      
      WRITE(LOUT,6001)TAUCDM 

      WRITE(LOUT,6002)

      WEGTM =TTLAYM *(GABM -GAWM )*GACM /
     +             (GACM -GAWM )
      WRITE(LOUT,6020)NLAYTM ,GAWM ,UNM ,GABM ,TTLAYM 
     +,GACM ,WEGTM ,ERCM 
      CALL GINPT(LIN,ID,DLIN)
      
      WRITE(LOUT,6021)
      DO L=1,NLAYTM 
	  IF(ID(1:4) .EQ. 'CSS4') THEN
CIPK APR06 ADD INPUT OPTION FOR TLAYM
CIPK MAY06   ADD EROSTM
          READ(DLIN,5002) I,SSM(I),GBM(I),TLAYM(I),EROSTM(I)

          CALL GINPT(LIN,ID,DLIN)
 	  ELSE
          WRITE(75,*) 'EXPECTED CSS4 DATA - NONE FOUND'
          WRITE(*,*) 'EXPECTED CSS4 DATA - NONE FOUND'
          STOP
	  ENDIF
      ENDDO
      DO I=1,NLAYTM 
        GADM(I)=(GBM(I)-GAWM )*GACM /(GACM -GAWM )
CIPK APR06 ADD INPUT OPTION FOR TLAYM
        IF(TLAYM(I) .EQ. 0.) THEN
          TLAYM(I)=WEGTM /GADM(I)
        ENDIF
CIPK MAY06   ADD EROSTM        
        WRITE(LOUT,6025)I,SSM(I),GBM(I),TLAYM(I),GADM(I),EROSTM(I)
      ENDDO

      DO N=1,NE
        IF(IMAT(N) .GT. 0) THEN
          DO I=1,NLMTP
            IF(IMTREF(I) .EQ. 0  .AND.  I .GT. 1) GO TO 350
            IF(IMTREF(I) .EQ. IMAT(N)  .OR.  IMTREF(I) .EQ. 0) THEN
              DO JJ=1,NCORN(N)
                J=NOP(N,JJ)
                NLAYTND(J)=NLAYTM
                TAUCDND(J)=TAUCDM
                GAWND(J)=GAWM
                UNND(J)=UNM
                GABND(J)=GABM
                TTLAYND(J)=TTLAYM
                GACND(J)=GACM
                WEGTND(J)=WEGTM
                ERCND(J)=ERCM
                DO K=1,NLAYTM
                  SSND(J,K)=SSM(K)
                  GBND(J,K)=GBM(K)
                  TLAYND(J,K)=TLAYM(K)
                  GADND(J,K)=GADM(K)
CIPK MAY06   ADD EROSTM
                  EROST(J,K)=EROSTM(K)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
  350     CONTINUE          
        ENDIF
      ENDDO


      GO TO 250
      
  300 CONTINUE      

      CALL BED

cipk June 1996      IF(IBNK .NE. 0) THEN
cipk June 1996        READ(IIQ,5010) BNKCD,BNKGAB,BNKERC,BRKS
cipk June 1996        WRITE(LOUT,6030) BNKCD,BNKGAB,BNKERC,BRKS
cipk June 1996      ENDIF

C      CALL GINPT(LIN,ID,DLIN)
      IF(ID(1:6) .NE. 'ENDSED') THEN
	  WRITE(75,*) 'NO ENDSED DATA LINE FOUND'
	  WRITE(*,*) 'NO ENDSED DATA LINE FOUND'
	  STOP
      ENDIF
C
      RETURN

 5001 FORMAT(I8,7F8.5)
 5002 FORMAT(I8,8F8.0)
 5010 FORMAT(8X,4F8.0)
 6010 FORMAT(/10X,'PROPERTIES FOR ELEMENT TYPES',/(I16,9I8))
 6001 FORMAT(/10X,'CRIT. SHEAR FOR DEPOSITION =',T50,F10.3,
     *' (N/SQ.M =.1*DYNES/SQ.CM)')
 6002 FORMAT(/10X,'LAYERED BED PROPERTIES...')
 6020 FORMAT(/14X,'NUMBER OF LAYERS               =',I3/
     '14X,'DENSITY OF SUSPENDING WATER    =',F7.1,' KG/CU.M'/
     '14X,'KIN. VISCOSITY OF SUSP. WATER  =',E8.2,' SQ.M/SEC'/
     '14X,'BULK DENSITY OF TOP LAYER      =',F7.1,' KG/CU.M'/
     '14X,'THICKNESS OF TOP LAYER         =',F7.3,' METERS'/
     '14X,'DENSITY OF SEDIMENT MINERAL    =',F7.1,' KG/CU. M'/
     '14X,'DRY WT. OF SED. IN TOP LAYER   =',F7.1,' KG/SQ.M'/
     '14X,'ERO. RATE CONS. FOR BOT. LAYER =',F7.5,'KG./SQ.M/SEC')
 6021 FORMAT(/10X,'LAYER',10X,'CRIT. SHEAR',10X,'BULK DENSITY',
     '10X,'THICKNESS',10X,'DRY DENSITY'/
     '26X,'NEWT./SQ.M',13X,'KG./CU.M',13X,'METERS',13X,'KG./CU.M'/)
cipk aug97 change format 6025 FORMAT(I13,F19.2,F24.2,F20.3,F20.2)
 6025 FORMAT(I13,F19.2,F24.2,F20.5,F20.2,F15.5)
 6030 FORMAT(5X,'BANK CRITICAL SHEAR STRESS',1PE15.4/
     +          'BULK DENSISTY OF BANK MAT.',1PE15.4/
     +          'BANK EROSION RATE CONSTANT',1PE15.4/
     +          'BANK ROUGHNESS HEIGHT     ',1PE15.4)
C 6032 FORMAT(1H1,30X,'SEDIMENT PROPERTIES'/)
 6033 FORMAT(//10X,'SETTLING VELOCITIES...'///20X,
CIPK APR 97 FIX LABEL AND FORMAT
     * 'CONC. RANGE(GM/CU.M)',15X,'SET. VEL(M/SEC)')
 6034 FORMAT(20X,' 0.',4X,'TO',F10.2,16X,1PE12.3)
 6035 FORMAT(13X,F10.3,4X,'TO',F10.2,13X,'VS=',1PE12.5,'*CONC**',F6.4)
 6036 FORMAT(23X,'ABOVE',F10.2,21X,1PE12.3)
 6037 FORMAT(10X,'SETTLING VELOCITY',T50,F8.4,'  (MM/SEC)')
 6038 FORMAT(//,
     *14X,'VON KARMANNS CONSTANT          =',F7.2,' CONSTANT'/
     *14X,'BED ROUGHNESS HEIGHT           =',F7.4,' METERS  '/
     *14X,'D90                            =',F7.4,' METERS  '/)
CIPK JUN97 ADD FORMATS
 6039 FORMAT('  WIND/WAVE DATA'//
     +    '  MAT TYP   WIND SPEED     FETCH    MANN')
 6040 FORMAT(I9,F13.2,F10.1,F8.3)
      END
C_______________________________________________________________________
