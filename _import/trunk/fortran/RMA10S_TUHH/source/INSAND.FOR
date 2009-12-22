CIPK  LAST UPDATE MAR 05 2006 USE AND INPUT NODAL PROPS
CIPK  LAST UPDATE DEC 22 2005 MAKE SURE RHOBS IS DEFINED
      SUBROUTINE INSAND
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD
      !*******************  DJW 15/02/05
      USE WBMMODS
      !*******************

C-ipk   This subroutine reads in all rates for sand

CIPK MAR06
      INTEGER ISMTREF(100)
      REAL  SDT(100)
      DATA NLMTP/100/

C      CHARACTER*8 ID
C      CHARACTER*72 DLIN
C       Read Title Line

C
C
C     INITIALIZE
C
      ACGR=9.807
      RHOF=1000.
CIPK MAR06
      POSA=0.4
      CONP=1.-POSA

C     Initialize METRIC to 0 for english units

      METRIC = 0
CIPK MAR06 MOVED      LBASE=0
      CLABL ='   SAND '
CIPK MAR06
      CALL KINVIS
C     Sand is simulated read global parameters

C      IF(LSAND > 0) THEN

      CALL GINPT(LIN,ID,DLIN)

CIPK MAR06 ADD LOOP TO GET ELEMENT TYPE DATA
  250 CONTINUE
      LBASE=0
      N1=1
      N9=9
  275 CONTINUE

      IF(ID(1:4) == 'SSMT') THEN
        READ(DLIN,'(9I8)') (ISMTREF(I),I=N1,N9)
        N1=N1+9
        CALL GINPT(LIN,ID,DLIN)
        GO TO 275
        
C        ENDIF
      ELSEIF(ID(1:3) == 'SAN') THEN

        DO K=1,NLMTP
          NTYPS=K-1
          IF(ISMTREF(K) == 0) GO TO 280
        ENDDO
      
  280   CONTINUE      
        IF(NTYPS > 0) THEN
          WRITE(LOUT,6010) (ISMTREF(K),K=1,NTYPS)
        ELSE 
          WRITE(LOUT,6011) 
        ENDIF

          READ(DLIN,1020) 
CIPK MAR06     +    SACLL,SACUL,ASACI,SGSA,GSF,CLDE,CLER,VSAND,AMANN
     +    SACLL,SACUL,ASACI,SGSAT,GSF,CLDET,CLERT,VSANDT,AMANNT
C        ELSE
C          WRITE(75,*) 'EXPECTED SAND DATA - NONE FOUND'
C          WRITE(*,*) 'EXPECTED SAND DATA - NONE FOUND'
C           STOP
C        ENDIF
        NSACI=ASACI+0.5

CIPK AUG00 PRINT SAND SETTLING LATER

!NiS,Nov06: Compiling leads to the error, that GSFT is never set; should be GSF not GSFT
!CIPK MAR06
!        WRITE(LOUT,2030) SGSAT,GSFT,CLDET,CLERT,AMANNT
        WRITE(LOUT,2030) SGSAT,GSF,CLDET,CLERT,AMANNT
!-
CIPK FEB95        VSAND=VSAND/86400.
CIPK DEC05
CIPK MAR06 MOVED        POSA=0.4
CIPK MAR06 MOVED        CONP=1.-POSA

        CALL GINPT(LIN,ID,DLIN)

        IF(ID(1:3) == 'DIA') THEN
          READ(DLIN,1030) d35T,d50T,d90T,ismodeT,GPMAXT
          if(gpmaxT == 0.) gpmaxT=1.e+6
            CALL GINPT(LIN,ID,DLIN)
        ELSE
          WRITE(75,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
          WRITE(*,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
          STOP
        ENDIF
       ! HN 17July 2009 Reading Geothechnical properties.
        IF(ID(1:3) == 'GTP') THEN                     
          READ(DLIN,1031) critical_slope,crepose,repose,
     +     EFFECTIVE_COHESION,FRICTION_ANGLE, MATRIC_ANGLE, ROOT,refine
          CALL GINPT(LIN,ID,DLIN)
        ENDIF  
        ! HN 23 Jul 2009: Reading polynomial factors , describing pore pressure distribution. 
        IF(ID(1:3) == 'POR') THEN                     
          READ(DLIN,1131) EXPO3,EXPO2,EXPO1
          CALL GINPT(LIN,ID,DLIN)
        ENDIF  
        
CIPK MAR06        RHOBS=1000.*CONP*SGSA
        
        IF(SACUL == SACLL)NSACI=1
        XSACI=NSACI
        RNSACI=1./XSACI
        SACI=(SACUL/SACLL)**RNSACI
        SACU=SACLL*SACI
        GMSAC=SQRT(SACLL*SACU)
        IGS=LBASE+1
        LGS=LBASE+NSACI
        DO 1100  I=1,NSACI
          ISAND=I+LBASE
CIPK MAR06 UPDATE FOR ELEMENT VALUES
          SDT(ISAND)=GMSAC
          GMSAC=GMSAC*SACI
 1100   CONTINUE
        LBASE=LGS
CIPK MAR06  UPDATE FOR ELEMENT VALUES
        WRITE(LOUT,1110)SACLL,SACUL,NSACI,(SDT(K),K=IGS,LGS)
 1110 FORMAT(10X'SAND GRAIN SIZES FROM    ',F10.4,'  TO',F10.4,/10X,
     * 'ARE ASSIGNED THIS CLASSIFICATION AND DIVIDED INTO',I5,
     *' CLASS INTERVALS'/9X,' GEOMETRIC MEAN DIAMETERS (MM) ARE',/10X,
     * 15F8.4)

C
CIPK MAR06 COMPUTE SETTLING VELOCITY IF NOT ENTERED
C
        D50M=D50T/1000.
        IF(VSANDT == 0.) THEN
          RNU=XNU(1)
          VSANDT = 
     +  ( 10.* RNU / (0.8 * D50M) ) * (SQRT(1.+(0.01 * (SGSAT-1.) 
     +* 9.81 * (0.8 * D50M ) ** 3) / RNU ** 2 ) -1.)
        ENDIF

        WRITE(LOUT,1112) d35T,d50T,d90T,VSANDT,gpmaxt
 1112 FORMAT(10X'SAND GRAIN SIZES: d35=',F10.4,'; d50=',F10.4,
     *' d90=',f10.4/
     +   10X,'FALL VELOCITY FOR SAND',T50,F10.4,3X,'(M/SEC)'/
     +   10X,'MAXIMUM CONC IN WATER COLUMN',T50,F10.1,3X,'(GM/M3)')
cipk mar06     
c      write(lout,'(8x,''Maximum allowed sand potential'',e15.4)') gpmaxt
        DO N=1,NE
 !        IF(IMAT(N) > 0) THEN
          IF(IMAT(N) /= 0) THEN       !HN 6.07.2009 use initial values for all elements and not only for active ones.
          DO I=1,NLMTP
            IF(ISMTREF(I) == 0 .AND. I > 1) GO TO 350
            IF(ISMTREF(I) == IMAT(N) .OR. ISMTREF(I) == 0) THEN
              DO JJ=1,NCORN(N)
                J=NOP(N,JJ)
                SGSAND(J)=SGSAT
                CLDEND(J)=CLDET
                CLERND(J)=CLERT
                VSANDND(J)=VSANDT
                AMANNND(J)=AMANNT
                D35ND(J)=D35T
                D50ND(J)=D50T
                D90ND(J)=D90T
                ISMODEND(J)=ismodeT
                GPMAXND(J)=GPMAXT
                DO K=IGS,LGS
                  SDND(K,J)=SDT(K)     
                ENDDO
              ENDDO
            ENDIF
          ENDDO
  350     CONTINUE          
         ENDIF
        ENDDO
        GO TO 250
      ENDIF
CIPK MAR06 MOVED        CALL KINVIS

CIPK MAR06        CALL GINPT(LIN,ID,DLIN)

CIPK MAR06 MOVED        IF(ID(1:3) == 'DIA') THEN
CIPK MAR06 MOVED          READ(DLIN,1030) d35,d50,d90,ismode,GPMAX
CIPK MAR06 MOVED          if(gpmax == 0.) gpmax=1.e+6
CIPK MAR06 MOVED        ELSE
CIPK MAR06 MOVED          WRITE(75,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
CIPK MAR06 MOVED          WRITE(*,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
CIPK MAR06 MOVED          STOP
CIPK MAR06 MOVED        ENDIF


CIPK MAR06 END CHANGES
     
        DO L=1,NMAT
          RCAE(L)=0.
          RWAE(L)=0.
        ENDDO

        DO L=1,NP
          RCAN(L)=0.
          RWAN(L)=0.
          IBN(L)=0
        ENDDO

 1150   CONTINUE
CIPK MAR06        CALL GINPT(LIN,ID,DLIN)

        IF(ID(1:3) == 'RGH') THEN
          NOTUSED=0
          READ(DLIN,1030) N,RCT,RWT
CIPK MAR06
          CALL GINPT(LIN,ID,DLIN)
          IF(N == 0) THEN
            DO L=1,NMAT
              RCAE(L)=RCT
              RWAE(L)=RWT
            ENDDO
          WRITE(LOUT,6000) RCT,RWT
 6000     FORMAT(/10X,'GLOBAL CURRENT RELATED ROUGHNESS',F17.4,' (M)'/
     +            10X,'GLOBAL WAVE    RELATED ROUGHNESS',F17.4,' (M)')
CIPK MAR06
            GO TO 1150
          ELSE
            RCAE(N)=RCT
            RWAE(N)=RWT
          WRITE(LOUT,6001) N,RCT,RWT
 6001     FORMAT(/10X,'ELEMENT TYPE',I10/
     +            10X,'CURRENT RELATED ROUGHNESS',F24.4,' (M)'/
     +            10X,'WAVE    RELATED ROUGHNESS',F24.4,' (M)')
            GO TO 1150
          ENDIF
        ELSE
          NOTUSED=1 
        ENDIF

CIPK JUN02 DISTRIBUTE ROUGHNESS TO NODAL VALUES

        DO N=1,NE
          IF(IMAT(N) > 0) THEN
            DO L=1,NCORN(N)
              RCAN(NOP(N,L))=RCAN(NOP(N,L))+RCAE(IMAT(N))
              RWAN(NOP(N,L))=RWAN(NOP(N,L))+RWAE(IMAT(N))
              IBN(NOP(N,L))=IBN(NOP(N,L))+1
            ENDDO
          ENDIF
        ENDDO
        DO N=1,NP
          IF(IBN(N) > 1) THEN
            RCAN(N)=RCAN(N)/IBN(N)
            RWAN(N)=RWAN(N)/IBN(N)
          ENDIF
        ENDDO

C
CIPK AUG00
C
CIPK MAR06 RENAME PRINT VALUES
      if(ismodet == 1) 
     +  write(lout,'(8X,''Using Ackers and White'')')
      if(ismodet == 2) write(lout,'(8X,''Using Van Rijn(1984)'')')
      if(ismodet == 3) write(lout,'(8X,''Using Browlie'')')
cipk jun00 add 4th option
      if(ismodet == 4) write(lout,'(8X,''Using Van Rijn(1989)'')')
      if(ismodet == 5) write(lout,'(8X,''Using Van Rijn(1993)'')')
      if((ismodet < 1) .OR. (ismodet > 5)) then
            write(*,*) 'Illegal transport formulae option'
            write(lout,*) 'Illegal transport formulae option'
            stop
      end if
cipk mar06      write(lout,'(8x,''Maximum allowed sand potential'',e15.4)') gpmax
cwlp jul96 end additions      
C      END IF

  200 CONTINUE
      IF(NOTUSED == 0) CALL GINPT(LIN,ID,DLIN)
      NOTUSED=0
      IF(ID(1:2) == 'IS') THEN
        READ(DLIN,'(I8,F8.0)') MT,TSAND
      ELSEIF(ID(1:7) == 'ENDSAND') THEN
        GO TO 300
      ELSE
        WRITE(75,*) 'NO ENDSAND DATA LINE FOUND'
        WRITE(*,*) 'NO ENDSAND DATA LINE FOUND'
        STOP
      ENDIF
      IF(MT == 0) THEN
        DO N=1,NP
          TTHICK(N)=TSAND
        ENDDO
        GO TO 200
      ELSE
        TTHICK(MT)=TSAND
      GO TO 200
      ENDIF

      IF(ID(1:7) /= 'ENDSAND') THEN
        WRITE(75,*) 'NO ENDSAND DATA LINE FOUND'
        WRITE(*,*) 'NO ENDSAND DATA LINE FOUND'
        STOP
      ENDIF
  300 CONTINUE
        DO N=1,NP
        ELEVB(N)=AO(N)
      ENDDO


      !******************************************************djw 15/02/05
      !
      !  Override of TTHICK values if scour limits are read in with SCOURLIM line in
      !  files section of input file.
      !
      !******************************************************
      If (wbm_ScourLim) Then
            Call wbm_InitScourLims(NP,wbm_ScourLims)
         MT = 0
         Do N=1,NP
           TTHICK(N)=wbm_ScourLims(N)
         End Do
      End If
      !******************************************************end djw 15/02/05





 1020 FORMAT(9F8.0)
 1030 FORMAT(3F8.4,i8,F8.0)
 1031 FORMAT(7F8.4, I8)
 1131 FORMAT(3F8.4)
 1032 FORMAT(3F8.3)
CIPK MAR06 UPDATE HEADER
 2030 FORMAT(//
     +   10x,'SPECIFIC GRAVITY OF SAND',T50,F10.4/
     +   10X,'GRAIN SHAPE FACTOR',T50,F10.4/
     +   10X,'CHAR TIME FOR DEPSN',T50,F10.4,'(HRS)'/
     +   10X,'CHAR TIME FOR EROSN',T50,F10.4,'(HRS)'/
     +   10X,'MANNING COEFFICIENT FOR BED',T50,F10.3)

CIPK MAR06 ADD FORMATS
 6010 FORMAT(//10X,'SAND PROPERTIES FOR ELEMENT TYPES',5I6/(I16,9I8))
 6011 FORMAT(//'GLOBAL SAND PROPERTIES')

      RETURN
      END
