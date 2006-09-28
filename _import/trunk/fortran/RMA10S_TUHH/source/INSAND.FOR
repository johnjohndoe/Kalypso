      SUBROUTINE INSAND
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD

C-ipk   This subroutine reads in all rates for sand

      COMMON NIP(14)

cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'

C      CHARACTER*8 ID
C      CHARACTER*72 DLIN
C       Read Title Line

C
C
C     INITIALIZE
C
      ACGR=9.807
      RHOF=1000.

C     Initialize METRIC to 0 for english units

      METRIC = 0
      LBASE=0
      CLABL ='   SAND '

C     Sand is simulated read global parameters

C      IF(LSAND .GT. 0) THEN

	  CALL GINPT(LIN,ID,DLIN)

        IF(ID(1:3) .EQ. 'SAN') THEN
          READ(DLIN,1020) 
     +    SACLL,SACUL,ASACI,SGSA,GSF,CLDE,CLER,VSAND,AMANN
        ELSE
          WRITE(75,*) 'EXPECTED SAND DATA - NONE FOUND'
          WRITE(*,*) 'EXPECTED SAND DATA - NONE FOUND'
           STOP
	  ENDIF
        NSACI=ASACI+0.5

CIPK AUG00 PRINT SAND SETTLING LATER

        WRITE(LOUT,2030) SGSA,GSF,CLDE,CLER,AMANN
CIPK FEB95        VSAND=VSAND/86400.
        IF(SACUL.EQ.SACLL)NSACI=1
        XSACI=NSACI
        RNSACI=1./XSACI
        SACI=(SACUL/SACLL)**RNSACI
        SACU=SACLL*SACI
        GMSAC=SQRT(SACLL*SACU)
        IGS=LBASE+1
        LGS=LBASE+NSACI
        DO 1100  I=1,NSACI
          ISAND=I+LBASE
          SD(ISAND)=GMSAC
          GMSAC=GMSAC*SACI
 1100   CONTINUE
        LBASE=LGS
        WRITE(LOUT,1110)SACLL,SACUL,NSACI,(SD(K),K=IGS,LGS)
 1110 FORMAT(10X'SAND GRAIN SIZES FROM    ',F10.4,'  TO',F10.4,/10X,
     * 'ARE ASSIGNED THIS CLASSIFICATION AND DIVIDED INTO',I5,
     *' CLASS INTERVALS'/9X,' GEOMETRIC MEAN DIAMETERS (MM) ARE',/10X,
     * 15F8.4)
        CALL KINVIS

	  CALL GINPT(LIN,ID,DLIN)

        IF(ID(1:3) .EQ. 'DIA') THEN
          READ(DLIN,1030) d35,d50,d90,ismode,GPMAX
	    if(gpmax .eq. 0.) gpmax=1.e+6
        ELSE
          WRITE(75,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
          WRITE(*,*) 'EXPECTED DIAMETER DATA - NONE FOUND'
          STOP
	  ENDIF

        DO L=1,NMAT
          RCAE(L)=0.
          RWAE(L)=0.
        ENDDO

        DO L=1,NP
          RCAN(L)=0.
          RWAN(L)=0.
	    IBN(L)=0
        ENDDO

 1150   CALL GINPT(LIN,ID,DLIN)

        IF(ID(1:3) .EQ. 'RGH') THEN
	    NOTUSED=0
          READ(DLIN,1030) N,RCT,RWT
	    IF(N .EQ. 0) THEN
	      DO L=1,NMAT
	        RCAE(L)=RCT
	        RWAE(L)=RWT
	      ENDDO
          WRITE(LOUT,6000) RCT,RWT
 6000     FORMAT(/10X,'GLOBAL CURRENT RELATED ROUGHNESS',F17.4,' (M)'/
     +            10X,'GLOBAL WAVE    RELATED ROUGHNESS',F17.4,' (M)')
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
	    IF(IMAT(N) .GT. 0) THEN
	      DO L=1,NCORN(N)
              RCAN(NOP(N,L))=RCAN(NOP(N,L))+RCAE(IMAT(N))
              RWAN(NOP(N,L))=RWAN(NOP(N,L))+RWAE(IMAT(N))
	        IBN(NOP(N,L))=IBN(NOP(N,L))+1
            ENDDO
	    ENDIF
	  ENDDO
	  DO N=1,NP
	    IF(IBN(N) .GT. 1) THEN
            RCAN(N)=RCAN(N)/IBN(N)
            RWAN(N)=RWAN(N)/IBN(N)
	    ENDIF
	  ENDDO

C
CIPK AUG00 COMPUTE SETTLING VELOCITY IF NOT ENTERED
C
        D50M=D50/1000.
        IF(VSAND .EQ. 0.) THEN
          RNU=XNU(1)
          VSAND = 
     +  ( 10.* RNU / (0.8 * D50M) ) * (SQRT(1.+(0.01 * (SGSA-1.) 
     +* 9.81 * (0.8 * D50M ) ** 3) / RNU ** 2 ) -1.)
        ENDIF

        WRITE(LOUT,1112) d35,d50,d90,VSAND
 1112 FORMAT(10X'SAND GRAIN SIZES: d35=',F10.4,'; d50=',F10.4,
     *' d90=',f10.4/
     +   10X,'FALL VELOCITY FOR SAND',T50,F10.4,3X,'(M/SEC)'/
     +   10X,'MAXIMUM CONC IN WATER COLUMN',T50,F10.1,3X,'(GM/M3)')

CIPK AUG00 END CHANGES

	if(ismode.eq.1) 
     +  write(lout,'(8X,''Using Ackers and White'')')
	if(ismode.eq.2) write(lout,'(8X,''Using Van Rijn(1984)'')')
	if(ismode.eq.3) write(lout,'(8X,''Using Browlie'')')
cipk jun00 add 4th option
	if(ismode.eq.4) write(lout,'(8X,''Using Van Rijn(1989)'')')
	if(ismode.eq.5) write(lout,'(8X,''Using Van Rijn(1993)'')')
	if((ismode.lt.1).or.(ismode.gt.5)) then
		write(*,*) 'Illegal transport formulae option'
		write(lout,*) 'Illegal transport formulae option'
		stop
	end if
	write(lout,'(8x,''Maximum allowed sand potential'',e15.4)') gpmax
cwlp jul96 end additions	
C      END IF

  200 CONTINUE
      IF(NOTUSED .EQ. 0) CALL GINPT(LIN,ID,DLIN)
      NOTUSED=0
      IF(ID(1:2) .EQ. 'IS') THEN
        READ(DLIN,'(I8,F8.0)') MT,TSAND
      ELSEIF(ID(1:7) .EQ. 'ENDSAND') THEN
	  GO TO 300
      ELSE
	  WRITE(75,*) 'NO ENDSAND DATA LINE FOUND'
	  WRITE(*,*) 'NO ENDSAND DATA LINE FOUND'
	  STOP
      ENDIF
	IF(MT .EQ. 0) THEN
	  DO N=1,NP
          TTHICK(N)=TSAND
	  ENDDO
	  GO TO 200
	ELSE
	  TTHICK(MT)=TSAND
	GO TO 200
	ENDIF

      IF(ID(1:7) .NE. 'ENDSAND') THEN
	  WRITE(75,*) 'NO ENDSAND DATA LINE FOUND'
	  WRITE(*,*) 'NO ENDSAND DATA LINE FOUND'
	  STOP
      ENDIF
  300 CONTINUE
  	DO N=1,NP
        ELEVB(N)=AO(N)
	ENDDO

 1020 FORMAT(9F8.0)
 1030 FORMAT(3F8.4,i8,F8.0)
 2030 FORMAT(/15X,' GLOBAL SAND RATES'//
     +   10x,'SPECIFIC GRAVITY OF SAND',T50,F10.4/
     +   10X,'GRAIN SHAPE FACTOR',T50,F10.4/
     +   10X,'CHAR TIME FOR DEPSN',T50,F10.4,'(HRS)'/
     +   10X,'CHAR TIME FOR EROSN',T50,F10.4,'(HRS)'/
     +   10X,'MANNING COEFFICIENT FOR BED',T50,F10.3)


      RETURN
      END
