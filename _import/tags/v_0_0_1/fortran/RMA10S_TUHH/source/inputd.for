C     Last change:  WP   14 Dec 2007   10:37 am
CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
CIPK  LAST UPDATE SEP 6 2004   add error file
cipk  last update Aug 06 2002 expand dlin to 80 char
CIPK  LAST UPDATE MAR 13 2001 ADD TIME STEP TRANSITION OPTION
cipk  last update Dec 9 1999 Allow for extra BC lines
cipk  last update Dec 7 1999 Allow for zero delta T
CIPK  LAST UPDATE MAR 23 1998 
CIPK  LAST UPDATED NOV 26 1997
CIPK  LAST UPDATE NOV 21 1997
cipk  last updated Sept 26 1996
CIPK  LAST UPDATED SEP 8 1995
      SUBROUTINE INPUTD(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSANMOD
      SAVE
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
      DIMENSION QDM(3)
C-
C-
C-.....ENTRY FOR TIME DEPENDENT INPUT DATA.....
C-
      DO I=1,3
        QDM(I)=0.
      ENDDO
      DO  J = 1, NP
CIPK JUN05
        nfix(j)=nfixp(j)
        DO  K= 1, NDF
          SPEC(J,K) = 0.00
        ENDDO
      ENDDO
      IBK=0
cipk apr96 define save parameter
      isvs=0
  570 CONTINUE
cipk apr96 keep track of data for end of time step
      if(iyend+idye+hrend .eq. 0) then
        isvs=1
        rewind nscrin
      elseif(iyrr .eq. iyend  .and.  dayofy .eq. idye) then
cipk mar98        if(abs(tet-hrend) .lt. 0.001) then
        if(tet .gt. hrend-0.001) then
          isvs=1
          ibin=ibinrst
        else
          ibin=nscrin
          rewind ibin
          isvs=0
        endif
cipk mar98
      elseif(tet .gt. (idye-dayofy)*24.+hrend-0.001) then
        isvs=1
        ibin=ibinrst
      else
        ibin=nscrin
        rewind ibin
        isvs=0
      endif
 1961 READ(IBIN,7000,END=197,ERR=197) ID,DLIN
      write(75,7000) id,dlin
      IF(ID(1:3) .EQ. 'com') GO TO 1961
      IF(ID(1:3) .EQ. 'COM') GO TO 1961
      IF(ID(1:3) .EQ. 'Com') GO TO 1961
      IF(ID(1:8) .EQ. '        ') GO TO 1961
      NDATLN=NDATLN+1
      IF(ID(1:6) .NE. 'ENDDAT') GO TO 198
C
C   Allow for partial B.C. input from unit 5 and unit IBUP
C
  197 IF(IBIN .EQ. IBUP) REWIND IBIN
      IBK=IBK+1
        IF(IBK .GT. 1) THEN
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          write(75,*) 'Error in dynamic boundary condition files'
          CALL ZVRS(1)
          STOP 'Error in dynamic boundary condition files'
        ENDIF
      IBIN=IBUP
      ibinrst=ibin
      GO TO 570
  198 CONTINUE
cipk apr96 end changes
      IF(ID(1:2) .NE. 'DT') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6999) ID(1:8)
        WRITE(75,6999) ID(1:8)
        WRITE(LOUT,6999) ID(1:8)
        STOP 'LOOKING FOR DT'
      ENDIF
cipk apr96 add ending time for time step
      READ(DLIN,5031) DELT,iyend,idye,hrend

CIPK MAR01  TEST FOR ELEVATION AND SCALE TIME STEP

      IF(NODETR .NE. 0) THEN
        IF(WSLL(NODETR) .GT. TRELEV) THEN
          DELT = DELT*TRFACT
        ENDIF
      ENDIF

      if(ibin .ne. nscrin) then
        write(lout,6156) delt,iyend,idye,hrend
      endif
      if(isvs .eq. 1) then
        rewind nscrin
        write(nscrin,7000) id,dlin
      endif
cipk sep96 add altm
cpk dec99 add test for zero DELT
      if(delt .gt. 0.) then
        altm=alpha/(delt*3600.)
      else
        altm=0.
      endif

      TTT=TET+DELT
      TETH=TETH+DELT
      WRITE(LOUT,6145) TTT,ICYC




cipk apr96 end changes
C      READ(DLIN,5010) DELT
      TET = TET + DELT

CIPK SEP02

C     ADD SIDE ERODED MATERIAL
CIPK FEB03 CATCH THE STEADY STATE CASE
        IF((LSAND .GT. 0  .OR.  LBED .GT. 0) .AND. DELT .GT. 0.) THEN
          CALL SLUMPIT
CIPK DEC05          
        ELSE 
          EXTLD=0          
        ENDIF

C
C-.... Read iteration controls
C
CIPK NOV97      READ(IBIN,7000) ID,DLIN
      call ginpt(ibin,id,dlin)
cipk apr96 save data to a scratch file
      if(isvs .eq. 1) then
        write(nscrin,7000) id,dlin
      endif
      NDATLN=NDATLN+1

CIPK JUN05 MODIFY BED PROFILE
c
c...... Input slope adjustment factor
c
      if(id(1:3) .eq. 'SAD') then

        read(dlin,'(2f8.0)') sadx,sadel

        if(isvs .eq. 1) then
          write(nscrin,7000) id,dlin
        endif
        call ginpt(ibin,id,dlin)
	  CALL REVAO
	  ISAD=1
      else
	  IF(ISAD .EQ. 1) THEN
          sadx=1.0
	    CALL REVAO
	    ISAD=0
	  ENDIF
      endif



      IF(ID(1:2) .NE. 'BC') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(LOUT,6999) ID(1:2)
        WRITE(75,6999) ID(1:2)
        WRITE(*,6999) ID(1:2)
        STOP 'LOOKING FOR BC'
      ENDIF 
      READ(DLIN,5011)
     +         (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=1,9)
      IF(NITN .GT. 9) THEN
        N1=1
  199   N1=N1+9
        N2=N1+8
CIPK NOV97        READ(IBIN,7000) ID,DLIN
        call ginpt(ibin,id,dlin)
cipk apr96 save data to a scratch file
      if(isvs .eq. 1) then
        write(nscrin,7000) id,dlin
      endif
        NDATLN=NDATLN+1
        IF(ID(1:2) .NE. 'BC') THEN
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,6999) ID(1:2)
          WRITE(75,6999) ID(1:2)
          WRITE(LOUT,6999) ID(1:2)
          STOP 'LOOKING FOR BC'
        ENDIF 
        READ(DLIN,5011)
     +         (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=N1,N2)
        IF(NITN .GT. N2) GO TO 199
      ENDIF    
C
C...... Set ITEQV based on input
C-
	DO  651 I=1,NITN
	  IF(ITEQS(I) .GT. 0) THEN
	    IF(ITEQV(I) .EQ. 0) THEN
	      ITEQV(I)=ITEQS(I)+5
	    ENDIF
	    IF(ITEQV(I) .EQ. 2) THEN
	      ITEQV(I)=ITEQS(I)+7
	    ENDIF
	    IF(ITEQV(I) .EQ. 3) THEN
	      ITEQV(I)=ITEQS(I)+10
cipk may02	      ITEQV(I)=ITEQS(I)+9
	    ENDIF
	  ENDIF
  651   CONTINUE
C-
C-..... READ BOUNDARY VALUES
C-
  210 CONTINUE
CIPK NOV97      READ(IBIN,7000) ID,DLIN

cipk dec99  read data line and see if it is a stray BC line, if so skip

  215 continue
      call ginpt(ibin,id,dlin)
      if(id(1:2) .eq. 'BC') go to 215

cipk apr96 save data to a scratch file
      if(isvs .eq. 1) then
        write(nscrin,7000) id,dlin
      endif
      NDATLN=NDATLN+1
      IF(ID(1:2) .EQ. 'BN') THEN
        READ(DLIN,5050) N,NFIX(N),NFIX1(N),(SPEC(N,M),M=1,NDF)
        write(lout,6050) n,nfix(n),nfix1(n),(spec(n,m),m=1,ndf)
        CALL BFORM(N)
        GO TO 210
      ENDIF    

C
C...... Go to read boundary conditions along line and wind data
C
      if (iaccyc <= icyc) then
        CALL SBGEN(IBIN)
      ELSEIF (iaccyc > icyc) then
        WRITE(*,*) 'Jump over input data of time step', icyc
        findEndStep: do
          call ginpt(ibin, id, dlin)
          if (ID(1:7) == 'ENDSTEP') EXIT findEndStep
        end do findEndStep

      end if

c-
c
C Fixed salinity nodes 
C
C      IF (ISALBC .GT. 0)  THEN
C	 DO 710 I=1,NFXSAL
C	    N = IFXSAL(I)
C	    NFIX(N) = (NFIX(N)/100)*100 + 11
C	    NFIX1(N)=1
C	    SPEC(N,4) = SALBC(1)
C	    SPEC(N,5) = SALBC(2)
C	    SPEC(N,6) = SALBC(3)
C  710    CONTINUE
C      ENDIF
C
CIPK NOV97 ADD CALL
      CALL ELFLOWS(IBIN)
C      do n=1,ne
C        if(sidf(n) .ne. 0.  .or. sidq(n,1) .ne. 0.)then
C          write(75,*) 'elflws',n,sidf(n),sidq(n,1)
C        endif
C      enddo
C
C-

C...... Call routine to establish boundary conditions and angles
C-
      CALL BLINE(1)
      DELT=DELT*3600.
C-

C......READ SPECIAL CASE BOUNDARY CONDITIONS AND INSERT FUNCTIONAL SHAPE
C-
      CALL BCS(IBIN,CMIN,CPR)
C-
      CALL QGENSPCL(1,0,0, 0.,0.,QDM)
C	
      CALL HGENSPCL(1,0,0, 0.,QDM)
C-
C-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
C-
      DO 800 N=1,NP
        IF(NSPL(N) .EQ. 1) THEN
          IF(NDEP(N) .GT. 1) THEN
            CALL BFORM(N)
            NL=NREF(N)+1
            NT=NL+NDEP(N)-2
            DO 790 M=NL,NT
              CALL BFORM(M)
  790       CONTINUE
          ENDIF
        ENDIF
  800 CONTINUE

C-
      RETURN
C-
C-.....INPUT DATA CARD FORMATS.....
C-
 5005 FORMAT( 20A4 )
cipk jan94 new format
 5006 FORMAT( A80)
 5010 FORMAT( F8.0)
 5031 FORMAT( F8.0,2I8,F8.0)
 5011 FORMAT(9(I5,3I1))
 5050 FORMAT(I8,I7,I1,6E8.0)
 6050 format(i8,i7,i1,6f10.3)
 6000 FORMAT( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRA
     1M RMA-10 '/ 10X, 'THREE-DIMENSIONAL HYDRODYNAMICS  WITH SALINITY-T
     2EMPERATURE-SEDIMENT'/18X,'VERSION 3.5d MAY 2006')
 6145 FORMAT( 1H1 / 10X, '*** BOUNDARY CONDITION UPDATE AT TIME =',
     1  F10.2, ' HOURS...CYCLE NUMBER', I5 //
     2  6X, 'NODE       FIX    X-FLOW    Y-FLOW      ELEV      CONC')
cipk APR96 new format below
 6156 format(/'     TIME STEP SET TO',F6.2,' UNTIL YEAR',I5,' DAY',I5,
     +' HOUR',F7.2)
 6999   FORMAT('UNABLE TO FIND LINE ',2X,A2)
cipk Au02 expand dlin to 80 char
cipk mar05 7000 FORMAT(A8,A80)
 7000 FORMAT(A8,A72)
      END
