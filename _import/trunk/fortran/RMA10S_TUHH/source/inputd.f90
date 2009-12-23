!     Last change:  MD   14 Jul 2009    1:47 pm
!IPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
!IPK  LAST UPDATE SEP 6 2004   add error file
!ipk  last update Aug 06 2002 expand dlin to 80 char
!IPK  LAST UPDATE MAR 13 2001 ADD TIME STEP TRANSITION OPTION
!ipk  last update Dec 9 1999 Allow for extra BC lines
!ipk  last update Dec 7 1999 Allow for zero delta T
!IPK  LAST UPDATE MAR 23 1998 
!IPK  LAST UPDATED NOV 26 1997
!IPK  LAST UPDATE NOV 21 1997
!ipk  last updated Sept 26 1996
!IPK  LAST UPDATED SEP 8 1995
      SUBROUTINE INPUTD(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSANMOD
! HN. June2009
      USE share_profile, ONLY : BANKEVOLUTION   
!
      use SchwarzIterationControl_Helper
      use mod_ContiLines
!
      SAVE
!
      DIMENSION QDM(3)
!
!MD: Local Varaible for checking BC-data
      INTEGER :: Check_BC_Data
!------------------------------------------------------------------------
! Description of this ROUTINE INPUTD(IBIN)
!------------------------------------------------------------------------
!MD:  SubR to read unsteady data out of CONTROL for iterations: Main routine
!MD    to read boundary conditions and equations, that should be solved.
!MD    Here the lines DT, BC, BN are read. For Boundary conditions seperate
!MD    SubRs are called like: SBGEN, QGEN a.s.o
!MD:   >> compare with ROUTINE GETBC(IBIN)
!------------------------------------------------------------------------
!
!
!-
!-.....ENTRY FOR TIME DEPENDENT INPUT DATA.....
!-
      DO I=1,3
        QDM(I)=0.
      ENDDO
      DO  J = 1, NP
!IPK JUN05
        nfix(j)=nfixp(j)
        DO  K= 1, NDF
          SPEC(J,K) = 0.00
        ENDDO
      ENDDO
      IBK=0
!ipk apr96 define save parameter
      isvs=0
  570 CONTINUE
!
!time handling is done by Kalypso
!thus, the time handling is switched off for the moment
!cipk apr96 keep track of data for end of time step
!      if(iyend+idye+hrend == 0) then
!        isvs=1
!        rewind nscrin
!      elseif(iyrr == iyend .AND. dayofy == idye) then
!cipk mar98        if(abs(tet-hrend) < 0.001) then
!        if(tet > hrend-0.001) then
!          isvs=1
!          ibin=ibinrst
!        else
!          ibin=nscrin
!          rewind ibin
!          isvs=0
!        endif
!cipk mar98
!      elseif (tet > (idye-dayofy)*24.+hrend-0.001) then
!        isvs=1
!        ibin=ibinrst
!      else
!        ibin=nscrin
!        rewind ibin
!        isvs=0
!      endif
      isvs = 1
      rewind nscrin
!-time handling      
!
 1961 READ(IBIN,7000,END=197,ERR=197) ID,DLIN
      write(75,7000) id,dlin
      IF(ID(1:3) == 'com') GO TO 1961
      IF(ID(1:3) == 'COM') GO TO 1961
      IF(ID(1:3) == 'Com') GO TO 1961
      IF(ID(1:8) == '        ') GO TO 1961
      IF(ID(1:6) /= 'ENDDAT') GO TO 198
!
!   Allow for partial B.C. input from unit 5 and unit IBUP
!
  197 IF(IBIN == IBUP) REWIND IBIN
      IBK=IBK+1
        IF(IBK > 1) THEN
!ipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          write(75,*) 'Error in dynamic boundary condition files'
          CALL ZVRS(1)
          STOP 'Error in dynamic boundary condition files'
        ENDIF
      IBIN=IBUP
      ibinrst=ibin
      GO TO 570
!
!MD: Read DT-Line for Time
!----------------------------------
  198 CONTINUE
!ipk apr96 end changes
      IF(ID(1:2) /= 'DT')                                               &
     &  call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'DT')
!
!ipk apr96 add ending time for time step
      istat = 0
      read (dlin, *, iostat = istat) delt, iyend, idye, hrend
      if (istat /= 0) then
        istat = 0
        read (dlin, *, iostat = istat) dlin
        if (istat /= 0) stop 'DT line set up wrongly'
      endif 
!
!
!Distinguish between unit of time step length:      
!---------------------------------------------      
!HOURS      
      if (ID (4:6) == 'HHH') then
        continue
!MINUTES      
      elseif (ID (4:6) == 'MIN') then
        DELT = DELT / 60
!
!SECONDS      
      elseif (ID (4:6) == 'SEC') then
        DELT = DELT / 3600
!MILLISECONDS      
      elseif (ID (4:6) == 'MSE') then
        DELT = DELT / 1000 / 3600
!HOURS (standard assumption)      
      else 
        continue
      endif
!
!
!IPK MAR01  TEST FOR ELEVATION AND SCALE TIME STEP
!MD: Only for 'TST' = Time step LENGTHENING
      IF(NODETR /= 0) THEN
        IF(WSLL(NODETR) > TRELEV) THEN
          DELT = DELT*TRFACT
        ENDIF
      ENDIF
!
!
      if(ibin /= nscrin) then
        write(lout,6156) delt,iyend,idye,hrend
      endif
      if(isvs == 1) then
        rewind nscrin
        write(nscrin,7000) id,dlin
      endif
!ipk sep96 add altm
!pk dec99 add test for zero DELT
      if(delt > 0.) then
        altm=alpha/(delt*3600.)
      else
        altm=0.
      endif
!
      TTT=TET+DELT
      TETH=TETH+DELT
      WRITE(LOUT,6145) TTT,ICYC
!
!
!ipk apr96 end changes
!      READ(DLIN,5010) DELT
      TET = TET + DELT
!
!
!     ADD SIDE ERODED MATERIAL
!IPK FEB03 CATCH THE STEADY STATE CASE
        IF((LSAND > 0 .OR. LBED > 0) .AND. DELT > 0.) THEN
!HN June2009. IN the case that bankevolution is activated, deactivate slumpit subroutine.
          if ( .NOT. bankevolution) CALL SLUMPIT
!ALL SLUMPIT
        ELSE 
          EXTLD=0.
        ENDIF
!
!
!-.... Read iteration controls
!---------------------------------------------
!IPK NOV97      READ(IBIN,7000) ID,DLIN
      call ginpt(ibin,id,dlin)
!ipk apr96 save data to a scratch file
      if(isvs == 1) then
        write(nscrin,7000) id,dlin
      endif
!
!IPK JUN05 MODIFY BED PROFILE
!
!...... Input slope adjustment factor
!
      if(id(1:3) == 'SAD') then
        read(dlin,'(2f8.0)') sadx,sadel
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
        call ginpt(ibin,id,dlin)
        CALL REVAO
        ISAD=1
      else
        IF(ISAD == 1) THEN
          sadx=1.0
          CALL REVAO
          ISAD=0
        ENDIF
      endif
!
!MD: Read Iteration-Data for URFC and equations
!----------------------------------------------
      IF(ID(1:2) /= 'BC') THEN
!ipk sep04
!MD: check if old Iteration-Data is available        
        DO I = 1, NITN
          Check_BC_Data = IURVL(I)+ITLVL(I)+ITEQV(I)+ITEQS(I)
!MD: Allow to use old Iteration-Data, if no new block is          
!MD:   defined in CONTROL          
!
!ERROR - Could not locate required BC line          
          IF (Check_BC_Data == 0)                                       &
     &      call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'BC')
        END DO
        goto 315
!MD: New Jump, because next line was already read by ginpt(..)        
!
      Elseif(ID(1:2) == 'BC') THEN
        READ(DLIN,5011)                                                 &
     &         (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=1,9)
        IF(NITN > 9) THEN
          N1=1
  199     N1=N1+9
          N2=N1+8
!IPK NOV97        READ(IBIN,7000) ID,DLIN
          call ginpt(ibin,id,dlin)
!ipk apr96 save data to a scratch file
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
!
!ERROR - Could not locate required BC line        
        IF(ID(1:2) /= 'BC')                                             &
     &    call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'BC')
!
          READ(DLIN,5011)                                               &
     &           (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=N1,N2)
          IF(NITN > N2) GO TO 199
        ENDIF
      Endif
!
!.....Set ITEQV based on input
!-
      SetCalcType: DO I=1,NITN
        IF (ITEQS (I) > 0) THEN
          IF (ITEQV (I) == 0) THEN
            ITEQV (I) = ITEQS (I) + 5
          ENDIF
          IF (ITEQV (I) == 2) THEN
            ITEQV (I) = ITEQS (I) + 7
          ENDIF
          IF (ITEQV (I) == 3) THEN
            ITEQV (I) = ITEQS (I) + 10
!ipk may02            ITEQV(I)=ITEQS(I)+9
          ENDIF
        ENDIF
      ENDDO SetCalcType
!-
!-..... READ BOUNDARY VALUES
!-
  210 CONTINUE
!IPK NOV97      READ(IBIN,7000) ID,DLIN
!
!ipk dec99  read data line and see if it is a stray BC line, if so skip
!MD: Does not make sense to have 'BC' lines, which are not used...
  215 continue
      call ginpt(ibin,id,dlin)
      if(id(1:2) == 'BC') goto 215
!
!MD: new jump in order to use old Iteration-Data, if no new block is
!MD:   defined in CONTROL
  315 continue
!
!
!ipk apr96 save data to a scratch file
      if(isvs == 1) then
        write(nscrin,7000) id,dlin
      endif
      IF(ID(1:2) == 'BN') THEN
        READ(DLIN,5050) N,NFIX(N),NFIX1(N),(SPEC(N,M),M=1,NDF)
        write(lout,6050) n,nfix(n),nfix1(n),(spec(n,m),m=1,ndf)
        CALL BFORM(N)
        GO TO 210
      ENDIF    
!
!
!...... Go to read boundary conditions along line and wind data
!
      if (iaccyc <= icyc) then
        CALL SBGEN(IBIN)
      ELSEIF (iaccyc > icyc) then
        WRITE(*,*) 'Jump over input data of time step', icyc
        findEndStep: do
          call ginpt(ibin, id, dlin)
          if (ID(1:7) == 'ENDSTEP') EXIT findEndStep
        end do findEndStep
!nis,jul08: leave subroutine, if time step is found and don't transform restart values!        
        return
!
      end if
!
!
!IPK NOV97 ADD CALL
      CALL ELFLOWS(IBIN)
!
!Set the boundary condition type for inner boundaries
      call setInnerBC_NFIX (ccls, ncl, nfix)
!
!...... Call routine to establish boundary conditions and angles
!-
      CALL BLINE(1)
      DELT=DELT*3600.
!-
!
!......READ SPECIAL CASE BOUNDARY CONDITIONS AND INSERT FUNCTIONAL SHAPE
!-
      CALL BCS(IBIN,CMIN,CPR)
!-
      CALL QGENSPCL(1,0,0, 0.,0.,QDM)
!      
      CALL HGENSPCL(1,0,0, 0.,QDM)
!-
!-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
!-
!
      DO 800 N=1,NP
        IF(NSPL(N) == 1) THEN
          IF(NDEP(N) > 1) THEN
            CALL BFORM(N)
            NL=NREF(N)+1
            NT=NL+NDEP(N)-2
            DO 790 M=NL,NT
              CALL BFORM(M)
  790       CONTINUE
          ENDIF
        ENDIF
  800 CONTINUE
!
!-
      RETURN
!-
!-.....INPUT DATA CARD FORMATS.....
!-
 5005 FORMAT( 20A4 )
!ipk jan94 new format
 5006 FORMAT( A80)
 5010 FORMAT( F8.0)
 5031 FORMAT( F8.0,2I8,F8.0)
 5011 FORMAT(9(I5,3I1))
 5050 FORMAT(I8,I7,I1,6E8.0)
 6050 format(i8,i7,i1,6f10.3)
 6000 FORMAT( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRA&
     &M RMA-10 '/ 10X, 'THREE-DIMENSIONAL HYDRODYNAMICS  WITH SALINITY-T&
     &EMPERATURE-SEDIMENT'/18X,'VERSION 3.5d MAY 2006')
 6145 FORMAT( 1H1 / 10X, '*** BOUNDARY CONDITION UPDATE AT TIME =',     &
     &  F10.2, ' HOURS...CYCLE NUMBER', I5 //                           &
     &  6X, 'NODE       FIX    X-FLOW    Y-FLOW      ELEV      CONC')
!ipk APR96 new format below
 6156 format(/'     TIME STEP SET TO',F6.2,' UNTIL YEAR',I5,' DAY',I5,  &
     &' HOUR',F7.2)
 6999   FORMAT('UNABLE TO FIND LINE ',2X,A2)
!ipk Au02 expand dlin to 80 char
!ipk mar05 7000 FORMAT(A8,A80)
 7000 FORMAT(A8,A72)
      END
