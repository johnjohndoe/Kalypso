!IPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
!IPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
!IPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
!ipk  last update Dec 9 1999 Allow for extra BC lines
!ipk  last update Mar 23 1998
!ipk  last update Nov 26 1997
!ipk  last update Oct 29 1996
!IPK  LAST UPDATE OCT 16 1996
!     Last change:  MD   14 Jul 2009    1:49 pm
!ipk  last updated April 25 1996
!IPK  LAST UPDATED SEP7 1995
      SUBROUTINE GETBC(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
!
      use SchwarzIterationControl_Helper
      use mod_ContiLines
      SAVE
!
!MD: Local Varaible for checking BC-data
      INTEGER :: Check_BC_Data
      integer :: istat 
!
!------------------------------------------------------------------------
! Description of this ROUTINE GETBC(IBIN)
!------------------------------------------------------------------------
!MD:  SubR to read unsteady data out of CONTROL for iterations: Main routine
!MD    to read boundary conditions and equations, that should be solved.
!MD    Here the lines DT, BC, BN are read. For Boundary conditions seperate
!MD    SubRs are called like: SBGEN, QGEN a.s.o
!------------------------------------------------------------------------
!
!-
!ipk apr96  drop initialization of TET
!      TET=TSTART
      IF(NITI > 0) THEN
        NITA=NITI
      ELSE
        NITA=NITN
      ENDIF
!
!
!EFa jun07, necessary for autoconverge      
      nitazero=niti
      call feldgroesse(1,90)
!-      
!
!IPKFEB94    NOTE THAT ALL READS FROM NOW ON WILL USE IBIN NOT LIN
!
      IBK=0
!ipk apr96 define save parameter
      isvs=0
  570 CONTINUE
!ipk apr96 keep track of data for end of time step
      if(iyend+idye+hrend == 0) then
        isvs=1
        rewind nscrin
      elseif(iyrr == iyend .AND. dayofy == idye) then
        if(abs(tet-hrend) < 0.001) then
          isvs=1
          ibin=ibinrst
        else
          ibin=nscrin
          rewind ibin
          isvs=0
        endif
      else
        ibin=nscrin
        rewind ibin
        isvs=0
      endif
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
!IPK SEP04 CREATE ERROR FILE
!nis,sep07          
!ERROR - ERROR IN DYNAMIC BOUNDARY CONDITION FILES          
          CALL ZVRS(1)
          call ErrorMessageAndStop(1402, 0, 0.0d0, 0.0d0)
!-          
        ENDIF
      IBIN=IBUP
      ibinrst=ibin
      GO TO 570
  198 CONTINUE
!
!
!Read time step length
!---------------------
!
!ipk apr96 end changes
!
!TODO: Why is there any need for time step definitions in a steady step?      
      IF(ID(1:2) /= 'DT')                                               &
     &  call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'DT')
!
!ipk apr96 add ending time for time step
!ipk mar98 remove iwind 
!READ(DLIN,5031) DELT,iyend,idye,hrend      
!FIXME: We need a generatlized input technique!      
      istat = 0
      READ(DLIN,*, iostat = istat) DELT,iyend,idye,hrend
      if (istat /= 0) then
        istat = 0
        read (dlin, *, iostat = istat) DELT
        if (istat /= 0) stop 'DT line set up wrongly'
      endif
!
      if(ibin /= nscrin) then
        write(lout,6156) delt,iyend,idye,hrend
      endif
      if(isvs == 1) then
        rewind nscrin
        write(nscrin,7000) id,dlin
      endif
!
!c      TTT=TET+DELT
!c      TETH=TETH+DELT
      TTT=TET
      WRITE(LOUT,6145) TTT,ICYC
!
!Distinguish between unit of time step length:      
!---------------------------------------------      
!HOURS      
      if (ID (4:6) == 'HHH') then
        DELT = DELT * 3600
!
!MINUTES      
      elseif (ID (4:6) == 'MIN') then
        DELT = DELT * 60
!
!SECONDS      
      elseif (ID (4:6) == 'SEC') then
        continue
!MILLISECONDS      
      elseif (ID (4:6) == 'MSE') then
        DELT = DELT / 1000
!HOURS (standard assumption)      
      else 
        DELT = DELT * 3600
      endif
!
!
!-.... Read iteration controls
!
      IF(NITI > 0) THEN
        NITA=NITI
      ELSE
        NITA=1
      ENDIF
!ipk nov97       READ(IBIN,7000) ID,DLIN
      call ginpt(ibin,id,dlin)
      write(*,'(a8,a72)') id,dlin
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
!
        read(dlin,'(2f8.0)') sadx,sadel
!
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
        call ginpt(ibin,id,dlin)
          CALL REVAO
          ISAD=1
        else
          ISAD=0
        sadx=1.0
        endif
!
!
      IF(ID(1:2) /= 'BC') THEN
!MD: check if old Iteration-Data is available       
        DO I = 1, NITN
          Check_BC_Data = IURVL(I)+ITLVL(I)+ITEQV(I)+ITEQS(I)
!MD: Allow to use old Iteration-Data, if no new block is          
!MD:   defined in CONTROL          
!
!ERROR, because BC line is expected          
          IF (Check_BC_Data == 0)                                       &
     &      call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'BC')
        ENDDO
        goto 315
!MD: New Jump, because next line was already read by ginpt(..)        
!
      Elseif(ID(1:2) == 'BC') THEN
!IPK OCT96 CLEAN UP STEADY CASE
        READ(DLIN,5011)                                                 &
     &         (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=1,9)
        IF(NITA > 9) THEN
          N1=1
  199     N1=N1+9
          N2=N1+8
!ipk nov97        READ(IBIN,7000) ID,DLIN
          call ginpt(ibin,id,dlin)
!ipk apr96 save data to a scratch file
          if(isvs == 1) then
            write(nscrin,7000) id,dlin
          endif
!
!ERROR, because BC line is expected          
          IF(ID(1:2) /= 'BC')                                           &
     &      call ErrorMessageAndStop (1801, 0, 0.0d0, 0.0d0, 'BC')
!
          READ(DLIN,5011)                                               &
     &          (IURVL(I),ITLVL(I),ITEQV(I),ITEQS(I),I=N1,N2)
          IF(NITA > N2) GOTO 199
        ENDIF
      Endif
!-
!-..... READ BOUNDARY VALUES
!-
  210 CONTINUE
!ipk nov97      READ(IBIN,7000) ID,DLIN
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
!
!ipk apr96 save data to a scratch file
      if(isvs == 1) then
        write(nscrin,7000) id,dlin
      endif
      IF(ID(1:2) == 'BN') THEN
        READ(DLIN,5050) N,NFIX(N),NFIX1(N),(SPEC(N,M),M=1,NDF)
        WRITE(LOUT,6155)N,NFIX(N),NFIX1(N),(SPEC(N,M),M=1,NDF)
        GO TO 210
      ENDIF    
!
!...... Go to read boundary conditions along line and wind data
!
!      if (iaccyc <= icyc .OR. iaccyc == 1 .OR. iaccyc == 0) then
        CALL SBGEN(IBIN)
!      ELSEIF (iaccyc > icyc) then
!        WRITE(*,*) 'Jump over steady boundary conditions input data'
!        findEndStep: do
!          call ginpt(ibin, id, dlin)
!          if (ID(1:7) == 'ENDSTEP') EXIT findEndStep
!        end do findEndStep
!      end if
!
!ipk jun05 save nfix and test for weir data
!
          do j=1,np
            nfixp(j)=nfix(j)
          enddo
!
        DO 725 J=1,NE
!IPK JUL98 ADD TEST FOR 999
          IF(IMAT(J) > 903 .AND. imat(j) < 999) THEN
            IF(NCORN(J) == 8) THEN
              DO 720 KK=1,8
!IPK MAY00 CHANGES FOR TYPE 10 WEIR
!
!
!      get nodal locations
!
                IF(KK < 4) THEN
                  N1=NOP(J,KK)
                  N2=NOP(J,8-KK)
!ipk sep00
                  alfak(n1)=atan2(cord(n2,2)-cord(n1,2),                &
     &                    cord(n2,1)-cord(n1,1))
                  alfak(n2)=alfak(n1)
                  IF(KK == 1) THEN
                    N2=NOP(J,8)
                    alfak(n2)=alfak(n1)
                  ELSEIF(KK == 3) THEN
                    N2=NOP(J,4)
                    alfak(n2)=alfak(n1)
                  ENDIF
!
                ENDIF
!ipk jun05 skip for matrix entries
                  if(NCTREF(imat(j)) > 0) go to 720
!
                IF(KK == 1) THEN
                  IF(WHGT(NOP(J,1)) < -9999.) THEN
                    WHGT(NOP(J,1))=WHGT(NOP(J,7))
                    WLEN(NOP(J,1))=WLEN(NOP(J,7))
                    TRANSEL(NOP(J,1))=TRANSEL(NOP(J,7))
                  ENDIF
!ipk sep03
                  IF(WHGT(NOP(J,7)) < -9999.) THEN
                    WHGT(NOP(J,7))=WHGT(NOP(J,1))
                    WLEN(NOP(J,7))=WLEN(NOP(J,1))
                    TRANSEL(NOP(J,7))=TRANSEL(NOP(J,1))
                  ENDIF
                  IF(WHGT(NOP(J,3)) < -9999.) THEN
                    WHGT(NOP(J,3))=WHGT(NOP(J,5))
                    WLEN(NOP(J,3))=WLEN(NOP(J,5))
                    TRANSEL(NOP(J,3))=TRANSEL(NOP(J,5))
                  ENDIF
!ipk sep03
                  IF(WHGT(NOP(J,5)) < -9999.) THEN
                    WHGT(NOP(J,5))=WHGT(NOP(J,3))
                    WLEN(NOP(J,5))=WLEN(NOP(J,3))
                    TRANSEL(NOP(J,5))=TRANSEL(NOP(J,3))
                  ENDIF
                ENDIF
                IF(MOD(KK,2) == 0) THEN
                  N2=MOD(KK+1,8)
                  WHGT(NOP(J,KK))=                                      &
     &             (WHGT(NOP(J,KK-1))+WHGT(NOP(J,N2)))/2.
                  WLEN(NOP(J,KK))=                                      &
     &             (WLEN(NOP(J,KK-1))+WLEN(NOP(J,N2)))/2.
                  TRANSEL(NOP(J,KK))=                                   &
     &             (TRANSEL(NOP(J,KK-1))+TRANSEL(NOP(J,N2)))/2.
                ENDIF
  720         CONTINUE
            ELSEIF(NCORN(J) == 3) THEN
!
              N1=NOP(J,1)
              N2=NOP(J,2)
              N3=NOP(J,3)
              IF(WHGT(N1) < -9999.) THEN
                WHGT(N1)=WHGT(N3)
                WLEN(N1)=WLEN(N3)
                TRANSEL(N1)=TRANSEL(N3)
              ELSEIF(WHGT(N3) < -9999.) THEN
                WHGT(N3)=WHGT(N1)
                WLEN(N3)=WLEN(N1)
                TRANSEL(N3)=TRANSEL(N1)
              ENDIF
              IF(WHGT(N2) < -9999.) THEN
                WHGT(N2)=WHGT(N1)
                WLEN(N2)=WLEN(N1)
                TRANSEL(N2)=TRANSEL(N1)
              ENDIF
!
            ENDIF
!
          ENDIF
  725   CONTINUE
!ipkoct93 4.5 end of additions
!
!
!ipk jul04 add test for weir heights
!
        DO J=1,NP
          IF(WHGT(J) > -9000.) THEN
            IF(WHGT(J) < AO(J)+0.1) THEN
              WRITE(*,6190) j,whgt(j)-ao(j)
              WRITE(75,6190) j,whgt(j)-ao(j)
!ipk jul04
 6190 FORMAT('Warning weir height clearance less that 0.1  node',i5,    &
     &       ' Clearance =', f7.2)
            ENDIF
          ENDIF
        ENDDO
!
!
!...... Set ITEQV based on input
!-
        SetCalcType: DO I = 1, NITA
          IF (ITEQS (I) > 0) THEN
            IF (ITEQV (I) == 0) THEN
              ITEQV (I) = ITEQS (I) + 5
            ENDIF
            IF(ITEQV (I) == 2) THEN
              ITEQV (I) = ITEQS (I) + 7
            ENDIF
            IF (ITEQV(I) == 3) THEN
!nis,jul08: in the unsteady reading subroutine inputd +9 is replaced by +10. This leads to the values 11 and 12 for calculations
!           of velocities and concentrations (11: temperature; 12: sediment)
!
!           The numbering gap ITEQV = 10 is open for 3D-calculations (it seems; referencing to update subroutine!)
!
!              ITEQV (I) = ITEQS (I) + 9
              ITEQV (I) = ITEQS (I) + 10
!-
            ENDIF
          ENDIF
        ENDDO SetCalcType
!
!
!ipk dec00   test for gate structure
      do n=1,ne
        if(imat(n) > 903 .AND. imat(n) < 1000) then
          if(ndupj(imat(n)-900) /= 0 .OR.                               &
     &       ndflj(imat(n)-900) /= 0) then
              igtp(n) = imat(n)-900
!ipk jun05
          else
              nfctp(n)= imat(n)-900
            endif
!
        endif
      enddo
!-
!...... Call routine to establish boundary conditions and angles
!-
      call setInnerBC_NFIX (ccls, ncl, nfix)
      CALL BLINE(0)
!-
      RETURN
 5010 FORMAT(F8.0)
!ipk oct96
 5011 FORMAT(9(I5,3I1))
!ipk mar98
 5031 FORMAT( F8.0,2I8,F8.0)
 5050 FORMAT(I8,I7,I1,6E8.0)
!IPK APR96 ADD FORMATS
 6145 FORMAT(/10X, '*** BOUNDARY CONDITION UPDATE AT TIME =',           &
     &  F10.2, ' HOURS... STEP', I5 /)
 6155 FORMAT(2I10,I1,6F10.2 )
!ipk APR96 new format below
 6156 format(/'     TIME STEP SET TO',F6.2,' UNTIL YEAR',I5,' DAY',I5,  &
     &' HOUR',F7.2)
 6999   FORMAT('UNABLE TO FIND LINE ',A8)
 7000 FORMAT(A8,A72)
      END
