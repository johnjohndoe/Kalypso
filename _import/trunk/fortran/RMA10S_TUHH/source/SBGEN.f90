!ipk  last update dec 13 2006 setup for type QI input (experimental)
!IPK  LAST UPDATE SEP 6 2004  add error file
!IPK  LAST UPDATE AUG 22 2001 ADD FOR POWER STATION RECYCLING
!IPK  LAST UPDATE APR 18 2001 MAKE THE HSPEC CONSISTENTLY "LN"
!IPK  LAST UPDATE FEB 26 2001 REVISE GATE INPUT
!IPK  LAST UPDATE DEC 21 2000 INPUT GATE DATA
!IPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
!ipk  last update Feb 4 1998
!IPK  LAST UPDATE DEC 16 1997
!IPK  LAST UPDATE NOV 26 1997
!     Last change:  MD   20 Aug 2008   10:21 am
!IPK  LAST UPDATED AUGUST 28 1995
      SUBROUTINE SBGEN(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      USE PARAMMOD
      USE BLKSBG
!EFa Dec06, neues Modul für 1d-Teschke-ELemente      
      USE Para1DPoly
!EFa jun07, modul necessary for autoconverge      
      USE parakalyps
      use blk_ifu
      SAVE
!-
!...... Input special boundary conditions
!-
!IPKJAN94 V4.5
!IPK DEC97      PARAMETER (NQBS=20)
!IPKJAN94 V4.5 END CHANGE
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN
      DIMENSION QQAL(3)
!IPKFEB94 V4.5
!IPK DEC97 CHANGE TO PERMIT PARAMETER DEFINITION
!
!      ALLOCATABLE NHYDQ(:),QDIRS(:),NHYDH(:),NHYDG(:)
!     +         ,NLAYH(:),NLAYQ(:),IINTERP(:),IQID(:)
!IPK AUG03 MOVE TO BLK11.COM     +         ,WNDSP(MNP),WNDDR(MNP)
!
      DATA ISTDY/0/,ITIMEH/0/
!
!read/write error catching      
      integer :: ioError
!
      IF(ITIMEH == 0) THEN
      ALLOCATE (NHYDQ(NQLDS),QDIRS(NQLDS),NHYDH(NHDS),NHYDG(NHDS)       &
     &         ,NLAYH(NHDS),NLAYQ(NQLDS),IINTERP(NHDS),IQID(NQLDS))
        ITIMEH=1
      ENDIF
!
!IPKFEB94 V4.5
      IF(ISTDY == 0) THEN
        DO N=1,NQLDS
          NHYDQ(N)=0
          NLAYQ(N)=0
!IPK DEC06
          IQID(N)=0          
        ENDDO
!IPK DEC97 CHANGE INITIAL DEF LOOP LIMIT
        DO N=1,NHDS
          NHYDG(N)=0
          NHYDH(N)=0
          NLAYH(N)=0
        ENDDO
        ISTDY=1
      ENDIF
!IPKFEB94 V4.5  END CHANGE
!
!IPK NOV97 DROP CALLS FOR ELEMENT INFLOWS
!
!-
!...... Input Q for a continuity line if required
!-
      IQGEN=0
!IPK DEC97 CHANGE LIMITS
      DO N=1,NQLDS
        IF(NHYDQ(N) > 0) THEN
          IQGEN=IQGEN+1
          IF(IQGEN == 1) THEN
            WRITE(LOUT,6033)
          ENDIF
          J=NHYDQ(N)
          LN=NLAYQ(IQGEN)
!IPK AUG95          TIME=TET+DELT
          TIME=TET
!ipk APR96  add logic for day of year
          IF(TIME > 24.) THEN
            DAYNOW=DAYOFY+1
            TIME=TIME-24.
          ELSE
            DAYNOW=DAYOFY
          ENDIF
          IF(MOD(IYRR,4) == 0) THEN
            IYDAYS=366
          ELSE
            IYDAYS=365
          ENDIF
          IYRN=IYRR
          IF(DAYNOW > IYDAYS) THEN
            IYRN=IYRR+1
            DAYNOW=DAYNOW-IYDAYS
          ENDIF
!EFa sep08, for calculation of qdir and output of external BC          
!           deactivated QDIR = QDIRS(N)          
!CALL GETQ(LN,J,QF,IYRN,DAYNOW,TIME,TETH,QQAL)          
!EFa jun09
          CALL GETQ(LN,J,QF,IYRN,DAYNOW,TIME,TETH,QQAL,QDIR) 
!QDIR=QDIRS(N) !EFa, dieses dann wieder deaktivieren, wenn mit altem qdir, jun09          
!-          
          WRITE(LOUT,6034) J,LN,QF,QDIR,QQAL
          IF (LN == 0) THEN
            CALL QGEN(J,QF,QDIR,QQAL)
          ELSE
!
!            Apply flows to single 3d layer
!
             call QGENSPCL(0,LN,J,QF,QDIR,QQAL)
!      
          ENDIF              
        ENDIF
      ENDDO
  204 CONTINUE
!ipk dec06
      IF(ID(1:2) == 'QC' .OR. ID(1:2) == 'QM') THEN
!nis,nov06: testing against present line-coupling. If so, stop the program and give an error message        
!find out the ID-number        
        READ(DLIN,'(i8)') J
!
!Test for every 1D-2D-line transition        
        do i = 1, MaxLT
!test,whether it is present in one coupling          
          if (TransLines(i,2) == J)                                     &
     &      call ErrorMessageAndStop (1409, 0, 0.0d0, 0.0d0)
        enddo
!
!Count boundary conditions and check for amount        
        IQGEN=IQGEN+1
        IF(ID(1:2) == 'QM') THEN
          IQID(IQGEN)=1
        ELSE
          IQID(IQGEN)=0
        endif
!
!Error, because of too many inflows        
        IF(IQGEN > NQLDS)                                               &
     &    call ErrorMessageAndStop (1408, 0, 0.0d0, 0.0d0)
!
!Write header for output-file, that includes a line with a headers for all columns        
        IF(IQGEN == 1) WRITE(LOUT,6033)
!
!If no hydrograph file for that boundary condition, then read values out of control file        
        IF(NHYDQ(IQGEN) == 0) THEN
!
        ioError = 0
        if (ID(3:4) == 'FF' .OR. (ID (3:4) == ' *')) then
          READ (DLIN, *, iostat = ioError) J, LN, QF, QDIR, QQAL, labl
!
!labl is not written in Kalypso          
          if (ioError /= 0) then
            ioError = 0
            labl = 0
            READ (DLIN, *, iostat = ioError) J, LN, QF, QDIR, QQAL
          endif 
!
        else
          READ(DLIN,5034) J,LN,QF,QDIR,QQAL,LABL
        endif
!nis,sep06,com: meaning of the values          
! J    = number of CCL, where discharge runs through          
! LN   = layer number for the specified discharge; enter 0 for 1D or 2D depth averaged flow          
! QF   = total discharge crossing the CCL          
! QDIR = direction of total discharge crossing the CCL; measured from the principal x-axis in ANTICLOCKWISE RADIANS)          
! QQAL = 3 values (array) for the three concentrations of salinity, temperature and sediment          
! LABL = 1 for using a hydrograph file in dynamic solution; chose 0 for a steady or specified solution          
!
!Check for i/o error        
        if (ioError /= 0)                                               &
     &    call ErrorMessageAndStop (1410, 0, 0.0d0, 0.0d0)
!
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!EFa jul07, necessary for autoconverge
!          specccfut(j,1) = 2.5
!          specccfut(j,2) = qf
!          specccfut(j,3) = qdir
!          specccfut(j,6) = qqal(1)
!          specccfut(j,7) = qqal(2)
!          specccfut(j,8) = qqal(3)
!          !-
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!
!
!IPK NOV97          READ(IBIN,7000) ID,DLIN
!nis,sep06,com: read new file line into memory          
          call ginpt(ibin,id,dlin)
!ipk APR96 save data to a scratch file
!nis,sep06,com: Scratch file storage if appropriate          
          if(isvs == 1) then
            write(nscrin,7000) id,dlin
          endif
!nis,sep06,com: Control output          
          WRITE(LOUT,6034) J,LN,QF,QDIR,QQAL,LABL
!
!nis,sep06,com: Power Station control, not for standard inflow          
!IPK AUG01   ADD FOR POWER STATION RECYCLING
          IF(NOUTCC(J) /= 0 .AND. ICYC > 0) THEN
            IF(AVES(NOUTCC(J)) > -100.) THEN
              IF(AVES(NOUTCC(J)) > 0.) THEN
                  QQAL(1)=AVES(NOUTCC(J))*(1.+ADDSAL(J))
              ENDIF
            ENDIF
            IF(AVET(NOUTCC(J)) > -100.) THEN
              TINC=1000.*ADDTMP(J,1)/(ADDTMP(J,3)*                      &
     &               (1000.-0.0178*(ABS(AVET(NOUTCC(J))-4.0))**1.7)*    &
     &             QF*4.19)
              IF(TINC > ADDMAX(J)) TINC=ADDMAX(J)
              IF(AVET(NOUTCC(J)) > 0.) THEN
                    QQAL(2)=AVET(NOUTCC(J))+TINC
              ENDIF
            ENDIF
            IF(AVESD(NOUTCC(J)) > -100.) THEN
              IF(AVESD(NOUTCC(J)) == 0.) THEN
                  QQAL(3)=AVESD(NOUTCC(J))*(1.+ADDSED(J))
              ENDIF
            ENDIF
            WRITE(75,*) 'POWER',J,QF,TINC,QQAL
          ENDIF
!
!nis,sep06,com: Call QGEN for not 3D-continuity line outflow/inflow          
! Passed variables are:          
! J    = number of CCL          
! QF   = total discharge of inflow          
! QDIR = direction of total discharge crossing the CCL; measured from the principal x-axis in ANTICLOCKWISE RADIANS)          
! QQAL = 3 values (array) for the three concentrations of salinity, temperature and sediment          
!-          
          IF (LN == 0) THEN
             CALL QGEN(J,QF,QDIR,QQAL)
!nis,sep06,com: Call QGEN for 3D-continuity line outflow/inflow          
          ELSE
!
!            Apply flows to single 3d layer
!
             call QGENSPCL(0,LN,J,QF,QDIR,QQAL)
!      
          ENDIF              
!
!nis,sep06,com:          
          IF(LABL == 1) THEN
 5034       FORMAT(2I8,2F8.1,3F8.2,I8)
            NHYDQ(IQGEN)=J
            NLAYQ(IQGEN)=LN
            QDIRS(IQGEN)=QDIR
            WRITE(LOUT,6030) J
 6030       FORMAT ( 'DYNAMIC HYDROGRAPH WILL BE INTERPOLATED ',        &
     &     'FOR LINE',I4)
          ENDIF
          GO TO 204
        ENDIF
      ENDIF
!nis,sep06,com: End of discharge-boundary condition for a continuity line
!
!
!-
!...... Input head for a continuity line if required
!-
      IHGNN=0
!IPK DEC97 MODIFY LIMITS     DO  N=1,NQBS
      DO N=1,NHDS
!water level boundary condition with hydrograph file        
        IF(NHYDH(N) /= 0) THEN
          IHGNN=IHGNN+1
          IF(IHGNN == 1) WRITE(LOUT,6035)
          J=NHYDH(N)
          TIME=TET
!ipk APR96  add logic for day of year
          IF(TIME > 24.) THEN
            DAYNOW=DAYOFY+1
            TIME=TIME-24.
          ELSE
            DAYNOW=DAYOFY
          ENDIF
          IF(MOD(IYRR,4) == 0) THEN
            IYDAYS=366
          ELSE
            IYDAYS=365
          ENDIF
          IYRN=IYRR
          IF(DAYNOW > IYDAYS) THEN
            IYRN=IYRR+1
            DAYNOW=DAYNOW-IYDAYS
          ENDIF
           LN=NLAYH(N)
!IPK AUG95          TIME=TET+DELT
!IPK AUG00   ADD HF2                
!EFa oct09, hfd          
!CALL GETH(LN,ABS(J),HF,HF2,IYRN,DAYNOW,TIME,TETH,QQAL)          
          CALL GETH(LN,ABS(J),HF,HF2,IYRN,DAYNOW,TIME,TETH,QQAL,hfd)   
!-          
          IF(IINTERP(IHGNN) == 0) THEN
!EFa oct09, hfd          
!WRITE(LOUT,5032) J,LN,HF,QQAL            
            if(hfd == 0.0) then
              WRITE(LOUT,5032) J,LN,HF,QQAL
            else
              WRITE(LOUT,5033) J,LN,HF,QQAL,hfd
            endif
!-          
          ELSE
            WRITE(LOUT,5032) J,LN,HF,HF2,QQAL
          ENDIF
          IF (LN == 0)  THEN
!IPK DEC97 CALLING SEQUENCE CHANGED
!EFa oct09, testing hfd             
             CALL HGEN(IBIN,J,HF,HF2,IINTERP(IHGNN),QQAL,hfd)
!-             
          ELSE
!
!  Apply elev bc to single 3d layer
!
             CALL HGENSPCL(0,LN,J,HF,QQAL)
          ENDIF
        ELSEIF(NHYDG(N) /= 0) THEN
          IHGNN=IHGNN+1
          IF(IHGNN == 1) WRITE(LOUT,6035)
          J=NHYDG(N)
          LN=NLAYH(N)
!IPK AUG95          TIME=TET+DELT
          TIME=TET
!ipk APR96  add logic for day of year
          IF(TIME > 24.) THEN
            DAYNOW=DAYOFY+1
            TIME=TIME-24.
          ELSE
            DAYNOW=DAYOFY
          ENDIF
          IF(MOD(IYRR,4) == 0) THEN
            IYDAYS=366
          ELSE
            IYDAYS=365
          ENDIF
          IYRN=IYRR
          IF(DAYNOW > IYDAYS) THEN
            IYRN=IYRR+1
            DAYNOW=DAYNOW-IYDAYS
          ENDIF
          CALL GTIDE(HF,IYRR,DAYNOW,TIME,TETH,QQAL)
          LN=0
!EFa oct09, hfd          
!WRITE(LOUT,5032) J,LN,HF,QQAL          
          if(hfd == 0.) then
            WRITE(LOUT,5032) J,LN,HF,QQAL
          else
            WRITE(LOUT,5033) J,LN,HF,QQAL,hfd
          endif
!-          
          IF (LN == 0)  THEN
!IPK DEC97 CALLING SEQUENCE CHANGED
!EFa oct09, testing hfd             
             CALL HGEN(IBIN,J,HF,HF2,0,QQAL,hfd)
!-             
          ELSE
!
!  Apply elev bc to single 3d layer
!
!IPK APR01
             CALL HGENSPCL(0,LN,J,HF,QQAL)
          ENDIF
        ENDIF
      ENDDO
 208  CONTINUE
!IPK AUG00
!IPK JUN03
      IF (ID (1:2) == 'HC' .OR. ID (1:2) == 'HI' .OR.                   &
     &    ID (1:2) == 'HJ' .OR. ID (1:2) == 'HS') THEN
!HC - normal water level boundary condition with constant water level        
!HI - water level boundary condition, where the water level is linearily distributed between two values        
!     from the outer nodes of the continuity line        
!HJ - same as the HI        
!HS - volume - water level boundary condition; practically it is a water level boundary condition, only that        
!     the water level is calcualed on the fly depending on the volume inside the polder        
!
!Get line number        
        READ (DLIN,'(i8)') J
!
!test for every 1D-2D-line-transitions        
        do i = 1, MaxLT
!test,whether it is present in one coupling          
          if (TransLines (i, 2) == J)                                   &
     &      call ErrorMessageAndStop (1407, J, 0.0d0, 0.0d0)
        enddo
!Count occurances of H-Boundaries        
        IHGNN = IHGNN + 1
!
!HC stands for 'normal' H-BCs, i.e. there will be no interpolation between two values        
        IF (ID (1:2) == 'HC') THEN
          IINTERP (IHGNN) = 0
        elseif (id (1:2) == 'HS') then
          iinterp (ihgnn) = 3
!
!HJ or HI are standing for H-BCs, where value is interpolated linearily between two values        
!IINTERP (IHGNN) == 1 and IINTERP (IHGNN) == 2 has the same meaning!        
        ELSEIF (ID (1:2) == 'HJ') THEN
          IINTERP (IHGNN) = 2
!HI
        ELSE 
          IINTERP (IHGNN) = 1
        ENDIF
!
!TEST FOR TOO MANY ELVATION SPECS        
        IF (IHGNN > NHDS)                                               &
     &    call ErrorMessageAndStop (1406, 0, 0.0d0, 0.0d0)
!
!write header for output        
        IF (IHGNN == 1) WRITE (LOUT, 6035)
!
        if (id (1:2) == 'HS') then
          read (dlin, *, iostat = ioError) j, ln, qqal
        else
!evaluate read in line of input file          
          read (dlin, 5031, iostat=ioError) j, ln, hf, qqal, labl, hf2
 5031     format (i8, i8, f8.0, 3f8.0, i8, f8.0)
        endif
!
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jul07, necessary for autoconverge
!        specccfut(j,1) = 1.5
!        specccfut(j,2) = hf
!        specccfut(j,3) = hf2
!        specccfut(j,4) = ibin
!        specccfut(j,5) = iinterp(ihgnn)
!        specccfut(j,6) = qqal(1)
!        specccfut(j,7) = qqal(2)
!        specccfut(j,8) = qqal(3)
!        !-
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!
!ipk nov97        READ(IBIN,7000) ID,DLIN
        call ginpt(ibin,id,dlin)
!
!TODEL: save data to a scratch file        
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
!
!IPK AUG00
        IF(IINTERP(IHGNN) == 0) THEN
!write out read data line          
!EFa oct09, hfd          
!WRITE(LOUT,5032) J,LN,HF,QQAL          
          if(hfd == 0.) then
            WRITE(LOUT,5032) J,LN,HF,QQAL
          else
            WRITE(LOUT,5033) J,LN,HF,QQAL,hfd
          endif
        ELSE
!IPK APR01
          WRITE(LOUT,5032) J,LN,HF,HF2,QQAL
        ENDIF
 5032   FORMAT(2I10,5F15.3)
 5033   FORMAT(2I10,6F15.3)
!
!
!switch for boundary condition to interpolate values from tidal graph or harmonic graph        
        IF(LABL == 1) THEN
          NHYDH(IHGNN)=ABS(J)
          NLAYH(IHGNN)=LN
          WRITE(LOUT,6031) J
 6031     FORMAT ( 'DYNAMIC TIDALGRAPH WILL BE INTERPOLATED ',          &
     &    'FOR LINE',I4)
        ELSEIF(LABL == 2) THEN
          NHYDG(IHGNN)=ABS(J)
          NLAYH(IHGNN)=LN
          WRITE(LOUT,6032) J
 6032     FORMAT ( 'DYNAMIC TIDES WILL BE GENERATED FROM HARMONICS',    &
     &    ' FOR LINE',I4)
        ENDIF
!
!Application of boundary conditions to all layers        
        if (ln == 0)  then
!determine the boundary condition settings          
!EFa oct09, testing hfd          
          call hgen (ibin, j, hf, hf2, iinterp (ihgnn), qqal,hfd)
!-          
!application of water level boundary conditions to specific layers        
        else
!Apply elev bc to single 3d layer          
          call hgenspcl(0,ln,j,hf,qqal)
        endif
!TODO: What does JLIN make?        
        IF(J < 0) THEN
          JLIN(IHGNN)=-J
        ELSE
          JLIN(IHGNN)=0
        ENDIF
!
        GO TO 208
      ENDIF
!
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      do i=1,ncl
!        do k=1,8
!          speccc(i,k)=specccfut(i,k)
!        end do
!      end do
!      !-
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!
!-
!...... Input stage-flow for a continuity line if required
!-
      ISTGEN=0
 212  CONTINUE
      IF(ID(1:3) == 'SQC') THEN
!
!nis,nov06: testing against present line-coupling. If so, stop the program and give an error message        
!find out the ID-number        
        READ(DLIN,'(i8)') J
!Test for every line Transition        
        do i = 1, MaxLT
!test,whether it is present in one coupling          
          if (TransLines(i,2) == J) then
            WRITE(*,*) 'ERROR!!!'
            WRITE(*,*) 'Transition Line has specified value!'
            WRITE(*,*) 'There can be no specified WL-Q-RELATIONSHIP.'
            WRITE(*,*) 'Leave Transition line without conditions!'
            stop
          end if
        enddo
!-        
!
        ISTGEN=ISTGEN+1
        READ(DLIN,5035) J,AC1,AC2,AC3,AC4,AC5,QQAL
        IF(ISTGEN == 1) WRITE(LOUT,6045)
        WRITE(LOUT,6036) J,AC1,AC2,AC3,AC4,AC5,QQAL
!ipk nov97        READ(IBIN,7000) ID,DLIN
        call ginpt(ibin,id,dlin)
!ipk APR96 save data to a scratch file
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
        CALL STGEN(J,AC1,AC2,AC3,AC4,AC5,QQAL)
        GO  TO 212
      ENDIF
!-
!-      Input salinity/temp/sed for a continuity line if required
!-
      ITGEN=0
 213  CONTINUE
!ipk feb98 revised option to CQ
      IF(ID(1:2) == 'CQ') THEN
        ITGEN=ITGEN+1
        IF(ITGEN == 1) WRITE(LOUT,6048)
        READ(DLIN,5035) J,QQAL
        WRITE(LOUT,6036) J,QQAL
!ipk nov97        READ(IBIN,7000) ID,DLIN
        call ginpt(ibin,id,dlin)
!ipk APR96 save data to a scratch file
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
!
        CALL SGEN(J,QQAL)
!
        GO TO 213
      ENDIF
!
!read wind data per time step and calculate the wind shear stress      
!is related to 'WVA' or 'WVN' line or explicite wind file      
      call windf (ibin)
!
!IPK DEC00
!-
!..... Input properties flow flow control elements
!-
  230 CONTINUE
      NCFLW=0
      IF(ID(1:2) == 'FC') THEN
        NCFLW=NCFLW+1
        READ(DLIN,5040) NJN,NJT1,AJ1,BJ1,CJ1,GAM1,QD1,DJ1
        WRITE(LOUT,6040) NJN,NJT1,AJ1,BJ1,CJ1,GAM1,QD1,DJ1
        call ginpt(ibin,id,dlin)
!ipk jan96 save data to a scratch file
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
        NJT(NJN-900)=NJT1
        AJ(NJN-900)=AJ1
        BJ(NJN-900)=BJ1
        CJ(NJN-900)=CJ1
        GAMJ(NJN-900)=GAM1
        QD(NJN-900)=QD1
        DJ(NJN-900)=DJ1
        GO TO 230
      ENDIF
!
  240 CONTINUE
      NCGT=0
      IF(ID(1:2) == 'GT') THEN
        NCFLW=NCFLW+1
!IPK FEB01  ADD EXTRA PARAMETER
        READ(DLIN,5045) NJN,NDUP,NDDN,AJ1,BJ1,NDFL
        WRITE(LOUT,6044) NJN,NDUP,NDDN,AJ1,BJ1,NDFL
        call ginpt(ibin,id,dlin)
!ipk jan96 save data to a scratch file
        if(isvs == 1) then
          write(nscrin,7000) id,dlin
        endif
        NDUPJ(NJN-900)=NDUP
        NDDNJ(NJN-900)=NDDN
!IPK FEB01 STORE NEW BVALUE
        NDFLJ(NJN-900)=NDFL
        AJ(NJN-900)=AJ1
        BJ(NJN-900)=BJ1
        NJT(NJN-900)=20
        GO TO 240
!
      ENDIF
      RETURN
!-
 5029 FORMAT( 3I8, 4F8.0, I8 )
 5035 FORMAT( I8, 9F8.0 )
 5040 FORMAT(2I8,6F8.0)
!IPK FEB01
 5045 FORMAT(3I8,2F8.0,I8)
 6019 FORMAT(/' ALL ELEMENTS HAVE DISTRIBUTED INFLOW'/                  &
     & /'               LAYER      INFLOW  INFLOW-QUAL'/                &
     &I20,4F12.3)
 6020 FORMAT(/' THE FOLLOWING ELEMENTS HAVE DISTRIBUTED INFLOW'/        &
     & /'    ELT NO     LAYER      INFLOW  INFLOW-QUAL')
 6021 FORMAT(2I10,4F12.3)
 6033 FORMAT(/'   BOUNDARY CONDITION DEFINED AS FLOW ACROSS A LINE'//   &
     &'      LINE     LEVEL      FLOW DIRECTION        QUALITY')
 6034 FORMAT(2I10,5F10.1,I10)
 6035 FORMAT(/'   BOUNDARY CONDITION DEFINED AS ELEV ALONG A LINE'//    &
     &'      LINE     LEVEL           ELEV                       QUALITY&
     &')
 6036 FORMAT(I10,8F15.1)
 6038 FORMAT(I11,F15.5)
 6029 FORMAT(/'   GLOBAL ELEMENT INFLOW'//'           FLOW')
 6040 FORMAT(/'   PROPERTIES OF FLOW CONTROL STRUCTURE NUMBER',I6//     &
     &'  TYPE        AJ        BJ        CJ       GAM  FLOW-DIR.        &
     &   DJ'/                                                           &
     &    I6,4F10.2,2F10.3)
 6041 FORMAT(/'   INDIVIDUAL ELEMENT INFLOW'//'   ELEMENT',             &
     &'           FLOW')
 6042 FORMAT(' ALL EXCEPT',F15.5)
 6043 FORMAT(/'   INTERPOLATED ELEMENT INFLOW'//'    ELEMENT',          &
     &'           FLOW')
!IPK FEB01
 6044 FORMAT(/'   PROPERTIES OF GATE STRUCTURE NUMBER',I6//             &
     &'      NDUP      NDDN        AJ        BJ     NDFLJ'/             &
     &    2I10,2F10.2,I10)
 6046 FORMAT(/'   WIND SPEED =',F8.1,'  DIRECTION =',F8.0,' DEGREES'/)
 6045 FORMAT(/'   BOUNDARY CONDITION DEFINED AS STAGE FLOW ALONG A LINE'&
     &//'      LINE             A1             A2             E0',      &
     &            '             C       DIRECTION        QUALITY')
 6048 FORMAT(/'   BOUNDARY CONDITION DEFINED AS SAL-TEMP-SED ON A LINE '&
     &//'      LINE        SALINITY    TEMPERATURE       SEDIMENT')
 7000 FORMAT(A8,A72)
      END
