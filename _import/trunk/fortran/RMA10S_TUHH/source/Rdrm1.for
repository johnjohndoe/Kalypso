C     Last change:  AF   13 Jul 2006   11:00 am
CIPK  LAST UPDATE SEP 6 2004  add error file
      SUBROUTINE RDRM1(NPTEMP,NETEMP,KSWIT)
      USE BLK10MOD

cipk aug05	INCLUDE 'BLK10.COM'
      REAL*8 CX,CY
      CHARACTER DLINE*140,ID1*3,DLIN2*77,BLANK*20
      DIMENSION ILN(8)

      data blank/'                    '/ 

!NiS,may06,comment: (Over)Reading header lines, that are interesting for other programs, only the IFORM-switch in the second line
!                   is interesting for file format
      READ(ifile,'(A80)') TITLE
      READ(IFILE,'(100X,I5)') IFORM
      READ(IFILE,'(A80)') TITLE

!NiS,may06,comment: Initializing the Number of Points and Number of elements arrays
      NP=0   !number of points for allocation of array for points storage
      NE=0   !number of elements for allocation of array for element storage
!-

cipk jun06
      NPTEMP=0
      NETEMP=0	

!NiS,may06,comment: read next line and test for TABS in line
  100 CALL GINPT1(IFILE,DLINE)
!-

!NiS,may06,comment: ELEMENT READING BLOCK
!     reading block one of RM1-geometry file. In that block the element informations are written down:
!     J         = running number of element; user specified ID
!     ILN(1-8)  = nodenumbers of element, anticlockwise direction, altering between corner nodes and midside nodes
!     IMT       = Element type number, for assignment of roughnesses (roughness classes)
!     EDIR      = principal axis-direction
!     INU       = running number of element internal for the program; user has no influence. This number is different from J, if there was a
!                 reordering sequence
!-
      READ(DLINE,'(10I5,F10.3,I5)') J,ILN,IMT,EDIR,INU           !Analyse read data line to install values in temporary variables

      IF(ILN(1).EQ.0 .AND. (J.EQ.9999 .OR. J.EQ.99999)) THEN     !end of block condition, that stops the reading/analysing cycle
        GO TO 120                                                !

      ELSE                                                       !reading/analysing with two options
        IF(KSWIT .EQ. 1) THEN                                    !1. KSWIT == 1
          DO K=1,8                                               !*************
	    NPTEMP=MAX(NPTEMP,ILN(K))                            !Just read the dimensions of the net, that means increase temporary
	    NETEMP=MAX(NETEMP,J)                                 !element and node numbers with the IDs. That makes gaps in element
	  ENDDO                                                  !numbering possible
	ELSE                                                     !2. KSWIT /= 0 (implicitly KSWIT == 0
          DO K=1,8                                               !*************
    	    NOP(J,K)=ILN(K)                                      !install read values in proper arrays; meaning
	  ENDDO                                                  !
	  IMAT(J)=IMT                                            !NOP  (element (J,1..NE) array for defining nodes (K,1..8))
	  TH(J)=EDIR                                             !IMAT (element (J,1..NE) array for roughness classes)
	  NFIXH(J)=INU                                           !TH   (element (J,1..NE) array for principal axis direction)
          NE=MAX(NE,J)                                           !NFIXH(element (J,1..NE) array for internal ID after reordering)
        ENDIF                                                    !NE   (again element counter; the same meaning as NETEMP)

	GO TO 100                                                !cycle loop, that means go back to read next line
      ENDIF	                                                 !
!     *******
  120 continue                                                   !Continue after block ended; condition '9999' or '99999', see above
!NiS,may06,comment: end of element read block

!NiS,may06,comment: take over user specified ID-numbers as internal calculation numbers, if no number was assigned
!                   (that means no reordering was applied)

      IF(KSWIT .EQ. 0) THEN

        DO N=1,NE
	  IF(NFIXH(N) .EQ. 0) THEN
	    NFIXH(N)=N
	  ENDIF
        ENDDO
!NiS,may06,comment: NODE READING BLOCK
!     reading of node informations, that can for 2D-nodes and 3D-basis-nodes be the ID-number, x- and y- coordinates as well as the bottom elevation.
!     For 1D-nodes there are additional informations concerning the cross section of the simulated river.
!-
      endif

      !NiS,apr06,comment: read next line and test for TABS in line
      CALL GINPT1(IFILE,DLINE)



      !NiS,apr06,comment: analyse DLINE for stored informations
      IF(IFORM .LT. 2) THEN                                             !test for format of stored informations
        READ(DLINE,'(I10,9F10.0,I10,F10.0)')  J,CX,CY,BELEV,            !
     +               WDTHX,SS1X,SS2X,WDSX,WEL,SSSO,LOCK1,BS11           !J      = lfd. Knotennummer
      ELSE                                                              !CX/CY  = X-Koordinate/Y-Koordinate
        READ(DLINE,'(I10,2F20.0,7F10.0,I10,F10.0)')  J,CX,CY,BELEV, 	!BELEV  = Z-Koordinate (Höhe)
     +      	     WDTHX,SS1X,SS2X,WDSX,WEL,SSSO,LOCK1,BS11           !others = informations for cross section at 1D-nodes
      ENDIF

      !NiS,apr06,comment: test for end condition of block
      IF(DLINE(11:30) .eq. blank
     +  .AND.  (J .EQ. 9999  .OR.  J .EQ. 99999)) THEN
        GO TO 140

      !NiS,apr06,comment: use into DLINE read in informations, if not end line
      ELSE                                                              !reading/analysing with two options
	IF(KSWIT .EQ. 1) THEN                                           !1. KSWIT == 1
	  NPTEMP=MAX(NPTEMP,J)                                          !*************
	ELSE                                                            !net dimension definition for nodes is continued
          CORD(J,1) = CX                                                !2. KSWIT /= 1 (implicitly KSWIT == 0)
          CORD(J,2) = CY                                                !install informations into proper arrays:
          AO(J) = BELEV                                                 !CORD (node(J,1..NP) array for x-(1) and y-(2) coordinate)
          WIDTH(J)=WDTHX                                                !AO   (node(J,1..NP) array for bottom elevation)
          SS1(J)=SS1X                                                   !WIDTH(node(J,1..NP) array for 1D-channel-width)
          SS2(J)=SS2X                                                   !SS1  (node(J,1..NP) array for 1D-sideslope channel left)
          WIDS(J)=WDSX                                                  !SS2  (node(J,1..NP) array for 1D-SideSlope channel right)
    	  WIDBS(J)=WEL                                                  !WIDS (node(J,1..NP) array for 1D-channel storage width)
          WSS(J)=SSSO                                                   !WIDBS(node(J,1..NP) array for 1D-???
CIPK NOT IN RMA10        BS1(J)=BS11                                    !WSS  (node(J,1..NP) array for 1D-???
          NP=MAX(NP,J)                                                  !NP   (maximum nodecounter; same meaning as NPTEMP)
	ENDIF                                                           !
        GO TO 120
      ENDIF
      !this position is never reached
      NCL=0      

  140 CONTINUE   !continue after block ended; condition '9999' or '99999'

!NiS,may06,comment: End of dimension reading; informations necessary for dimensioning arrays are read; return from RDRM1 in this case:
      IF(KSWIT .EQ. 1) THEN
        write(*,*) 'ne',netemp                                                                         
        REWIND IFILE
        RETURN
      ENDIF
!-

!NiS,may06: Copy elevation field, because this might be given out in Kalypso-2D format
      do i = 1, MaxP
        aour(i) = ao(i)
      end do
!-

!NiS,may06,comment: CONTINUITY LINES READING BLOCK

      !NiS,may06,comment: Read initial line for continuity block
      CALL GINPT1(IFILE,DLINE)
      IF(DLINE(1:7) .EQ. 'ENDDATA') GO TO 375
      IF(DLINE(26:29) .NE. 'NCLM') GO TO 140
      READ(DLINE(1:5),'(I5)') NCL

      IF(NCL .GT. 0) THEN
        READ(IFILE,'(A3,A77)') ID1,DLIN2
        IF(ID1 .EQ. 'CC1') THEN
  330     READ(DLIN2,'(I5,9I8)') I,(LINE(I,J),J=1,9)
          NL=1
  340     NL=NL+9
          NCL=MAX(NCL,I)
          READ(IFILE,'(A3,A77)',end=375) ID1,DLIN2
          IF(ID1 .EQ. 'CC2') THEN

!NiS,mar06: Interpretation line is wrong. Changed
!            READ(IFILE,'(5X,9I8)') (LINE(I,J),J=NL,NL+8)
            READ(DLIN2,'(5X,9I8)') (LINE(I,J),J=NL,NL+8)
!-

          ELSEIF(ID1 .EQ.'CC1' .OR. ID1.EQ.'END') THEN
            DO K=1,350

              IF(LINE(I,K) .EQ. 0) THEN
                LMT(I)=K-1
	          IF(ID1 .EQ. 'END') GO TO 375

                GO TO 330
              ENDIF
            ENDDO
            LMT(J)=350
            !NiS,mar06,comment:
            !LMT(I)=350 should be right!, not LMT(J)
            GO TO 330

          ELSEIF(ID1 .EQ. 'END') THEN
            GO TO 375
          ENDIF
          GO TO 340
        ENDIF
      ENDIF
      !NiS,mar06,comment: used array meanings
      !LINE   Feld für die Def-Knoten            für jede KL
      !LMT    Feld für die Anzahl der Def-Knoten für jede KL
      !NCL    Anzahl der definierten KL

  375 CONTINUE

      close(ifile)

!NiS,mar06: insert output testblock
!	  OPEN(UNIT=555,FILE='test-s.txt')
!          do zzz=1,np
!	    WRITE(555,*)cord(zzz,1),cord(zzz,2),AO(zzz),wss(zzz)
!          ENDdo
!          CLOSE(555,STATUS='keep')
!          OPEN(UNIT=555,FILE='test2-s.txt')
!          do yyy=1,ne
!            WRITE(555,*)(NOP(yyy,K),K=1,8),IMAT(yyy),TH(yyy),NFIXH(yyy)
!          enddo
!          CLOSE(555,STATUS='keep')
!          OPEN(UNIT=555,FILE='test3-s.txt')
!          do yyy=1,np
!            WRITE(555,*)WIDTH(yyy),SS1(yyy),SS2(yyy),WIDS(yyy)
!          enddo
!          CLOSE(555,STATUS='keep')
!          OPEN(555,FILE='test4-s.txt')
!          do yyy=1,NCL
!            WRITE(555,*)(line(yyy,k),k=1,lmt(yyy))
!          enddo
!          CLOSE(555,STATUS='keep')
!-

!NiS,may06,comment: End of file; give back values and end execution of subroutine
	RETURN
	END
!-

!************************************************************************
      SUBROUTINE GINPT1(IIN,DLIN)

      CHARACTER DLIN*140
  100 CONTINUE
      READ(IIN,7000) DLIN
      write(75,7000) dlin
 7000 FORMAT(A140)
	do i=1,140
	  if(dlin(i:i) .eq. char(9)) go to 200
	enddo
      RETURN
  200 continue
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
	write(*,*) 'Error Tab character found in the following line'
	write(75,*) 'Error Tab character found in the following line'
      write(75,7000) dlin
      write(*,7000) dlin
      stop
      END
