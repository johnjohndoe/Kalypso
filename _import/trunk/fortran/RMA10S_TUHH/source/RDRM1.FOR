CIPK  LAST UPDATE SEP 6 2004  add error file
      SUBROUTINE RDRM1(NPTEMP,NETEMP,KSWIT)
      USE BLK10MOD

cipk aug05	INCLUDE 'BLK10.COM'
	REAL*8 CX,CY
      CHARACTER DLINE*140,ID1*3,DLIN2*77,BLANK*20
	DIMENSION ILN(8)

      data blank/'                    '/ 

      READ(ifile,'(A80)') TITLE
      READ(IFILE,'(100X,I5)') IFORM
      READ(IFILE,'(A80)') TITLE

      NP=0
	NE=0
  100	CALL GINPT1(IFILE,DLINE)

      READ(DLINE,'(10I5,F10.3,I5)') J,ILN,IMT,EDIR,INU
	IF(ILN(1) .EQ. 0  .AND.  (J .EQ. 9999  .OR.  J .EQ. 99999)) THEN
        GO TO 120
      ELSE
	  IF(KSWIT .EQ. 1) THEN
          DO K=1,8
	      NPTEMP=MAX(NPTEMP,ILN(K))
	      NETEMP=MAX(NETEMP,J)
	    ENDDO
	  ELSE
          DO K=1,8
    	      NOP(J,K)=ILN(K)
	    ENDDO
	    IMAT(J)=IMT
	    TH(J)=EDIR
	    NFIXH(J)=INU
          NE=MAX(NE,J)
        ENDIF
	  GO TO 100
      ENDIF	  
  
  120 continue                                                                           

      DO N=1,NE
	  IF(NFIXH(N) .EQ. 0) THEN
	    NFIXH(N)=N
	  ENDIF
      ENDDO

      CALL GINPT1(IFILE,DLINE)

      IF(IFORM .LT. 2) THEN
        READ(DLINE,'(I10,9F10.0,I10,F10.0)')  J, CX, CY, BELEV,                   &
     &               WDTHX,SS1X,SS2X,WDSX,WEL,SSSO,LOCK1,BS11  
      ELSE
        READ(DLINE,'(I10,2F20.0,7F10.0,I10,F10.0)')  J, CX, CY, BELEV,                   &
     &      WDTHX,SS1X,SS2X,WDSX,WEL,SSSO,LOCK1,BS11  
      ENDIF


	IF(DLINE(11:30) .eq. blank
     +  .AND.  (J .EQ. 9999  .OR.  J .EQ. 99999)) THEN
	  GO TO 140
	ELSE
	  IF(KSWIT .EQ. 1) THEN
	    NPTEMP=MAX(NPTEMP,J)
	  ELSE
          CORD(J,1) = CX 
          CORD(J,2) = CY 
          AO(J) = BELEV 
          WIDTH(J)=WDTHX 
          SS1(J)=SS1X 
          SS2(J)=SS2X 
          WIDS(J)=WDSX
    	    WIDBS(J)=WEL 
          WSS(J)=SSSO
CIPK NOT IN RMA10        BS1(J)=BS11
	    NP=MAX(NP,J)
	  ENDIF
	GO TO 120 
	ENDIF
	NCL=0
  140 CONTINUE

      IF(KSWIT .EQ. 1) THEN
        REWIND IFILE
        RETURN
      ENDIF

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
            READ(IFILE,'(5X,9I8)') (LINE(I,J),J=NL,NL+8)
          ELSEIF(ID1 .EQ. 'CC1'  .OR.  ID1 .EQ. 'END') THEN
            DO K=1,350
              IF(LINE(I,K) .EQ. 0) THEN
                LMT(I)=K-1
	          IF(ID1 .EQ. 'END') GO TO 375
                GO TO 330
              ENDIF
            ENDDO
            LMT(J)=350
            GO TO 330
          ELSEIF(ID1 .EQ. 'END') THEN
            GO TO 375
          ENDIF
          GO TO 340
        ENDIF 
      ENDIF



  375 CONTINUE

	close(ifile)

	RETURN
	END

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
