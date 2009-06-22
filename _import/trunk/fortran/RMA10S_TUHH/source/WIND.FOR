      SUBROUTINE WINDF(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      use blk_ifu
C
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05	INCLUDE 'BLK11.COM'
C
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN

      WRITE(LOUT,6050)
 6050 FORMAT('0   WIND STRESS INFORMATION:')

c 
c    Three options are available for input of wind data
c
CIPK AUG98  CHECK AND READ FILE FIRST
      IF(IWINDIN .GT. 0) THEN
	    IF(IWINDIN .EQ. 69) THEN
		  READ(IWINDIN) NXX,IYFL,DYOFY,TFL,(WNDSP(J),WNDDR(J),J=1,NXX)
	    ELSE
 		  READ(IWINDIN,5400) NXX,IYFL,DYOFY,TFL
		  READ(IWINDIN,5401) (WNDSP(J),WNDDR(J),J=1,NXX)
 5400     FORMAT(2I7,F7.0,F14.3)
 5401     FORMAT(5(2F7.2))
	    ENDIF
	    DO J=1,NXX
            SIGMA(J,1)=CHI*COS(WNDDR(J)/57.3)*WNDSP(J)**2
            SIGMA(J,2)=CHI*SIN(WNDDR(J)/57.3)*WNDSP(J)**2
	    ENDDO
            WRITE(75,*) 'WIND',SIGMA(1,1),SIGMA(1,2),WNDSP(1)

      ELSEIF(ID(1:3) .EQ. 'WVA') THEN
        READ(DLIN,'(I8,2F8.0)') N,TW,TA
cipk nov97        READ(IBIN,7000) ID,DLIN
        call ginpt(ibin,id,dlin)
cipk APR96 save data to a scratch file
        if(isvs .eq. 1) then
          write(nscrin,7000) id,dlin
        endif

        WRITE(LOUT,6046) TW,TA
        WRITE(*,6046) TW,TA
        WRITE(*,*) 'CHI',CHI
        DO N=1,NP
          SIGMA(N,1)=CHI*COS(TA/57.3)*TW**2
          SIGMA(N,2)=CHI*SIN(TA/57.3)*TW**2
        ENDDO
      ENDIF
  215 CONTINUE
      IF(ID(1:3) .EQ. 'WVN') THEN
        READ(DLIN,'(I8,2F8.0)') N,TW,TA

        call ginpt(ibin,id,dlin)

        if(isvs .eq. 1) then
          write(nscrin,7000) id,dlin
        endif
        SIGMA(N,1)=CHI*COS(TA/57.3)*TW**2
        SIGMA(N,2)=CHI*SIN(TA/57.3)*TW**2
        GO TO 215
      ENDIF

      RETURN
 6046 FORMAT(/'   WIND SPEED =',F8.1,'  DIRECTION =',F8.0,' DEGREES'/)
 7000 FORMAT(A8,A72)
	END
