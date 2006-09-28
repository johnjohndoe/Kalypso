      SUBROUTINE SECOND(TA)
      SAVE
C This version is for microsoft fortran
      DATA ITIM/0/
      integer clock1, rate
      real*8 rrate, told, tadbl
      CALL SYSTEM_CLOCK (clock1,rate)  ! get benchmark (n) start time
      rrate = rate
      tadbl = clock1/rrate
      
c      CALL GETTIM(IHR,IMIN,ISEC,IHUN)
c      TA=3600.*IHR+60.*IMIN+ISEC+ FLOAT(IHUN)/100.
      IF(ITIM .EQ. 0) THEN
	TOLD=TADBL
	ITIM=1
      ENDIF
        TA=TADBL-TOLD
      RETURN
      END
