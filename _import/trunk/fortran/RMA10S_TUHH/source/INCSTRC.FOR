C     Last change:  WP   28 Aug 2007    8:50 am
      SUBROUTINE INCSTRC
      
      USE CSVAR
      USE BLK10MOD
      
cipk aug05	INCLUDE 'BLK10.COM'
      CHARACTER*8  IDC
      CHARACTER*72 DLINC

      NROWM=0
      NMAXM=0
      NSETS=0
      CALL GINPT(INCSTR,IDC,DLINC)
      IF(IDC(1:2) .EQ. 'TI') THEN
        WRITE(*,*) 'Reading title on control structure',DLINC
      ENDIF

  100 CONTINUE
      CALL GINPT(INCSTR,IDC,DLINC)

      IF(IDC(1:3) .EQ. 'IDC') THEN
        READ(DLINC,'(3I8)') ID1,NROW,NCOL
        IF(NROW .GT. NROWM) NROWM=NROW
        IF(NCOL .GT. NCOLM) NCOLM=NCOL
        NSETS=NSETS+1
      ELSEIF(IDC(1:7) .EQ. 'ENDDATA') THEN
        GO TO 200

      ENDIF
      GO TO 100

  200 CONTINUE
      REWIND INCSTR

      !weir file shall also be present if no data is available
      if (nrowm == 0) return

      CALL GINPT(INCSTR,IDC,DLINC)

      ALLOCATE (NROWCS(NROWM),NCOLCS(NCOLM),HRW(NSETS,NROWM),
     +          HCL(NSETS,NCOLM),FLWCS(NSETS,NROWM,NCOLM))
      CALL GINPT(INCSTR,IDC,DLINC)
      DO K=1,NSETS
        READ(DLINC,'(3I8)') ID1,NROWCS(K),NCOLCS(K)
        NCTREF(ID1)=K

        CALL GINPT(INCSTR,IDC,DLINC)
        NLO=1

  220   IF(IDC(1:3) .EQ. 'HRW') THEN
          NHI=NLO+8
          IF(NROWCS(K) .LT. NHI) THEN
            NHI=NROWCS(K)
          ENDIF
          READ(DLINC,5010) (HRW(K,J), J=NLO,NHI)
          CALL GINPT(INCSTR,IDC,DLINC)
          NLO=NHI+1
        ENDIF
        IF(NHI .LT. NROWCS(K)) GO TO 220

        NLO=1
  240   IF(IDC(1:3) .EQ. 'HCL') THEN
          NHI=NLO+8
          IF(NCOLCS(K) .LT. NHI) THEN
            NHI=NCOLCS(K)
          ENDIF
          READ(DLINC,5010) (HCL(K,J), J=NLO,NHI)
          CALL GINPT(INCSTR,IDC,DLINC)
          NLO=NHI+1
        ENDIF
        IF(NHI .LT. NCOLCS(K)) GO TO 240

        DO J=1,NROWCS(K)
          IF (J.EQ.101) Then
            Dummy = 0
          End If
          NLO=1
  260     IF(IDC(1:3) .EQ. 'FLW') THEN
            NHI=NLO+8
            IF(NCOLCS(K) .LT. NHI) THEN
              NHI=NCOLCS(K)
            ENDIF
            READ(DLINC,5010) (FLWCS(K,J,L), L=NLO,NHI)
            CALL GINPT(INCSTR,IDC,DLINC)
            NLO=NHI+1
          ENDIF
          IF(NHI .LT. NCOLCS(K)) GO TO 260
        ENDDO

      ENDDO

 5010 FORMAT(9F8.0)
      RETURN
      END