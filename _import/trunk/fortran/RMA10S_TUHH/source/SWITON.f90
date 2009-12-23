      SUBROUTINE SWITON(K,ISWT,IYRR,DAYOFY,TET,QFACT)
      USE TSVAR
      INTEGER DAYOFY
!
      ISWT=1
!
      IF(K == 0) RETURN
!
      IF(IYRR == IYTIM) THEN
        RTIME=(DAYOFY-1)*24.+TET
      ENDIF
      DO L=1,NTIMEMX
!
        IF(CTIMEON(K,L) < -9000) RETURN 
        IF(RTIME >= CTIMEON(K,L)) THEN
          IF(RTIME < CTIMEOF(K,L)) THEN
            ISWT=0
!IPK DEC05
            QFACT=1.
            IF(RTIME < HTIMEON(K,L))      THEN
             if(HTIMEON(K,L) > CTIMEON(K,L))                            &
     &       QFACT=(RTIME- CTIMEON(K,L))/(HTIMEON(K,L)- CTIMEON(K,L))
            ENDIF
            IF(RTIME > HTIMEOF(K,L))      THEN
             IF(CTIMEOF(K,L) > HTIMEOF(K,L))                            &
     &       QFACT=1.-(RTIME- HTIMEOF(K,L))/(CTIMEOF(K,L)- HTIMEOF(K,L))
            ENDIF
            RETURN
          ENDIF
        ELSE
          RETURN
        ENDIF
      ENDDO
      RETURN
      END