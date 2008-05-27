      SUBROUTINE REPLY(ANAM,ISWT)
      CHARACTER*32 ANAM
      ISWT=0
      IF(ANAM .EQ. 'NO') GO TO 200
      IF(ANAM .EQ. 'no') GO TO 200
      IF(ANAM .EQ. 'NONE') GO TO 200
      IF(ANAM .EQ. 'none') GO TO 200
      IF(ANAM .EQ. 'NOMORE') GO TO 220
      IF(ANAM .EQ. 'nomore') GO TO 220
      IF(ANAM .EQ. ' ') GO TO 220
      RETURN
  200 ISWT=1
      RETURN
  220 ISWT=2
      RETURN
      END
