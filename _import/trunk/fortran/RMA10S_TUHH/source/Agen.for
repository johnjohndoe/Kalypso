C     Last change:  JAJ  31 May 2006    5:00 pm
cipk  last update feb 09 2001 allow for zero length cc lines
      SUBROUTINE AGEN
      USE BLK10MOD
      SAVE
C-
C-..... Generate specified total flow boundary conditions
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
C-
C...... Calculate total projected area for all stage flow lines
C-
      DO 300 J=1,NCL
      !NiS,may06:testing
      WRITE(*,*) 'processing NCL', J
      !-
        MAX=LMT(J)
*
*...... Skip if 1-d or line is not stage flow
*
cipk feb01
        IF(MAX .lt. 3) GO TO 300
        IF(STQA(J) .EQ. 0.) GO TO 300
        !NiS,may06:testing
        WRITE(*,*)'coming to agen-calc,NCL',NCL
        !-
        ALN(J)=0.
        THET=STQT(J)
        MAX = LMT(J)-2
        DO 150 K = 1, MAX, 2
        NA = LINE(J,K)
          NC = LINE(J,K+2)
          DX=CORD(NC,1)-CORD(NA,1)
          DY=-(CORD(NC,2)-CORD(NA,2))
          XL=SQRT(DX**2+DY**2)
          ALP=ATAN2(DX,DY)
          D1=VEL(3,NA)
          D3=VEL(3,NC)
          D2=(D1+D3)/2.
          ALN(J) = ALN(J)+XL*COS(ALP-THET)*D2
  150   CONTINUE
        ALN(J)=ABS(ALN(J))
  300 CONTINUE
C-
      RETURN
      END
