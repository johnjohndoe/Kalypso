C     Last change:  JAJ  26 Aug 2007    8:33 pm
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
      AllLines: DO J = 1, NCL
        MAXL = LMT(J)
*
*...... Skip if 1-d or line is not stage flow
*
cipk feb01
        IF(MAXL < 3) CYCLE AllLines

        !EFa jul07, deactivated/moved for stage-flow boundaries
        IF (STQA(J) == 0. .or. istab(Line(j, 1)) == 0) CYCLE AllLines

        ALN(J)=0.
        THET=STQT(J)
        MAXL = LMT(J)-2
        ThroughCornerNodes: DO K = 1, MAXL, 2
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
        ENDDO ThroughCornerNodes

        ALN(J) = ABS(ALN(J))
      ENDDO AllLines
C-
      RETURN
      END
