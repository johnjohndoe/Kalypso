SUBROUTINE AGEN

USE BLK10MOD, only: ncl, lmt, stqa, line, istab, aln, stqt, cord, vel
implicit none

integer (kind = 4) :: j, k
integer (kind = 4) :: na, nc, maxl

real (kind = 8) :: thet, alp, xl
real (kind = 8) :: dx, dy
real (kind = 8) :: d1, d2, d3

!C-
!C...... Calculate total projected area for all stage flow lines
!C-

AllLines: DO J = 1, NCL
  MAXL = LMT(J)
!
!...... Skip if 1-d or line is not stage flow
!
  !Skip 1D boundary lines
  IF(MAXL < 3) CYCLE AllLines
  !Skip line that is not a stage flow BC
  IF (STQA(J) == 0. .OR. istab(Line(j, 1)) == 0) CYCLE AllLines

  !Initialize
  ALN (J) = 0.
  THET = STQT(J)
  MAXL = LMT(J)-2
  !Calculate cross section area with present water levels
  ThroughCornerNodes: DO K = 1, MAXL, 2
    !get first and last node of line segment
    NA = LINE(J,K)
    NC = LINE(J,K+2)
    !calculate length of line segment
    DX=CORD(NC,1)-CORD(NA,1)
    DY=-(CORD(NC,2)-CORD(NA,2))
    XL=SQRT(DX**2+DY**2)
    !calcualate angle of line segment
    ALP=ATAN2(DX,DY)
    !get depth of corner nodes and midside node of line segment
    D1=VEL(3,NA)
    D3=VEL(3,NC)
    D2=(D1+D3)/2.
    !calculate segment's projected cross sectional area
    ALN(J) = ALN(J)+XL*COS(ALP-THET)*D2
  ENDDO ThroughCornerNodes
  !get absolute value of cross sectional area
  ALN(J) = ABS(ALN(J))
ENDDO AllLines

RETURN
END