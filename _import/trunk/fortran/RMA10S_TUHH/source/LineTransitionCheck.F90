!     Last change:  K     2 Mar 2007    6:03 pm
!nis,nov06: This subroutine checks all the 1D-2D-line-Transitions on consistency
!           That is:
!           - well definition
!           - Setting of Transmember to 1 or 2;
!             1 means part of line
!             2 means not part of line
!           - if the connecting node was not part of the line (Transmember(J).eq.2) , it checks whether the distance is near
!             enough to the line (5 percent criterium)
!
!future:           - bed level

subroutine check_linetransition

!Modules
USE BLK10MOD

!This is good standard
implicit none

!Declarations
INTEGER :: matchno

!Counters and Temps (Counter, Counter, Linenumber, 1D-Connectionnode, Linelength)
INTEGER        :: J, I, LiNo, CoNode, LiLe
!line lengths: line's x-length, line's y-length, line's euclidean length
REAL (KIND=4)  :: lxlen, lylen, leulen
!distances for checking the transitioning 1D-element's corner node
REAL (KIND=4)  :: deltal1, deltal2
!temporary coordinates of the line's chord middle point
REAL (KIND=4)  :: xave, yave
!get the two minimum distances (smallest Distance, second smallest Distance, ...
REAL (KIND=4)  :: smDi, SeSmDi, deltax, deltay, deltal, deltal_min
!get the global angles of the computed lenghts to get a 2D-distance-vector definition
REAL (KIND=4)  :: alphaSm, alphaSeSm, DeltaAlpha

!testing
!WRITE(*,*) 'bin hier in line transition check'
!pause
!-

!Initializations-------------------------
matchno   = 0

lxlen  = 0
lylen  = 0
leulen = 0

smDi       = 1e20
SeSmDi     = 1e20
alphaSm    = 0
alphaSeSm  = 0
DeltaAlpha = 0
deltax     = 0
deltay     = 0
deltal     = 0
deltal_min = 0

!Processing------------------------------

!Run through all possible transitions
DO J = 1, MaxLT

  !Check, whether Transition exists
  if (TransLines(J,1).ne.0) then

    !local copy of connecting node, Line Number and it's length
    CoNode = TransLines(J,3)
    LiNo   = TransLines(J,2)
    LiLe   = lmt (LiNo)

    !objective 1
    !Checking, whether assignments are correct, that is easily done by comparing the nodal storage of transitioning element with the connection node
    !-----------------------------------------
    if (CoNode .ne. nop(TransLines(J,1),3)) then
      WRITE(*,*) 'ERROR - the assignment of the connecting node was not successful'
      WRITE(*,*) 'Check, whether the connecting element and the transition line defintion'
      WRITE(*,*) 'use the same connecting node'
      WRITE(*,*) 'Program can not be executed'
      WRITE(*,*) 'STOP'
      stop
    end if


    !objective 2
    !Setting the TransMember array
    !-----------------------------
    !Remember, whether the transitioning node of the 1D-part is part of the TLine or whether it is a seperate node
    !The value is initialized by 2, which means it is NOT part of the TLine (so not placed in line-array)
    TransMember(J) = 2

    !Check, whether node is part of line, so transmember(CoNode) becomes 1
    linecheck: do i=1,LiLe
      !Overgive the name of the connecting node. Purpose: Direct access to connecting nodes, shorter if-constructions
      !Show, that node is member of a 1D-2D-line-transition
      if (Line(LiNo,J).eq.CoNode) then
        TransMember(J) = 1
        EXIT linecheck
      endif
    end do linecheck


    !objective 3
    !Checking whether the distance between connecting node, that is not part of line, is small enough
    !-----------------------------------------------------------------------------------------------
    !Show result of linecheck, if the node is not part of the line, there's not generally a problem, but it has to be tested, whether it
    !it is in a range of maximum 5 percent of the whole line width (direct distance from first to last node). It is a geometric adjustment
    !One can change it if he wants
    if (TransMember(J).eq.2) then

      !get distance from first to last node
      lxlen  = cord (Line(LiNo,1) , 1) - cord (Line(LiNo,lmt(LiNo)), 1)
      lylen  = cord (Line(LiNo,1) , 2) - cord (Line(LiNo,lmt(LiNo)), 2)
      leulen = SQRT (lxlen*lxlen + lylen*lylen)

      !get the smallest distance from 1D-element's node to line
      do i=1,LiLe
        deltax = cord (Line(LiNo,i) , 1) - cord (CoNode, 1)
        deltax = cord (Line(LiNo,i) , 2) - cord (CoNode, 2)
        deltal = SQRT (deltax*deltax + deltay*deltay)

        !Remember the two nearst distances, node numbers are not interesting
        IF (deltal.lt.smDi) THEN
          SeSmDi    = smDi
          alphaSeSm = alphaSm
          smDi      = deltal
          alphaSm   = ATAN(deltax/deltay)
        ELSEIF (deltal.lt.SeSmDi .AND. deltal.ge.smDi) THEN
          SeSmDi    = deltal
          alphaSeSm = ATAN(deltax/deltay)
        ENDIF

      enddo

      !get the perpendicular distance to the chord of the two nearst nodes to 1D-node
      DeltaAlpha = alphaSm - alphaSeSm
      !using sinus and law of cosines, the distance can be calculated:
      deltal_min = (smDi * SeSmDi * SIN (ABS(DeltaAlpha)))
      deltal_min = deltal_min/ SQRT(smDi*smDi + SeSmDi*SeSmDi - 2*smDi*SeSmDi*COS(ABS(deltaAlpha)))

      !if the perpendicular distance to nearest continuity line chord is less than 5 percent of the total length of the
      !  transitioning continuity line, then everything is fine
      if (deltal_min .gt. (5./100.*leulen) ) then
        WRITE(*,*) 'deltal_min: ', deltal_min, (5./100.*leulen)
        WRITE(*,*) 'Connection is not properly defined.'
        WRITE(*,*) 'Connect last node of 1D-line to Transitionline.'
        WRITE(*,*) 'Problem occured at line: ', LiNo
        WRITE(*,*) 'Program can not be executed'
        WRITE(*,*) 'STOP'
        stop
      endif

    endif
  end if
END DO

end subroutine
