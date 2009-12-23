!     Last change:  NIS   2 Feb 2008    8:58 pm
!nis,nov06: This subroutine checks all the 1D-2D-line-Transitions on consistency
!           That is:
!           - well definition
!
!future:           - bed level

subroutine check_linetransition

!Modules
USE BLK10MOD
USE ParaKalyps

!This is good standard
implicit none

!Declarations
INTEGER :: matchno

!Counters and Temps (Counter, Counter, Linenumber, 1D-Connectionnode, Linelength)
INTEGER        :: I, J, K, LiNo, CoNode, LiLe
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

smDi       = 1.e20
SeSmDi     = 1.e20
alphaSm    = 0.
alphaSeSm  = 0.
DeltaAlpha = 0.
deltax     = 0.
deltay     = 0.
deltal     = 0.
deltal_min = 0.

!Processing------------------------------

!Run through all possible transitions
DO J = 1, MaxLT

!Check, whether Transition exists  
  if (TransLines (J, 1) /= 0) then

!local copy of connecting node, Line Number and it's length    
    CoNode = TransLines(J,3)
    LiNo   = TransLines(J,2)
    LiLe   = lmt (LiNo)

  end if

!nis,jul07: Adding logical variable of nodes being part of a 2D-line in 1D-2D-transitions  
!Connected 1D-node becomes Member of a transition line  
  Transitionmember (TransLines (J, 3)) = .true.

!Connected 2D-nodes become Member of a transition line  
  do k = 1, lmt (LiNo)
    if (TransitionMember (line (LiNo, k))) then
      WRITE(*,*) 'Node ', line(LiNo,k), ' in more than one transition!'
      WRITE(*,*) "That doesn't work"
      WRITE(*,*) 'Program stopped'
      stop
    end if
    TransitionMember (line (LiNo,k)) = .true.
  end do
END DO

end subroutine
