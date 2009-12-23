module mod_vegetation

contains
!     Last change:  WP   28 Jul 2008    7:05 pm
!--------------------------------------------------------------------------------------------
! This code, pasche_trees.f90,determines the impact of tree vegetation
! for hydrodynamic simulations in the library 'Kalypso-2D'.
! Copyright (C) 2004  ERIK PASCHE & WOLF PLOEGER.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-2D'.
!
! Sebastian Rath, 09 August 2004
! Research Associate
!
!
subroutine get_element_cwr (m_SimModel)
!
!--------------------------------------------------------------------------------------------
! Some additional information of relevance:
!--------------------------------------------------------------------------------------------
! This subroutine is calculating the parameter C_WR for a detailled
! validation of the roughness induced by trees depending on the
! flow depth, the flow velocity, the mean distance between the trees
! and the slope of the energy (here: slope of water surface)
!
! The parameter C_WR is used in subroutine WALD to evaluate the
! friction factor for trees.
!--------------------------------------------------------------------------------------------

!NiS,apr06: The necessary variables/ arrays, that were declared in common.cfg in Kalypso-2D
!           are now declared in the modules Blk10mod.module and PARAKalyps.module:
!include "common.cfg"
use mod_Model

USE PARAKalyps
USE Blk10mod
!-
!arguments
type (simulationModel), pointer :: m_SimModel

! Local variables
! Slope of watersurface at node
REAL(kind=8), allocatable :: slope (:)  
! Slope of energy level at node
REAL(kind=8), allocatable :: eslope (:) 
! Mean roughness coefficient at element
REAL(KIND=8)              :: lambda_s   

REAL(KIND=8) :: sumvx = 0.0
REAL(KIND=8) :: sumvy = 0.0
REAL(KIND=8) :: sumh = 0.0
REAL(kind=8) :: sumslope = 0.0
REAL(kind=8) :: sumeslope = 0.0
REAL (KIND = 8) :: NikuradseRoughness
! Mean slope of water surface at element
REAL(kind=8), allocatable :: mslope (:)  
! Mean slope of energy level at element
REAL(kind=8), allocatable :: meslope (:) 
INTEGER :: i, cycle_number
character (len = 96) :: inputFileName


allocate (slope (1: MaxP), eslope (1: MaxP))
allocate (mslope (1:MaxE), meslope (1: MaxE))

! Main procedure to calculate the slope of the watersurface
! for all nodes.
call GET_NODE_SLOPE(slope, eslope, m_SimModel)

! Initialising of parameters
DO i=1,ne

  mh(i)        = 0.0
  mvx(i)       = 0.0
  mvy(i)       = 0.0
  mvxvy(i)     = 0.0
  mslope(i)    = 0.0
  meslope(i)   = 0.0
end do

do i=1,ne

  sumvx      = 0.0
  sumvy      = 0.0
  sumh       = 0.0
  sumslope   = 0.0
  sumeslope  = 0.0

! Loop over all nodes of the element (also including the mid-side nodes).  
! The mean velocity in x- and y-direction and the mean waterdepth in  
! each element is calculated.  
! (nop:   Node numbers of element)  
! (ncorn: Number of nodes of the element)  
! (cord:  coordinates of node)  
! vel(3,...): degrees of freedom at node)  

  do j = 1,ncorn(i)
    sumvx       = sumvx + vel(1,nop(i,j))
    sumvy       = sumvy + vel(2,nop(i,j))
    sumh        = sumh  + vel(3,nop(i,j))
    sumslope    = sumslope + slope(nop(i,j))
    sumeslope   = sumeslope + eslope(nop(i,j))
  end do


! Average of values for one element  
  mvx(i)     = sumvx/ncorn(i)
  mvy(i)     = sumvy/ncorn(i)
  mh(i)      = sumh/ncorn(i)
  mvxvy(i)   = SQRT(mvx(i)**2 + mvy(i)**2)
  mslope(i)  = sumslope / ncorn(i)
  meslope(i)  = sumeslope / ncorn(i)

end do


all_elements: do i = 1, ne

  if (imat(i) <= 0 .OR. abst(i) < 0.0001 .OR. imat(i) == 89) then

! Deactivated element or element without trees    
    c_wr(i) = 1.30
    CYCLE all_elements

  else

    if (mh(i) < 0.01) then
      c_wr(i) = 1.30
      cycle all_elements
    end if

    NikuradseRoughness = cniku(i)
    lambda_s = cole (mvxvy(i), mh(i), NikuradseRoughness)

    if (mslope(i) > 0.000001) then

! Main subroutine to calculate the parameter C_WR for the    
! element i    

!WP Now changing slope from water surface slope to    
!WP energy slope.  ( mslope(i) -> meslope(i) )    

      call get_cwr(meslope(i), &
               & mh(i),        &
               & abst(i),      &
               & durchbaum(i), &
               & lambda_s,     &
               & c_wr(i))
     else
       c_wr(i) = 0.0d0
     endif
  end if

end do all_elements


!NiS,may06,comment: Creation of file name dependent on steady or dynamic solution:
! Creation of file name for the documentation of the
! friction due to trees.

!NiS,jun06: name creation dependent on steady or dynamic as well as within iteration or after convergence:
if (maxn == 0) then
!-
  IF (icyc == 0) THEN
    call GenerateOutputFileName ('stat', niti, 0, maxn, 'cwr',modellein, modellrst, ct, nb,name_cwr, inputFileName)
  ELSE
    call GenerateOutputFileName ('inst', niti, icyc, maxn, 'cwr', modellein,modellrst, ct, nb, name_cwr, inputFileName)
  ENDIF
elseif (nprti /= 0) then
  if (mod(maxn, nprti) == 0) then
    IF (icyc == 0) THEN
      call GenerateOutputFileName ('stat', niti, 0, maxn, 'cwr',modellein, modellrst, ct, nb,name_cwr, inputFileName)
    ELSE
      call GenerateOutputFileName ('inst', niti, icyc, maxn, 'cwr', modellein,modellrst, ct, nb, name_cwr, inputFileName)
    ENDIF
  endif
endif
!-  

! Writing detailled information of the calculated values
! for the friction due to trees.
!nis,dec06: Replacing mel with MaxE
!call cwr_write(name_cwr, ne, mcord, c_wr, mslope, meslope, mh, mvxvy, mel)
call cwr_write(name_cwr, ne, mcord, c_wr, mslope, meslope, mh, mvxvy, MaxE)
!-

deallocate (slope, mslope)

RETURN

end subroutine get_element_cwr






!--------------------------------------------------------------------------------------------
subroutine GET_NODE_SLOPE(slope, eslope, m_SimModel)

!
! For each node the slope of the water surface is calculated.
!
!                                       Wolf Ploeger, Jul 2004
!--------------------------------------------------------------------------------------------

!NiS,apr06: The necessary variables/ arrays, that were declared in common.cfg and the Block
!           definition COMMON/raus in Kalypso-2D are now declared in the modules Blk10mod.module
!           and PARAKalyps.module:
!include "common.cfg"
!COMMON / raus / rausv (4, mnd), iauslp, iausnpm, zeigma (mnd)
  use mod_Model
  use mod_Nodes
  use mod_Node_DecimalTree
  use mod_meshModelFE
USE PARAKalyps, only: rausv
USE Blk10mod, only: maxp, np, ne, vel, ao, cord, ncorn, nop
!-

implicit none


!arguments
! Calculated slope of water surface
REAL (kind=8) :: slope (1:*)   
! Calculated slope of energy curve/surface
REAL (kind=8) :: eslope (1:*)  
type (simulationModel), pointer :: m_SimModel

! Local variables
integer (kind = 4) :: nconnect


! Neighbour nodes of the flow vector
INTEGER :: first_loc            
! Neighbour nodes of the flow vector
INTEGER :: second_loc            

!nis,sep06: Declaring missing variable and overgiving the proper value
INTEGER                         :: nodecnt
INTEGER                         :: elcnt
integer :: node1, node2, node3
integer (kind = 4) :: length, length2

! direction of the actual flow vector
REAL(kind=8), DIMENSION(1:2)    :: angle_v              
! direction from actual point to the neighbour points
REAL(kind=8), DIMENSION(1:2)    :: vector_to_point      
! angle between the flow vector and the vector to the
REAL(kind=8), DIMENSION(1:100)  :: angle_delt = 0.0     
! neighbour points                                                        
! absolut value of velocity
REAL(kind=8)                    :: vecq                 

!NiS,apr06: changing mnd to MaxP
! Marker if slope has been calculated (true) or not(false).
LOGICAL, allocatable :: marker_slope (:) 
! The slope of all nodes with MARKER_SLOPE=.false. will be interpolated                                         
! from the neighbouring points in subroutine FILL_SLOPES                                         

INTEGER                     :: i,j,m
type (node), pointer :: tmpNode => null()
type (linkedNode), pointer :: neighbNode => null()
type (node), pointer :: firstNeighb => null()
type (node), pointer :: secondNeighb => null()

allocate (marker_slope (1: MaxP))


!nis,sep06: nodecnt value must be specified, it is not global
nodecnt = np
!nis,dec06: adding element number 
 elcnt   = ne
!-
!nis,dec06: Initializing the marker_slope Vector
do i = 1, nodecnt
  marker_slope(i) = .false.
!  WRITE(*,*) marker_slope(i)
end do
!-

outer: do i = 1, nodecnt

  tmpNode => findNodeInMeshByID (m_SimModel%femesh, i)
  nconnect = noOfNeighbours (tmpNode)

  if (nconnect == 0) CYCLE outer

  slope(i) = 0.0

  angle_v(1) = vel(1,i)
  angle_v(2) = vel(2,i)
  vecq = SQRT(vel(1,i)**2 + vel(2,i)**2)

!nis,jul08: Test for smaller velocity (original value: 0.001). Problems are very low flow areas, where trees are present. If values are too small,  
!           they will become better and better with each iteration, because the calculation of the cwr-values takes place every iteration. This  
!           is the opposite to BCE-2D (old RMA2-adaptation), where this comes from  
  if (vecq < 0.001 .OR. rausv(3,i) < ao(i)) then
! If velocity is very small or water surface is below node,    
! no slope can be calculated.    
    marker_slope(i) = .false.
    CYCLE outer
  end if

! For each neighbour node the angle between the velocity  
! vector and the vector from point i to neighbour j is  
! determined  
  neighbNode => tmpNode%neighbourList
  inner: do j = 1, nconnect
!generate vector    
    do m = 1,2
      vector_to_point(m) = cord(neighbNode%thisNode%ID, m) - cord(tmpNode%ID,m)
    end do
! Angle between the two vectors    
    call GET_ANGLE(angle_v, vector_to_point, angle_delt(j) )
!next node of neighbours    
    neighbNode => neighbNode%next
  end do inner

! Detecting the neighbour with the smallest angle (FIRST_LOC) and the neighbour  
! with the smallest angle with the other sign (SECOND_LOC).  
! If there is no neighbour with an angle with the other sign (e.g. at the  
! boundary of the mesh) the SECOND_LOC is set to 0.  
! The slope of this point will not be calculated!  
  nullify (firstNeighb, secondNeighb)
  call GET_MIN_ANGLE_POINTS(angle_delt, nconnect, tmpNode%NeighbourList, firstNeighb, secondNeighb)

  if (associated (secondNeighb)) then
!nis,dec06: Only the marker_slope of the passed node i is wanted!!!
    call GET_SLOPE(angle_v, i, firstNeighb, secondNeighb, slope, eslope, marker_slope(i))
  else
    marker_slope(i) = .false.
  end if

end do outer

!for 1D-elts, the slopes have to be resetted
resetslopes: do i = 1,elcnt
  if (ncorn(i) == 3) then
!nis,dec06,testing    
!WRITE(*,*) '1D-Elemente:',i    
!-    
    slope(nop(i,1)) = 0
    eslope(nop(i,1))= 0
    slope(nop(i,2)) = 0
    eslope(nop(i,2))= 0
    slope(nop(i,3)) = 0
    eslope(nop(i,3))= 0
  end if
end do resetslopes
!-

!nis,dec06: using another subroutine for 1D-elements
outer1D: do i = 1, elcnt
  if (ncorn(i) == 3) then
    do j = 1,3
!node, whose slopes shall be calculated      
      node2=nop(i,j)
      tmpNode => findNodeInMeshByID (m_SimModel%femesh, node2)
      nconnect = noOfNeighbours (tmpNode)

      if (nconnect > 5) CYCLE outer1D

      if (j == 2) then
!if midside node get the two corner nodes        
        node1=nop(i,1)
        node3=nop(i,3)
      ELSE
!if corner node, get the midside node        
        node1=nop(i,2)
        node3=0
      endif

      CALL Get_1D_slope(node1,node2,node3, slope(nop(i,j)), eslope(nop(i,j)))
      marker_slope(node2) = .true.

    end do
  end if
end do outer1D
!-

! All point that have been marked as not detected (MARKER_SLOPE = .false.)
! the slope will be interpolated from the neighbouring points.
!nis,dec06: Correction of line, replacing mnd with MaxP
call FILL_SLOPES(nodecnt, slope,eslope, marker_slope, m_SimModel%femesh)


deallocate (marker_slope)
Return


end subroutine GET_NODE_SLOPE

!--------------------------------------------------------------------------------------
!nis,dec06: Getting the slopes at 1D-nodes
! This subroutine calculates the bottom and the watersurface slope at 1D-nodes
!
!
!December 2006
!--------------------------------------------------------------------------------------

subroutine GET_1D_SLOPE(node1,node2,node3,slope_temp, eslope_temp)

  USE blk10mod

  REAL(kind=8)                :: dx1, dy1, dl1, dx2, dy2, dl2
  REAL(kind=8), intent(inout) :: slope_temp, eslope_temp
  INTEGER, INTENT (IN)        :: node1, node2, node3

  dx1 = cord(node1,1) - cord(node2,1)
  dy1 = cord(node1,2) - cord(node2,2)
  dl1 = SQRT(dx1*dx1 +dy1*dy1)
  dh1 = vel(3,node1) - vel(3,node2)
  dz1 = ao(node1) - ao(node2)

  if (node3 /= 0) then
    dx2 = cord(node3,1) - cord(node2,1)
    dy2 = cord(node3,2) - cord(node2,2)
    dl2 = SQRT(dx2*dx2 +dy2*dy2)
    dh2 = vel(3,node3) - vel(3,node2)
    dz2 = ao(node3) - ao(node2)
    n = 2
  else
!dividing by zero is not good
    dl2 = 1 
    dh2 = 0
    dz2 = 0
    n = 1
  endif

  if (slope_temp == 0.0) then
    slope_temp = (dz1/dl1+dz2/dl2) / n
  else
    slope_temp = 1/2 * slope_temp + 1/2* (1/n * (dz1/dl1+dz2/dl2))
  end if

  if (eslope_temp == 0.0) then
    eslope_temp = 1/n * (dh1/dl1+dh2/dl2)
  else
    eslope_temp = 1/2 * eslope_temp + 1/2* (1/n * (dh1/dl1+dh2/dl2))
  end if

end subroutine
!--------------------------------------------------------------------------------------


!--------------------------------------------------------------------------------------
subroutine GET_MIN_ANGLE_POINTS(angle_delt, anz, neighbourList, firstNode, secondNode)
!
! The array of the calculated angles to all neighbour nodes ANGLE_DELT is
! analysed in this subroutine. The values of ANGLE_DELT are sorted for negative
! and positive values. The lowest absolut values is declared as FIRST neighbour
! the value with the lowest value with the other sign as SECOND neighbour.
! The crossing point of the flow direction must lie between these two points!

! If the lowest absolut value of ANGLE_DELT is less than 0.0001, the velocity
! vector is nearly exactly heading towards one point. In this case there is no need
! to calculate the crossing point of the velocity vector and the line between the
! FIRST and the SECOND point. In this case the SECOND point is set to -1.
!
!                                               Wolf Ploeger, Jul 2004
!--------------------------------------------------------------------------------------
use mod_Nodes

implicit none

!arguments
REAL(kind=8), DIMENSION(1:100), INTENT(IN)      :: angle_delt
INTEGER, INTENT(IN)                             :: anz
type (linkedNode), pointer :: neighbourList
! node no%ID of nearest neighbour
type (node), pointer :: firstNode    
! node no ID of second nearest neighbour
type (node), pointer :: secondNode   

!local variables
type (linkedNode), pointer :: tmpNode
type (node), pointer :: negNode, posNode
INTEGER :: i, first
REAL (kind=8) :: min_angle_pos, min_angle_neg
INTEGER :: min_nr_pos, min_nr_neg

min_nr_pos = 0
min_nr_neg = 0

min_angle_pos =  100.0
min_angle_neg = -100.0


!Initialize
firstNode => null()
secondNode => null()

! Two points are detected: The nearest neighbour
! with a positiv sign and the nearest neighbour
! with a negative sign.
tmpNode => neighbourList
do i = 1, anz
  if (angle_delt(i) < 0.0) then
    if (ABS (angle_delt (i)) < ABS (min_angle_neg) ) then
      min_angle_neg = angle_delt(i)
      negNode => tmpNode%thisNode
      min_nr_neg = i
    end if
! angle_delt(i) => 0.0
  else 
    if (angle_delt(i) < min_angle_pos) then
      min_angle_pos = angle_delt(i)
      posNode => tmpNode%thisNode
      min_nr_pos = i
    end if
  end if
  tmpNode => tmpNode%next
end do

!Turn around the order of the first and the second node, depending on the minimum absolute angle
if (ABS(min_angle_pos) < ABS(min_angle_neg)) then
  firstNode => posNode
  secondNode => negNode
  first = min_nr_pos
else
  firstNode => negNode
  secondNode => posNode
  first = min_nr_neg
end if

if (ABS(angle_delt(first)) < 0.0001) then
  secondNode => null()
end if

end subroutine GET_MIN_ANGLE_POINTS



!----------------------------------------------------------------------------------------
subroutine FILL_SLOPES (nodecnt, slope, eslope, marker_slope, FE_Mesh)
!
! For some nodes the slope could not be detected directly (e.g. if the node is dry).
! To have a completely filled array of slopes for each node of the mesh, the slope of the
! missing nodes (MARKER_SLOPE = .false.) is interpolated using the neighbouring nodes.
!
!                                               Wolf Ploeger, Jul 2004
!----------------------------------------------------------------------------------------

!nis,jan07: For checking, whether nodes are active, the coordinates must be present
USE blk10mod, only: cord
use parakalyps, only: ispolynomnode
use mod_Nodes
use mod_meshModelFE

implicit none

!arguments
INTEGER, INTENT(IN)  :: nodecnt
REAL(kind=8), INTENT(INOUT) :: slope (1:*)
REAL(kind=8), INTENT(INOUT) :: eslope (1:*)
LOGICAL, INTENT (INOUT) :: marker_slope (1:*)
type (femesh), pointer :: fe_mesh

!local variables
INTEGER :: i, j, anz, temp_anz
integer :: nconnect
REAL (kind=8) :: temp_slope, temp_eslope
type (linkedNode), pointer :: neighbourNode
type (node), pointer :: tmpNode

tmpNode => null()
neighbourNode => null()

anz = 0

! 1.) Number of missing nodes is counted.
! ---------------------------------------
!nis,jan07: Naming this loop
!do i = 1, nodecnt
numbertest: do i = 1, nodecnt

!dead nodes within the network (their coordinates are initialized by -1e20) have to be ignored  
  if (cord(i,1) < (-0.5E20) .AND. cord(i,2) < (-0.5E20)) then
!mark dead nodes as processed    
    marker_slope(i) = .True.
    cycle numbertest
  end if

!mark 1D nodes as having a slope  
  if (IsPolynomNode (i)) then
    marker_slope (i) = .true.
    CYCLE numbertest
  endif

  tmpNode => findNodeInMeshByID (fe_mesh, i)
  nconnect = noOfNeighbours (tmpNode)

!all other nodes, that have no slope yet and have a number of neighbours are counted  
  if ( .NOT. marker_slope(i) .AND. nconnect /= 0) then
  
          anz = anz + 1
  else
    continue
  end if
!end do
end do numbertest
!-

! 2a) Checking for enough energy slope at all
if (anz == nodecnt) then
  do i = 1, nodecnt
    slope (i) = 0.0d0
    eslope (i) = 0.0d0
    return
  enddo
endif
! 2.) Filling
! -----------
! missing values of slope for nodes are filled
! using interpolation from values of neighbour nodes.
! If there are large dry floodplains, it is not possible
! to interpolate the missing values in one loop.
! It has to be iterative!
eliminate: do

!nis,dec06: modelerror causes this temporary change  
  if (anz == 0) exit eliminate

  all_nodes: do i = 1, nodecnt
  
    tmpNode => findNodeInMeshByID (FE_Mesh, i)
    nconnect = noOfNeighbours (tmpNode)

    if (marker_slope(i)) CYCLE all_nodes

!initialize local counters     
     temp_anz = 0
     temp_slope = 0.0
     temp_eslope= 0.0

     neighbourNode => tmpNode%neighbourList
     all_neighb: do j = 1, nconnect
       if ( marker_slope(NeighbourNode%thisNode%ID) .AND. ( .NOT. IsPolynomNode (NeighbourNode%thisNode%ID))) then
         temp_anz   = temp_anz + 1
         temp_slope = temp_slope + slope(NeighbourNode%thisNode%ID)
         temp_eslope = temp_eslope + eslope(NeighbourNode%thisNode%ID)
       end if

       neighbourNode => neighbourNode%next
     end do all_neighb

! Interpolated value will only be applied     
! if more than 2 neighbours have valid values!     
     if (temp_anz >= 2) then
       slope(i) = temp_slope/temp_anz
       eslope(i) = temp_eslope/temp_anz
       marker_slope(i) = .true.
       anz = anz - 1
     end if


  end do all_nodes

end do eliminate

end subroutine FILL_SLOPES




!----------------------------------------------------------------------------------------
!nis,dec06: Only the marker_slope of the passed node i is wanted!!!
! subroutine GET_SLOPE(angle_v, pn, first_neighb, second_neighb, slope, eslope, marker_slope)
 subroutine GET_SLOPE(angle_v, pn, first_neighb, second_neighb, slope, eslope, marker_slope_pn)
!-

! The SLOPE at point PN of the watersurface is calculated.
!
! First the crossing point of the velocity vector and the line between
! FIRST_NEIGHB and SECOND_NEIGHB is calculated. Then the waterlevel at
! this point is determined using the intercept theorem. After calculating
! the distance between point PN and the crossing point, the SLOPE for
! point PN is calculated.
!
!                                                   Wolf Ploeger, Jul 2004
!----------------------------------------------------------------------------------------

!NiS,apr06: The necessary variables/ arrays, that were declared in common.cfg and the Block
!           definition COMMON/raus in Kalypso-2D are now declared in the modules Blk10mod.module
!           and PARAKalyps.module:
!include "common.cfg"
!COMMON / raus / rausv (4, mnd), iauslp, iausnpm, zeigma (mnd)
USE PARAKalyps
USE Blk10mod
use mod_Nodes
!-

! Calling variables
! Velocity vector
REAL(kind=8), DIMENSION(1:2), INTENT(IN)       :: angle_v      
! Point of velocity vector
INTEGER, INTENT(IN) :: pn            
! First neighbour to calculated cross-point.
type (node), pointer :: first_neighb      
! Second neighbour to calculated cross-point.
type (node), pointer :: second_neighb   
!NiS,apr06: changing mnd to MaxP
! Slope to be calculated
REAL(kind=8), INTENT(OUT) :: slope (1:*)       
! Energy-Slope to be calculated
REAL(kind=8), INTENT(OUT) :: eslope (1:*)      
!nis,dec06, just one value is needed here
! Marker if slope could be determined
LOGICAL,INTENT(OUT)                             :: marker_slope_pn 
!-
! Local variables
REAL(KIND=8)       :: ax,ay,bx,by,cx,cy,dx,dy, he_pn, he_cross
REAL(KIND=8)       :: cross_x, cross_y, cross_h, cross_v, v_pn
REAL(KIND=8)       :: h_1_2, d_1_2, d_cross_2, d_cross_pn
REAL(KIND=8)       :: temp1, temp2


! Solving the linear equation
INTEGER :: n = 2
REAL (KIND=8), DIMENSION (2,2) :: A
REAL (KIND=8), DIMENSION (2) :: B
REAL (KIND=8), DIMENSION (2) :: X
LOGICAL :: sing = .false.

! initialize v_pn and he_pn
v_pn  = 0.0
he_pn = 0.0

!nis,dec06: Initializing the marker_slope array entry because of bas passing problems
marker_slope_pn = .false.
!-

!nis,dec06,testing
!WRITE(*,*) 'Reaches get_slope.subroutine'
!-

! calculate v_pn and he_pn
v_pn  = SQRT(rausv(1,pn)**2 + rausv(2,pn)**2)
he_pn = rausv(3,pn) + ((v_pn**2)/(2*9.81))

! 1. CASE
! -------
! The vector is heading exactly towards point FIRST_NEIGHB
! SECOND_NEIGHB is set to 0 in subroutine GET_MIN_ANGLE_POINTS.
if (second_neighb%ID == 0) then
  cross_v    = SQRT(rausv(1,first_neighb%ID)**2 + rausv(2,first_neighb%ID)**2)
  d_cross_pn = SQRT( (cord(first_neighb%ID,1) - cord(pn,1))**2 + (cord(first_neighb%ID,2) - cord(pn,2))**2 )
  slope(pn)  = ABS( (rausv(3,first_neighb%ID) - rausv(3,pn)) / d_cross_pn )
  he_cross   = rausv(3,first_neighb%ID) + ((cross_v**2)/(2*9.81))
  eslope(pn) = ABS( (he_pn - he_cross) / d_cross_pn )
!nis,dec06: Detected error, because line just counts for one node, by the way: only one value is needed in this subroutine
!  marker_slope = .true.
  marker_slope_pn = .true.
!-
  return
end if


! 2. CASE
! -------
! Flow depth of at point FIRST_NEIGHB or SECOND_NEIGHB is very small
! e.g. the node is about to fall dry or rewet or the node is dry.
! => No slope will be calculated.
if (vel(3,first_neighb%ID) < 0.01 .OR. vel(3,second_neighb%ID) < 0.01) then
  slope(pn) = 0.0
  eslope(pn)= 0.0
!nis,dec06: Only one value is needed in this subroutine
!  marker_slope(pn) = .false.
  marker_slope_pn = .false.
!-
  return
END if


! 3. CASE  (normal case!)
! -----------------------
! The velocity vector is heading between the two nodes FIRST_NEIGHB and
! SECOND_NEIGHB. Both nodes are wet!


! 3.1 Calculating the cross-point of velocity vector and
!     connection line between FIRST_NEIGHB and SECOND_NEIGHB
! ----------------------------------------------------------
! The following system of linear equations is solved:
!
!   | ax |            | bx |    | cx |        | dx |
!   |    | + lambda * |    |  = |    | + nu * |    |
!   | ay |            | by |    | cy |        | dy |
!
! where (bx;by) is the velocity vector at point (ax;ay) which
! is in fact PN. (dx;dy) is the connection direction between
! FIRST_NEIGHB and SECOND_NEIGHB and (cx;cy) is the FIRST_NEIGHB.
!
! So the following linear equation must be solved:
!
!    | bx dx |   | lambda |   | cx - ax |
!    |       | * |        | = |         |
!    | by dy |   |   nu   |   | cy - ay |
!
! <=>   A      *     X      =      B


!nis,dec06: Only one value is needed in this subroutine
!marker_slope(pn) = .true.
marker_slope_pn = .true.
!-

ax = cord(pn,1)
ay = cord(pn,2)
bx = angle_v(1)
by = angle_v(2)
cx = cord(first_neighb%ID,1)
cy = cord(first_neighb%ID,2)
dx = cord(second_neighb%ID,1) - cord(first_neighb%ID,1)
dy = cord(second_neighb%ID,2) - cord(first_neighb%ID,2)

a(1,1) = bx
a(1,2) = -dx
a(2,1) = by
a(2,2) = -dy
b(1) = (cx - ax)
b(2) = (cy - ay)

! Solving the linear equation using the subroutine
! GAUSS taken from the book "Mathematik fuer Ingenieure"
! by Ansorge/Oberle, Akademie Verlag GmbH, berlin, 1994
call gauss( N, A, B, X, SING, N)

if (SING) then
! No success for solving equations (singularity)  
  slope(pn) = 0.0
  eslope(pn)= 0.0
!nis,dec06: Only one value is needed in this subroutine
!  marker_slope(pn) = .false.
  marker_slope_pn = .false.
!-
  return
end if

cross_x = ax + X(1) * bx
cross_y = ay + X(1) * by


! 3.2 Calculation of elevation and velocity at cross point
! -------------------------------------------

! Difference of water-level elevation between first- and second_neighb.
h_1_2 = rausv(3,first_neighb%ID) - rausv(3,second_neighb%ID)

! Difference of velocity between first- and second_neighb.
v_1   = SQRT(rausv(1, first_neighb%ID)**2 + rausv(2,first_neighb%ID)**2)
v_2   = SQRT(rausv(1, second_neighb%ID)**2 + rausv(2,second_neighb%ID)**2)
v_1_2 = v_1 - v_2

! Distance between first- and second_neighb.
temp1 = (cord(second_neighb%ID,1) - cord(first_neighb%ID,1))**2
temp2 = (cord(second_neighb%ID,2) - cord(first_neighb%ID,2))**2
d_1_2 = SQRT(temp1 + temp2)

! Distance between cross point and second_neighb.
temp1 = (cross_x - cord(second_neighb%ID,1))**2
temp2 = (cross_y - cord(second_neighb%ID,2))**2
d_cross_2 = SQRT(temp1 + temp2)

! Elevation at cross point
cross_h = h_1_2 * (d_cross_2/d_1_2) + rausv(3,second_neighb%ID)

! Velocity at cross point
cross_v = v_1_2 * (d_cross_2/d_1_2) + v_2


! 3.3 Slope of water surface
! --------------------------

! Distance from PN to cross point
d_cross_pn = SQRT( (cross_x - cord(pn,1))**2 + (cross_y - cord(pn,2))**2 )

if (d_cross_pn < 0.0001) then
  slope(pn) = 0.0
  eslope(pn)= 0.0
!nis,dec06: Only one value is needed in this subroutine
!  marker_slope(pn) = .false.
  marker_slope_pn = .false.
!-
  return
end if

! Slope of water level
slope(pn) = ABS( (cross_h - rausv(3,pn)) / d_cross_pn )

! Slope of energy level
he_cross = cross_h + ((cross_v**2)/(2*9.81))

eslope(pn) = ABS( (he_cross - he_pn) / d_cross_pn )

end subroutine GET_SLOPE





!--------------------------------------------------------------------------
subroutine GET_ANGLE(vec1, vec2, delt_angle)
!
! The angle between the two vector is calculated.
!
!                                          Wolf Ploeger, Jul 2004
!--------------------------------------------------------------------------

implicit none

! Calling variables
! Flow direction
REAL(kind=8), DIMENSION(1:2), INTENT(IN)       :: vec1       
! Direction between points
REAL(kind=8), DIMENSION(1:2), INTENT(IN)      :: vec2       
! angle difference
REAL(kind=8), INTENT(OUT)                   :: delt_angle       

! Local variables
REAL(kind=8)                    :: alpha1, alpha2
REAL(kind=8), parameter         :: pi = 3.14159265359


! Based on the vector of the flow direction VEC1 the angle to the
! different points is calculated with:
!   left of VEC1  : positiv
!   right of VEC1 : negativ

alpha1 = ATAN2(vec1(2),vec1(1))
alpha2 = ATAN2(vec2(2),vec2(1))

if (alpha1 < 0.0) then

  if (alpha2 < 0.0) then

    delt_angle = alpha2 - alpha1

  else
! Alpha1 < 0.0 .AND. alpha2 >= 0.0    

    if ((alpha1 + pi) > alpha2) then

      delt_angle = ABS(alpha1) + alpha2

    else
! alpha1 < 0.0, alpha2 >= 0.0      
      delt_angle = -(2.0 * pi) + ( ABS(alpha1) + alpha2)

    end if

  end if

else
! Alpha1 >= 0.0  
  if (alpha2 >= 0.0) then

    delt_angle = alpha2 - alpha1

  else
! Alpha1 >= 0.0 .AND. alpha2 < 0.0    
    if ((alpha1 - pi) < alpha2) then

      delt_angle = - ( ABS(alpha2) + alpha1)

    else

      delt_angle = (2.0 * pi) - (ABS(alpha2) + alpha1)
    end if

  end if

end if

end subroutine GET_ANGLE





!********************************************************************************
!***    Subroutine GET_CWR
!********************************************************************************

!--------------------------------------------------------------------------------
subroutine GET_CWR(I_R,      h_m, a_x, d_p, lambda_s, c_wr)
!
! The drag coefficient C_WR of vegetation in an element with the
! characteristic properties
! - slope of water surface (I_R),
! - mean flow depth (H_M),
! - mean distance between trees/branches (A_X),
! - mean diameter of trees/branches (D_P) and
! - friction factor of bottom surface
! is calculated.
! The method of LINDNER (modified by PASCHE) like published in
! DVWK 220: Hydraulische Berechnung von Fließgewaessern" 1991 is applied.
! This method contains some nested iterations that may lead to undesirable
! convergence problems espacially with very small values for the slope and
! the water depth. Therefor some limitations have been implemented to
! always get realistic results.

implicit none

! calling variables
! Slope
REAL, INTENT(IN) :: I_R          
! Mean flow depth
REAL (KIND = 8), INTENT(IN) :: h_m 
!-
! distance of trees/branches
REAL, INTENT(IN)            :: a_x      
! diameter of trees/branches
REAL, INTENT(IN)            :: d_p      
! friction factor by COLBROOK/WHITE
REAL (KIND = 8), INTENT(IN) :: lambda_s 
! Drag coefficient for the vegetation
REAL, INTENT(OUT)           :: c_wr          

! Local variables
REAL :: a_y
! temp c_wr for iteration
REAL :: c_wr2   = 0.0          
! mean flow velocity
REAL :: v_vor   = 0.0          
! drag coefficient for a single cylinder
REAL :: c_wr_un = 0.0          
! wake length
REAL :: a_NL    = 0.0          
! temp wake length
REAL :: a_NL_anf= 0.0          
! wake width
REAL :: a_NB    = 0.0          
! velocity ratio
REAL :: vn_vvor2= 0.0          
! water depth ratio
REAL :: y_stern = 0.0          
! c_temp = a_y / (a_y -d_p)
REAL :: c_temp  = 0.0          
! temporary total lambda
REAL :: lambda_g= 0.0          
! temporary vegeation lambda
REAL :: lambda_p= 0.0          
! Froudenumber 1
REAL :: Fr1     = 0.0          
! Froudenumber 2
REAL :: Fr2     = 0.0          
! Delta C_W
REAL :: delta_cw= 0.0          
! viscosity of water for 10 degrees.
REAL :: nu_10       = 0.0          

!REAL :: GET_CW_UN
!REAL :: GET_A_NL
!REAL :: GET_A_NB

INTEGER :: i, j

nu_10 =  1.e-06

! Symetrical arangement of plants
a_y = a_x

! Schätzung von C_WR = 1.0
c_wr  = 1.0                             
! Schätzung von C_WR2 = 1.5 (nur am Anfang, damit
c_wr2 = 1.5                             
! CW_R und CW_R2 unterschiedliche Startwerte haben)                                        

! Schätzung von a_NL_anf = 2 * ax (Für Funktion GET_A_NL)
a_NL_anf  = 2 * a_x                     

! Temporäre Variable, nur zur Vereinfachung der
c_temp = a_y / (a_y -d_p)               
! folgenden Berechnungen                                        

! Zähler der äußeren Iteration von C_WR
j = 0                                   

! -------------------   Iteration bis Konvergenzkriterium für C_WR erfüllt ist
iteration_cwr: do 

! Bei jeder neuen Iteration müssen die beiden Froudezahlen
      Fr1 = 0.0                       
! neu bestimmt werden -> Y_STERN
        Fr2 = 0.0                       

! Inkrementieren des Schleifenzählers
        j = j + 1                       

! Abbruchkriterium für die Konvergenz, ist jetzt willkürlich                                        
! auf 0.01 gesetzt, kann auch erhöht oder verringert werden.                                        
! (Komprimiss Geschwindigkeit <-> Genauigkeit)                                        


        if ( ABS(1-(c_wr/c_wr2)) < 0.002 ) exit iteration_cwr

        if (c_wr2 > 2.5) then
            c_wr = 1.30
            EXIT iteration_cwr
        end if

        c_wr = c_wr2

!WP        
! Mit Hilfe von LAMBDA_SO wird ein neuer Gesamtwiderstandsbeiwert        
! durch lineare Überlagerung berechnet. Damit wird eine neue        
! Fließgeschwindigkeit auf dem Vorland V_VOR ermittelt.        
! -> Nötig für Berechnung von C_WR_UN und a_NL!        
!WP        

        lambda_p = (4 * c_wr * h_m * d_p)/(a_x * a_y)
        lambda_g = lambda_p + lambda_s

        v_vor = 1/SQRT(lambda_g) * SQRT(8 * 9.81 * I_R * h_m)

! Bestimmung von C_WR_UN
        c_wr_un = GET_CW_UN(v_vor, d_p, nu_10)  

! Bestimmung von a_NL, gibt 0.0 zurueck, falls Fehler aufgetreten ist.
        a_NL = GET_A_NL(c_wr_un, d_p, I_R, &    
                 & v_vor, a_NL_anf)

        if (a_NL == 0.0) then
          write (14,*) 'Fehler bei GET_A_NL! -> ABBRUCH'
          stop
        end if

! Nachlaufbreite
      a_NB = GET_A_NB(c_wr_un, d_p, a_NL)     

! WP 19.10.2004: Der Faktor 0.5 vor dem zweiten Term fehlte!        
        vn_vvor2 = 1.15 * (a_NL/a_x) ** (-0.48) + 0.5 * (a_NB/a_y) ** 1.1

! Bestimmung der Froudezahl der Strömung
        Fr1 = v_vor / SQRT(9.81 * h_m)             

        y_stern = 1.00

        i = 0

!--------      Iteration für Fr2 über y_stern-----------------------
        iteration_y_stern: do 

                i = i+1

!WP                
! Mehrere Versuche haben gezeigt, dass sich bei der                
! Iteration von Fr nach y_stern Problemem bei sehr                
! niederigen Froudzahlen einstellen! Da die zu lösende                
! Gleichung quadratisch ist, und der Wert von y_stern                
! nahe 1 liegen muss, ist gerade am Anfang in sehr                
! kleinen Schritten vorzugehen!                
! Bei Werten von Fr1 < 0.1 wird zur Erhöhung der                
! Stabilität von einem konstanten y_stern = 0.999 ausgegangen.                
!WP                

                if (Fr1 < 0.1 ) then
                    y_stern = 0.9999
                    exit iteration_y_stern
                else if (Fr1 < 0.3 .AND. Fr1 >= 0.1) then
                    y_stern = y_stern - 0.0001
                else if (Fr1 < 0.6 .AND. Fr1 >= 0.3) then
                    y_stern = y_stern - 0.001
                else
                    y_stern = y_stern - 0.01
                end if

                Fr2 = (y_stern * (y_stern**2 -1)) / (2 * (y_stern - c_temp))
                if (fr2<0.0) then 
                  y_stern = 0.9999
                  exit iteration_y_stern
                endif
                Fr2 = SQRT(Fr2)

                if ( ABS(1-(Fr1/Fr2)) < 0.1) exit iteration_y_stern

                if (i > 100) then
                    y_stern = 0.9999
                    exit iteration_y_stern
                END if

!-------- Iteration für Fr2 über y_stern-----------------------
        end do iteration_y_stern 

!WP        
! Während der ersten fünf Iterationszyclen wird zunächst        
! "ungebremst" iteriert, um eine möglichst schnelle        
! Annäherung an den Zielwert zu erreichen.        
! Es hat sich gezeigt, dass sich die Iteration kurz        
! vor Erreichen des Zielwertes manchmal in einer stabilen        
! Schwingung "verfängt". Deshalb wird ab der fünften        
! Iteration eine Begrenzung des Iterationsschrittes        
! eingebaut.        
!WP        

        if (j < 5) then
            delta_cw = 2 * (1-y_stern) / (Fr1**2)
        else if (j >= 5 .AND. j < 10) then
            delta_cw = (delta_cw + (2 * (1-y_stern) / (Fr1**2))) / 2
        else if (j >= 10) then
            delta_cw = (2*delta_cw + (2 * (1-y_stern) / (Fr1**2))) / 3
        end if

! Auch hier wird die Veränderung des Iterationsparameters durch Einbeziehung        
! des vorherigen Wertes abgebremst!        
        c_wr2 = (c_wr + (1.3124 * c_wr_un * vn_vvor2 + delta_cw)) / 2

! Keine Konvergenz nach 100 Iterationen
        if (j > 50) then               
            c_wr = 1.30
            EXIT iteration_cwr
        END if

! -----------------  Iteration bis Konvergenzkriterium für C_WR erfüllt ist
end do iteration_cwr 

end subroutine GET_CWR




!------------------------------------------------------------
REAL function GET_CW_UN(v, d_p, nu)
!
! Calculation of drag coefficient for a single
! cylindrical object, depending on Reynolds number.
!
!                               Wolf Ploeger, Jul 2004
!------------------------------------------------------------

implicit none
! calling variables
! Fließgeschwindigkeit
REAL, INTENT(IN) :: v            
! Durchmesser des Bewuchselementes
REAL, INTENT(IN) :: d_p        
! kinematische Viskosität
REAL, INTENT(IN) :: nu         

! Local variables
! Reynolds-Zahl, bezogen auf den Bewuchs
REAL :: Re_p                  

Re_p = (v * d_p) / nu

if (Re_p <= 800) then
  GET_CW_UN = 3.07 * Re_p ** (-0.168)
else if (Re_p > 800 .AND. Re_p <= 8000) then
  GET_CW_UN = 1.0
else
  GET_CW_UN = 1.2
end if

end function GET_CW_UN



!------------------------------------------------------------
REAL function GET_A_NL(c_wr_un, d_p, I_R, v, anf)
!
! Calculation of wake length
!
!                             Wolf Ploeger, Jul 2004
!------------------------------------------------------------

implicit none
! calling variables
! Widerstandsbeiwert
REAL, INTENT(IN) :: c_wr_un    
! Durchmesser des Bewuchselementes
REAL, INTENT(IN) :: d_p        
! Stationäres Gefälle
REAL, INTENT(IN) :: I_R        
! Maßgebende Fließgeschwindigkeit
REAL, INTENT(IN) :: v            
! Anfangswert für Iteration ( = 2 * ax )
REAL, INTENT(IN) :: anf            

! Local variables
INTEGER :: i
REAL :: a_NL
REAL :: a_NL2
REAL :: g

! Initialisierung
g = 9.81
a_NL  = anf
a_NL2 = 2 * anf

! Zähler der Iteration von a_NL
i = 0                                 

! Iteration bis Konvergenzkriterium für a_NL erfüllt ist
iteration_a_NL: do                  

  if ( ABS(1-(a_NL/a_NL2)) < 0.001 ) exit iteration_a_NL
! Inkrementieren des Schleifenzählers
  i = i + 1                           
  a_NL  = a_NL2
  a_NL2 = (a_NL + 128.9 * c_wr_un * d_p * &
     & (1 + (g * a_NL * I_R)/((v**2)/2))**(-2.14)) / 2

! Keine Konvergenz nach 100 Iterationen
  if (i > 50) then                   
    WRITE(*,*) 'Fehlerhaftes Element ist: '
    WRITE(*,*) 'anl = ', a_nl
! Der Wert 0.0 zeigt dem aufrufenden Programm
    GET_A_NL = 0.0                      
! an, dass die Iteration NICHT erfolgreich war!
    GOTO 100                            
  END if

! Iteration von a_NL erfolgreich!
end do iteration_a_NL                  

! Zuweisung des endgültigen Wertes
GET_A_NL = a_NL                         

100 continue

END function GET_A_NL




!------------------------------------------------------------
REAL function GET_A_NB(c_wr_un, d_p, a_NL)
!
! Calculation of wake width
!
!                             Wolf Ploeger, Jul 2004
!------------------------------------------------------------

implicit none
! calling variables
! Widerstandsbeiwert
REAL, INTENT(IN) :: c_wr_un    
! Durchmesser des Bewuchselementes
REAL, INTENT(IN) :: d_p        
! Nachlauflänge
REAL, INTENT(IN) :: a_NL            

GET_A_NB = 0.24 * (a_NL ** 0.59) * (c_wr_un * d_p) ** 0.41

END function GET_A_NB



!--------------------------------------------------------------------------------
SUBROUTINE cwr_write(name_cwr, ne, mcord, c_wr, mslope, meslope, mh, mvxvy, mel)
!
! Writing the drag coefficients for all elements to a additional file (*.cwr)
!
!                                                        Wolf Ploeger, Jul 2004
!--------------------------------------------------------------------------------

implicit none

! Calling variables
CHARACTER(LEN=32), INTENT(IN)                   :: name_cwr
INTEGER, INTENT(IN)                         :: ne
INTEGER, INTENT(IN)                             :: mel
REAL(KIND=8), DIMENSION(1:mel,1:2), INTENT(IN)       :: mcord
REAL(kind=8), DIMENSION(1:mel), INTENT(IN)       :: c_wr
REAL(kind=8), DIMENSION(1:mel), INTENT(IN)       :: mslope
REAL(kind=8), DIMENSION(1:mel), INTENT(IN)       :: meslope
!NiS,jul06: Consistent data type for passing parameters
!REAL(kind=8), DIMENSION(1:mel), INTENT(IN)       :: mh
REAL(KIND=8), DIMENSION(1:mel), INTENT(IN)       :: mh
!-
REAL(kind=8), DIMENSION(1:mel), INTENT(IN)       :: mvxvy


! Local variables
INTEGER :: istat
INTEGER :: i

! mcord - Koordinaten des Elementmittelpunktes

OPEN(14, FILE=name_cwr, STATUS='REPLACE', IOSTAT=istat)
if (istat /= 0) then
  write (*,9000) name_cwr
  9000 format (1X, 'Fehler beim Oeffnen der Datei: ', A / &
             & 1X, 'Abbruch!')
  stop
end if
REWIND(14)

write (14,1000) 'Element', 'X-Coord', 'Y-Coord', 'C_wr', 'Slope','E-Slope', 'MH', 'MV'

write_elements: do i = 1, ne

! This check is necessary, if there are elements defined, that have no connection  
! to the mesh and are not calculated. Then for example mcord(i,1) = -NaN.  
  if (mcord(i,2) > 0.0 .AND. mcord(i,2) > 0.0) then
    write (14,1001) i, mcord(i,1), mcord(i,2), c_wr(i), mslope(i), meslope(i), mh(i), mvxvy(i)
  end if

end do write_elements

close (14)

1000 format (1X, A8, 2A15,   A10,   A15,    A15,    A10,   A10 )
!NiS,may06: format for slope and eslope generates asterisks sometimes; changed to scientific notation
!1001 format (1X, I8, 2F15.5, F10.4, F15.10, F15.10, F10.4, F10.4)
1001 format (1X, I8, 2F15.5, F10.4, 2ES15.5, F10.4, F10.4)
!-
END subroutine cwr_write





!----------------------------------------------------------------------------------
subroutine cwr_init
!
! Initialising and control of start values for C_WR values.
!
!                                                        Wolf Ploeger, Jul 2004
!----------------------------------------------------------------------------------

!NiS,apr06: arrays of common-block now in module Blk10mod and PARAKalyps:
!include "common.cfg"
USE Blk10mod
USE PARAKalyps
!-

! local variables
INTEGER :: i
INTEGER :: pos_dot
LOGICAL :: lexist

!NiS,apr06: name of variable changed!
!do i = 1, mel
do i = 1, MaxE
!-
  c_wr(i) = 1.0
end do

pos_dot = INDEX(modellein, TRIM(modellaus))

!NiS,apr06: unit name changed; changed iout to Lout
write (Lout,1000)
1000 format (/1X, '-------------------------------------------------------' /&
            & 1X, 'Initialisierung fuer Berechnung von cwr fuer Vegetation' /&
            & 1X, '-------------------------------------------------------' /)

!NiS,may,06: unit name changed; changed iout to Lout
write (Lout,1001) modellein, modellaus
1001 format (1X, 'Modell/Restart Datei: ',A/ &
            &1X, 'Endung der Dateien:   ',A)

! Erzeugen des Dateinamens fuer die Morphodatei mit anderer Endung
! Variable 'name_bedform' definiert in 'common.cfg', character(length=32); 'modellein' Name der Startloesung
name_cwr = modellein(1:(pos_dot-2)) // '.cwr'

1003 format (/1X, 'C_WR Datei:        ',A)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Nis,apr06,commment:
!Test, whether the mentioned file exist or default values have to be used
!Tell the user, if the file does not exist.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
inquire (FILE=name_cwr, EXIST=lexist)
if (lexist) then

!NiS,apr06: unit name has changed; changed iout to Lout
  write (Lout, 1010)
  write (*   , 1010)
!-
  1010 format (1X, 'Die entsprechende C_WR Datei existiert bereits!')
!NiS,apr06: name of variable changed; changed mel to MaxE  
  call cwr_read(name_cwr, c_wr, MaxE)
!-  
else
!NiS,apr06: unit name changed; changed iout to Lout  
  write (Lout, 1011)
  write (*   , 1011)
!-  
  1011 format (1X, 'Es muessen neue C_WR Werte erzeugt werden...')

!Initialisierung der Parameter  
!NiS,apr06: name of variable changed; why is there sometimes ne and sometimes  
!           mel, used for the number of elements; this is irritating!  
!do i=1,ne  
  do i=1,MaxE
!-  
    c_wr(i) = 1.30
  end do

!NiS,apr06: unit name changed; changed iout to Lout  
  write (Lout, 1012)
  write (*   , 1012)
!-  
  1012 format (1X, '...fertig!')

end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!End if-clause for setting up the values for cwr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!NiS,apr06: unit name changed, changed iout to Lout
write (Lout,1100)
write (*   ,1100)
!-
1100 format (/1X, '-------------------------------------------------------' /&
            & 1X, 'Initialisierung von C_WR beendet'                        /&
            & 1X, '-------------------------------------------------------' /)

end subroutine cwr_init




!--------------------------------------------------------------------------------------------
SUBROUTINE cwr_read(name_cwr, c_wr, mel)
!
! Reading a file containing C_WR values for all elements
!
!                                                                   Wolf Ploeger, Jul 2004
!--------------------------------------------------------------------------------------------

implicit none

! Calling variables
!name of cwr initial input data file
CHARACTER(LEN=32), INTENT(IN)               :: name_cwr  
!local copy of variable for maximum number of elements
INTEGER, INTENT(IN)                         :: mel       
!array for saving all the initial c_wr-values for every element
REAL(KIND=8), DIMENSION(1:mel), INTENT(OUT) :: c_wr      

! Local variables
INTEGER :: istat
INTEGER :: temp_nr
INTEGER :: i
CHARACTER(LEN=80) :: zeile
REAL(kind=8) :: temp_y, temp_x, temp_cwr

OPEN(13, FILE=name_cwr, STATUS='OLD', IOSTAT=istat)
if (istat /= 0) then
  write (*,1000) name_cwr
  stop
end if
REWIND(13)

! Ueberschreiben der Werte fuer c_wr bei Bewuchselemente
read (13, *) zeile

do

  READ (13, *, IOSTAT=istat) temp_nr, temp_x, temp_y, temp_cwr
  if (istat<0) exit
  if (istat>0) then
    write (*,1001) name_cwr, temp_nr
    close (13)
    stop
  end if
! temp - Elementnummer
  c_wr(temp_nr) = temp_cwr            

end do

close (13)

1000 format (1X, 'Error while opening the file: ', A / &
           & 1X, '=> STOP!')
1001 format (1X, 'Error while reading the file: ', A, /&
           & 1X, 'Error occurs near element ', I7,'. Maybe this', / &
           & 1X, 'element is corrupted?', / &
           & 1X, '=> STOP!')

END subroutine cwr_read




end module
