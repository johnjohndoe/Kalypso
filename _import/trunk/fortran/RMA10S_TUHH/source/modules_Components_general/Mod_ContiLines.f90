module mod_ContiLines
  use mod_Nodes
  use mod_Arcs
  use mod_storageElt

  type contiLine
    integer (kind = 4) :: ID = 0
    logical :: isInnerBoundary = .false.
    type (linkedNode), pointer :: firstNode => null()
    type (linkedNode), pointer :: lastNode => null()
    type (arc), pointer :: firstSegment => null()
    real (kind = 8) :: posNormal (1:2) = (/0.0d0, 0.0d0/)
    real (kind = 8) :: km
    type (StorageElement), pointer :: storageElt => null()
  end type
  
CONTAINS
  
  function newContiLine (ID, km)
    implicit none
    !function name
    type (contiLine), pointer :: newContiLine
    !arguments
    integer (kind = 4) :: ID
    real (kind = 8), optional :: km
    !local variables
    type (contiLine), pointer :: new
    
    !allocate new contiLine
    allocate (new)
    !set parameters
    new.ID = ID
    if (present (km)) new.km = km
    !overgive the new contiLine
    newContiLine => new
    return
  end function
  
  subroutine addNode (ccl, nextNode)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    type (linkedNode), pointer :: nextNode
    !local variables
    type (linkedNode), pointer :: temp => null()
    
    !associate the passedNode
    if (.not. (associated (ccl.firstNode))) then
      ccl.firstNode => nextNode
    else
      ccl.lastNode.next => nextNode
      ccl.lastNode.next.prev => ccl.lastNode
    endif 
    !change last node
    ccl.lastNode => nextNode
  end subroutine
  
  !add segment constructs to continuity line
  subroutine calcSegments (ccl)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    !local variables
    type (linkedNode), pointer :: currNode => null()
    type (arc), pointer :: nextArc => null()
    type (arc), pointer :: currArc => null()
    !check for 1D ccl
    if (associated (ccl.firstNode.next) .and. (.not. (associated (ccl.lastNode.next)))) then
      calcSegs: do 
        !get ID of previous arc
        if (.not. (associated (nextArc)))  allocate (nextArc)
        if (.not. (associated (ccl.firstSegment))) currNode => ccl.firstNode
        if (.not. (associated (currNode.next))) exit calcSegs
        nextArc = newArc (currNode.thisNode, currNode.next.thisNode)
        if (.not. (associated (ccl.firstSegment))) ccl.firstSegment => nextArc
        if (.not. (associated (currArc))) then
          currArc => nextArc
        else
          currArc.nextSeg => nextArc
          currArc => nextArc
        endif
        currNode => currNode.next
        nullify (nextArc)
      enddo calcSegs
    endif 
  end subroutine
  
  subroutine assignSegPosNormals (ccl)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    !local variables
    type (arc), pointer :: tmpSeg
    real (kind = 8), dimension (1:2) :: cclVec
    real (kind = 8) :: cclNormalPointer
    
    if (associated (ccl.firstSegment)) then
      allocate (tmpSeg)
      tmpSeg = newArc (ccl.firstNode.thisNode, ccl.lastNode.thisNode)
      cclVec = arcVector (tmpSeg)
      !z-component of vector cross product
      cclNormalPointer = ccl.posNormal(1) * cclVec (2) - ccl.posNormal(2) * cclVec (1)
      if (cclNormalPointer < 0.0d0) then
        cclNormalPointer = -1.0d0
      elseif (cclNormalPointer > 0.0d0) then
        cclNormalPointer = 1.0d0
      else
        continue
        !TODO: ErrorMessage
      endif
      deallocate (tmpSeg)
   
      tmpSeg => ccl.firstSegment
    
      assignSegNormals: do
        tmpSeg.posNormal = defaultNormal (tmpSeg, cclNormalPointer)
        if (.not. (associated (tmpSeg.nextSeg))) exit assignSegNormals
        tmpSeg => tmpSeg.nextSeg
      enddo assignSegNormals
    endif
  end subroutine
  
  !add an ID to the continuity line
  subroutine addID (ccl, ID)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    integer (kind = 4), intent (in) :: ID
    ccl.ID = ID
  end subroutine
  
  subroutine addkm (ccl, km)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    real (kind = 8), intent (in) :: km
    ccl.km = km
  end subroutine
  
  subroutine setChordNormal (ccl)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    !localVariables
    type (arc), pointer :: tmpArc
    
    !check for 1D ccls (on 2D ccls, the first node always has a following one)
    if (associated (ccl.firstNode.next)) then
      !if to process allocate new arc
      allocate (tmpArc)
      !create temporary arc out of ccl's chord
      tmpArc = newArc (ccl.firstNode.thisNode, ccl.lastNode.thisNode)
      
      !calculate the standard positiveNormal, if there's no normal already calculated
      if (ccl.posNormal(1) == 0.0d0 .and. ccl.posNormal(2) == 0.0d0) then
        ccl.posNormal = defaultNormal (tmpArc)
      !check, whether it is really a normal
      else
        tmpArc.posNormal = ccl.posNormal
        ccl.posNormal = gramSchmidtUnitNormal (tmpArc)
      endif
    endif 
  end subroutine

  subroutine calcPositiveSegmentNormals (ccl)
    !implicit none
  end subroutine
  
end module