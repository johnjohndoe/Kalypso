module mod_Arcs
  use mod_Nodes
  type arc
    type (node), pointer :: first => null()
    type (node), pointer :: last => null()
    type (node), pointer :: midside => null()
    type (arc), pointer :: nextSeg => null()
    real (kind = 8) :: posNormal (1:2) = (/0.0d0, 0.0d0/)
  end type
  
CONTAINS
  
  function newArc (first, last)
    implicit none
!function name    
    type (arc), pointer :: newArc
!arguments    
    type (node), pointer :: first, last
!local variables    
    type (arc), pointer :: new
!allocate new node    
    allocate (new)
!set parameters    
    new%first => first
    new%last => last
!overgive new arc    
    newArc => new
    return
  end function
  
  function arcVector (inArc)
    implicit none
!function name    
    real (kind = 8), dimension (1:2) :: arcVector
!arguments    
    type (arc), pointer :: inArc
!get Arc vector    
    arcVector (1) = inArc%last%cord(1) - inArc%first%cord(1)
    arcVector (2) = inArc%last%cord(2) - inArc%first%cord(2)
    return
  end function
  
  subroutine addMidside (inArc, midside)
    implicit none
    type (arc), pointer :: inArc
    type (node), pointer :: midside
!assign midside node    
    if ( .NOT. (associated (inArc%midside))) inArc%midside => midside
  end subroutine
  
  function defaultNormal (inArc, scaling)
    implicit none
!function name    
    real (kind = 8), dimension (1:2) :: defaultNormal
!arguments    
    type (arc), pointer :: inArc
    real (kind = 8), optional, intent (in) ::scaling
!local variables    
    real (kind = 8), dimension (1:2) :: arcVec
    real (kind = 8) :: vecNorm
    
!get Arc vector    
    arcVec = arcVector (inArc)
!get vector norm (length)    
    vecNorm = sqrt (arcVec(1)**2 + arcVec(2)**2)
!calculate default normal    
    if (arcVec(1) == 0.0d0) then
      defaultNormal (1) = 1.0d0 * arcVec(2)/ vecNorm
      defaultNormal (2) = 0.0d0
    elseif (arcVec(2) == 0.0d0) then
      defaultNormal (1) = 0.0d0
      defaultNormal (2) = (-1.0d0) * arcVec(1)/ vecNorm
    else
      defaultNormal (2) = ((-1.0) * vecNorm / (arcVec(2)**2/ arcVec(1) + arcVec(1)))
      defaultNormal (1) = ((-1.0) * defaultNormal (2) * arcVec(2)/ arcVec(1))
    endif
    if (present (scaling)) then
      defaultNormal (1) = defaultNormal (1) * scaling
      defaultNormal (2) = defaultNormal (2) * scaling
    endif
    return
  end function
    
  function gramSchmidtUnitNormal (inArc)
    implicit none
!function name    
    real (kind = 8), dimension (1:2) :: gramSchmidtUnitNormal
!arguments    
    type (arc), pointer :: inArc
!local variables    
    real (kind = 8), dimension (1:2) :: vecBase
    real (kind = 8), dimension (1:2)  :: vec2Proj
    real (kind = 8) :: vecBaseBaseProd
    real (kind = 8) :: vecProjBaseProd
    real (kind = 8) :: gramSchmidtUnitNormalNorm
    integer (kind = 2) :: i
!initializations    
    vecBaseBaseProd = 0.0d0
    vecProjBaseProd = 0.0d0
    gramSchmidtUnitNormalNorm = 0.0d0
    
!assign vectors    
    vecBase = arcVector (inArc)
    vec2Proj = inArc%posNormal
!calculate vector products v1*v1 and v2*v1    
    do i = 1, 2
      vecBaseBaseProd = vecBaseBaseProd + vecBase (i)**2
      vecProjBaseProd = vecProjBaseProd + vec2Proj (i) * vecBase (i)
    end do
!calculate the gramSchmidtNormal    
    do i = 1, 2
      gramSchmidtUnitNormal (i) = vec2Proj(i) - vecProjBaseProd/ vecBaseBaseProd * vecBase (i) 
    end do
    gramSchmidtUnitNormalNorm = sqrt (gramSchmidtUnitNormal(1)**2 + gramSchmidtUnitNormal(2)**2)
    do i = 1, 2
      gramSchmidtUnitNormal (i) = gramSchmidtUnitNormal (i) / gramSchmidtUnitNormalNorm
    enddo
    return
  end function
  
  function normalDirPointer (inArc)
    implicit none
!function name    
    real (kind = 8) :: normalDirPointer
!arguments    
    type (arc), pointer :: inArc
!local variables    
    real (kind = 8), dimension (1:2) :: arcVec
    
    arcVec = arcVector (inArc)
    normalDirPointer = inArc%posNormal(1) * arcVec(2) - arcVec(1) * inArc%posNormal(2)
    if (normalDirPointer < 0.0) then
      normalDirPointer = -1.0d0
    elseif (normalDirPointer > 0.0) then
      normalDirPointer = 1.0d0
    else
      continue
!TODO: Error message      
    endif
    return
  endfunction
  
  function projectionDirPointer (Vector1, Vector2)
    implicit none
!function name    
    real (kind = 8) :: projectionDirPointer
!arguments    
    real (kind = 8), dimension (1:2), intent(in) :: Vector1, Vector2
!local variables    
    real (kind = 8), dimension (1:2) :: vecOne, vecTwo
    real (kind = 8) :: normVecOne, normVecTwo, dotproduct
!local changeable copy of Vectors    
    vecOne = Vector1
    vecTwo = Vector2
!scale both vectors to unit vectors    
    call scaleToUnitVector (vecOne)
    call scaleToUnitVector (vecTwo)
!compare vector directions    
!Because both vectors are of unit length, the dot-product gives the angle between them!    
    dotproduct = vecOne(1) * vecTwo(1) + vecOne(2) * vecTwo(2)
    if (dotproduct >= 0) then
      projectionDirPointer = 1.0
    else
      projectionDirPointer = (-1.0)
    endif
    return
  end function
  
  subroutine scaleToUnitVector (Vector, direction)
    implicit none
!arguments    
    real (kind = 8), intent (inout), dimension (1:2) :: Vector
    real (kind = 8), optional :: direction
!local variables    
    real (kind = 8) :: normOfVector
    integer (kind = 2) :: i
    
    normOfVector = sqrt (Vector(1)**2 + Vector(2)**2)
    if (normOfVector == 0) then
      continue
    endif
    do i = 1, 2
      Vector(i) = Vector (i) / normOfVector
      if (present (direction)) Vector(i) = Vector (i) * direction
    enddo 
  end subroutine

end module
