module mod_Nodes
  !linked types
  use mod_BCs
  
  integer (kind = 4), parameter :: enum_empty_BCtype = 0
  integer (kind = 4), parameter :: enum_V_BCtype = 1
  integer (kind = 4), parameter :: enum_H_BCtype = 2
  

  type node
    integer (kind = 4) :: ID = 0
    real (kind = 8) :: cord (1:2) = (/0.0d0, 0.0d0/)
    real (kind = 8) :: ao = 0.0d0

    type (boundaryCondition), pointer :: currentBC => null()
    type (boundaryCondition), pointer :: previousBC => null()
  end type

  type linkedNode
    type (node), pointer :: thisNode => null()
    type (linkedNode), pointer :: prev => null()
    type (linkedNode), pointer :: next => null()
  end type
  
  type boundaryCondition
    integer (kind = 4) :: bctype = enum_empty_BCtype
    real (kind = 8) :: H = 0.0
    real (kind = 8) :: V (1:2) = [0.0, 0.0]
  end type
  
  
contains

  function newBC (bc_type, bc_value)
    !function name
    type (boundaryCondition), pointer :: newBC
    !arguments
    integer (kind = 4) :: bc_type
    real (kind = 8), optional :: bc_value (*)
    !local variables
    type (boundaryCondition), pointer :: tmp_bc => null()
    
    !allocate boundary condition memory
    allocate (tmp_bc)
    !generate BC, by type
    tmp_bc.bctype = bc_type
    !set value if present
    if (present (bc_value)) call setBC (tmp_bc, bc_value)
    !overgive boundary condition and leave function
    newBC => tmp_bc
    return
  end function
  
  subroutine setBC (bc, bc_value)
    !arguments
    type (boundaryCondition), pointer :: bc
    real (kind = 8) :: bc_value (*)
    
    switch: select case (bc.bctype)
      case (enum_H_BCtype)
        bc.h = bc_value (3)
      case (enum_V_BCtype)
        bc.v(1) = bc_value (1)
        bc.v(2) = bc_value (2)
    end select switch
  end subroutine


  function newNode (ID, xcord, ycord, zcord)
    implicit none
    !function name
    type (node), pointer :: newNode
    !arguments
    integer (kind = 4) :: ID
    real (kind = 8), intent (in) :: xcord, ycord
    real (kind = 8), intent (in), optional :: zcord
    !local variables
    type (node), pointer :: new
    
    !allocate new node
    allocate (new)
    
    new.ID = ID
    if (present (zcord)) then
      call setCoords (new, (/xcord, ycord/), zcord)
    else
      call setCoords (new, (/xcord, ycord/))
    endif
    !overgive the new node
    newNode => new
    return
  end function
  
  function newLinkedNode (ID, xcord, ycord, prev, next, zcord)
    implicit none
    !function name
    type (linkedNode), pointer :: newLinkedNode
    !arguments
    integer (kind = 4) :: ID
    real (kind = 8), intent (in) :: xcord, ycord
    type (linkedNode), pointer, optional :: prev, next
    real (kind = 8), intent (in), optional :: zcord
    !local variables
    type (linkedNode), pointer :: new => null()
    type (Node), pointer :: tmpNode => null()
    
    allocate(tmpNode)
    if (present (zcord)) then
      tmpNode = newNode (ID, xcord, ycord, zcord)
    else
      tmpNode = newNode (ID, xcord, ycord)
    endif
    
    if (present (next) .and. present (prev)) then
      new => makeNodeALinkedNode (tmpNode, prev, next)
    elseif (present (next) .and. (.not. (present (prev)))) then
      new => makeNodeALinkedNode (tmpNode, next = next)
    elseif (present (prev) .and. (.not. (present (next)))) then
      new => makeNodeALinkedNode (tmpNode, prev = prev)
    else
      new => makeNodeALinkedNode (tmpNode)
    endif
    !overgive the new linked node
    newLinkedNode => new
    return
  end function
  
  !create a linked node from an existing node
  function makeNodeALinkedNode (node2link, prev, next)
    implicit none
    !function name
    type (linkedNode), pointer :: makeNodeALinkedNode
    !arguments
    type (node), pointer :: node2link
    type (linkedNode), pointer, optional :: prev, next
    !local arguments
    type (linkedNode), pointer :: new
    !allocate new linked node
    allocate (new)
    !assign parameters
    new.thisNode => node2link
    if (present (prev) ) then
      if (associated (prev)) then
        new.prev => prev
        new.prev.next => new
      endif
    endif
    if (present (next)) then
      if (associated (next)) then
        new.next => next
        new.next.prev => new
      endif
    endif
    !overgive new linked node
    makeNodeALinkedNode => new
    return
  end function
  
  !set coordinates of a node
  subroutine setCoords (emptyNode, cord, cordz)
    !arguments
    type (node), pointer :: emptyNode
    real (kind = 8), dimension (1:2) :: cord
    real (kind = 8), optional :: cordz
    
    emptyNode.cord(1) = cord (1)
    emptyNode.cord(2) = cord (2)
    if (present (cordz)) emptyNode.ao = cordz
  end subroutine
  
end module