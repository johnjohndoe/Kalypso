module mod_Node_DecimalTree

use mod_Nodes

!tree definition
type decimalTree_Nodes
    integer (kind = 4) :: level
    integer (kind = 4) :: ID
    type (decimalTree_Nodes), pointer :: nextTree => null()
    type (decimalTree_Nodes), pointer :: subTree => null()
    type (node), pointer :: nodeData
end type decimalTree_Nodes

!
! Define the subroutines and functions
!
contains

function newBaseDTree (noOfTotalEntries) result (baseTree)
  implicit none
  !function definition
  type (decimalTree_Nodes), pointer :: baseTree
  !arguments
  integer (kind = 4), intent (in) :: noOfTotalEntries
  !local variables
  integer (kind = 4) :: level
  !find level of base tree
  level = 1
  findLevel: do
    if (10**level > noOfTotalEntries) then
      level = level + 1
      exit findLevel
    end if
    !cycle with next level
    level = level + 1
  end do findLevel
  !set up tree and assign base level
  allocate (baseTree)
  baseTree.level = level
  baseTree.ID = 0
  return
end function


function newTree (level, ID)
  implicit none
  !function definition
  type (decimalTree_Nodes), pointer :: newTree
  !arguments
  integer (kind = 4) :: level
  integer (kind = 4) :: ID
  !generate the tree
  allocate (newTree)
  newTree.level = level
  newTree.ID = ID
  return
end function


subroutine addElement2Subtree (tree, dataElement)
  implicit none
  !arguments
  type (decimalTree_Nodes), pointer :: tree
  type (node), pointer :: dataElement
  !add dataset to tree
  tree.nodeData => dataElement
end subroutine

subroutine addElement2Tree (baseTree, elementID, dataElement)
  implicit none
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: elementID
  type (node), pointer :: dataElement
  !local variables
  type (decimalTree_Nodes), pointer :: subTree => null()
  !generate the proper tree structure
  call generateTreeForID (baseTree, elementID)
  subTree => getSubtreeFromBaseTreeByID (baseTree, elementID)
  call addElement2Subtree (subTree, dataElement)
end subroutine


function getElementByID (baseTree, ID) result (dataElement)
  implicit none
  !funciton definition
  type (node), pointer :: dataElement
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: ID
  !local variables
  type (decimalTree_Nodes), pointer :: subTree => null()
  
  subTree => getSubtreeFromBaseTreeByID (baseTree, ID)
  dataElement => subTree.nodeData
  return
end function


subroutine generateTreeForID (baseTree, ID)
  implicit none
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: ID
  !local variables
  integer (kind = 4), allocatable :: levelID (:)
  integer (kind = 4) :: localID
  type (decimalTree_Nodes), pointer :: localTree => null ()
  type (decimalTree_Nodes), pointer :: levelTree => null ()
  integer (kind = 4) :: level, i, startLevel
  
  !get the level IDs by the element ID
  startLevel = baseTree.level - 1
  allocate (levelID (1:startLevel))
  !get level IDs
  localID = ID
  do i = startLevel, 1, -1
    if (10**(i-1) > localID) then
      levelID (i) = 0
    else
      levelID (i) = (localID - mod (localID, 10**(i-1)))/ 10**(i-1)
    end if
    localID = mod (localID, 10**(i-1))
  end do
    
  !generate the tree structure based on the levelIDs
  localTree => baseTree
  do i = startLevel, 1, -1
    if (.not. hasSubtreeWithID (localTree, levelID(i))) then
      levelTree => newTree (i, levelID(i))
      call addTree (localTree, levelTree)
    endif
    localTree => getSubtreeFromCurrTreeByID (localTree, levelID(i))
  end do
end subroutine


function getSubtreeFromBaseTreeByID (baseTree, ID) result (subTree)
  implicit none
  !funciton definition
  type (decimalTree_Nodes), pointer :: subTree
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: ID
  !local variables
  integer (kind = 4), allocatable :: levelID (:)
  integer (kind = 4) :: startLevel, i, localID
  type (decimalTree_Nodes), pointer :: tmpTree
  
  !get the level IDs by the element ID
  startLevel = baseTree.level - 1
  allocate (levelID (1:startLevel))
  !get level IDs
  localID = ID
  do i = startLevel, 1, -1
    if (10**(i-1) > localID) then
      levelID (i) = 0
    else
      levelID (i) = (localID - mod (localID, 10**(i-1)))/ 10**(i-1)
    end if
    localID = mod (localID, 10**(i-1))
  end do
  
  tmpTree => baseTree
  do i = startLevel, 1, -1
    tmpTree => getSubtreeFromCurrTreeByID (tmpTree, levelID(i))
  end do
  subTree => tmpTree
  return
end function


subroutine addTree (baseTree, tree2Add)
  implicit none
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree, tree2Add
  
  if (associated (baseTree.subTree)) then
    tree2Add.nextTree => baseTree.subTree
  endif
  baseTree.subTree => tree2Add
end subroutine


function hasSubtreeWithID (baseTree, ID)
  implicit none
  !function definition
  logical :: hasSubtreeWithID
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: ID
  !local variables
  type (decimalTree_Nodes), pointer :: tmpTree => null()
  !check for tree occurance
  if (.not. (associated (baseTree.subtree))) then
    hasSubtreeWithID = .false.
  else
    tmpTree => baseTree.subTree
    findTree: do
      if (tmpTree.ID == ID) then
        hasSubtreeWithID = .true.
        exit findTree
      end if
      if (associated (tmpTree.nextTree)) then
        tmpTree => tmpTree.nextTree
      else
        hasSubtreeWithID = .false.
        exit findTree
      endif
    end do findTree
  endif 
  return
end function


function getSubtreeFromCurrTreeByID (baseTree, ID) result (tree)
  implicit none
  !function definition
  type (decimalTree_Nodes), pointer :: tree
  !arguments
  type (decimalTree_Nodes), pointer :: baseTree
  integer (kind = 4) :: ID
  !local variables
  type (decimalTree_Nodes), pointer :: tmpTree => null()
  
  tmpTree => baseTree.subtree
  findTree: do
    if (tmpTree.ID == ID) then
      tree => tmpTree
      exit findTree
    else
      tmpTree => tmpTree.nextTree
    endif
  enddo findTree
  return
end function


end module