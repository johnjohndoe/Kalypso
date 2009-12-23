module mod_meshModelFE
  
  use mod_Node_DecimalTree
  use mod_Nodes

  type FEmesh
    integer (kind = 4) :: ID = 1
    type (decimalTree_Nodes), pointer :: nodes => null()
  end type 
  
contains


  function newFEMesh (ID)
    implicit none
!function name    
    type (FEMesh), pointer :: newFEMesh
!arguments    
    integer (kind = 4), optional :: ID
!local variables    
    type (FEMesh), pointer :: newMesh
    
    allocate (newMesh)
    if (present (ID)) then
      newMesh%ID = ID
    else
      newMesh%ID = 1
    endif
    newFEMesh => newMesh
    return
  end function


!generate the base tree for the node's tree structure  
  subroutine setUpNodes (mesh, size)
    implicit none
!arguments    
    type (FEmesh), pointer :: mesh
    integer (kind = 4), intent (in) :: size
    mesh%nodes => newBaseDTree (size)
  end subroutine

!add a new node to the mesh; it will be sorted to the proper locatin by the decimal tree  
  subroutine addNodeToMesh (mesh, node2Add)
    implicit none
!arguments    
    type (FEmesh), pointer :: mesh
    type (node), pointer :: node2Add
    call addElement2Tree (mesh%nodes, node2Add%ID, node2Add)
  end subroutine
  
  
  function findNodeInMeshByID (mesh, nodeID) result (node)
    implicit none
!function definition    
    type (node), pointer :: node
!arguments    
    type (FEmesh), pointer :: mesh
    integer (kind = 4) :: nodeID
!get the node; it will be searched by the decimal tree    
    node => getElementByID (mesh%nodes, nodeID)
    return
  end function
  
  

end module
