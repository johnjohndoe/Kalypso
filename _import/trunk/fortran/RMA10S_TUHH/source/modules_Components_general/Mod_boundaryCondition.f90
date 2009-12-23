module mod_BCs

!used types  
  use mod_storageElt
  
!water level boundary condition type due to volume-waterlevel relationship  
  type bcVolWLRelation
    type (StorageElement), pointer :: volWLRelationship => null()
  end type
  

end module