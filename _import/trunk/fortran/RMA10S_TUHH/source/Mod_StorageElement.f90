module StorageElt
  type StorageElement
    integer (kind = 4) :: ContinuityLineID = 0
    real (kind = 8) :: StorageContent = 0.0d0
    real (kind = 8) :: StorageAddition = 0.0d0
  end type
  
  CONTAINS
   
  function VolumeContained (StorageElt)
    real (kind = 8) :: VolumeContained
    type (StorageElement) :: StorageElt
    VolumeContained = StorageElt.StorageContent + StorageElt.StorageAddition
    return
  end function
  
end module
