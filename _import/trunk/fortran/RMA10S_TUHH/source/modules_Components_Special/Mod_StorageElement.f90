module mod_storageElt

  use mod_discreteFunction
  type StorageElement
    integer (kind = 4) :: ID = 0
    integer (kind = 4) :: CCLID = 0
    real (kind = 8) :: storageContent = 0.0d0
    real (kind = 8) :: storageAddition = 0.0d0
    real (kind = 8) :: minWaterLevel = 0.0d0
    real (kind = 8) :: currQ = 0.0d0
    real (kind = 8) :: prevQ = 0.0d0
!optional Volume Waterlevel Relationship    
    type (discreteFunction), pointer :: volWlRel => null()
  end type
  
  private CalcStorageContent
  
  CONTAINS
  
  function newStorElt (ID, storageContent)
    implicit none
!function name    
    type (StorageElement), pointer :: newStorElt
    real (kind = 8), optional :: storageContent
!arguments    
    integer (kind = 4) :: ID
!local variables    
    type (StorageElement), pointer :: new
!allocate new node    
    allocate (new)
    new.ID = ID
    if (present(storageContent)) new.storageContent = storageContent
!overgive the new storage element    
    newStorElt => new
    return
  end function


!Add Volume Waterlevel Relationship to StorageElt  
  subroutine addVolWaterlevelRel (StorageElt)
!Storage Element to put the new relationship to    
    type (StorageElement), pointer :: StorageElt
!local variables    
    type (discreteFunction), pointer :: new
    
!check for already existing relationship    
    if (associated (StorageElt.volWlRel)) call ErrorMessageAndStop (1701, StorageElt.ID, 0.0d0, 0.0d0)
    
!generate a new Volume Waterlevel Relationship    
    allocate (new)
!Assign the Relationship to the Storage Element    
    StorageElt.volWlRel => new
  end subroutine
  
!add value pair  
  subroutine addValuePair (StorageElt, Volume, Waterlevel)
!intent in parameters     
    type (StorageElement), pointer :: StorageElt
!parameters for the realtionship    
    real (kind = 8), intent (in) :: Volume, Waterlevel
!check for associated discrete function    
    if ( .NOT. (associated (StorageElt.volWlRel))) call ErrorMessageAndStop (1702, StorageElt.ID, 0.0d0, 0.0d0)
!add pair to discrete function    
    call addPair (StorageElt.volWlRel, Volume, Waterlevel)
  end subroutine
  
!get the water level of a storage element  
  function waterlevel (StorageElt, currentWaterlevel)
!function name    
    real (kind = 8) :: waterlevel
!arguments    
    type (StorageElement), pointer :: StorageElt
    logical, optional :: currentWaterlevel
!local variables    
    real (kind = 8) :: volume

!get water level    
    if (associated (StorageElt.volWlRel)) then
!get the water level during time step, that means the addition/ subtraction within the current      
!time step is already considered      
      if (present (currentWaterlevel)) then
        if (currentWaterlevel) then
          volume = VolumeContained(StorageElt)
        else
          volume = StorageElt.storageContent
        endif
      else
        volume = StorageElt.storageContent
      endif
      waterlevel = functionValue (StorageElt.volWlRel, volume)
    else
      waterlevel = StorageElt.minWaterLevel
    endif
    return
  end function
  
!  !get change of water level of storage element wrt to discharge
!  function waterlevelWRTDischarge (StorageElt) result dWLdQ
!    !function name
!    real (kind = 8) :: dWLdQ
!    !arguments
!    type (StorageElement), pointer :: StorageElt
!    !local variables
!    real (kind = 8) :: waterlevel
!    
!    !get contained volume of storage element including the current time step
!    volume = 
!  
!  
!  end function
   
!Calculate the Volume that would be contained after the time step  
  function VolumeContained (StorageElt)
    implicit none
    real (kind = 8) :: VolumeContained
    type (StorageElement) :: StorageElt
    VolumeContained = StorageElt.StorageContent + StorageElt.StorageAddition
    return
  end function
  
!Average discharge within time step  
  function QAverage (StorageElt)
    implicit none
    real (kind = 8) :: QAverage
    type (StorageElement) :: StorageElt
    QAverage = 0.5 * (StorageElt.prevQ + StorageElt.currQ)
    return
  end function

!Settle the Storage Content that is contained in the element  
  subroutine CalcStorageContent (StorageElt)
!private    
    implicit none
    type (StorageElement) :: StorageElt
    StorageElt.StorageContent = VolumeContained (StorageElt)
  end subroutine

!run through all Storage Elements and get their storage content  
  subroutine CalcAllStorageContents (StorageElts)
    implicit none
    type (storageElement), intent (in) :: StorageElts(1:)
    integer (kind = 4) :: number, counter
    
    number = ubound (storageElts,1)
    findStorageContents: do counter = 1, number
      call CalcStorageContent (StorageElts (counter))
    enddo findStorageContents
  end subroutine
  
end module
