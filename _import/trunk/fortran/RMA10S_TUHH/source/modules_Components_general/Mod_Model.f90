!data type definition
module mod_Model
  !use global constants
  use const_modelConvConstants
  !use type modules
  use mod_ContiLines
  use mod_fileType
  use mod_meshModelFE
  
  !type definition for global simulation model
  !-------------------------------------------
  type simulationModel
    character (len = 1000) :: ID = 'defaultID'
    logical :: hasInnerBoundaries = .false.
    logical :: isSchwarzConv = .false.
    logical :: isNewtonConv = .false.
    !neighbouring models
    type (linkedSimModel), pointer :: modelNeighb => null()
    
    !topography model
    type (FEmesh), pointer :: FEmesh => null()
  end type
  
  type linkedSimModel
    type (simulationModel), pointer :: m_simModel => null()
    type (linkedSimModel), pointer :: next => null()
  end type
  
  
contains

  function newSimulationModel (ModelID)
    implicit none
    !function name
    type (SimulationModel), pointer :: newSimulationModel
    character (1000), optional :: modelID
    !local variables
    type (SimulationModel), pointer :: newModel
    
    allocate (newModel)
    newSimulationModel => newModel
    if (present (modelID)) newSimulationModel.ID = modelID
    return
  end function
  
  function checkForInnerBCs (ccls, ncl)
    implicit none
    !function name
    logical :: checkForInnerBCs
    !arguments
    type (contiLine), intent (in) :: ccls (*)
    integer (kind = 4), intent (in) :: ncl
    !local variables
    integer (kind = 4) :: i

    checkForInnerBCs = .false.
    checkBoundaries: do i = 1, ncl
      if (ccls(i).isInnerBoundary) then
        checkForInnerBCs = .true.
        exit checkBoundaries
      endif
    enddo checkBoundaries

    return
  end function
  
  function checkForSchwarzConvergence (ccls, ncl)
    implicit none
    !function name
    logical :: checkForSchwarzConvergence
    !arguments
    type (contiLine), intent (in) :: ccls (*)
    integer (kind = 4), intent (in) :: ncl
    !local variables
    integer (kind = 4) :: i
    
    checkForSchwarzConvergence = .false.
    checkSchwarzConv: do i = 1, ncl
      if (ccls(i).isInnerBoundary) then
        if (ccls(i).innerCondition.isSchwarzConv) then
          checkForSchwarzConvergence = .true.
          exit checkSchwarzConv
        endif
      endif
    enddo checkSchwarzConv
    return
  end function
  
  subroutine writeConvStatus (convStatus, simModelIDString, schwarzIt, icyc)
  
    implicit none
    
    !arguments
    integer (kind = 4), intent (in) :: convStatus
    integer (kind = 4), intent (in) :: schwarzIt, icyc
    character (len = *), intent (in) ::simModelIDString
    !local variables
    type (file), pointer :: convStatusFile => null()
    character (len = 1000) :: convStatusFileName
    logical :: fileWithUnitExisting = .false.
    
    !write filename based on the global Schwarz Conv status
    switch: select case (convStatus)
      case (case_SchwarzConv)
        write (convStatusFileName, '(a10,a,a1,i3.3,a1,i6.6)')  'Converged_', trim(simModelIDString),'_', schwarzIt, '_', icyc
      case (case_NotSchwarzConv)
        write (convStatusFileName, '(a13,a,a1,i3.3,a1,i6.6)')  'NotConverged_', trim(simModelIDString),'_', schwarzIt, '_', icyc
      case (case_diverged)
        write (convStatusFileName, '(a8)')  'Diverged'
     end select switch
     
     !generate file object and check before, whether unit no. is existing
     inquire (unit = 1212, exist = fileWithUnitExisting)
     if (fileWithUnitExisting) close (1212)

     !create status file
     convStatusFile => newFile (1212, convStatusFileName, 'replace')
     call openFileObject (convStatusFile)
     call closeFileObject (convStatusFile)

  end subroutine
  
  subroutine readDistributedModelIDs (simModel)
  
    !arguments
    type (simulationModel), pointer :: simModel
    !local variables
    type (file), pointer :: modIDsFile => null()
    type (simulationModel), pointer :: neighbModel2Add => null()
    integer :: fileUnit
    character (len = 1000) :: tempID
    integer :: ioStatus
        
    !find a free unit and open the file
    fileUnit = findFreeUnit()
    modIDsFile => newFile (unitNo = fileUnit, fileName = 'neighbours', fileStatus = 'OLD')
    
    !wait until file is there
    waitForNeighbInfo: do
      if (fileExists (modIDsFile)) exit waitForNeighbInfo
      call sleep (1)
    end do waitForNeighbInfo
    
    !open the info file
    call openFileObject (modIDsFile)
    
    !read the model's ID
    read (modIDsFile.unit, *) simModel.ID
    
    ioStatus = 0
    !read the neighbour IDs
    readNeighb: do 
      !read the next data line to find a new neighbour
      read (modIDsFile.unit, *, iostat = ioStatus) tempID

      !get out of here, if file is at the end
      if (ioStatus /= 0) then
        call closeFileObject (modIDsFile)
        exit readNeighb

      else
        !else read the new element and store it to the list
        neighbModel2Add => newSimulationModel (tempID)
        call addNeighbourModel (simModel, neighbModel2Add)
      endif 
    enddo readNeighb
  end subroutine
  
  function newLinkedModel (simModel)
    implicit none
    !type definition
    type (linkedSimModel), pointer :: newLinkedModel
    !arguments
    type (simulationModel), pointer :: simModel
    !local variables
    type (linkedSimModel), pointer :: newLnkdMod => null()
    
    allocate (newLnkdMod)
    newLnkdMod.m_SimModel => simModel
    newLinkedModel => newLnkdMod
    return
  end function
  
  subroutine addNeighbourModel (simModel, simModel2Add)
  
    !arguments
    type (simulationModel), pointer :: simModel
    type (simulationModel), pointer :: simModel2Add
    
    !local variables
    type (linkedSimModel), pointer :: last
    
    if (associated (simModel.modelNeighb)) then
      last => simModel.modelNeighb

      findLast: do
        if (associated (last.next)) then
          last => last.next
        else
          exit findLast
        endif
      end do findLast
      
      last.next => newLinkedModel (simModel2Add)
    else
      last => newLinkedModel (simModel2Add)
      simModel.modelNeighb => last
    endif
  end subroutine

end module