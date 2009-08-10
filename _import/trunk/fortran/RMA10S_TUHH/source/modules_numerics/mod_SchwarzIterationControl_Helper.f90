module SchwarzIterationControl_Helper

use mod_fileType
use mod_ContiLines
use mod_Model

!Schwarz Case constants
integer (kind = 4), parameter :: enum_continueStep = 1
integer (kind = 4), parameter :: enum_nextStep = 2
integer (kind = 4), parameter :: enum_stop = 3


contains

!-------------------------------------------------------------------------------------------
!function: getSchwarzIteration Command
!-------------------------------------------------------------------------------------------
  function getSchwarzIterationCommand (schwarzStep, currCalcStep) result (continueCommand)
    implicit none
    !function definition
    integer (kind = 4) :: continueCommand
    
    !arguments
    integer (kind = 4), intent (in) :: schwarzStep
    integer (kind = 4), intent (in) :: currCalcStep
    !local variables
    integer (kind = 4) :: nextCalcStep
    type (file), pointer :: nextCalcstepFile => null()
    type (file), pointer :: nextSchwarzStepFile => null()
    type (file), pointer :: stopFile => null()
    character (len = 96) :: fileNameChar
    
    
    !initializations
    nextCalcStep = currCalcStep + 1
    !Create command files
    !--------------------
    !next calculation step command file
    write (fileNameChar,'(a9,i5.5,a1,i3.3)' ) 'nextStep_', nextCalcStep, '_', 1
    nextCalcStepFile => newFile (fileName = fileNameChar)
    !next schwarz step command file
    write (fileNameChar, '(a9,i5.5,a1,i3.3)') 'nextStep_', currCalcStep, '_', schwarzStep
    nextSchwarzStepFile => newFile (fileName = fileNameChar)
    !stop command file
    write (fileNameChar, '(a4)')  'stop'
    stopFile => newFile (fileName = fileNameChar)

    !Wait for command files
    !----------------------
    waitForInfo: do
      if (fileExists (stopFile)) then
        continueCommand = enum_stop
        exit waitForInfo
      elseif (fileExists (nextSchwarzStepFile)) then
        continueCommand = enum_continueStep
        exit waitForInfo
      elseif (fileExists (nextCalcstepFile)) then
        continueCommand = enum_nextStep
        exit waitForInfo
      end if
      !FIXME: Introduce a time out!
      !wait for the files
      call sleep(1)
    end do waitForInfo
    
    nullify (nextCalcStepFile, nextSchwarzStepFile, stopFile)
    return
  end function


!-------------------------------------------------------------------------------------------
!subroutine: writeInnerBoundaryConditons
!-------------------------------------------------------------------------------------------
subroutine writeInnerBoundaryConditons (m_simModel, schwarzIt, calcStep, ccls, ncl, maxp, vel, wsll, cord)
  implicit none
  !arguments
  type (SimulationModel), pointer, intent (in) :: m_simModel
  integer (kind = 4), intent (in) :: schwarzIt
  integer (kind = 4), intent (in) :: calcStep
  type (contiLine), target :: ccls (1:)
  integer (kind = 4), intent (in) :: ncl, maxp
  real (kind = 8), intent (in) :: vel (:,:), wsll(:), cord(:,:)
  
  !local variables
  character (len = 96) :: BCOutFilename
  type (file), pointer :: BCOutFile
  type (contiLine), pointer :: tmpCCL
  integer (kind = 4) :: i

  !Generate and open BC output file
  write (BCOutFilename, '(a3,a,a1,i5.5,a1,i3.3,a3)') 'BC_', trim(m_SimModel.ID), '_', calcStep, '_',schwarzIt , '.bc'
  BCOutFile => newFile (fileName = BCOutFilename, fileStatus = 'REPLACE')
  call openFileObject (BCOutFile, .true.)

  !write the boundary condition values to the output file
  !content:
  !1. line ID and BC type classification
  !2. values for each node inside the line
  writeInnerConditions: do i = 1, ncl
    if (ccls(i).isInnerBoundary) then
      tmpCCL => ccls(i)
      call write_innerBC (BCOutFile, tmpCCL, vel(1:3, 1:maxp), wsll(1:maxp), cord(1:maxp, 1:2))
      !unassign read in boundary conditions
      tmpCCL.innerCondition.isAssigned = .false.
    endif 
  enddo writeInnerConditions
  !close the BC file
  call closeFileObject (BCOutFile)
end subroutine

!-------------------------------------------------------------------------------------------
!function: getNeighbourBCFiles
!-------------------------------------------------------------------------------------------
function getNeighbourBCFiles (schwarzIt, calcStep, m_SimModel) result (bcFiles)
  use mod_fileType_lists
  implicit none
  !function definition
  type (linked_List), pointer :: bcFiles
  !arguments
  integer (kind = 4), intent (in) :: schwarzIt, calcStep
  type (simulationModel) :: m_SimModel
  !local variables
  type (file), pointer :: bcFile
  character (len = 96) :: bcFileName
  type (linkedSimModel), pointer :: tmpNeighbour
  
  !initializations
  bcFiles => null()
  tmpNeighbour => m_simModel.modelNeighb

  generateFileNames: do
    write(bcFileName, '(a3,a,a1,i5.5,a1,i3.3,a3)') 'BC_',trim(tmpNeighbour.m_simModel.ID), '_', calcStep, '_', schwarzIt, '.bc'
    bcFile => newFile (fileName = bcFileName, fileStatus = 'OLD')
    !Add file to the list
    if (associated (bcFiles)) then
      call list_insert_head (bcFiles, bcFile)
    else
      call list_create (bcFiles, bcFile)
    endif
    !prepare for next file or leave loop
    nullify (bcFile)
    if (.not. (associated (tmpNeighbour.next))) exit generateFileNames
    tmpNeighbour => tmpNeighbour.next
  end do generateFileNames
  
  return
end function

!-------------------------------------------------------------------------------------------
!subroutine: getNeighbourBCs
!-------------------------------------------------------------------------------------------
subroutine getNeighbourBCs (bcFiles, ccls, ncl, maxp, spec, nfix)
  use mod_fileType_lists
  implicit none
  !arguments
  type (linked_List), pointer :: bcFiles
  type (contiLine), target :: ccls (1:)
  integer (kind = 4) :: ncl, maxp
  real (kind = 8) :: spec (1:,1:)
  integer (kind = 4) :: nfix (1:)
  
  !local variables
  type (file), pointer :: bcFile
  type (linked_List), pointer :: currentFile, file2Remove
  type (contiLine), pointer :: tmpCCL

  type (discreteFunction), pointer :: hFunction
  type (discrQuadrFun), pointer :: vxFunction
  type (discrQuadrFun), pointer :: vyFunction
  
  character (len = 1000) :: readInLine
  character (len = 96)   :: lineString, typeString
  integer (kind = 4) :: tmpBCLineID, tmpBCLineType
  integer (kind = 4) :: istat
  integer (kind = 4) :: i
  
  !initializations
  bcFile => null()
  currentFile => bcFiles
  file2Remove => null()
  hFunction => null()
  vxFunction => null()
  vyFunction => null()
  
  !wait loop: Wait until all neighbour boundary condition informations are coming
  waitLoop: do
      
    if (.not. (associated (currentFile))) exit waitLoop

    checkFiles: do
      bcFile => currentFile.data
      !open the found file
      if (FileExists(bcFile)) then
        call openFileObject (bcFile)
            
        !read the file until new boundary line is found with the file
        findBCLine: do
          istat = 0
          read (bcFile.unit, '(a)', iostat = istat) readInLine
          read (readInLine,*) lineString
          if (istat /= 0) exit findBCLine

          !Check whether a continuity line block was found
          if (lineString == 'Line:') then
            read (readInLine,*) lineString, tmpBCLineID, TypeString, tmpBCLineType
            !Find the line with the same ID
            findProperCCL: do i =1, ncl
              tmpCCL => ccls(i)
              !Don't care about outer boundary lines
              if (.not. (tmpCCL.isInnerBoundary)) cycle findProperCCL
              if (tmpCCL.InnerCondition.IsAssigned) cycle findProperCCL
  
                if (tmpCCL.innerCondition.globalID == tmpBCLineID) then
                call createInnerBCFunction (tmpBCLineType, bcFile, hFunction, vxFunction, vyFunction)
                call getBCValues (tmpCCL, hFunction, vxFunction, vyFunction, spec(1:maxp,1:3), nfix(1:maxp))
              end if
            end do findProperCCL
          end if 
        end do findBCLine
              
        call closeFileObject (bcFile)
        file2Remove => currentFile
        currentFile => list_next(currentFile)
        call list_delete_element (bcFiles, file2Remove)
              
      else
        currentFile => list_next(currentFile)
      endif
          
      if (.not. (associated (currentFile))) exit checkFiles

    end do checkFiles
        
    currentFile => bcFiles
    if (.not. (associated (currentFile))) exit waitLoop

    call sleep (1)
  end do waitLoop
end subroutine
      
      
!-------------------------------------------------------------------------------------------
!subroutine: storeOldInnerBoundaryConditions
!-------------------------------------------------------------------------------------------
subroutine storeOldInnerBoundaryConditions (ccls, spec, ncl, maxp)
  implicit none
  !arguments
  type (contiLine), target :: ccls (1:)
  real (kind = 8) :: spec (1:, 1:)
  integer (kind = 4) :: ncl, maxp
  !local variables
  type (contiLine), pointer :: tmpCCL
  integer (kind = 4) :: i
      
  !Store old BCs
  do i = 1, ncl
    tmpCCL => ccls(i)
    if (tmpCCL.isInnerBoundary) call storeOldBCs (tmpCCL, spec(1:maxp, 1:3))
  end do
end subroutine

!-------------------------------------------------------------------------------------------
!function: checkSchwarzConvergence
!-------------------------------------------------------------------------------------------
function checkSchwarzConvergence (ccls, ncl, schwarzConvCheckBorder) result (schwarzConvStatus)
  implicit none
  !function definition
  logical :: schwarzConvStatus
  !arguments
  type (contiLine), target :: ccls (1:)
  integer (kind = 4), intent (in) :: ncl
  real (kind = 8), intent (in) :: schwarzConvCheckBorder
  
  !local variables
  type (contiLine), pointer :: tmpCCL
  integer (kind = 4) :: i
  
  !initializations; assume it is converged and check assumption
  schwarzConvStatus = .true.
  
  !Check, if assumption is true
  checkSchwarzConv: do i = 1, ncl
    tmpCCL => ccls (i)
    if (.not. (tmpCCL.isInnerBoundary)) cycle checkSchwarzConv
    call checkBoundaryConv (tmpCCL, schwarzConvCheckBorder)
    if (.not. (tmpCCL.innerCondition.isSchwarzConv)) schwarzConvStatus = .false.
  end do checkSchwarzConv
  
  return

end function

!-------------------------------------------------------------------------------------------
!subroutine: nullifyInnerBCstatus
!-------------------------------------------------------------------------------------------

subroutine nullifyInnerBCstatus (ccls, nfix, nfixp, alfa, ncl)
  implicit none
  !arguments
  type (contiLine), target :: ccls (1:)
  integer (kind = 4) :: nfix (1:), nfixp(1:)
  integer (kind = 4) :: ncl
  real (kind = 4) :: alfa (1:)
  
  !local variables
  type (contiLine), pointer :: tmpCCL
  type (linkedNode), pointer :: currNode
  integer (kind = 4) :: i

  !initializations
  tmpCCL => null()
  
  do i = 1, ncl
    tmpCCL => ccls(i)
    if (tmpCCL.isInnerBoundary) then
      currNode => tmpCCL.firstNode
      resetBCStatus: do 
        nfix (currNode.thisNode.ID) = -1
        nfixp(currNode.thisNode.ID) = -1
        if (.not. (associated (currNode.next))) exit resetBCStatus
        currNode => currNode.next
      end do resetBCStatus
    end if
  end do
end subroutine 


end module
