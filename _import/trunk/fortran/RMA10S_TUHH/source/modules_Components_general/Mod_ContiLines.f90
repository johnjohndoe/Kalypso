module mod_ContiLines
  use mod_Nodes
  use mod_Arcs
  USE TYPES
  use mod_storageElt
  use mod_fileType
  
  type contiLine
    integer (kind = 4) :: ID = 0
    type (linkedNode), pointer :: firstNode => null()
    type (linkedNode), pointer :: lastNode => null()
    type (arc), pointer :: firstSegment => null()
    TYPE (PROFILE), POINTER :: MorphoProfile => NULL()
    real (kind = 8) :: posNormal (1:2) = (/0.0d0, 0.0d0/)
    real (kind = 8) :: km
    type (StorageElement), pointer :: storageElt => null()
    logical         :: HasProfile
    !for Schwarz iterations
    logical :: isInnerBoundary = .false.
    type (innerBC), pointer :: innerCondition => null()
  end type
  
  type innerBC
    integer (kind = 4) :: globalID
    integer (kind = 4) :: BCtype
    logical :: isSchwarzConv = .false.
    logical :: isAssigned = .false.
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
  
  function newInnerBC (globalID, BCtype)
    implicit none
    !function name
    type (innerBC), pointer :: newInnerBC
    !arguments
    integer (kind = 4), optional :: globalID
    integer (kind = 4), optional :: BCtype
    !local variables
    type (innerBC), pointer :: newBC
    
    allocate (newBC)
    if (present (globalID)) newBC.globalID = globalID
    if (present (BCtype))newBC.BCtype = BCtype
    
    newInnerBC => newBC
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
    type (arc), pointer :: nextArc
    type (arc), pointer :: currArc
    
    !Initializations
    nextArc => null()
    currArc => null()
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

  subroutine addInnerBC (ccl, globalID, BCtype)
    implicit none
    !arguments
    type (contiLine), pointer :: ccl
    integer (kind = 4), optional :: globalID
    integer (kind = 4), optional :: BCtype
    
    if (.not. (associated (ccl.innerCondition))) then
      if (present (globalID) .and. present (BCtype)) then
        ccl.innerCondition => newInnerBC (globalID, BCtype)
      elseif (present (globalID)) then
        ccl.innerCondition => newInnerBC (globalID = globalID)
      elseif (present (BCtype)) then
        ccl.innerCondition => newInnerBC (BCtype = BCtype)
      else
        ccl.innerCondition => newInnerBC ()
      endif
    endif
  end subroutine
  
  subroutine write_innerBC (BCOutFile, ccl, vel, wsll, cord)
    !FIXME:
    !This here is very bad; AVOID GLOBAL ARRAYS
    implicit none
    !arguments
    type (file), pointer :: BCOutFile
    type (contiLine), pointer, intent (in) :: ccl
    real (kind = 8), intent (in) :: vel(1:,1:), wsll(1:), cord(1:,1:)
    !local variables
    integer (kind = 4) :: i
    type (linkedNode), pointer :: tmpNode => null()
    character (len = 1000) :: tmpString
    

    !Write header of boundary condition block, containing the line ID and the boundary condition type to be written
    setFileName: select case (ccl.InnerCondition.BCtype)
      case (enum_H_BCtype)
        write (BCOutFile.unit, *) 'Line: ', ccl.innerCondition.globalID, 'Type: ', enum_V_BCtype
      case (enum_V_BCtype)
        write (BCOutFile.unit, *) 'Line: ', ccl.innerCondition.globalID, 'Type: ', enum_H_BCtype
    end select setFileName
    
    !get first node
    tmpNode => ccl.firstNode
    
    forNodes: do
      !write values
      switch: select case (ccl.InnerCondition.BCtype)

        !H-Boundary condition has to hand out velocity boundary conditions
        case (enum_H_BCtype)
          write (tmpString, *)  tmpNode.thisNode.ID, tmpNode.thisNode.cord(1), tmpNode.thisNode.cord(2), vel(1,tmpNode.thisNode.ID), vel (2, tmpNode.thisNode.ID)

        !V-boundary conditions has to hand out water level boundary conditions
        case (enum_V_BCtype)
          write (tmpString, *) tmpNode.thisNode.ID, tmpNode.thisNode.cord(1), tmpNode.thisNode.cord(2), wsll(tmpNode.thisNode.ID)
      end select switch
      write (BCOutFile.unit,'(a)')  tmpString
      
      !get next node
      if (.not. (associated (tmpNode.next))) exit forNodes
      tmpNode => tmpNode.next
    end do forNodes
  end subroutine



  subroutine createInnerBCFunction (tmpBCLineType, inputFile, hFunction, vxFunction, vyFunction)
    implicit none
    !arguments
    integer (kind = 4) :: tmpBCLineType
    type (file), pointer, intent (in) :: inputFile
!    type (contiLine), pointer, intent (in) :: ccl


    !FIXME:
    !This here is very bad; these data sets should become part of the nodes
!    real (kind = 8) :: spec(1:,1:)
!    integer (kind = 4) :: nfix (1:)

    !local variables
    character (len = 1000) :: inputLine, lineString
    type (Node), pointer :: tmpNode => null()
    type (Node), pointer :: lastNode => null()
    type (valuePair), pointer :: firstKoteVx => null()
    type (valuePair), pointer :: firstKoteVy => null()
    type (valuePair), pointer :: MidsideKoteVx => null()
    type (valuePair), pointer :: MidsideKoteVy => null()
    type (valuePair), pointer :: lastKoteVx => null()
    type (valuePair), pointer :: lastKoteVy => null()
    type (DiscrQuadrFunSeg), pointer :: segVx
    type (DiscrQuadrFunSeg), pointer :: segVy
    type (linkedDiscrQuadrFunSeg), pointer :: linkedSegVx
    type (linkedDiscrQuadrFunSeg), pointer :: linkedSegVy
    type (discreteFunction), pointer, intent (out) :: hFunction => null()
    type (discrQuadrFun), pointer, intent (out) :: vxFunction => null()
    type (discrQuadrFun), pointer, intent (out) :: vyFunction => null()
    
    real (kind = 8) :: xCord, yCord, ordinate
    real (kind = 8) :: tmpWSLL, tmpVx, tmpVy
    integer (kind = 4) :: nodeID
    integer (kind = 4) :: istat
    
    !Initializations
    ordinate = 0.0d0
    istat = 0
    nullify (tmpNode, lastNode)
    nullify (firstKoteVx, firstKoteVy, MidsideKoteVx, MidsideKoteVy, lastKoteVx, lastKoteVy)
    nullify (segVx, segVy, linkedSegVx, linkedSegVy)
    nullify (hfunction, vxfunction, vyfunction)
    
    readCCLData: do
      !Read the data lines
      read (inputFile.unit, '(a)', iostat = istat) inputLine
      if (istat /= 0) exit readCCLData
      read (inputLine, *) LineString
      
      if (lineString == 'Line:') then
        backspace (inputFile.unit)
        exit readCCLData
      else
        
        switch: select case (tmpBCLineType)

          !H-Boundary condition has to hand out velocity boundary conditions
          case (enum_H_BCtype)
            
            if (associated (tmpNode)) lastNode => tmpNode
            if (.not. (associated (hFunction))) hFunction => newDiscrFun()
            read (inputLine, *) nodeID, xCord, yCord, tmpWSLL

            tmpNode => newNode (nodeID, xCord, yCord)
            
            if (.not. (associated (hFunction.first))) then
              ordinate = 0.0d0
            else
              ordinate = ordinate + sqrt ((tmpNode.cord(1) - lastNode.cord(1))**2 + (tmpNode.cord(2) - lastNode.cord(2))**2)
            endif
            call addPair (hFunction, ordinate, tmpWSLL)

          !V-boundary conditions has to hand out water level boundary conditions
          case (enum_V_BCtype)
          
            if (associated (tmpNode)) lastNode => tmpNode

            if (.not. (associated (vxFunction))) then
              vxFunction => newDiscrQuadrFun()
              vyFunction => newDiscrQuadrFun()
            endif

            read (inputLine, *) nodeID, xCord, yCord, tmpVx, tmpVy
            tmpNode => newNode (nodeID, xCord, yCord)
            
            !3 cases
            if (.not. (associated (firstKoteVx))) then
              firstKoteVx => newValuePair (0.0d0, tmpVx)
              firstKoteVy => newValuePair (0.0d0, tmpVy)

            else
              ordinate = ordinate + sqrt ((tmpNode.cord(1) - lastNode.cord(1))**2 + (tmpNode.cord(2) - lastNode.cord(2))**2)
              if (.not. (associated (MidsideKoteVx))) then
                MidsideKoteVx => newValuePair (ordinate, tmpVx)
                MidsideKoteVy => newValuePair (ordinate, tmpVy)
              else
                LastKoteVx => newValuePair (ordinate, tmpVx)
                LastKoteVy => newValuePair (ordinate, tmpVy)
                !Now a segment is full, generate a new function segment
                segVx => newDiscrQuadrFunSeg (FirstKoteVx, MidsideKoteVx, LastKoteVx)
                segVy => newDiscrQuadrFunSeg (FirstKoteVy, MidsideKoteVy, LastKoteVy)
                call calcCoefs (segVx)
                call calcCoefs (segVy)
                linkedSegVx => newLinkedDiscrQuadrFunSeg (segVx)
                linkedSegVy => newLinkedDiscrQuadrFunSeg (segVy)
                call addSegment (vxFunction, linkedSegVx)
                call addSegment (vyFunction, linkedSegVy)
                firstKoteVx => LastKoteVx
                firstKoteVy => LastKoteVy
                nullify (LastKoteVx, LastKoteVy, MidsideKoteVx, MidsideKoteVy)
              endif
            endif
          end select switch
        end if
      enddo readCCLData

  end subroutine




  subroutine getBCValues (tmpCCL, hFunction, vxFunction, vyFunction, spec, nfix)
    !arguments
    type (contiLine), pointer :: tmpCCL
    type (discreteFunction), pointer :: hFunction
    type (discrQuadrFun), pointer :: vxFunction
    type (discrQuadrFun), pointer :: vyFunction
    real (kind = 8) :: spec(1:,1:)
    integer (kind = 4) :: nfix(1:)
    !local variables
    type (LinkedNode), pointer :: tmpNode => null()
    real (kind = 8) :: localOrdinate
    
    tmpNode => tmpCCL.firstNode
    localOrdinate = 0.0d0
    throughNodes: do

      if (associated (hFunction)) then
        spec (tmpNode.thisNode.ID, 3) = functionValue (hFunction, localOrdinate)
        nfix (tmpNode.thisNode.ID) = 00200
        if (tmpNode.thisNode.ID == tmpCCl.firstNode.thisNode.ID .or. tmpNode.thisNode.ID == tmpCCl.lastNode.thisNode.ID) nfix(tmpNode.thisNode.ID) = nfix(tmpNode.thisNode.ID) + 1000
      elseif (associated (vxFunction)) then
        spec (tmpNode.thisnode.ID, 1) = quadrFunValue (vxFunction, localOrdinate)
        spec (tmpNode.thisnode.ID, 2) = quadrFunValue (vyFunction, localOrdinate)
        nfix (tmpNode.thisNode.ID) = 11000
      end if
      !update boundary condition to the model
      call bform (tmpNode.thisNode.ID)
      
      !Write them to the node
      if (associated (tmpNode.thisNode.currentBC)) then
        !set new value
        call setBC (tmpNode.thisNode.currentBC, spec (tmpNode.thisNode.id, 1:3))
      !assign new boundary condition, if not existent yet and set values
      else
        tmpNode.thisNode.currentBC => newBC (bc_type = tmpccl.innerCondition.bctype, bc_value = spec (tmpNode.thisNode.id, 1:3))
      endif

      if (associated (tmpNode.next)) then
        tmpNode => tmpNode.next
        localOrdinate = localOrdinate + sqrt ((tmpNode.thisNode.cord(1) - tmpNode.prev.thisNode.cord(1))**2 + (tmpNode.thisNode.cord(2) - tmpNode.prev.thisNode.cord(2))**2)
      else
        exit throughNodes
      endif

    end do throughNodes
  
  end subroutine       
  


!      
!      readNodeCounter = 0
!      readNodeData: do 
!        readNodeCounter = readNodeCounter + 1
!        BCType: select case (ccl.innerCondition.BCtype)
!          case (enum_H_BCtype)
!            !read H line
!            read (inputFile.unit, *) dummy1, dummy2, dummy3, spec(tmpNode.thisNode.ID, 3)
!            nfix (tmpNode.thisNode.ID) = 00200
!            !Add the boundary direction fix!
!            if (tmpNode.thisNode.ID == ccl.firstNode.thisNode.ID .or. tmpNode.thisNode.ID == ccl.lastNode.thisNode.ID) nfix(tmpNode.thisNode.ID) = nfix(tmpNode.thisNode.ID) + 1000
!            !set up boundary condition
!          case (enum_V_BCtype)
!            !read V line
!            read (inputFile.unit, *) dummy1, dummy2, dummy3, spec(tmpNode.thisNode.ID, 1), spec(tmpNode.thisNode.ID, 2)
!            nfix (tmpNode.thisNode.ID) = 11000
!        end select BCType
!        !Finish setting up boundary condition
!        call bform (tmpNode.thisNode.ID)
!
!
!        if (associated (tmpNode.next)) then
!          tmpNode => tmpNode.next
!        else
!          exit readNodeData
!        endif
!      enddo readNodeData
!      
!      ccl.innerCondition.isAssigned = .true.
!
!      !Closing file again
!      call closeFileObject (inputFile)
!    endif
  
  subroutine storeOldBCs (ccl, spec)
    !arguments
    type (contiLine), pointer :: ccl
    real (kind = 8), intent (in) :: spec (1:, 1:)
    !local variables
    type (linkedNode), pointer :: tmpNode => null()
    real (kind = 8) :: local_spec(1:3)
    
    tmpNode => ccl.firstNode
    
    storeBCs: do
      local_spec (1:3) = spec(tmpNode.thisNode.ID, 1:3)
      !assign boundary condition, if not existent yet
      if (associated (tmpNode.thisNode.previousBC)) then
        !set old value
        call setBC (tmpNode.thisNode.previousBC, local_spec(1:3))
      !assign new boundary condition, if not existent yet and set values
      else
        tmpNode.thisNode.previousBC => newBC (bc_type = ccl.innerCondition.bctype, bc_value = local_spec(1:3))
      endif

      !go to next node of line or leave loop
      if (associated (tmpNode.next)) then
        tmpNode => tmpNode.next
      else
        exit storeBCs
      endif
    end do storeBCs
  end subroutine

  subroutine checkBoundaryConv (ccl, convBorder)
    !arguments
    type (contiLine), pointer :: ccl
    real (kind = 8), intent (in) :: convBorder
    !local variables
    type (linkedNode), pointer :: tmpNode => null()
    real (kind = 8) :: absChange(1:3) = [0.0d0, 0.0d0, 0.0d0]
    real (kind = 8) :: maxChange(1:3) = [0.0d0, 0.0d0, 0.0d0]
    integer (kind = 4) :: i
    
    tmpNode => ccl.firstNode
    ccl.innerCondition.isSchwarzConv = .true.
    
    !initializations
    maxChange (1:3) = [0.0d0, 0.0d0, 0.0d0]
    absChange (1:3) = [0.0d0, 0.0d0, 0.0d0]
    
    convCheck: do
      selectConvCheckCase: select case (ccl.innerCondition.BCtype)
        case (enum_H_BCtype)

          if (tmpNode.thisNode.previousBC.h > 0.0) then

            !Check for maximum change in boundary condition
            absChange(3) = abs ((tmpNode.thisNode.currentBC.h - tmpNode.thisNode.previousBC.h)/ tmpNode.thisNode.previousBC.h)
            if (absChange(3) > maxChange(3)) maxChange(3) = absChange(3)
            if (maxChange(3) > convBorder .and. ccl.innerCondition.isSchwarzConv) ccl.innerCondition.isSchwarzConv = .false.
          else
            ccl.innerCondition.isSchwarzConv = .false.
          end if
          
        case (enum_V_BCtype)
        
          do i = 1, 2
            if (tmpNode.thisNode.previousBC.v(i) > 0.0) then
            
              !Check for maximum change in boundary condition
              absChange(i) = abs ((tmpNode.thisNode.currentBC.v(i) - tmpNode.thisNode.previousBC.v(i))/ tmpNode.thisNode.previousBC.v(i))
              if (absChange(i) > maxChange(i)) maxChange(i) = absChange(i)
              if (maxChange(i) > convBorder .and. ccl.innerCondition.isSchwarzConv) ccl.innerCondition.isSchwarzConv = .false.
            else
              ccl.innerCondition.isSchwarzConv = .false.
            end if
          enddo
         end select selectConvCheckCase
       if (associated (tmpNode.next)) then
         tmpNode => tmpNode.next
       else
         exit convCheck
       endif
    end do convCheck
    
  end subroutine
  ! SUBROUTINE TO FORM PROFILES OUT OF PROFILE DATA AND CONTINUITY LINES
 ! FUNCTION ADDPROFILE (BANKPROFILE,CCL)
  !IMPLICIT NONE
  
  !TYPE (PROFILE),
  

end module