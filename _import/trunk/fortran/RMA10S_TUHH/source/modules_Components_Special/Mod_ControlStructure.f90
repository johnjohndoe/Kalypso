module mod_ControlStructure

  use mod_discreteFunction

  type ControlStructure
    integer (kind = 4) :: ID
    integer (kind = 4) :: TypeID
    real (kind = 8) :: posFlowDir = 0.0d0
!type of control structure (different types are possible like QCurves, weir formula, tabular data etc.;    
!                           has to be implemented!)    
    logical :: hasType = .false.
    type (discreteFunctionGroup), pointer :: QCurves
!type ...    
  endtype
  
contains

!GENERATE A NEW CONTROL STRUCTURE  
!--------------------------------  
  function newCstrc (ID, TypeID, posFlowDir)
!function definition    
    type (ControlStructure), pointer :: newCstrc
!arguments    
    integer (kind = 4), optional :: ID
    integer (kind = 4), optional :: TypeID
    real (kind = 8), optional :: posFlowDir
!local variables    
    type (ControlStructure), pointer :: tmpCstrc => null()
!generate a new control structure instance    
    allocate (tmpCstrc)
!assign values to control structure, if given    
    if (present (ID)) tmpCstrc%ID = ID
    if (present (TypeID)) tmpCstrc%TypeID = TypeID
    if (present (posFlowDir)) tmpCstrc%posFlowDir = posFlowDir
!overgive created control structure and leave creation routine    
    newCstrc => tmpCstrc
    return 
  end function
  
  subroutine addQCurveGroup (thisCstrc, QGroupID)
!arguments    
    type (ControlStructure), pointer :: thisCstrc
    integer (kind = 4), optional :: QGroupID
!local Variables    
    type (discreteFunctionGroup), pointer :: newQCurvesGroup => null()
!generate new Q Curves group        
    allocate (newQCurvesGroup)
!add optional parameters    
    if (present (QGroupID)) newQCurvesGroup%ID = QGroupID
!assign the group    
    thisCstrc%QCurves => newQCurvesGroup
!initialize with 0.0-flow line: Borders -500 mNN; +8900 mNN: Has to be changed when Mt. Everst rises higher :)    
    call addQCurve (thisCstrc, 0.0d0)
    call addValueTriple (thisCstrc, 0.0d0, -500.0d0, -500.0d0)
    call addValueTriple (thisCstrc, 0.0d0, 8900.0d0, 8900.0d0)
!tell control structure that it has a type    
    thisCstrc%hasType = .true.
  end subroutine
  
  subroutine addQCurve (thisCstrc, Q, ID)
!arguments    
    type (ControlStructure), pointer :: thisCstrc
    real (kind = 8) :: Q
    integer (kind = 4), optional :: ID
!local variables    
    type (linkedDiscreteFunction), pointer :: tmpFun => null()
    type (linkedDiscreteFunction), pointer :: newFun => null()
    

!Check, if cstrc has Q Group already available    
    if (associated (thisCstrc%QCurves)) then
!Check, if Curve with Curve value does already exist      
      if (associated (thisCstrc%QCurves%firstFun)) then
        tmpFun => findQCurveByQ (thisCstrc%QCurves, Q)
        if (associated (tmpFun)) return
      endif
    else
      call addQCurveGroup (thisCstrc = thisCstrc)
    endif
!add a new QCurve    
    newFun => newLinkedFun (CurveValue=Q)
    if ( .NOT. (associated (thisCstrc%QCurves%firstFun))) then
      thisCstrc%QCurves%firstFun => newFun
      thisCstrc%QCurves%lastFun => newFun
    else
      tmpFun => thisCstrc%QCurves%lastFun
      tmpFun%next => newFun
      newFun%prev => tmpFun
      thisCstrc%QCurves%lastFun => newFun
    endif
  end subroutine
  
  
  subroutine addValueTriple (thisCstrc, Q, how, huw)
!arguments    
    type (controlStructure), pointer :: thisCstrc
    real (kind = 8) :: Q
    real (kind = 8) :: how
    real (kind = 8) :: huw
    
!local variables    
    type (linkedDiscreteFunction), pointer :: tmpFun => null()
    
    if (associated (thisCstrc%QCurves%firstFun)) then
      tmpFun => findQCurveByQ (thisCstrc%QCurves, Q)
    endif 
    if ( .NOT. (associated (tmpFun))) then
      call addQCurve (thisCstrc, Q)
      tmpFun => findQCurveByQ (thisCstrc%QCurves, Q)
    endif
    call addPair (tmpFun%this, huw, how)
  end subroutine

  function findQCurveByQ (QCurves, Q)
    implicit none
!function type    
    type (linkedDiscreteFunction), pointer :: findQCurveByQ
!arguments    
    type (discreteFunctionGroup), pointer :: QCurves
    real (kind = 8), intent (in) :: Q
!local variables    
    type (linkedDiscreteFunction), pointer :: tmpFun => null()
    
!initialize    
    findQCurveByQ => null()
!find corresponding value    
    if (associated (QCurves%firstFun)) then
      tmpFun => QCurves%firstFun
      checkForExisting: do
        if (tmpFun%CurveValue == Q) then
          findQCurveByQ => tmpFun
          exit checkForExisting
        endif
!take the next function        
        if (associated (tmpFun%next)) then
          tmpFun => tmpFun%next
        else
          findQCurveByQ => null()
          exit checkForExisting
        endif
      enddo checkForExisting
    endif 
    return

  end function


end module
