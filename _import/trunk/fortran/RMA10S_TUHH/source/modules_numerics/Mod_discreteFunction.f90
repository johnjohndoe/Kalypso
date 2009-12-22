module mod_discreteFunction

  type discreteFunctionGroup
    integer (kind = 4) :: ID
    type (linkedDiscreteFunction), pointer :: firstFun => null()
    type (linkedDiscreteFunction), pointer :: lastFun => null()
  endtype
  
  type linkedDiscreteFunction
    integer (kind = 4) :: ID
    real (kind = 8) :: CurveValue
    type (discreteFunction), pointer :: this => null()
    type (linkedDiscreteFunction), pointer :: prev => null()
    type (linkedDiscreteFunction), pointer :: next => null()
  endtype
  
  type discreteFunction
    type (valuePair), pointer :: first => null()
    type (valuePair), pointer :: last => null()
  endtype
  
!--------------------------------------------------------------------------------------------------------------------
!piecewise quadratic functions
!--------------------------------------------------------------------------------------------------------------------
  type discrQuadrFunSeg
    type (valuePair), pointer :: first => null()
    type (valuePair), pointer :: middle => null()
    type (valuePair), pointer :: last => null()
    real (kind = 8) :: coefs (0:2)
  end type
  
  type linkedDiscrQuadrFunSeg
    type (discrQuadrFunSeg), pointer :: this => null()
    type (linkedDiscrQuadrFunSeg) , pointer :: nextSeg => null()
    type (linkedDiscrQuadrFunSeg), pointer :: prevSeg => null()
  
  end type
  
  type discrQuadrFun
    type (linkedDiscrQuadrFunSeg), pointer :: first => null()
    type (linkedDiscrQuadrFunSeg), pointer :: last => null()
  end type
  
  type valuePair
    real (kind = 8) :: abszissa = 0.0d0
    real (kind = 8) :: ordinate = 0.0d0
    type (valuePair), pointer :: nextPair => null()
    type (valuePair), pointer :: prevPair => null()
  endtype
  
  contains
  
!---------------------------------------------------------------------------------
!function: newDiscrQuadrFunSeg
!---------------------------------------------------------------------------------

  function newDiscrQuadrFunSeg(firstPair, middlePair, lastPair)
    !function definition
    type (discrQuadrFunSeg), pointer :: newDiscrQuadrFunSeg
    !arguments
    type (valuePair), pointer, optional :: firstPair, middlePair, lastPair
    !local variables
    type (discrQuadrFunSeg), pointer :: new => null()
    
    allocate (new)
    if (present (firstPair)) new.first => firstPair
    if (present (firstPair)) new.middle => middlePair
    if (present (firstPair)) new.last => lastPair
    
    newDiscrQuadrFunSeg => new
    return
  end function
  
!---------------------------------------------------------------------------------
!function: newDiscrQuadrFunSeg
!---------------------------------------------------------------------------------

  function newLinkedDiscrQuadrFunSeg (QuadrFunSeg, next, prev) result (linkedFun)
    implicit none
    !function definition
    type (linkedDiscrQuadrFunSeg), pointer :: linkedFun
    !arguments
    type (discrQuadrFunSeg), pointer :: QuadrFunSeg
    type (linkedDiscrQuadrFunSeg), pointer, optional :: next
    type (linkedDiscrQuadrFunSeg), pointer, optional :: prev
    
    allocate (linkedFun)
    linkedFun.this => QuadrFunSeg
    if (present (next)) linkedFun.nextSeg => next
    if (present (prev)) linkedFun.prevSeg => prev
    
    return
  end function
  
!---------------------------------------------------------------------------------
!function: calcCoefs
!---------------------------------------------------------------------------------
  subroutine calcCoefs (quadrFun)
    implicit none
    !arguments
    type (discrQuadrFunSeg), pointer :: quadrFun
    !local variables
    real (kind = 8) :: zaehler, nenner
    real (kind = 8) :: x1, x2, x3, y1, y2, y3
    
    !Assignments for shorter code
    x1 = quadrFun.first.abszissa
    x2 = quadrFun.middle.abszissa
    x3 = quadrFun.last.abszissa
    y1 = quadrFun.first.ordinate
    y2 = quadrFun.middle.ordinate
    y3 = quadrFun.last.ordinate
    
    !calculate quadratic coefficient
    zaehler = (y1 - y2) * (x1 - x3) - (y1 - y3) * (x1 - x2)
    nenner = (x1**2 - x2**2) * (x1 - x3) - (x1**2 - x3**2) * (x1 - x2)
    quadrFun.coefs(2) = zaehler / nenner
    
    !calculate linear coefficient
    zaehler = ((y1 - y2) - quadrFun.coefs(2) * (x1**2 - x2**2))
    nenner = (x1 - x2)
    quadrFun.coefs(1) = zaehler/ nenner
    
    !calculate constant
    quadrFun.coefs(0) = y1 - quadrFun.coefs(1) * x1 - quadrFun.coefs(2) * x1**2

  end subroutine

!---------------------------------------------------------------------------------
!function: newDiscrQuadrFun
!---------------------------------------------------------------------------------

  function newDiscrQuadrFun ()
    !function type
    type (discrQuadrFun), pointer :: newDiscrQuadrFun
    !local variables
    type (discrQuadrFun), pointer :: tmpDiscrFun => null()
    
    allocate (tmpDiscrFun)
    newDiscrQuadrFun => tmpDiscrFun
    return
  end function
  
!---------------------------------------------------------------------------------
!function: newDiscrQuadrFun
!---------------------------------------------------------------------------------

  subroutine addSegment (relationship, Segment)
    implicit none
    !intent in parameters for the value pair
    type (linkedDiscrQuadrFunSeg), pointer :: Segment
    !discrete function to put the pair to
    type (discrQuadrFun), pointer :: relationship
    
    if ( .NOT. (associated (relationship.first))) then
      relationship.first => Segment
    else
      if ( .NOT. (associated (relationship.first.nextSeg))) relationship.first.nextSeg => Segment
      relationship.last.nextSeg => Segment
      relationship.last.nextSeg.prevSeg => relationship.last
    endif

    relationship.last => Segment
  end subroutine

!---------------------------------------------------------------------------------
!function: newLinkedFun
!---------------------------------------------------------------------------------

  function newLinkedFun (ID, CurveValue)
    !function type
    type (linkedDiscreteFunction), pointer :: newLinkedFun
    !arguments
    integer (kind = 4), optional :: ID
    real (kind = 8), optional :: CurveValue
    !local variables
    type (linkedDiscreteFunction), pointer :: tmpFun => null()
    
    allocate (tmpFun)
    tmpFun.this => newDiscrFun ()
    if (present (ID)) tmpFun.ID = ID
    if (present (CurveValue)) tmpFun.CurveValue = CurveValue
    
    newLinkedFun => tmpFun
    return
  end function
  
!---------------------------------------------------------------------------------
!subroutine: addPair
!---------------------------------------------------------------------------------

  subroutine addPair (relationship, absz, ordin)
    implicit none
    !intent in parameters for the value pair
    real (kind = 8), intent (in) :: absz, ordin
    !discrete function to put the pair to
    type (discreteFunction), pointer :: relationship
    !local variables
    type (valuePair), pointer :: new, temp
    
    allocate (new)
    new.abszissa = absz
    new.ordinate = ordin
    
    if ( .NOT. (associated (relationship.first))) then
      relationship.first => new
    else
      if ( .NOT. (associated (relationship.first.nextPair))) relationship.first.nextPair => new
      relationship.last.nextPair => new
      relationship.last.nextPair.prevPair => relationship.last
    endif
    relationship.last => new
  end subroutine
  
!---------------------------------------------------------------------------------
!function: newValuePair
!---------------------------------------------------------------------------------

  function newValuePair (absz, ordin)
    implicit none

    !function definition
    type (valuePair), pointer :: newValuePair
    !arguments
    real (kind = 8), intent (in) :: absz, ordin

    allocate (newValuePair)
    newValuePair.abszissa = absz
    newValuePair.ordinate = ordin
    
    return
  end function

!---------------------------------------------------------------------------------
!function: newDiscrFun
!---------------------------------------------------------------------------------
  
  function newDiscrFun ()
    !function type
    type (discreteFunction), pointer :: newDiscrFun
    !local variables
    type (discreteFunction), pointer :: tmpDiscrFun => null()
    
    allocate (tmpDiscrFun)
    newDiscrFun => tmpDiscrFun
    return
  end function

!---------------------------------------------------------------------------------
!function: functionValue
!---------------------------------------------------------------------------------

  function functionValue (relationship, xValue)
    implicit none
    !function name
    real (kind = 8) :: functionValue
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8) :: xValue
    !local variables
    type (valuePair), pointer :: lB
    
    !allocate local pointer variables
    allocate (lb)
    !check for position below first entry
    if (xValue <= relationship.first.abszissa) then
      functionValue = relationship.first.ordinate
    
    else
      !find lower Bound
      lB = lowerBoundPair (relationship, xValue)
      
      !linear extrapolation beyond upper border by given slope or slopeextrapolation f
      if ( .NOT. (associated (lB.nextPair))) then
        functionValue = lB.prevPair.ordinate + difference (lB.prevPair, xValue) * derivative(relationship, xValue)
      
      !normal situation
      else
        !calculate function value
        if (xValue == lB.abszissa) then
          !if it is exactly the same as the sampling point of the function
          functionValue = lB.ordinate
        else
          functionValue = lB.ordinate + difference (lB, xValue)* derivative (relationship, xValue)
        endif
      endif
    endif
    
    return
  end function
  
!---------------------------------------------------------------------------------
!function quadrFunValue  
!---------------------------------------------------------------------------------

  function quadrFunValue (relationship, xValue)
    implicit none
    !function name
    real (kind = 8) :: quadrFunValue
    !arguments
    type (discrQuadrFun), pointer :: relationship
    real (kind = 8) :: xValue
    type (linkedDiscrQuadrFunSeg), pointer :: funSeg
    real (kind = 8) :: calcPolynomial
    
    funSeg => relationship.first
    
    calcValue: do
      if ((associated (funSeg.nextSeg) .AND. xValue <= funSeg.this.last.abszissa) .OR. ( .NOT. (associated (funSeg.nextSeg)))) then
        quadrFunValue = calcPolynomial(funSeg.this.coefs(0:2), xValue, 2)
        return
      endif
      funSeg => funSeg.nextSeg
    end do calcValue

  end function      

!---------------------------------------------------------------------------------
!function: derivative
!---------------------------------------------------------------------------------

  function derivative (relationship, xValue)
    implicit none
    !function name
    real (kind = 8) :: derivative
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8) :: xValue
    !local variables
    type (valuePair), pointer :: lB
    
    !allocate local pointer variables
    allocate (lB)
    
    !check for position below first entry
    if (xValue < relationship.first.abszissa) then
      !send derivative of constant function
      derivative = 0.0d0
      
    else
      !find lower Bound
      lB = lowerBoundPair (relationship, xValue)
      
      !extrapolate derivative, if beyond upper border
      if ( .NOT. (associated (lB.nextPair))) then
        derivative = (lB.ordinate - lB.prevPair.ordinate) / interval(lB.prevPair)
        
      !normal situation
      else
        !calculate segments numerical derivative (chord)
        derivative = lB.nextPair.ordinate - lB.ordinate
        derivative = derivative / interval (lB)
      endif
    endif

    return
  end function
  
!---------------------------------------------------------------------------------
!function: lowerBoundPair
!---------------------------------------------------------------------------------

  function lowerBoundPair (relationship, xValue)
    implicit none
    !function value
    type (valuePair) :: lowerBoundPair
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8) :: xValue
    
    lowerBoundPair = relationship.first
    
    !if value below lower border
    if (xValue < relationship.first.abszissa) return

    findBound: do
      !no higher abszissa value available, so finished
      if ( .NOT. (associated (lowerBoundPair.nextPair))) exit findBound
      !exit on x-Value in between to discrete points
      if (lowerBoundPair.abszissa <= xValue .AND. xValue < lowerBoundPair.nextPair.abszissa) exit findBound
      !take next Pair
      lowerBoundPair = lowerBoundPair.nextPair
    enddo findBound

    return
  end function
  
!---------------------------------------------------------------------------------
!function: interval
!---------------------------------------------------------------------------------

  function interval (lowerBound)
    !function name
    real (kind = 8) :: interval
    !arguments
    type (valuePair), pointer :: lowerBound
    
    if (associated (lowerBound.nextPair)) then
      interval = lowerBound.nextPair.abszissa - lowerBound.abszissa
    else
      interval = 0.0d0
    endif
    return
  end function
  
!---------------------------------------------------------------------------------
!function: difference
!---------------------------------------------------------------------------------

  function difference (lowerBound, xValue)
    !function name
    real (kind = 8) :: difference
    !arguments
    type (valuePair), pointer :: lowerBound
    real (kind = 8) :: xValue


    difference = xValue - lowerBound.abszissa
    return
  end function
    

end module
