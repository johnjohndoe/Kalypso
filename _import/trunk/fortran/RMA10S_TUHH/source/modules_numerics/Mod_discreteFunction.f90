module mod_discreteFunction

  type discreteFunction
    type (valuePair), pointer :: first => null()
    type (valuePair), pointer :: last => null()
  endtype
  
  type valuePair
    real (kind = 8) :: abszissa = 0.0d0
    real (kind = 8) :: ordinate = 0.0d0
    type (valuePair), pointer :: nextPair => null()
    type (valuePair), pointer :: prevPair => null()
  endtype
  
  contains
  
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
    
    if (.not. (associated (relationship.first))) then
      relationship.first => new
    else
      if (.not. (associated (relationship.first.nextPair))) relationship.first.nextPair => new
      relationship.last.nextPair => new
      relationship.last.nextPair.prevPair => relationship.last
    endif
    relationship.last => new
  end subroutine
  
  function functionValue (relationship, xValue)
    implicit none
    !function name
    real (kind = 8) :: functionValue
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8), pointer :: xValue
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
      
      !linear extrapolation beyond upper border
      if (.not.(associated (lB.nextPair))) then
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
  
  function derivative (relationship, xValue)
    implicit none
    !function name
    real (kind = 8) :: derivative
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8), pointer :: xValue
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
      if (.not. (associated (lB.nextPair))) then
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
  
  function lowerBoundPair (relationship, xValue)
    implicit none
    !function value
    type (valuePair) :: lowerBoundPair
    !arguments
    type (discreteFunction), pointer :: relationship
    real (kind = 8), pointer :: xValue
    
    lowerBoundPair = relationship.first
    
    !if value below lower border
    if (xValue < relationship.first.abszissa) return

    findBound: do
      !no higher abszissa value available, so finished
      if (.not. (associated (lowerBoundPair.nextPair))) exit findBound
      !exit on x-Value in between to discrete points
      if (lowerBoundPair.abszissa <= xValue .and. xValue < lowerBoundPair.nextPair.abszissa) exit findBound
      !take next Pair
      lowerBoundPair = lowerBoundPair.nextPair
    enddo findBound

    return
  end function
  
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
  
  function difference (lowerBound, xValue)
    !function name
    real (kind = 8) :: difference
    !arguments
    real (kind = 8), pointer :: xValue
    type (valuePair), pointer :: lowerBound


    difference = xValue - lowerBound.abszissa
    return
  end function
    

end module
