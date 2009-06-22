module mod_Model

  type SimulationModel
    integer (kind = 4) :: ID = 0
    logical :: SchwarzConv = .false.
    logical :: NewtonConv = .false.
  end type
  
contains

  function newSimulationModel ()
    implicit none
    !function name
    type (SimulationModel), pointer :: newSimulationModel
    !local variables
    type (SimulationModel), pointer :: newModel
    
    allocate (newModel)
    newSimulationModel => newModel
    return
  end function

end module