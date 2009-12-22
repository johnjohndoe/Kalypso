module const_modelConvConstants
  !global status constants: Convergence cases
  !------------------------------------------
  integer (kind = 4), parameter :: case_SchwarzConv = 1
  integer (kind = 4), parameter :: case_notSchwarzConv = 2
  integer (kind = 4), parameter :: case_NewtonConv = 3
  integer (kind = 4), parameter :: case_notNewtonConv = 4
  integer (kind = 4), parameter :: case_diverged = 5

end module