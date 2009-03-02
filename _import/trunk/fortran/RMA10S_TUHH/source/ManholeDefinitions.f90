module ManholeDefinitions
  type pipeManhole
    real (kind = 8) :: ks = 0.01
    real (kind = 8) :: diameter = 0.40
    real (kind = 8) :: mue = 0.577
    real (kind = 8) :: zetaInflowUpper = 0.1
    real (kind = 8) :: zetaOutflowUpper = 0.1
    real (kind = 8) :: zetaInflowLower = 0.1
    real (kind = 8) :: zetaOutflowLower = 0.1
    real (kind = 8) :: length = 2.0
  end type
  
  type energyLevel
    real (kind = 8) :: bedLevel = 0.0d0
    real (kind = 8) :: pressHead = 0.0d0
    real (kind = 8) :: veloHead = 0.0d0
    real (kind = 8) :: total = 0.0d0
    real (kind = 8) :: specific = 0.0d0
  end type
end module
