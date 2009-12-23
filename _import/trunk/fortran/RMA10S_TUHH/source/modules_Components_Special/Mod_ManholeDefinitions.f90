module ManholeDefinitions
  type pipeManhole
    real (kind = 8) :: ks = 0.01
    real (kind = 8) :: diameter = 0.50
    real (kind = 8) :: mue = 0.577
    real (kind = 8) :: zetaInflowUpper = 0.1
    real (kind = 8) :: zetaOutflowUpper = 0.1
    real (kind = 8) :: zetaInflowLower = 0.1
    real (kind = 8) :: zetaOutflowLower = 0.1
    real (kind = 8) :: length = 2.0
  end type
end module
