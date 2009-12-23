module LineTransition
  type TransLine1D2D
    integer (kind = 4) :: Elt1D = 0
    integer (kind = 4) :: CCL = 0
    integer (kind = 4) :: Node1D = 0
    integer (kind = 4) :: TransType = 0
!TransType = 1: 2D -> 1D (H-Q)    
!TransType = 2: 2D <- 1D (Q-H)    
!TransType = 3: 2D <> 1D (H-H)    
  end type
end module
