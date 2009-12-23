!     Last change:  HN    9 Apr 2009    8:32 pm
module param
USE globalConstants

implicit none
! the out coomented varaibles are now defined in module BLKSANMOD and are read in control file by
! the line GTP (Geothechnical properties).
!real , parameter :: critical_slope = 65.    ! Unsaturated angle of repose
!real , parameter :: repose = 26.0           ! Saturated angle of repose after avalanche.
!real , parameter :: crepose = 27.0          ! Saturated critical angle of repose.
REAL           , PARAMETER        :: GRAVITY = 9.81 , RHOS = 2650. , RHO = 1000. ,Porosity = 0.4
!REAL           , PARAMETER        :: EFFECTIVE_COHESION = 10.0 , FRICTION_ANGLE= 26.0 , MATRIC_ANGLE = 15.0
!REAL           , PARAMETER        :: PI = 3.141593 defined in globalconstantans module

end module
