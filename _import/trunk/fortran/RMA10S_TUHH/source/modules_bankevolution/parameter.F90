!     Last change:  HN    9 Apr 2009    8:32 pm
module param
USE globalConstants
implicit none

real , parameter :: critical_slope = 65.
real , parameter :: repose = 26.0
real , parameter :: crepose = 27.0
REAL           , PARAMETER        :: GRAVITY = 9.81 , RHOS = 2650. , RHO = 1000. ,Porosity = 0.4
REAL           , PARAMETER        :: EFFECTIVE_COHESION = 10.0 , FRICTION_ANGLE= 26.0 , MATRIC_ANGLE = 15.0
!REAL           , PARAMETER        :: PI = 3.141593 defined in globalconstantans module

end module
