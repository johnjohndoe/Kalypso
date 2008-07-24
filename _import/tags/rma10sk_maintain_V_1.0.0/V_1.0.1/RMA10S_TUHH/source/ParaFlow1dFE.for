C     Last change:  WP    8 Nov 2007   10:38 am
      MODULE PARA1DPoly


      !polynom coefficients
      REAL (KIND = 8) , ALLOCATABLE  :: apoly (:, :, :)              !cross sectional area polynom coefficients
      REAL (KIND = 8) , ALLOCATABLE  :: qpoly (:, :, :)              !discharge polynom coefficients
      REAL (KIND = 8) , ALLOCATABLE  :: alphapoly (:, :, :)          !energy flow coefficient reduced polynom coefficients (transient water depth)
      REAL (KIND = 8) , ALLOCATABLE  :: betapoly (:, :, :)           !energy flow coefficient polynom coeffients
      REAL (KIND = 8) , ALLOCATABLE  :: polyrangeA (:, :)
      REAL (KIND = 8) , ALLOCATABLE  :: polyrangeQ (:, :)
      REAL (KIND = 8) , ALLOCATABLE  :: polyrangeB (:, :)
      !validity range for polynoms
      REAL (KIND = 8) , ALLOCATABLE  :: hhmin (:), hhmax (:)         !validity borders for polynoms
      !reference friction slope
      REAL (KIND = 8) , ALLOCATABLE  :: qgef(:)                      !reference friction slope
      !intersecting water depth for flow coefficient (e.g. Boussinesq)
      REAL (KIND = 8) , ALLOCATABLE  :: hbordv(:)                    !bankful discharge
      !flow kilometer of node
      REAL (KIND = 8) , ALLOCATABLE  :: kmx(:)                       !flow kilometer of node/ profile
      !cross sectional area, its derivative and the nodal discharge
      REAL (KIND = 8) , ALLOCATABLE  :: ah(:)                        !cross sectional area dependent on water depth
      REAL (KIND = 8) , ALLOCATABLE  :: dahdh(:)                     !derivative of cross sectional area dependent on water depth over water depth
      REAL (KIND = 8) , ALLOCATABLE  :: qh(:)                        !nodal discharge dependent on water depth
      !time dependent values
      REAL (KIND = 8) , ALLOCATABLE  :: hht(:)                       !water depth of last time step
      REAL (KIND = 8) , ALLOCATABLE  :: vvt(:)                       !velocity of last time step
      REAL (KIND = 8) , ALLOCATABLE  :: dhht(:)                      !derivative of water depth over t of last time step
      REAL (KIND = 8) , ALLOCATABLE  :: dvvt(:)                      !derivative of velocity over t of last time step
      !decision switch for way of coefficient calculation
      INTEGER                        :: beient                       !decision switch for flow coefficient (2: beta, 1: alpha, 0. ignore)
      INTEGER                        :: Moment_off                   !number of iterations in a not restarted model, in which the momentum term is switched off (only Polynom-approach)
      INTEGER, ALLOCATABLE           :: polySplitsA(:)
      INTEGER, ALLOCATABLE           :: polySplitsQ(:)
      INTEGER, ALLOCATABLE           :: polySplitsB(:)

      END MODULE
