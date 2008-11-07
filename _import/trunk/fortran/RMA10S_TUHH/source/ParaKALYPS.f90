module parakalyps

!model dimension parameters
!--------------------------
integer (kind = 4) :: maxIntPolElts, NodesToIntPol, statElSz, statNoSz
!meaning of the variables
!------------------------
!nodestointpol    number of nodes to be added via interpolation (only 1D-nodes)
!maxintpolelts    maximum number of elements to be added within one original element via interpolation
!TODO: statElSz and statNoSz should be substituted and removed completely
!------------------------------------------------------------------------
!statelsz         element number to start counting the interpolated element ID numbers at; actually it's maxe+1
!statnosz         node number to start counting the interpolated node ID numbers at; actually it's maxp+1


!resistance parameters (lambdas)
!-------------------------------
real (kind = 8), allocatable, dimension (:) :: lambdatot
real (kind = 8), allocatable, dimension (:) :: lambdaks, lambdap
real (kind = 8), allocatable, dimension (:) :: lambdadunes
!meaning of the variables
!------------------------
!lambdatot        total calculated lambda value of an element
!lambdaks         lambda due to bed roughness (ks)
!lambdap          lambda due to vegetation resistance (ax, ay, dp)
!lambdadunes      lambda due to dunes resistance (not operative yet)

!resistance correction parameters for calibration purposes
!---------------------------------------------------------
real (kind = 8), allocatable, dimension (:) :: correctionks, correctionaxay, correctiondp
!meaning of the variables
!------------------------
!correctionks     correction scaling factor for the ks value of an element; default must be 1.0d0
!correctionaxay   correction scaling factor for the ax and ay values of an element; default must be 1.0d0
!correctiondp     correction scaling factor for the dp values of an element; default must be 1.0d0

!node informations
!-----------------
logical, allocatable, dimension (:)              :: intpolprof, ispolynomnode
integer (kind = 4), allocatable, dimension (:)   :: nconnect
integer (kind = 4), allocatable, dimension (:,:) :: neighb, neighprof
real (kind = 8), allocatable, dimension (:)      :: kmweight
integer (kind = 4), allocatable, dimension (:,:) :: isnodeofelement
!meaning of the variables
!------------------------
!intpolprof       switch for sort of 1D node within polynomial approach
!                 .t. - node is an interpolation profile
!                 .f. - node is an original profile
!ispolynomnode    switch for type of 1D node
!                 .t. - 1D-node described via polynomials
!                 .f. - 1D-node described via geometry definition (trapezoidal approach)
!nconnect         number of connected nodes to another node
!neighb           stores the IDs of the nodes that are connected to a certain node
!neighprof        stores the two neighbouring original profiles' flow kilometers to an interpolated node
!kmweight         stores the weighted influence of two neighbouring original nodes to interpolated ones via the distance
!isnodeofelement  stores all element IDs, where a node is inside; restriction is that nodes are having maximum 12 elements, where they are part of

!element informations
!--------------------
integer (kind = 4), allocatable, dimension (:)   :: intpolno (:)
integer (kind = 4), allocatable, dimension (:,:) :: intpolelts
real (kind=8), allocatable  	                   :: mcord(:,:)
logical, allocatable, dimension (:)              :: transitionelement
!meaning of the variables
!------------------------
!intpolno         number of interpolation nodes within an element
!intpolelts       stores the IDs of the interpolated elements in their sequence for each original element
!mcord            centre coordinates of an element
!transitionelement  switch for information about 1D element's nature
!                 .t. - is the 1D part of a 1D/2D line-2-element transition
!                 .f. - is a normal 1D element

!file name inforamtions
!----------------------
character (len = 96) :: name_cwr
!meaning of the variables
!------------------------
!name_cwr         name string of the cwr-data input file, if restarting
                  !TODO: Probably not needed this way any longer

!results copies (for writing out purposes)
!--------------
real(kind=8), allocatable, dimension (:,:) :: rausv
!meaning of the variables
!------------------------
!rausv            vector field as a copy of the results
!                 1 - x-velocity
!                 2 - y-velocity
!                 3 - water depth
!                 4 - water surface elevation

!vegetation usage switch
INTEGER   :: IVEGETATION
!using energy level for cstrc
INTEGER   :: UseEnergyCSTRC

!NiS,mar06:	The following arrays and variables belonged in Kalypso-2D to the common-block "raus"; now they are part of this module.
!		At the moment they are allocated in the RDKALYPS-subroutine and deallocated in the main program RMA10, so that they ARE
!               deallocated and that they can be used in oter subroutines; because their meaning is not totally clear at the moment later
!               programming activities might change this. A better way for global access might be to allocate them after the call of getgeo1
!               from the main program RMA10, because the arrays are dependent on the network size.
INTEGER 		  :: iauslp, iausnpm
REAL(KIND=8), allocatable :: zeigma(:)
REAL (KIND = 8), ALLOCATABLE :: minvel (:,:), maxvel (:,:), minrausv(:), maxrausv(:)
!-

!-

!NiS,apr06:     The following variables are necessary for the implementation of the Darcy-Weisbach (DW) flow equation. Within the subroutines the
!               flow-resistance 'lambda' is calculated as well as the resistance coefficient c_wr. The necessary arrays are declared at this point.
!               They will be allocated and initialized in the subroutine 'initl'. The arrays are compatible with the arrays ZMANN and CHEZ out of
!               the Blk10mod.for module. They could also be declared there. In the first step all variables necessary for the implementation of
!               the DW-roughness method are put together to see possible errors directly:
REAL, ALLOCATABLE :: abst(:), c_wr(:), durchbaum(:), mvx(:), mvy(:), mvxvy(:)
ALLOCATABLE cniku (:)
REAL(KIND=8), ALLOCATABLE, DIMENSION (:) :: mh

!filename for saving cwr-file
!frequency pointer for output after itertion
INTEGER            :: itefreq

!NiS,jul06: At the moment as a dummy argument, the bedform-array in a global version. It is passed to the subroutine
!           formrauheit in the file roughness.f90
real (KIND=8),allocatable :: gl_bedform(:,:)

!nis,jun07: Adding specific discharge and difference quotients of that over h and v for alle transition nodes
!           Add pointer for all nodes to be part of a transition or not
logical, allocatable :: TransitionMember(:)
INTEGER, allocatable :: TransLinePart (:)
REAL   , allocatable :: dspecdh (:), dspecdv (:)
REAL                 :: q2d(1:50), dq2ddh (1: 50)
!-

!EFa may07, the following variables are necessary for the implementation of the subroutine turbulence
REAL(KIND=8)             :: dsid(1:5)
REAL(KIND=8)             :: semp,dside,gsc1,gsc2,artr1,artr2,gscal
!-

!EFa jun07, autoconverge
REAL, ALLOCATABLE 	:: rss(:), specccold(:,:),speccc(:,:)
REAL, ALLOCATABLE 	:: specccfut(:,:),temp_vel(:,:)
REAL, ALLOCATABLE 	:: temp_vdot(:,:),temp_vdoto(:,:)
REAL               :: rrr,hhh,hhh2,qqq,qqqdir
INTEGER            :: nitnzero,nitazero,nitizero,extranita
INTEGER            :: beiauto,nnnunst,nnnst,linlog,exterr
INTEGER            :: emp_nan
ALLOCATABLE        :: temp_nbc(:,:)
INTEGER            :: temp_maxnn,temp_iteqs,temp_iteqv
INTEGER            :: temp_iurvl,temp_itlvl,temp_iurvl1
!EFa aug07
REAL               :: autoindex
REAL(KIND = 8)     :: deltsum
REAL,allocatable   :: temp_speccc(:)

!nis,aug07: for correction purposes in Kalypso-GUI, there must be a correction based on the elements
!           correction applies for ks-value, diameter of ... and distance between vegetation elements
REAL (KIND = 8), DIMENSION (1:50, 1:350) :: LineCorrectionKS, LineCorrectionAxAy, LineCorrectionDp



!nis,jan08: New output control paramters
INTEGER :: WriteNodeBlock     ! (==1: Writes the nodal values into output.out file; ==0: Writes no nodal values into output.out)

!nis,jan08: new variables for profile interpolation

!new parameters for weirs defined by linear functions
REAL (KIND = 8), ALLOCATABLE :: cstrcRange (:,:,:), cstrcCoefs (:,:,:,:), cstrcdisch (:, :)

INTEGER :: testoutput

end module parakalyps
