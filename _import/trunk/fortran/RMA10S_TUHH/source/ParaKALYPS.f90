!     Last change:  WP   25 Oct 2007    9:11 am
!     Last change:  NIS  15 Aug 2007    4:22 pm
MODULE ParaKALYPS

!vegetation usage switch
INTEGER   :: IVEGETATION

!NiS,mar06:	The following arrays and variables belonged in Kalypso-2D to the common-block "raus"; now they are part of this module.
!		At the moment they are allocated in the RDKALYPS-subroutine and deallocated in the main program RMA10, so that they ARE
!               deallocated and that they can be used in oter subroutines; because their meaning is not totally clear at the moment later
!               programming activities might change this. A better way for global access might be to allocate them after the call of getgeo1
!               from the main program RMA10, because the arrays are dependent on the network size.
INTEGER 		  :: iauslp, iausnpm
REAL(KIND=4), allocatable :: rausv(:,:), zeigma(:)
!-

!NiS,apr06:     arrays for neighbourhood relations.
INTEGER,       ALLOCATABLE 	:: nconnect(:)
INTEGER,       ALLOCATABLE 	:: neighb(:,:)
REAL (KIND=8), ALLOCATABLE  	:: mcord(:,:)
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
CHARACTER (LEN=32) :: name_cwr
!frequency pointer for output after itertion
INTEGER            :: itefreq

!NiS,jul06: At the moment as a dummy argument, the bedform-array in a global version. It is passed to the subroutine
!           formrauheit in the file roughness.f90
real (KIND=4),allocatable :: gl_bedform(:,:)

!nis,jun07: Adding specific discharge and difference quotients of that over h and v for alle transition nodes
!           Add pointer for all nodes to be part of a transition or not
logical, allocatable :: TransitionMember(:)
REAL   , allocatable :: dspecdh (:), dspecdv (:)
REAL                 :: q2d(1:50)
!-

!if restart with kalypso 2D-file, then RESTARTSwitch = 1, else RESTARTSwitch = 0
INTEGER                  :: RESTARTSwitch

!EFa may07, the following variables are necessary for the implementation of the subroutine turbulence
REAL(KIND=8)             :: dsid(1:5)
REAL(KIND=8)             :: semp,dside,gsc1,gsc2,artr1,artr2,gscal
!-

!EFa jun07, autoconverge
REAL, ALLOCATABLE 	:: rss(:), specccold(:,:),speccc(:,:)
REAL, ALLOCATABLE 	:: specccfut(:,:),temp_vel(:,:)
REAL, ALLOCATABLE 	:: temp_vdot(:,:),temp_vdoto(:,:)
REAL                    :: rrr,hhh,hhh2,qqq,qqqdir
INTEGER                 :: nitnzero,nitazero,nitizero,extranita
INTEGER                 :: beiauto,nnnunst,nnnst,linlog,exterr
INTEGER                 :: emp_nan
ALLOCATABLE             :: temp_nbc(:,:)
INTEGER                 :: temp_maxnn,temp_iteqs,temp_iteqv
INTEGER                 :: temp_iurvl,temp_itlvl,temp_iurvl1
!EFa aug07
REAL                    :: autoindex
REAL(KIND = 8)          :: deltsum
REAL,allocatable        :: temp_speccc(:)

!nis,aug07: for correction purposes in Kalypso-GUI, there must be a correction based on the elements
!           correction applies for ks-value, diameter of ... and distance between vegetation elements
REAL (KIND = 8), allocatable :: correctionKS(:), correctionAxAy(:), correctionDp(:)
REAL (KIND = 8), DIMENSION (1:50, 1:350) :: LineCorrectionKS, LineCorrectionAxAy, LineCorrectionDp

INTEGER, allocatable:: IsNodeOfElement(:,:)

REAL (KIND = 8), ALLOCATABLE, dimension (:):: lambdaKS, lambdaP, lambdaDunes, lambdaTot


END MODULE 
