C     Last change:  AF   17 Jul 2006   12:13 pm
      MODULE ParaKALYPS
!NiS,mar06:     Parameter for Kalypso-program-structure and for CVFEM-control as far as necessary to implement parts of
!               Kalypso-2D into RMA10S.

      INTEGER   :: FEM, IVEGETATION

!NiS,mar06:	The following arrays and variables belonged in Kalypso-2D to the common-block "raus"; now they are part of this module.
!		At the moment they are allocated in the RDKALYPS-subroutine and deallocated in the main program RMA10, so that they ARE
!               deallocated and that they can be used in oter subroutines; because their meaning is not totally clear at the moment later
!               programming activities might change this. A better way for global access might be to allocate them after the call of getgeo1
!               from the main program RMA10, because the arrays are dependent on the network size.
      INTEGER 			:: iauslp, iausnpm
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
!      REAL(KIND=8), ALLOCATABLE, DIMENSION (:)
      REAL, ALLOCATABLE, DIMENSION (:)
     +                  	:: abst, c_wr, cniku, durchbaum,
     +                             mvx, mvy, mvxvy
      REAL(KIND=8), ALLOCATABLE, DIMENSION (:) :: mh

      CHARACTER (LEN=32)        :: name_cwr
      INTEGER                   :: itefreq                          !frequency pointer for output after itertion
!-

!NiS,jul06: At the moment as a dummy argument, the bedform-array in a global version. It is passed to the subroutine
!           formrauheit in the file roughness.f90
      real (KIND=4),allocatable:: gl_bedform(:,:)
!-      


!FEM            switch for the control of CVFEM; switch is parat but the CVFEM-code is not yet translated to RMA10S



      END MODULE 
