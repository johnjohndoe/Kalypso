!******************************************************************************************
!  module para1dpoly collects many parameters that are connected to the 1D polynomial
!    approach
!
!******************************************************************************************
module para1dpoly

!single profile descriptive parameters
!-------------------------------------
real (kind = 8), allocatable, dimension (:) :: kmx
real (kind = 8), allocatable, dimension (:) :: hhmin, hhmax
real (kind = 8), allocatable, dimension (:) :: hbordv
!meaning of the variables
!------------------------
!kmx              kilometer of a 1D-profile
!hhmin            minimum water depth of the valid range of polynomials
!hhmax            maximum water detph of the valid range of polynomials
!hbordv           bankful elevation to determine the minimum water depth to start calculating with flow coefficient

!A(h) polynomial
!---------------
real (kind = 8), allocatable, dimension (:,:,:) :: apoly
real (kind = 8), allocatable, dimension (:,:)   :: polyrangea
integer (kind = 4), allocatable, dimension (:)  :: polysplitsa
!meaning of the variables
!------------------------
!polysplitsa      number of intersections of the A(h)-polynomial description at a profile
!polyrangea       all the lower border water depth values for the intersections of A(h)-polynomials at a profile
!apoly            all the coefficients for all A(h) polynomials of all intersections at a profile

!Q(h) polynomial (Schluesselkurve; translation?)
!-----------------------------------------------
real (kind = 8), allocatable, dimension (:,:,:) :: qpoly
real (kind = 8), allocatable, dimension (:,:)   :: polyrangeq
integer (kind = 4), allocatable, dimension (:)  :: polysplitsq
real (kind = 8), allocatable, dimension (:)     :: qgef
!meaning of the variables
!------------------------
!polysplitsq      number of intersections of the Q(h)-polynomial description at a profile
!polyrangeq       all the lower border water depth values for the intersections of Q(h)-polynomials at a profile
!qpoly            all the coefficients for all Q(h) polynomials of all intersections at a profile
!qgef             reference slope for the q-curve (Schluesselkurve)

!alpha/beta polynomial (Boussinesq/energy flow coefficient)
!----------------------------------------------------------
real (kind = 8), allocatable, dimension (:,:,:) :: alphapoly, betapoly
real (kind = 8), allocatable, dimension (:,:)   :: polyrangeb
integer (kind = 4), allocatable, dimension (:)  :: polysplitsb
!meaning of the variables
!------------------------
!polysplitsb      number of intersections of the alpha/beta(h)-polynomial description at a profile
!polyrangeb       all the lower border water depth values for the intersections of alpha/beta(h)-polynomials at a profile
!alphapoly        all the coefficients for all alpha(h) polynomials of all intersections at a profile
!betapoly         all the coefficients for all beta(h) polynomials of all intersections at a profile

!control parameters for the run time
!-----------------------------------
integer (kind = 4) :: beient, moment_off, percentcheck
!meaning of the variables
!------------------------
!beient           decision switch for the usage of the flow coefficient/ of the convective term at all
!                 0 - use no coefficient (alpha = 1.0)
!                 1 - use momentum coefficient (alpha); "Boussinesq-coeffient"
!                 2 - use energy coefficient (beta)
!                 3 - use no flow coefficient, i.e. switch of convective term (alpha = 0.0)
!percentcheck     switch
!moment_off       number of iterations in a not restarted model, in which the momentum term is switched off (only polynom-approach)


!cross section data for global use
!---------------------------------
real (kind = 8) , allocatable  :: ah(:)                        
real (kind = 8) , allocatable  :: dahdh(:)                     
!meaning of the variables
!------------------------
!ah               cross section area dependent on water depth at a node
!dahdh            derivative of cross section area dependent on water depth over water depth at a node

end module para1dpoly
