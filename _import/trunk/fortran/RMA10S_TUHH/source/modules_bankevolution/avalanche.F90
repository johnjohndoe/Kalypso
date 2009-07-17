!     Last change:  HN    9 Apr 2009    8:44 pm
!Avalanche ver.1.1
! This program simulates the avalanche of the submerged bank in non-cohesive soil, when the slope of the bank exeeds the compacted angle of repose
! due to fluvial bank and bank-toe erosion (Exner equation and in case of submerged overhang, after failure of this part of overhang).
! The work flow of the program is as follows:
!
!  1- The program first find the bank-toe for the left and the right banks seperately (local minima).
!  2- Starting form the bank-toe searchs for the slopes graeter than angle of repose (first loop over left bank and second loop over the right bank).
!  and computes the avalanche volume and assigns it to the lower node and updates Profile as well as Fe nodes. Here three main parts are distiguishable:

!   a) The nodes below water elevation.
!   b) The nodes above water elevation.
!   c) The nodes in transition zone, in which the FE element passes through the water surface.
!
!    Case a) the nodes are simply projected on an extrapolation line starting from the first unstable node up to the bank's top.
!    Case b) cares should be taken , for the case the front of undercut progresses (calculated by subroutine projection). In this case
!      Profile nodes should be projected between old and new front and FE nodes should be projected above the new front on an extrapolation line with
!      the slope of angle of repose of unsaturated sand.
!    Case c) this case is the most complex one, since it should create the sarting of undercut and overhang formation as well as its porpogation.
!
!  3- It computes the formation of undercut and overhang, progression of undercut zone and computation of the new front. And sort the left and right
!  banks in a tempaorary  array of profile temp_pr in between, after which the whole new profile is finally written to the ava_pr.
!-----------------------------------------------------------------------------------------------------------------------------------------------------
MODULE avalanch_modul
USE types
USE share_profile
use projectn
USE param
USE BLKSANMOD , ONLY : critical_slope,crepose,repose

implicit none

TYPE(profile),save :: ava_pr        ! ava_pr is the profile after avalanche calculations

CONTAINS

subroutine avalanche (trans_pr,totalnode, fenode, last_submerged_fenode, index_lowest_fenode,EffectiveWidth_Overhang,delvolume)!,lsubmerge,rsubmerge)                              ! trans_pr is a transition profile after tensile failure computation, if applicable.

!USE types
implicit none

INTEGER                                   ,INTENT (IN) :: totalnode
TYPE(profile),INTENT (IN) :: trans_pr
TYPE (finite_element_node), dimension (:) , INTENT (INOUT):: fenode   
Real(kind=8), dimension (2), INTENT (OUT) :: EffectiveWidth_Overhang,delVolume 
!INTEGER     , DIMENSION(2) , INTENT (OUT) ::last_submerged_node 
INTEGER , dimension (2),     INTENT (OUT) :: last_submerged_fenode  ! this node is required to assiagn the probable avalanche volume of the unsaturated bank to it.
!INTEGER , dimension (2),     INTENT (OUT) :: index_lowest           ! this specifies the profile node number of the left and right bank-toes.
INTEGER , dimension (2),     INTENT (OUT) :: index_lowest_fenode      ! this specifies the FEnode number of the left and right bank-toes.
!LOGICAL,INTENT (IN) :: Lsubmerge, Rsubmerge

TYPE (potential_nose),DIMENSION (2) :: nose
TYPE (profile)       ,DIMENSION (2) :: temp_pr
TYPE (profile_node)  ,DIMENSION (2) :: front, basepoint
TYPE (profile)                      :: tens_pr                 ! a variable, which holdes the trans_pr (with intent(IN)) and modified in the progarm.
TYPE (profile_node)                 :: point                   ! the changes in tens_pr includes the vertical displacement in avalanche process in the current (i) and next node (i+p), which will be node (i) for the next cycle of loop.

INTEGER              , DIMENSION(2) :: st,en,inc,noz,added_node
!INTEGER              , DIMENSION(2) :: last_submerged_fenode
INTEGER              , dimension (2):: index_lowest           ! this specifies the profile node number of the left and right bank-toes.
INTEGER                             :: i,j,m,n,p,r,r1,last,k,s,t,a, b, begin, endd, LR
INTEGER                             :: last_submerged_node  ! this node is required to assiagn the probable avalanche volume of the unsaturated bank to it.

!REAL :: repose,crepose                  ! crepose is critical angle of repose > repose (initiation of over critical slopes to a stable slope)
REAL(KIND=8) :: temp,temp0,del_d, delz, delz0,delarea,delarea0,sumarea,del_distance,deld,delz2,del_dis2
REAL(KIND=8) :: z1,z2,z3,d1,d2,wsl1,angl,y, SLOPE,tempVolume , resus_rate  !(resuspension rate)
real         :: radian
LOGICAL,DIMENSION (2) :: front_change, base


Write (*,*) ' Entering to Avalanche subroutine ....'

! initialization of variables
last_submerged_node   =0
last_submerged_fenode = 0
index_lowest_fenode   = 0

resus_rate = 0.
EffectiveWidth_Overhang = 0.0
n = 0
nose%node     = 0
nose%nextnode = 0
nose%dist     = 0.0
! left bank
st(1) = 1               ! start
en(1) = totalnode       ! end
inc(1)= 1               ! increment
! right bank
st(2) = totalnode
en(2) = 1
inc(2)= -1

noz(1) = trans_pr%lnose
noz(2) = trans_pr%rnose

base = .FALSE.
front_change = .FALSE.

!initialize temp_pr and ava_pr
do i = 1,2
 temp_pr(i)%cl_number = trans_pr%cl_number
 temp_pr(i)%max_nodes = 0

 
 do j = 1,200
  temp_pr(i)%prnode(j)%fe_nodenumber = 0
  temp_pr(i)%prnode(j)%distance = 0.
  temp_pr(i)%prnode(j)%elevation = 0.
  temp_pr(i)%prnode(j)%water_elev = 0.
  temp_pr(i)%prnode(j)%attribute = ''

  ava_pr%prnode(j)%fe_nodenumber = 0
  ava_pr%prnode(j)%distance = 0.
  ava_pr%prnode(j)%elevation = 0.
  ava_pr%prnode(j)%water_elev = 0.
  ava_pr%prnode(j)%attribute = ''

 end do

 temp_pr(i)%water_elev = trans_pr%water_elev
 temp_pr(i)%Rnose = 0
 temp_pr(i)%Lnose = 0
 temp_pr(i)%Rfront = 0
 temp_pr(i)%Lfront = 0
 temp_pr(i)%activation = .TRUE.

end do

 ava_pr%cl_number = trans_pr%cl_number
 ava_pr%max_nodes = 0
 ava_pr%water_elev = trans_pr%water_elev
 ava_pr%Rnose = 0
 ava_pr%Lnose = 0
 ava_pr%Rfront = 0
 ava_pr%Lfront = 0
 ava_pr%activation = .TRUE.
!-----------------------------------------
! determine if there are one bank or two banks available.
! if the right bank is totally submerged, it implies there is no rightbnak morphological evolution
! is meant.

if (trans_pr%water_elev > trans_pr%prnode(trans_pr%max_nodes)%elevation ) then
   LR = 1                  ! only left bank is available.
else 
   LR = 2                  ! Left and rigtht banks area available.
end if      

tens_pr = trans_pr       ! define tens_pr profile, since this profile will be modified(at least the attributes in overhang) and trans_pr can not be modified since it is defined with intent (IN).

! find the local deepest point (local minima in a " monotonically increasing/decreasing " curve) of the profile
outt: do j = 1,LR

temp = 1000.                           ! initial deepest point (it shows the highest probable elevation).
!?????????????????????   the following algorithm to find local minima should be developed more ?????????????????
!                        since if there is a berm then the local minima, might not be th estarting poit
!                        where the collapsed bank deposits, but it might be deposited on berm.
!                        or if there is a deep point on the top of the bank, it assumes it as bank-toe.

Inn:do i = st(j),en(j),inc(j)

  if (trans_pr%prnode(i)%elevation==0.) cycle

   if (trans_pr%prnode(i)%elevation< temp) then

    temp= trans_pr%prnode(i)%elevation
    index_lowest(j)=i                             ! local minima, bank-toe
    index_lowest_fenode(j) = trans_pr%prnode(i)%fe_nodenumber
   ELSE IF(trans_pr%prnode(i)%elevation> temp) then

     if (temp > trans_pr%water_elev) then             !HN20April09 if the found local minima is greater than water elevation then it still locates on bank or top of the bank.
       cycle inn                                      ! it is for the case that a deep point is on the top of the bank and create an unwanted local minima.
     else
         EXIT inn
     end if

   end if

  end do inn
end do outt

!**************************************************************************************
! defining the extent of the inner loop and the direction of search for critical slope
!**************************************************************************************

   if (trans_pr%lfront /= 0) then          ! if there is already a front available in left bank, then search for critical slope up to front.
      en(1) = trans_pr%lfront              ! in the case of tensile failure, the new front has been already calculated before calling this
   else                                   ! subroutine in sorting part of the bank-evolution subroutine and the elevation is equal to the current water level
      en(1)=1                             ! otherwise upto the beginning of the profile.
   end if

   inc(1)=-1

   if (trans_pr%rfront /= 0) then
      en(2) = trans_pr%rfront
   else
      en(2) = trans_pr%max_nodes      ! the search for critical slopes towards the end of profile (right)
   end if

   inc(2)=1

!****************************************************************************************
added_node = 0

!start once from left bank-toe towards left bank and once from right bank-toe towards the right bank to find critical slopes (>= angle of repose) and adjust the profile accordingly.
delvolume = 0.
outer: do j= 1,LR      ! J=1  IS LEFT BANK; J=2 IS RIGHT BANK.

delarea   = 0.
k = 1
sumarea= 0.0
temp0 = trans_pr%prnode(index_lowest(j))%elevation
temp_pr(j)%prnode(k) = tens_pr%prnode(index_lowest(j))         ! the first node of temporaray left and right profiles are their first bank-toe nodes
!*****************************************************************************************************************************
 ! inside loop over the nodes of the profile, not all of the nodes, but from local minimum (bank-toe) towards top of the bank
 ! the calculated profile nodes below water stage are saved in temp_pr, and the rest of the profile nodes over water stage are
 ! sorted in the the mentioned variable in subroutine projection.
!*****************************************************************************************************************************
!last = en(j)-inc(j)
p = inc(j)
i = index_lowest(j) - p        ! if j= 1 => p=-1 then i = index_lowest(1)-(-1), e.g. index_lowest = 45, i =45--1=46
!write (*,*) 'tens_pr', tens_pr

write (*,*) 'i = ', i
write (*,*) 'p = ', p
write (*,*) 'lowest_index = ', index_lowest(j)

inner: do                           ! if inc=-1, it starts from index_lowest node to node number en(1)-inc(1) = (1--1=2)
        i = i+p
        if (i-en(j)==0) EXIT inner
        z1=tens_pr%prnode(i)%elevation
        z2=tens_pr%prnode(i+p)%elevation
        delz=z2-z1

        d1=tens_pr%prnode(i)%distance
        d2=tens_pr%prnode(i+p)%distance
        del_distance = d2-d1

        slope=atan(ABS(delz/del_distance))*180./PI        !HN20April09, Arctan: the lateral slope of the element across the section
        wsl1=trans_pr%water_elev

        k = k + 1                                        ! index of temporary left/right profile
        temp_pr(j)%max_nodes = k

main: if ((z1<wsl1).AND.(z2<wsl1)) then                   ! In the submerged area, the element is totally submerged

            last_submerged_node = i + p
            last_submerged_fenode(j) = tens_pr%prnode(i+p)%fe_nodenumber

slp1:   if (slope>crepose) THEN  ! crepose = critical angle of repose

            delz0 = ABS(z1 - temp0)
            temp=z2
            z2 = z1 + ((-1)**j) * TAN (radian(repose)) * (del_distance)        ! the projection of profile node on the slip surface after avalanche occurs.

            tens_pr%prnode(i+p)%elevation = z2

            delz= ABS (z2-temp)
            delArea = (delz + delz0)* Abs(del_distance)/2.  ! trapezoidal rule
            r = abs(tens_pr%prnode(i)%fe_nodenumber)
            r1 = abs(tens_pr%prnode(i+p)%fe_nodenumber)
            
            CALL ResuspensionRate (resus_rate,delz,r,r1,1)

            fenode(r)%sed_source = fenode(r)%sed_source + resus_rate    ! computation of the source trem

            fenode(r1)%elevation = z2                                  ! updating the corresponding FE node's elevation.

            temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)                 ! assigning the new node to the temporaray profile for left (j=1), and right(j=2)
            temp0 = temp                                               ! assign th eold i+p elevation to temp for the next loop.
            delArea = 0.
        else   slp1

            temp0 = z2
            temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)
        end if   slp1

      elseif ( ((z1 - wsl1)<0.01).AND. ( ( (z2 - wsl1) >= 0. ).or.(abs(z2 - wsl1) <= 0.01) )  )then    main          ! transition through water

!********************************************************************************************************************
! The FE element is in transition, partly submerged, therefore the upper node (j+p) is dry or just the last wet.    *
! in the following it is assumed that if there is already an overhang available on the side of the profile being    *
! cosidered the new overhang is computed but assumed to be collapsed, leaving the profile in that zone with the     *
! angle of unsaturated consolidated angle of repose. The collapsed volue is assigned to the fe_nodes below the zone.*                                               *
!********************************************************************************************************************

slp2:    if (slope>crepose) THEN  ! crepose = critical angle of repose
!****************************************************************
  ! compute the potential noses for left and right banks
!*****************************************************************
lrnose:     if (abs (z2-wsl1)<= 0.001) then                      ! the node coincides with water level and can be taken as potential nose      !(tens%lnose ==0).AND.

                nose(j)%node     = i + p                         ! if z2 is equal to water level then i+p is the potential nose.
                nose(j)%nextnode = -(i + 2*p)
                nose(j)%dist     = tens_pr%prnode(i+p)%distance  ! the index j has been used here only while it has the same value that nose should have. In this way a double nested if-statment has beend avoided.
!3O April2009, 11:00, THE FOLLOWING OUT COMMENTED CODE WAS WRONG ; SINCE POTENTIALNOSE DOESNOT NEED TO COINCIDE WITH ACTUAL NOSE; IT CAN COINCIDE WITH EXISTING FRONT, IT IS ONLY USED TO DETERMINE VOLUME LOST DUE TO AVALANCHE AND THE BOUNDS OF PROJECTION.                  
!                 if (noz(j) /= 0) then                           ! if there is already a nose existing in the profile and the water elevation is equal to the old nose elevation, then old nose should be assigned to the potential nose.
 !                  if ( abs ( wsl1 - tens_pr%prnode( noz(j))%elevation ) <= 0.001 ) then
  !                   nose(j)%dist = tens_pr%prnode(noz(j))%distance
   !                  nose(j)%node     = i + 2*p
    !                 nose(j)%nextnode = -(i + 3*p)
     !              end if 
      !           end if   
           
            else

                nose(j)%nextnode = (i + p)           ! if the node doesnot  coincide with water then potential nose should be computed.
                call mk_potentialnose(trans_pr,nose(j))

            end if      lrnose
!******************** potentialnose end **********************************************
! it should be noted here that the computation of lost mass /volume /area due to avalanche of under water surface of the bank or
! assumed collapse of the bank over water surface due to multiple undercutting(receeding hydrograph) is done together (lumped to one mass) and then assiagned to the last submerged node.
! in the case of instability in sediment transport computation, it should be changed.
            delz0 = ABS ( z1 - temp0 )
            temp  = z2
            z2    = z1 + ( (-1) ** j ) * TAN (radian(repose)) * (del_distance)    ! repose is less than crepose, repose is initiation of slope after avalanche

! ********************************  Algorithm of Overhang formation   ****************************************
! if there is no nose on this side of profile, then the current node's elevation should not be changed TO z2 *
! , but a new profile node should be created on the extrapolation line, retaing the node(i+P).               *
! In the following the overhang is created for both cases , where i+p = water_elev and also when it is not.  *
! For the first case it means i+p is itself the new nose, and for the second case it means nose is between   *
! i and i+p.                                                                                                 *
! ************************************************************************************************************
             del_dis2 = nose(j)%dist - d1
             z3       = z1 + ( (-1) **j ) * TAN (radian(repose)) * (del_dis2)    ! the projection of nose on slip surface.
             delz2    = wsl1 - z3                                  ! The distance between the current water level and projection of the nose on slip surface.
                                                                         ! and the area between water level and the old point, forms the overhang.
  !HN. 10June2009           delArea = (abs(delz2) + delz0) * Abs (del_dis2) / 2.             ! trapezoidal rule, the area under water surface, betweem old node (i) and its projection on slip surface with nose and its projection on slip surafce.
                 
               delVolume (j) = 0.  
overhang:      if (noz(j) == 0) then                                     ! if there is no overhang available in side = j then:

                 r                      = ABS (tens_pr%prnode(i)%fe_nodenumber)
! here the computation of source rate should be done in other way so that g/m^2/s is obtained.
! it can be assumed that all the mass is given to the last submerged node and distribute it there
! or distribute over all the nodes between last submerged node and the new front.
! in the later case, it must be reassured that fenodes in that region are already displaced before assigning 
! source them to them , so that they are already wet!!!!
 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ! HN 10June2009. It is decided here to use effective area of elements and vertical displacement of nodes to compute
 ! HN 10June2009. the lost volume due to erosion. and then distribute this volume over bank-toe instead of last submerged node.
 ! HN 10June2009. which will improve the convergence of sediment transport model.
 ! HN 10June2009. This volume will be distributed later in the main program, bank_evolution together with mass failure
 ! HN 10June2009. resulting from cantilever failure (shear and tensile failure).
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 ! HN 10June2009.DUE TO ATHE BOVE REASON COMMENTED OUT                fenode(r)%sed_source  = fenode(r)%sed_source + delarea       ! Assignment of failed volume between node i and i+p to fe_node conjugate to i.
 
 ! generation of the front for the first time:
                 front(j)%fe_nodenumber = 0                                                                  ! at this stage we first assume that the new front doesnot have a conjugate, howerver it will be later determined in subroutine projection.
                 front(j)%distance      = nose(j)%dist  + ( (- 1) ** j ) * 1./TAN (radian(repose)) * delz2   ! the distance component of the new front
                 front(j)%elevation     = wsl1                                                               ! current water elevation.
                 front(j)%attribute     = 'front'
                 front(j)%water_elev    = wsl1
                 
                 if (front(j)%distance < 0.) then
                  write (*,*)
                  write (*,*) ' WARNING '
                  write (*,*) ' The distance coordinate of undercutting front is negative...'
                  pause
                 end if  

nodisnose:        if ( nose(j)%nextnode< 0) then                                        ! if the just created nose coincides with the Profile node (i+p) then:

                    r1        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
 !                   delz     = ABS (wsl1 - z2)                                         ! only the area between current water level and the new projected point is calculated
  !HN.10june2009                  delarea  = ABS (d2 - front(j)%distance) * delz2 /2.                 ! include also the triangular area between new front and nose
                    
                   
 !HN.10june2009                   fenode(r1)%sed_source             = fenode(r1)%sed_source + delarea ! Assignment of failed volume between node i+p and new front to fe_node conjugate to i+p.
                    
                    r                     = ABS (tens_pr%prnode(i)%fe_nodenumber)! 10June2009
                    delz     = ABS (wsl1 - z2) ! 10June2009
                    CALL ResuspensionRate (tempVolume,delz,r,r1,2)! 10June2009

                    delVolume (j) = tempVolume
                    
                    temp_pr(j)%prnode(k)              = tens_pr%prnode(i+p)             ! the current i + p node is assigned as a normal node with "profile" attribute.
                    temp_pr(j)%prnode(k)%elevation    = z2                              ! since this node in temp_pr is the projection of the real nose. The nose itself is made in projection subroutine.
                    
                    tens_pr%prnode(i+p)%fe_nodenumber = -1 * r1
                    tens_pr%prnode(i+p)%attribute     = 'nose'
                     
                     if (j==1) then 
                       tens_pr.Lnose = i+p
                     else
                       tens_pr.Rnose = i+p
                     end if
                    
                    if (r1 /= 0) then
                     fenode(r1)%elevation = z2
                     fenode(r1)%statuss   = 'deactivate'
                    end if


                    call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose) &
                    &                ,tens_pr%prnode(i+p), lost = tempvolume)
                    
                    delVolume (j) = delVolume (j) + tempvolume
! the rest of profile is built in potetial module. Since over water surface no avalanche process is assumed. Because, in the mentioned region only failure mechanism acts.
                  else     nodisnose                                              ! if the just created nose is between two profile nodes.

                    r        = ABS (tens_pr%prnode(i)%fe_nodenumber)
                    !June 2009 delarea  = ABS (nose(j)%dist - front(j)%distance) * delz2 /2. ! include also the triangular area between new front and nose
                    
                    !June 2009 fenode(r)%sed_source = fenode(r)%sed_source + delarea       ! Assignment of failed volume between node i+p and new front to fe_node conjugate to i+p.

                    point%fe_nodenumber   = 0                                     ! the nose type is converted to prnode type as a nose prnode called point.
                    point%distance        = nose(j)%dist
                    point%elevation       = tens_pr%prnode(i)%water_elev
                    point%water_elev      = tens_pr%water_elev
                    point%attribute       = 'nose'

belowabove:          if (wsl1 -z2 >0.001) then                                       ! the case that the elevation of the projected node i+p is below water level.

                       temp_pr(j)%prnode(k)           = tens_pr%prnode(i+p)          ! the current i + p node is assigned as a normal node with "profile" attribute.
                       temp_pr(j)%prnode(k)%elevation = z2
                       r1        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
                       tens_pr%prnode(i+p)%fe_nodenumber = -1 * r1
                       tens_pr%prnode(i+p)%attribute  = 'overhang'
      
                       delz     = ABS (wsl1 - z2) ! 10June2009
                       CALL ResuspensionRate (tempVolume,delz,r,r1,2)! 10June2009
                  
                       delVolume (j) = tempVolume
                       
                        if (r1 /= 0) then
                         fenode(r1)%elevation = z2
                         fenode(r1)%statuss   = 'deactivate'
                        end if
   
                    ! nose should be added as a new profile node to tens_pr as well as temp_pr.

                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose),point, &
                       &                 lost = tempvolume)
                       
                       delVolume (j) = delVolume (j) + tempvolume
                     
                     ELSE IF (ABS(wsl1-z2)<=0.001) THEN   belowabove      ! the case that the elevation of the projected node i+p is equal to water level, which means coincides with new front.

                       front(j)%fe_nodenumber        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
                       temp_pr(j)%prnode(k)          = front(j)
                       r                             = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
                       fenode(r)%elevation          = z2
                       fenode(r)%statuss            = 'deactivate'
                       tens_pr%prnode(i+p)%attribute = 'overhang'
                       tens_pr%prnode(i+p)%fe_nodenumber = - ABS(tens_pr%prnode(i+p)%fe_nodenumber)   
                       delVolume (j) = 0.               !HN. 10June2009. Since delz     = ABS (wsl1 - z2)= 0. No erosion on prnode i + p
                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose) &
                            & ,point,.true.)                                            
                                                                                                                        ! here true signals the subroutine not to reconsider adding the front
                     ELSE IF (wsl1 < z2 ) THEN   belowabove               ! the case in which, projected nose is above water level, meaning no projection is required for this node. 
                       
                       delVolume (j) = 0.               !HN. 10June2009. Since delz     = ABS (wsl1 - z2)< 0. No erosion on prnode i + p

                       s= k - 1
                       tens_pr%prnode(i+p)%attribute = 'overhang'

                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),s,j,tens_pr,radian(repose),point)

                       k = s
                     End if belowabove

                  end if    nodisnose

                 EXIT inner          ! since the remaining nodes between the projection zone to new front and from there to the top of the profile,
                                     ! has been made in the above callee subroutine, no need to loop over the rest of the nodes.
               else     overhang     ! the case where overhang already exists, however a new undercutting zone due to water front under overhang is being created.
!**********************************************************************************************
! From here, the case of erosion (avalanche) inspite of the existence of overhang is modelled.
!**********************************************************************************************
! HN June 2009. Here a lump mass should be calculated for the nodes between the new nose and base point
! this type of calculation has been also done in cantilever failure.
                 basepoint(j)%fe_nodenumber = 0                                            ! at this stage we first assume that the new base doesnot have a conjugate, howerver it will be later determined in subroutine projection.
                 basepoint(j)%distance = d1 + ((- 1)**j)* 1./TAN(radian(repose)) * (wsl1-z1)    ! the distance component of the new base
                 basepoint(j)%elevation = wsl1                                             ! current water elevation.
                 basepoint(j)%attribute = 'basepoint'
                 basepoint(j)%water_elev = tens_pr%prnode(i+p)%water_elev
                 base(j) = .TRUE.

    !            delz= ABS (z2-temp)
                 del_d   = ABS( nose(j)%dist - basepoint(j)%distance )
                 delArea = delz2 * Abs(del_d)/2.                ! triangular rule
                 r = ABS(tens_pr%prnode(i+p)%fe_nodenumber)

                if ((nose(j)%nextnode< 0).AND. (trim(tens_pr%prnode(i+p)%attribute) /='front')) then    ! if the just created (potential) nose coincides with the Profile node (i+p) then:
                    tens_pr%prnode(i+p)%elevation = z2                  ! the node i+p will be the node i for the next iteration of the loop: inner.
                    tens_pr%prnode(i+p)%attribute = 'profile'
                    temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)          ! the current i + p node is assigned as a normal node with "profile" attribute.

                    fenode(r)%elevation = z2                           ! this block of code works correctly even if i+p is a front!
                    fenode(r)%statuss = 'deactivate'
                    s = k
                    b = i + p
                 else
                    s = k - 1
                    b = i
                 END if
                 ! note that in the following projections start from node i and sorted after the node s = k-1 in temp_pr.

                  if (j == 1) then                     ! change the maximum number of searching index to maximum number of avaialbe nodes on each side of the profile.
                    a = 1                              ! the reason is that, it is possible that between front and base, there is so much space so that
                  else                                 ! a few profile nodes on the top of the profile could be projected between these two control points.
                    a = trans_pr%max_nodes             ! therefore, it is necessary that the index goes beyond current front.
                  end if
                   
               call  projection (fenode, nose(j)%dist,basepoint(j),radian(critical_slope),b,a,temp_pr(j),s,j,tens_pr,radian(repose), &
                 !               & oldpr_index = .true. , lost = delarea0 )
                                 & oldpr_index = .true. , lost = delVolume (j))
                ! oldpr_index = true means that there is already at least one overhang existing in profile.
                ! The reason that en(j)-p is used in above call is that the last node must not be a Front (here en(j) is front). because front should
                ! be treated in a special way, as in " else fr2" statement.
                ! the current index of the profile(oldpr_index) and temp_pr (which is k) after inclusion of new nodes between the nose and the base point (base point is a front of undercutting, but since, there exits already a front it has another name and functionality).
                  k = s
       !HN. 10June2009           delArea = delArea + delarea0
       !HN. 10June2009           r = abs(tens_pr%prnode(last_submerged_node)%fe_nodenumber)
                  
        !HN. 10June2009          fenode(r)%sed_source = fenode(r)%sed_source + delarea

                  EXIT inner

               end if   overhang
!---------------------------- The end of the generation of the overhang ---------------------------------------

         else   slp2
             temp0 = z2
             temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)

         end if   slp2

      elseif ((z1>=wsl1).AND.(z2>wsl1)) then    main            ! unsubmerged area under overhang.

 ! the element is in dry part of the profile
 !???????????????? this part should be changed if negative slope is going to be allowed for unsaturated part.
 ! generally speaking no avalanche process takes place above water surface, but if any, it is failure mechanism.
            temp0 = z2
            temp_pr(j)%prnode(k) = tens_pr%prnode(i + p)


      end if main

 end do  inner

 ! sorting the new profile after computation of avalanche in new variable ava_pr.
 if (j == 1) then 
   begin = k
   endd = 1
 else 
   begin = 1
   endd = k
 end if
   
  aval: do p = begin,endd,inc(j)                           ! p= 1 is equal to index_lowest(J)
        

            n = n + 1
        
           !test if the deepest point of left and right bank are equal
           !then include this point once in ava_pr profile
        
            if (j == 2) then
            
            if( (ava_pr%prnode(n-1)%distance == temp_pr(j)%prnode(p)%distance) &
   &           .and.(ava_pr%prnode(n-1)%elevation == temp_pr(j)%prnode(p)%elevation)) then
              n = n - 1
              cycle aval
            end if
            end if 
            
            ava_pr%prnode(n) = temp_pr(j)%prnode(p)

            if (( J == 1).and.( trim(temp_pr(j)%prnode(p)%attribute) == 'front') ) then 
               temp_pr(j)%Lfront = n
               ava_pr.Lfront     = n
            else if ( (J == 1).and.( trim(temp_pr(j)%prnode(p)%attribute) == 'nose')) then   
               temp_pr(j)%Lnose = n
               ava_pr.Lnose     = n
            else if ( (J == 2).and.( trim(temp_pr(j)%prnode(p)%attribute) == 'front')) then
               temp_pr(j)%Rfront = n
               ava_pr.Rfront     = n
            else if ( (J == 2).and.( trim(temp_pr(j)%prnode(p)%attribute) == 'nose')) then   
               temp_pr(j)%Rnose = n
               ava_pr.Rnose     = n
            end if   

        end do aval
! include the nodes between the deepest point of left and right bank, before adding the right bank to ava_pr.
           if (j==1) then
              do t = index_lowest(1)+1,index_lowest(2)-1
               n = n + 1
               ava_pr%prnode(n) = trans_pr%prnode(t)
              end do
            end if
            
            ! assign the sum of avalanched area over water surface (sumarea) to the last submerged node? what would happen if a berm was formed in between?
  !HN. 10June2009.   fenode(last_submerged_fenode(j))%sed_source= fenode(last_submerged_fenode(j))%sed_source + sumarea

end do  outer

if ( ava_pr.Lfront/= 0) then
  EffectiveWidth_Overhang (1)= (ava_pr.Prnode(ava_pr.Lnose).distance - ava_pr.Prnode(ava_pr.Lfront).distance)*1.2
elseif ( ava_pr.Rfront/= 0) then
  EffectiveWidth_Overhang (2)= ABS(ava_pr.Prnode(ava_pr.Rnose).distance - ava_pr.Prnode(ava_pr.Rfront).distance)*1.2
end if

   ava_pr%max_nodes = n
end subroutine  avalanche
END module   avalanch_modul
!--------------------------------------
 !  contains
real function radian (angle)
USE PARAM
IMPLICIT NONE 
real       , intent (in) :: angle

radian = angle * PI / 180.

end function radian

!-----------------------------------------------------
subroutine ResuspensionRate (sourceterm, delz, lowernode, uppernode, mode)

USE BLKSANMOD , ONLY : TRIBAREA, EXTLD,SGSAND ! ,ELEVB,TTHICK,DELBED
USE BLK10MOD  , ONLY : DELT 
USE param     , ONLY : Porosity !, GRAVITY, RHOS

implicit none 

real (kind = 8), intent (out)   :: sourceterm
real (kind = 8), intent (in)    :: delz
integer        , intent (in)    :: lowernode, uppernode , mode
real(kind = 8)                  :: RHOBS , AREA1, AREA2

!RHOBS=1000.*(1-porosity)* RHOS
RHOBS=1000.*(1-porosity)* SGSAND (lowernode)   ! [Kg/m^3]
 
! When mode = 1 then the source term is computed as g/(m^2.s), otherwise only the volume of erosion is computed.
if (mode == 1) then	 
	 
	  if ( (TRIBAREA(lowernode)/= 0.).AND.(TRIBAREA(uppernode)/=0.) ) then
          
          !sourceterm = delz *TRIBAREA(uppernode)*RHOBS /(DELT*3600.*TRIBAREA(lowernode))*1000.
         ![g/m^2.s]  =  [m] *  [m^2]           *[kg/m^3]/([s]*[m^2])              * [1000 g/kg]
          sourceterm = delz *TRIBAREA(uppernode)*RHOBS /(DELT*TRIBAREA(lowernode))*1000.
          EXTLD(lowernode)=EXTLD(lowernode)+ sourceterm
     
      !If the upper node area is in deactive area (dry area) its value is zero
      ! therefore compute its contributing area irrespective of TRIBAREA, which consideres
      ! only active element area.
     
      elseif ( (TRIBAREA(lowernode)/= 0.).AND.(TRIBAREA(uppernode)==0.) ) then 
     
          call EffectiveArea(area1,uppernode)
          
         ! sourceterm = delz *AREA1*RHOBS /(DELT*3600.*TRIBAREA(lowernode))*1000.
           sourceterm = delz *AREA1*RHOBS /(DELT*TRIBAREA(lowernode))*1000.
          EXTLD(lowernode)=EXTLD(lowernode)+ sourceterm

      ! if both nodes are in dry area, which will not occur in the computations of Avlanche.
      elseif ( (TRIBAREA(lowernode)== 0.).AND.(TRIBAREA(uppernode)==0.) ) then 
     
          call EffectiveArea(area1,uppernode)
          call EffectiveArea(area2,lowernode)
          
          !sourceterm = delz *AREA1*RHOBS /(DELT*3600.*Area2)*1000.
          sourceterm = delz *AREA1*RHOBS /(DELT*Area2)*1000.
          EXTLD(lowernode)=EXTLD(lowernode)+ sourceterm
     
      end if    
          
!          sourceterm = EXTLD(lowernode)  ! is it correct???? shouldn' it be only the adding part to EXTLD, since source will be added
!	                                     ! in avalanche to fenode_sed_source.
! compute only the volume of erosion
else
	  if ( (TRIBAREA(lowernode)/= 0.).AND.(TRIBAREA(uppernode)/=0.) ) then
          
          SourceTerm = delz * TRIBAREA(uppernode)
          
      !If the upper node area is in deactive area (dry area) its value is zero
      ! therefore compute its contributing area irrespective of TRIBAREA, which consideres
      ! only active element area.
      
      else 
      
          call EffectiveArea(area1,uppernode)
          
          SourceTerm = delz *AREA1
      
      end if    
end if

! the folowing will be done collectively in BAnk_erosion subroutine
! after all types of erosion and deposition have already occured and been computed.
!     ELEVB(N)=ELEVB(N)-DELTAB(N)
            
!	  TTHICK(N)=TTHICK(N)-DELTAB(N)
!	  DELBED(N)=DELBED(N)-DELTAB(N)
!	ENDIF
 !     ENDDO
      
end subroutine  ResuspensionRate     





