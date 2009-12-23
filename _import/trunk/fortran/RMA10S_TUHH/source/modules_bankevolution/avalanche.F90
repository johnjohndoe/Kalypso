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
USE init_type
USE BLKSANMOD , ONLY : critical_slope,crepose,repose
USE ControlNodes

implicit none

!TYPE(profile),save :: ava_pr        ! ava_pr is the profile after avalanche calculations

CONTAINS

!subroutine avalanche (trans_pr, fenode, last_submerged_fenode, index_lowest_fenode,EffectiveWidth_Overhang,delvolume)!,lsubmerge,rsubmerge)                              ! trans_pr is a transition profile after tensile failure computation, if applicable.

! trans_pr is a transition profile after tensile failure computation, if applicable.
subroutine avalanche (ava_pr , trans_pr, fenode,delvolume)                                 

implicit none

TYPE(profile),INTENT (IN) :: trans_pr
TYPE (finite_element_node), dimension (:) , INTENT (INOUT):: fenode 
TYPE(profile)                             , INTENT (INOUT):: ava_pr   
! EffectiveWidth_Overhang
Real(kind=8)               , dimension (2), INTENT (OUT)  :: delVolume 

TYPE (potential_nose),DIMENSION (2) :: nose
TYPE (profile)       ,DIMENSION (2) :: temp_pr
TYPE (profile_node)  ,DIMENSION (2) :: front, basepoint
! a variable, which holdes the trans_pr (with intent(IN)) and modified in the progarm.
TYPE (profile)                      :: tens_pr                          
! the changes in tens_pr includes the vertical displacement in avalanche process in the current (i) and next node (i+p), which will be node (i) for the next cycle of loop.
TYPE (profile_node)                 :: point                            
LOGICAL              ,DIMENSION (2) :: front_change, base

INTEGER              ,DIMENSION (2) :: st,en,inc,noz,added_node
! this specifies the profile node number of the left and right bank-toes.
INTEGER              ,DIMENSION (2) :: index_lowest                     
INTEGER                             :: i,j,m,n,p,r,r1,last,k,s,t,a, b, begin, endd, LR
! this node is required to assiagn the probable avalanche volume of the unsaturated bank to it.
INTEGER                             :: last_submerged_node              

!REAL :: repose,crepose                                                 ! crepose is critical angle of repose > repose (initiation of over critical slopes to a stable slope)
REAL(KIND=8)                        :: temp,temp0,del_d, delz, delz0,delarea,delarea0, &
                            &          sumarea, del_distance,deld,delz2,del_dis2
REAL(KIND=8)                        :: z1,z2,z3,d1,d2,wsl1,angl,y, SLOPE,tempVolume, & 
!(resuspension rate)
                            &          resus_rate  
real         :: radian


! Program body
!-----------------

Write (*,*) ' Entering to Avalanche subroutine ....'

! initialization of variables
last_submerged_node   = 0
!last_submerged_fenode= 0
!index_lowest_fenode  = 0
index_lowest          = 0

!EffectiveWidth_Overhang = 0.0

resus_rate    = 0.
n             = 0
nose%node     = 0
nose%nextnode = 0 
nose%dist     = 0.0

noz(1)        = trans_pr%lnose
noz(2)        = trans_pr%rnose

base          = .FALSE.
front_change  = .FALSE.

!initialize temp_pr and ava_pr
call init_profile (temp_pr,2)

do i = 1,2
 temp_pr(i)%cl_number = trans_pr%cl_number
 temp_pr(i)%water_elev = trans_pr%water_elev
end do

ava_pr = temp_pr(1)
! define tens_pr profile, since this profile will be modified(at least the attributes 
tens_pr = trans_pr                                                   
! in overhang) and trans_pr can not be modified since it is defined with intent (IN).                                                                     
!---------------

! left bank
 
  if (trans_pr%lfront /= 0) then
      st(1) = trans_pr%lfront  
  else                        
      st(1) =1                   
  end if

! end
  en(1)     = trans_pr%max_nodes  
! increment
  inc(1)    = 1                   

! right bank

   if (trans_pr%rfront /= 0) then
      st(2) = trans_pr%rfront
   else
! the search for local minima(deepest node) towards the end of profile (right)
      st(2) = trans_pr%max_nodes      
   end if

   en(2)    = 1
   inc(2)   = -1

!-----------------------------------------
! determine if there are one bank or two banks available.
! if the right bank is totally submerged, it implies there is no rightbnak morphological evolution
! is meant.

if (trans_pr%water_elev > trans_pr%prnode(trans_pr%max_nodes)%elevation ) then
! only left bank is available.
   LR = 1                  
else 
! Left and rigtht banks area available.
   LR = 2                  
end if      

! find the local deepest point (local minima in a " monotonically increasing/decreasing " curve) of the profile
outt: do j = 1,LR

! initial deepest point (it shows the highest probable elevation).
       temp = 1000.                                 

       CALL CriticalNodes (trans_pr, st(j), en(j), inc(j), index_lowest(j))
 
      end do outt

!**************************************************************************************
! defining the extent of the inner loop and the direction of search for critical slope
!**************************************************************************************
! otherwise upto the beginning of the profile.
      en (1) = 1                             
      inc(1) =-1
! the search for critical slopes towards the end of profile (right)
      en (2) = trans_pr%max_nodes      
      inc(2) = 1

!****************************************************************************************
added_node = 0

!start once from left bank-toe towards left bank and once from right bank-toe towards the right bank to find critical slopes (>= angle of repose) and adjust the profile accordingly.
delvolume = 0.
tempvolume = 0.

! J=1  IS LEFT BANK; J=2 IS RIGHT BANK.
outer: do j= 1,LR      

delarea   = 0.
delarea0  = 0.
k = 1
sumarea= 0.0
temp0 = trans_pr%prnode(index_lowest(j))%elevation
! the first node of temporaray left and right profiles are their first bank-toe nodes
temp_pr(j)%prnode(k) = tens_pr%prnode(index_lowest(j))         
!*****************************************************************************************************************************
! inside loop over the nodes of the profile, not all of the nodes, but from local minimum (bank-toe) towards top of the bank 
! the calculated profile nodes below water stage are saved in temp_pr, and the rest of the profile nodes over water stage are 
! sorted in the the mentioned variable in subroutine projection. 
!*****************************************************************************************************************************
p = inc(j)
! if j= 1 => p=-1 then i = index_lowest(1)-(-1), e.g. index_lowest = 45, i =45--1=46
i = index_lowest(j) - p                                         

write (*,*) 'i = ', i
write (*,*) 'p = ', p
write (*,*) 'lowest_index = ', index_lowest(j)

! if inc=-1, it starts from index_lowest node to node number en(1)-inc(1) = (1--1=2)
inner: do                           
        i = i+p
        if (i-en(j)==0) EXIT inner
        z1=tens_pr%prnode(i)%elevation
        z2=tens_pr%prnode(i+p)%elevation
        delz=z2-z1

        d1=tens_pr%prnode(i)%distance
        d2=tens_pr%prnode(i+p)%distance
        del_distance = d2-d1

!HN20April09, Arctan: the lateral slope of the element across the section
        slope=atan(ABS(delz/del_distance))*180./PI        
        wsl1=trans_pr%water_elev

! index of temporary left/right profile
        k = k + 1                                        
        temp_pr(j)%max_nodes = k

! In the submerged area, the element is totally submerged
main: if ((z1-wsl1)<-0.001 .AND. (z2-wsl1)<-0.001) then                   

            last_submerged_node = i + p
!   last_submerged_fenode(j) = tens_pr%prnode(i+p)%fe_nodenumber         

! crepose = critical angle of repose
slp1:   if (slope>crepose) THEN  

            delz0 = ABS(z1 - temp0)
            temp=z2
! the projection of profile node on the slip surface after avalanche occurs.
            z2 = z1 + ((-1)**j) * TAN (radian(repose)) * (del_distance)        

            tens_pr%prnode(i+p)%elevation = z2

            delz= ABS (z2-temp)
! trapezoidal rule
            delArea = (delz + delz0)* Abs(del_distance)/2.  
            r = abs(tens_pr%prnode(i)%fe_nodenumber)
            r1 = abs(tens_pr%prnode(i+p)%fe_nodenumber)
! summing all eroded mass together and distribute it later in bank erosion subroutine over the bank-toe together 
! with cantilver failed mass
!18.08.09           CALL ResuspensionRate (tempvolume,delz,r,r1,2)  
            tempvolume    = delarea   
            delVolume (j) = delVolume (j) + tempVolume
! Assing the eroded mass of the upper node to the lower node                                                           
! 18.08.09           CALL ResuspensionRate (resus_rate,delz,r,r1,1,Pr_lower=tens_pr%prnode(i))

!18.08.09     temp_pr(j)%prnode(k-1) = tens_pr%prnode(i)       
            
!18.08.09     fenode(r)%sed_source = fenode(r)%sed_source + 0.1 * resus_rate ! computation of the source trem(suspended part)       
! updating the corresponding FE node's elevation.
            if ( r1 > 0 ) fenode(r1)%elevation = z2                                 

! assigning the new node to the temporaray profile for left (j=1), and right(j=2)
            temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)                              
! assign the old i+p elevation to temp for the next loop.
            temp0 = temp                                                            
            delArea = 0.
            tempVolume = 0.
        
        else   slp1

            temp0 = z2
            temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)
            
        end if   slp1

! transition through water
      elseif ( (i/=noz(j)) .AND. ((z1 - wsl1)<-0.001) .AND. ( ( (z2 - wsl1) >= 0. ) .OR. (abs(z2 - wsl1) <= 0.001) )  )then    main          

!********************************************************************************************************************
! The FE element is in transition, partly submerged, therefore the upper node (j+p) is dry or just the last wet.    *
! in the following it is assumed that if there is already an overhang available on the side of the profile being    *
! cosidered the new overhang is computed but assumed to be collapsed, leaving the profile in that zone with the     *
! angle of unsaturated consolidated angle of repose. The collapsed volume is assigned to the fe_nodes below the zone.*                                               *
!********************************************************************************************************************

! crepose = critical angle of repose
slp2:    if (slope>crepose) THEN  
!****************************************************************
! compute the potential noses for left and right banks  
!*****************************************************************
! the node coincides with water level and can be taken as potential nose      !(tens%lnose ==0) .AND. 
lrnose:     if (abs (z2-wsl1)<= 0.001) then                      

! if z2 is equal to water level then i+p is the potential nose.
                nose(j)%node     = i + p                         
                nose(j)%nextnode = -(i + 2*p)
! the index j has been used here only while it has the same value that nose should have. In this way a double nested if-statment has beend avoided.
                nose(j)%dist     = tens_pr%prnode(i+p)%distance  
           
            else

! if the node doesnot  coincide with water then potential nose should be computed.
                nose(j)%nextnode = (i + p)           
                call mk_potentialnose(trans_pr,nose(j),p)

            end if      lrnose
!******************** potentialnose end **********************************************

            delz0 = ABS ( z1 - temp0 )
            temp  = z2
! repose is less than crepose, repose is initiation of slope after avalanche
            z2    = z1 + ( (-1) ** j ) * TAN (radian(repose)) * (del_distance)    

! ********************************  Algorithm of Overhang formation   ****************************************
! if there is no nose on this side of profile, then the current node's elevation should not be changed TO z2 *
! , but a new profile node should be created on the extrapolation line, retaining the node(i+P).             *
! In the following the overhang is created for both cases , where i+p = water_elev and also when it is not.  *
! For the first case it means i+p is itself the new nose, and for the second case it means nose is between   *
! i and i+p.                                                                                                 *
! ************************************************************************************************************
             del_dis2 = nose(j)%dist - d1
! the projection of nose on slip surface.
             z3       = z1 + ( (-1) **j ) * TAN (radian(repose)) * (del_dis2)    
! The distance between the current water level and projection of the nose on slip surface.
             delz2    = wsl1 - z3                                  
! and the area between water level and the old point, forms the overhang.                                                                         
! trapezoidal rule, the area under water surface, betweem old node (i) and its projection on slip surface with nose and its projection on slip surafce.
             delArea = (abs(delz2) + delz0) * Abs (del_dis2) / 2.             
               tempvolume = delarea  
               delVolume (j) = delvolume(j) + tempvolume
               
! if there is no overhang available in side = j then:
overhang:      if (noz(j) == 0) then                                     

                 r                      = ABS (tens_pr%prnode(i)%fe_nodenumber)
! distribute lost mass(g/m^2/s)/area/volume over all the nodes between last submerged node and the new front.
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
! HN 10June2009. It is decided here to use effective area of elements and vertical displacement of nodes to compute 
! HN 10June2009. the lost volume due to erosion. and then distribute this volume over bank-toe instead of last submerged node. 
! HN 10June2009. which will improve the convergence of sediment transport model. 
! HN 10June2009. This volume will be distributed later in the main program, bank_evolution together with mass failure 
! HN 10June2009. resulting from cantilever failure (shear and tensile failure). 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
! HN 10June2009.DUE TO THE ABOVE REASON COMMENTED OUT                fenode(r)%sed_source  = fenode(r)%sed_source + delarea       ! Assignment of failed volume between node i and i+p to fe_node conjugate to i. 
 
! generation of the front for the first time: 
! at this stage we first assume that the new front doesnot have a conjugate, howerver it will be later determined in subroutine projection.
                 front(j)%fe_nodenumber = 0                                                                  
! the distance component of the new front
                 front(j)%distance      = nose(j)%dist  + ( (- 1) ** j ) * 1./TAN (radian(repose)) * delz2   
! current water elevation.
                 front(j)%elevation     = wsl1                                                               
                 front(j)%attribute     = 'front'
                 front(j)%water_elev    = wsl1
                 
                 if (front(j)%distance < 0.) then
                  write (*,*)
                  write (*,*) ' WARNING '
                  write (*,*) ' The distance coordinate of undercutting front is negative...'
                  pause
                 end if  

! if the just created nose coincides with the Profile node (i+p) then:
nodisnose:        if ( nose(j)%nextnode< 0) then                                        

                    r1        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
!                   delz     = ABS (wsl1 - z2)                                         ! only the area between current water level and the new projected point is calculated
! include also the triangular area between new front and nose
                   delarea  = ABS (d2 - front(j)%distance) * delz2 /2.                 
                    
                   
!HN.10june2009                   fenode(r1)%sed_source             = fenode(r1)%sed_source + delarea ! Assignment of failed volume between node i+p and new front to fe_node conjugate to i+p. 
                    
! 10June2009
                    r                     = ABS (tens_pr%prnode(i)%fe_nodenumber)
! 10June2009
                    delz     = ABS (wsl1 - z2) 
!18.08.09                CALL ResuspensionRate (tempVolume,delz,r,r1,2)! 10June2009    
                    tempvolume    = delarea
                    delVolume (j) = delVolume (j) + tempVolume
                    
! the current i + p node is assigned as a normal node with "profile" attribute.
                    temp_pr(j)%prnode(k)              = tens_pr%prnode(i+p)             
! since this node in temp_pr is the projection of the real nose. The nose itself is made in projection subroutine.
                    temp_pr(j)%prnode(k)%elevation    = z2                              
                    
                    tens_pr%prnode(i+p)%fe_nodenumber = -1 * r1
                    tens_pr%prnode(i+p)%attribute     = 'nose'
                     
                     if (j==1) then 
                       tens_pr%Lnose = i+p
                     else
                       tens_pr%Rnose = i+p
                     end if
                    
                    if (r1 /= 0) then
                     fenode(r1)%elevation = z2
                     fenode(r1)%statuss   = 'deactivate'
                    end if

                    tempvolume = 0.
                    call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose) &
                    &                ,tens_pr%prnode(i+p), lost = tempvolume)
                    
                    delVolume (j) = delVolume (j) + tempvolume
! the rest of profile is built in potetial module. Since over water surface no avalanche process is assumed. Because, in the mentioned region only failure mechanism acts.
! if the just created nose is between two profile nodes.
                  else     nodisnose                                              

                    r        = ABS (tens_pr%prnode(i)%fe_nodenumber)
! include also the triangular area between new front and nose
                    delarea  = ABS (nose(j)%dist - front(j)%distance) * delz2 /2. 
                    tempvolume    = delarea
                    delVolume (j) = delVolume (j) + tempVolume
                    
!June 2009 fenode(r)%sed_source = fenode(r)%sed_source + delarea       ! Assignment of failed volume between node i+p and new front to fe_node conjugate to i+p.                    

! the nose type is converted to prnode type as a nose prnode called point.
                    point%fe_nodenumber   = 0                                     
                    point%distance        = nose(j)%dist
                    point%elevation       = wsl1
                    point%water_elev      = wsl1
                    point%attribute       = 'nose'

! the case that the elevation of the projected node i+p is below water level.
belowabove:          if (wsl1 -z2 >0.001) then                                       
                      
                      r1        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
                      
                      if (r1/=0) then 
! the current i + p node is assigned as a normal node with "profile" attribute.
                       temp_pr(j)%prnode(k)           = tens_pr%prnode(i+p)          
                       temp_pr(j)%prnode(k)%elevation = z2
                       tens_pr%prnode(i+p)%fe_nodenumber = -1 * r1
                       tens_pr%prnode(i+p)%attribute  = 'overhang'
                       fenode(r1)%elevation = z2
                       fenode(r1)%statuss   = 'deactivate'
 
                     else
                       k = k - 1
                       tens_pr%prnode(i+p)%attribute  = 'overhang'
                     end if   
! 10June2009
                       delz     = ABS (wsl1 - z2) 
!                      CALL ResuspensionRate (tempVolume,delz,r,r1,2)! 10June2009 
                  
!                     delVolume (j) = delVolume (j) + tempVolume  
                       
!                        if (r1 /= 0) then
!                         fenode(r1)%elevation = z2
!                         fenode(r1)%statuss   = 'deactivate'
!                        end if
   
! nose should be added as a new profile node to tens_pr as well as temp_pr.                    
                       
                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose),point, &
                       &                 lost = tempvolume)
                       delVolume (j) = delVolume (j) + tempvolume
                     
! the case that the elevation of the projected node i+p is equal to water level, which means coincides with new front.
                     ELSE IF (ABS(wsl1-z2)<=0.001) THEN   belowabove      
                       
                       r                             = ABS(tens_pr%prnode(i+p)%fe_nodenumber)
                       front(j)%fe_nodenumber        = r
!                   front(j)%fe_nodenumber        = ABS(tens_pr%prnode(i+p)%fe_nodenumber)    
                       temp_pr(j)%prnode(k)          = front(j)
                       
                       tens_pr%prnode(i+p)%attribute = 'overhang'
!     tens_pr%prnode(i+p)%fe_nodenumber = - ABS(tens_pr%prnode(i+p)%fe_nodenumber)                     
                       tens_pr%prnode(i+p)%fe_nodenumber = -r
                       
                       if (r/=0) then
                       fenode(r)%elevation          = z2
                       fenode(r)%statuss            = 'deactivate'
                       end if
!         delVolume (j) = 0.               !HN. 10June2009. Since delz     = ABS (wsl1 - z2)= 0. No erosion on prnode i + p              
                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),k,j,tens_pr,radian(repose) &
                            & ,point,.true.)                                            
! here true signals the subroutine not to reconsider adding the front                                                                                                                        
! the case in which, projected nose is above water level, meaning no projection is required for this node. 
                     ELSE IF (wsl1 < z2 ) THEN   belowabove               
                       
!                     delVolume (j) = 0.               !HN. 10June2009. Since delz     = ABS (wsl1 - z2)< 0. No erosion on prnode i + p  

                       s= k - 1
                       tens_pr%prnode(i+p)%attribute = 'overhang'

                       call  projection (fenode,nose(j)%dist,front(j),radian(critical_slope),i+p,en(j),temp_pr(j),s,j,tens_pr,radian(repose),point)

                       k = s
                     End if belowabove

                  end if    nodisnose

! since the remaining nodes between the projection zone to new front and from there to the top of the profile,
                 EXIT inner          
! has been made in the above callee subroutine, no need to loop over the rest of the nodes.                                     
! the case where overhang already exists, however a new undercutting zone due to water front under overhang is being created.
               else     overhang     
!**********************************************************************************************
! From here, the case of erosion (avalanche) inspite of the existence of overhang is modelled.
!**********************************************************************************************
! HN June 2009. Here a lump mass should be calculated for the nodes between the new nose and base point
! this type of calculation has been also done in cantilever failure.
! at this stage we first assume that the new base doesnot have a conjugate, howerver it will be later determined in subroutine projection.
                 basepoint(j)%fe_nodenumber = 0                                            
! the distance component of the new base
                 basepoint(j)%distance = d1 + ((- 1)**j)* 1./TAN(radian(repose)) * (wsl1-z1)    
! current water elevation.
                 basepoint(j)%elevation = wsl1                                             
                 basepoint(j)%attribute = 'basepoint'
                 basepoint(j)%water_elev = wsl1
                 base(j) = .TRUE.

! In the case that the node (i+p) is front and water level is equal to the front elevation                
! and base distance is less than 1cm away from front distance, then donot need to project                
! nodes and compute new front.                 

                 If ( trim(AdjustL(tens_pr%prnode(i+p)%attribute)) =='front' )then
                  If (abs(wsl1 - tens_pr%prnode(i+p)%elevation)<=0.001) then
                   if ( abs(basepoint(j)%distance - tens_pr%prnode(i+p)%distance)<=0.01) then
! include front
                     temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)                                 
                     k = k + 1
                     i = i + p
! include nose
                     temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)                                  
                     cycle inner
                   end if
                  end if
                 end if  
                   
!            delz= ABS (z2-temp)    
                 del_d   = ABS( nose(j)%dist - basepoint(j)%distance )
! triangular rule
                 delArea = delz2 * Abs(del_d)/2.                

                 r = ABS(tens_pr%prnode(i+p)%fe_nodenumber)

! if the just created (potential) nose coincides with the Profile node (i+p) then:
                if ((nose(j)%nextnode< 0) .AND. (trim(tens_pr%prnode(i+p)%attribute) /='front')) then    
! the node i+p will be the node i for the next iteration of the loop: inner.
                    tens_pr%prnode(i+p)%elevation = z2                  
                    tens_pr%prnode(i+p)%attribute = 'profile'
! the current i + p node is assigned as a normal node with "profile" attribute.
                    temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)          
                    
                    if (r/=0) then 
! this block of code works correctly even if i+p is a front!
                    fenode(r)%elevation = z2                           
                    fenode(r)%statuss = 'deactivate'
                    end if
                    
                    s = k
                    b = i + p
                 else
                    s = k - 1
                    b = i
                 END if
! note that in the following projections start from node i and sorted after the node s = k-1 in temp_pr.                 

! change the maximum number of searching index to maximum number of avaialbe nodes on each side of the profile.
                  if (j == 1) then                     
! the reason is that, it is possible that between front and base, there is so much space so that
                    a = 1                              
! a few profile nodes on the top of the profile could be projected between these two control points.
                  else                                 
! therefore, it is necessary that the index goes beyond current front.
                    a = trans_pr%max_nodes             
                  end if
                   
               call  projection (fenode, nose(j)%dist,basepoint(j),radian(critical_slope),b,a,temp_pr(j),s,j,tens_pr,radian(repose), &
                                & oldpr_index = .true. , lost = delarea0 )
!                 & oldpr_index = .true. , lost = delVolume (j))                
! oldpr_index = true means that there is already at least one overhang existing in profile.                
! The reason that en(j)-p is used in above call is that the last node must not be a Front (here en(j) is front). because front should                
! be treated in a special way, as in " else fr2" statement.                
! the current index of the profile(oldpr_index) and temp_pr (which is k) after inclusion of new nodes between the nose and the base point                
! (base point is a front of undercutting, but since, there exits already a front it has another name and functionality).                
                  k = s
                  delArea = delArea + delarea0
                  tempvolume    = delarea
                  delVolume (j) = delVolume (j) + tempVolume

!HN. 10June2009           r = abs(tens_pr%prnode(last_submerged_node)%fe_nodenumber)       
                  
!HN. 10June2009          fenode(r)%sed_source = fenode(r)%sed_source + delarea        

                  EXIT inner

               end if   overhang
!---------------------------- The end of the generation of the overhang ---------------------------------------

         else   slp2
             temp0 = z2
             temp_pr(j)%prnode(k) = tens_pr%prnode(i+p)

         end if   slp2

       else
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
   
! p= 1 is equal to index_lowest(J)
  aval: do p = begin,endd,inc(j)                           
        

            n = n + 1
        
!test if the deepest point of left and right bank are equal           
!then include this point once in ava_pr profile           
        
            if (j == 2) then
            
            if( (ava_pr%prnode(n-1)%distance == temp_pr(j)%prnode(p)%distance) &
   &          .AND. (ava_pr%prnode(n-1)%elevation == temp_pr(j)%prnode(p)%elevation)) then
              n = n - 1
              cycle aval
            end if
            end if 
      
            if (( J == 1) .AND. ( trim(AdjustL(temp_pr(j)%prnode(p)%attribute)) == 'front') ) then 
! Exclude front if it is very close (1 mm) to the nose               
              if ( ABS(temp_pr(j)%prnode(p)%distance - temp_pr(j)%prnode(p - inc(j) )%distance)<0.001) then
                
                if (temp_pr(j)%prnode(p)%fe_nodenumber == 0 ) then
                  n = n - 1
                  cycle
                else if (temp_pr(j)%prnode(p)%fe_nodenumber /= 0 ) then
                  temp_pr(j)%prnode(p)%attribute = 'profile'
                end if
                
              else 
                temp_pr(j)%Lfront = n
                ava_pr%Lfront     = n
              end if    
                 
            else if ( (J == 1) .AND. ( trim(AdjustL(temp_pr(j)%prnode(p)%attribute)) == 'nose')) then   
! Exclude nose if it is very close (1 mm) to the front               
              if ( ABS(temp_pr(j)%prnode(p)%distance - temp_pr(j)%prnode(p + inc(j) )%distance)<0.001) then
                n = n - 1
                cycle
              else 
                temp_pr(j)%Lnose = n
                ava_pr%Lnose     = n
              end if    

            else if ( (J == 2) .AND. ( trim(AdjustL(temp_pr(j)%prnode(p)%attribute)) == 'front')) then
! Exclude front if it is very close (1 mm) to the nose               
               if ( ABS(temp_pr(j)%prnode(p)%distance - temp_pr(j)%prnode(p + inc(j) )%distance)<0.001) then
                
                if (temp_pr(j)%prnode(p)%fe_nodenumber == 0 ) then
                  n = n - 1
                  cycle
                else if (temp_pr(j)%prnode(p)%fe_nodenumber /= 0 ) then
                  temp_pr(j)%prnode(p)%attribute = 'profile'
                end if
                
              else 
               temp_pr(j)%Rfront = n
               ava_pr%Rfront     = n
              end if    
 
            else if ( (J == 2) .AND. ( trim(AdjustL(temp_pr(j)%prnode(p)%attribute)) == 'nose')) then   
! Exclude nose if it is very close (1 mm) to the front               
              if ( ABS(temp_pr(j)%prnode(p)%distance - temp_pr(j)%prnode(p - inc(j) )%distance)<0.001) then
                n = n - 1
                cycle
              else 
               temp_pr(j)%Rnose = n
               ava_pr%Rnose     = n
              end if    
          
            end if   
            
            ava_pr%prnode(n) = temp_pr(j)%prnode(p)
        
        end do aval
! include the nodes between the deepest point of left and right bank, before adding the right bank to ava_pr.
            if ( (j==1) .AND. (LR == 2) ) then
              
              do t = index_lowest(1)+1,index_lowest(2)-1
               n = n + 1
               ava_pr%prnode(n) = trans_pr%prnode(t)
              end do
            
! the case of only left bank, but with exiting nodes right hand of talweg.
            elseif ( (j==1) .AND. (LR == 1) ) then      
              t = index_lowest(1)
              
              do 
               t = t + 1
               if ( t > trans_pr%max_nodes) exit
               n = n + 1
               ava_pr%prnode(n) = trans_pr%prnode(t)
              end do  
            
            end if
            

end do  outer

!if ( ava_pr%Lfront/= 0) then
!  EffectiveWidth_Overhang (1)= (ava_pr%Prnode(ava_pr%Lnose).distance - ava_pr%Prnode(ava_pr%Lfront).distance)*1.2
!elseif ( ava_pr%Rfront/= 0) then
!  EffectiveWidth_Overhang (2)= ABS(ava_pr%Prnode(ava_pr%Rnose).distance - ava_pr%Prnode(ava_pr%Rfront).distance)*1.2
!end if

   ava_pr%max_nodes = n
end subroutine  avalanche
!-----------------------------------------------------
subroutine ResuspensionRate (sourceterm, delz, lowernode, uppernode, mode, Pr_lower)

USE BLKSANMOD    , ONLY : TRIBAREA, EXTLD,SGSAND,ELEVB,TTHICK,DELBED
USE BLK10MOD     , ONLY : DELT 
!, GRAVITY, RHOS
USE param        , ONLY : Porosity 
USE types

implicit none 

type (Profile_node), intent(inout),optional:: Pr_lower
real (kind = 8), intent (out)   :: sourceterm
real (kind = 8), intent (in)    :: delz
integer        , intent (in)    :: lowernode, uppernode , mode
real(kind = 8)                  :: RHOBS , AREA1, AREA2, DELTAB

integer                         :: Tlowernode, Tuppernode, upper, lower

AREA1 = 0.
AREA2 = 0.
DELTAB= 0.

!RHOBS=1000.*(1-porosity)* RHOS
! [Kg/m^3]
RHOBS=1000.*(1-porosity)* SGSAND (lowernode)   

! in the case that either of the upper node or lower node are zero ,  
! meaning that they have no conjugated (pair) FE node. 
! As a matter of fact it may only happen when the upper point is a front  
! which has no conjugate FEnode, then it is located on the same element as  
! the lower node. Therefore, they are identical in respect to TRIBAREA. 

 if (lowernode ==0) then
  Tlowernode = uppernode
  Tuppernode = uppernode
 elseif (uppernode == 0) then
  Tuppernode = lowernode
  Tlowernode = lowernode
 else
  Tuppernode = uppernode
  Tlowernode = lowernode
 end if
  
! When mode = 1 then the source term is computed as g/(m^2.s), otherwise only the volume of erosion is computed.
if (mode == 1) then       
       
        if ( (TRIBAREA(Tlowernode)/= 0.) .AND. (TRIBAREA(Tuppernode)/=0.) ) then
          
![g/m^2.s]  =  [m] *  [m^2]           *[kg/m^3]/([s]*[m^2])              * [1000 g/kg]         
          sourceterm = delz *TRIBAREA(Tuppernode)*RHOBS /(DELT*TRIBAREA(Tlowernode))*1000.
          
!TODO: Later the factor of 0.01(the part of the failed material, taken as source term and the rest   
! is assumed to be deposited directly at bed) will be defined by the user.    
          EXTLD(Tlowernode)=EXTLD(Tlowernode)+ 0.1*sourceterm
     
! If the upper node area is in deactive area (dry area) its value is zero      
! therefore compute its contributing area irrespective of TRIBAREA, which consideres      
! only active element area.      
     
      elseif ( (TRIBAREA(Tlowernode)/= 0.) .AND. (TRIBAREA(Tuppernode)==0.) ) then 
     
          call EffectiveArea(area1,Tuppernode)
          
           sourceterm = delz *AREA1*RHOBS /(DELT*TRIBAREA(Tlowernode))*1000.
    
          EXTLD(Tlowernode)=EXTLD(Tlowernode)+ 0.1* sourceterm

! if both nodes are in dry area, which will not occur in the computations of Avlanche.      
      elseif ( (TRIBAREA(Tlowernode)== 0.) .AND. (TRIBAREA(Tuppernode)==0.) ) then 
     
          call EffectiveArea(area1,Tuppernode)
          call EffectiveArea(area2,Tlowernode)
          
          sourceterm = delz *AREA1*RHOBS /(DELT*Area2)*1000.
 
          EXTLD(Tlowernode)=EXTLD(Tlowernode)+ 0.1 * sourceterm
     
      end if    
          
!     assigning the vloume part (90% at the moment, later defined by the user)
!     directly to the bed as deposited material
      DELTAB           = 0.9 * sourceterm * DELT /(RHOBS*1000.)
! currently fenode deactivated (since only mode=2 is used),otherwise fenode should be defined     
! as argument of the subroutine     
      
!    fenode(Tlowernode)%elevation   = fenode(Tlowernode)%elevation  + DELTAB
     
      pr_lower%elevation = pr_lower%elevation+ DELTAB
      ELEVB(Tlowernode)  = ELEVB(Tlowernode) + DELTAB
        TTHICK(Tlowernode) = TTHICK(Tlowernode)+ DELTAB
        DELBED(Tlowernode) = DELBED(Tlowernode)+ DELTAB

! compute only the volume of erosion
else
        if ( (TRIBAREA(Tlowernode)/= 0.) .AND. (TRIBAREA(Tuppernode)/=0.) ) then
          
          SourceTerm = delz * TRIBAREA(Tuppernode)
          
!If the upper node area is in deactive area (dry area) its value is zero      
! therefore compute its contributing area irrespective of TRIBAREA, which consideres      
! only active element area.      
      
      else 
      
          call EffectiveArea(area1,Tuppernode)
          
          SourceTerm = delz *AREA1
      
      end if    
end if

      
end subroutine  ResuspensionRate     

END module   avalanch_modul
!--------------------------------------
real function radian (angle)
USE PARAM
IMPLICIT NONE 
real       , intent (in) :: angle

radian = angle * PI / 180.

end function radian






