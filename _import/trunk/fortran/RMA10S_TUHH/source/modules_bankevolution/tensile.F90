!     Last change:  HN    9 Apr 2009    8:46 pm
! The following depicts left and right bank of a cross section in the river with existing two overhangs
! the Fe-node number corresponding to the profile nodes in overhang region are equal
! to the negative value of the corresponding fe-node number
! pr(i)%prnode(j)%Fe_nodenumber = - Fe_node%number
!      |<-      here is    the overhang zone   <-------------------------------------|
! *****************
!      +           *                                                  upper edge
!       +           *                                                **************************
!New     +           *          WATER LEVEL                         *            +
!Front <- +'''''''''' * '''''''''''''''''''''''''''''''''''''''''' *->Potential +
!          +########## * ----> | Failing                          *       nose +
!    Front  *************      | mass                            *            +
!            *                                                  *            + -> Extrapolation line (where the Fe nodes are formed by projecting
!             *                                                *            +     the upper edge profile nodes on this extrapolation line)
!              *                                              *            +
!               *                            Nose (RIGHT)->  ************** -> Front (right)
!                *                                            lower edge *
!                 *                                                     *
!                  *                                                   *
!                   *                                                 *
!                                                                    *
!
!      LEFT BANK                                           RIGHT BANK
!

!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! **  Tensile Failure in Non-cohesive river overhang, M.Sc. M.Hassan Nasermoaddeli, TUHH, Institute Fuer Wasserbau ** !
! *****************************************  Hamburg, Germany  (2009)************************************************ !
!*********************************************************************************************************************!

!*********************************************************************************************************************!
!      This program is to model tensile failure of the overhang of the non-cohesive sandy banks, under the condition  !
!      of unsteady flow , when a part of the overhang is submerged due to water rise in the river.                    !
!      It is assumed that as soon as a part of the overhang is submerged it fails and the lower edge of the overhang  !
!      is formed directly over water surface, by producing a horizontal line and projecting upper  profile nodes      !
!      on the extrapolation line to compute area (volume) lost due to tensile failure, but later the actual elevation !
!      of the fenode computed in Exner equation will be assigned to the profile's node elevation on the extrapolation !
!      line (buffer(l,3) = fenode(q)%elevation.                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         ****************************       History of debugging            ***************************
! coming soon : making the if structures modular so that the if clause are excuted dynamically , when it is needed.
!                and for the case that no nodes are within the extrapolation line region between old and new front.
! coming soon : reduce the code by omitting the extra if blocks for the case of right bank, by introducing dynamic
!                GREATER and CLOSER (>), (<) for left and right banks.
!
! HN06March09: The code was corrected for the case that potential nose is closer to the origin than old front,
!                which otherwise creates problem in computation of lost volume.
!
! HN05March09: Correction for the case , where the profile node has no conjugate FE node, (for example old nose)
!                by introducing the function q to compute the nearest FE node under the profile node.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE tensile
USE types
USE share_profile
USE param
USE cantilever
USE init_type
USE BLK10MOD  , ONLY : DELT
USE BLKSANMOD , ONLY : TRIBAREA, EXTLD, SGSAND, critical_slope 

implicit none

! the save statment make the array available to the main program, since in Fortran95 allocatable arrays are automatically deallocated after ending the subroutine.
TYPE(profile_node) , allocatable ,DIMENSION (:), save :: newnodes                   

CONTAINS

subroutine tensile_failure(pr,fenode,side,newwater_elev, potnose ,new_front,number_newnodes,lost_nodes,Sumsource,refined)

 implicit none

! Subroutine arguments
!----------------------
 TYPE (finite_element_node), DIMENSION (:), INTENT (INOUT) :: fenode
 TYPE ( profile )                         , INTENT (INOUT) :: pr
 TYPE (potential_nose)                    , INTENT (IN)    :: potnose
! is the distance of the new front to the origin of the profile
 TYPE (profile_node)                      , INTENT (OUT)   :: new_front        
 Real (kind = 8)                          , INTENT (out)   :: Sumsource
 Real (kind = 8)                          , INTENT (IN)    :: newwater_elev
 INTEGER                                  , INTENT (OUT)   :: number_newnodes, lost_nodes
 CHARACTER (LEN=5)                        , INTENT (IN)    :: side
 LOGICAL                                  , INTENT (IN)    :: refined

! Local variables
!-------------------
 INTEGER , parameter            :: DOUBL = 8

 TYPE (profile_node)            :: frontnode
 TYPE (PROFILE), DIMENSION (1)  :: TMP_PROFILE
 TYPE (PROFILE)                 :: TEMP_PROFILE ,TENSILE_PR
! the variable holding THE fe_ node NUMBER, distance and elevation of THE new nodes projected ON extrapolation line.
 REAL(KIND=8), DIMENSION (20,3) :: buffer    
! the effective volume of collapsed overhang
 REAL(KIND=8), DIMENSION (2)    :: EffectVolume            
 REAL(KIND=8), DIMENSION (2)    :: SF
! this critical slope is consolidated unsaturated critical slope (unsubmerged area)
 INTEGER     , DIMENSION (2)    :: front     
 INTEGER                        :: i,j,k,l,m,n,p,q,r,s,s1,s2,t,u,b,bb,counter
 INTEGER                        :: status, q1, INDEX

 REAL (KIND=DOUBL)              :: dis,z,z1,x,d,d1,d2,h,h1,h2,del_h1,del_h2, sourceterm,sourceterm1
! these two variables define the borders defined for the computation of the lost volume due to tensile failure.
 REAL (KIND=DOUBL)              :: trans1, trans2          
 REAL (KIND=DOUBL)              :: RHOBS, deld, delz, ELEV, AREA
 REAL                           :: radian

 LOGICAL                        :: FrontCounted, TwistFlag, potnose_coincid_newfront
 
!Program Body 
!------------ 

 Write (*,*) ' Entering to Tensile failure Modelling ....'

! Initializing the variables 
!--------------------------- 
 
! To make the array available for the the next call, make it deallocated if it is allocated.
 if (ALLOCATED(newnodes)) then               
  deallocate (newnodes)
 end if
 
 call INIT_PROFILE(TMP_PROFILE, 1)
 
 TEMP_PROFILE = TMP_PROFILE(1)
 
 potnose_coincid_newfront = .FALSE.
 FrontCounted = .FALSE.
 TwistFlag    = .FALSE.
 Sumsource    = 0.0
 EffectVolume = 0.0
! the profile's node number corresponding to fronts (left and/or) right , if available, if not, they are equal to zero. 
 front(1) = pr%Lfront
 front(2) = pr%Rfront
!*******************************************************************************************************************************
! In the following if-clause the local coordiante of the new front is computed, where elevation is equal to the water elevation*
! and the only unkwon is the distance to the origin of the profile. That is here the variable x.                               *
!*******************************************************************************************************************************
 
 if (side =='left') then

! the distance component of the old front
    dis = pr%prnode(front(1))%distance                        
    z = pr%prnode(front(1))%elevation
! the distance component of the new front
    x = dis - 1./TAN(radian(critical_slope)) * (newwater_elev - z)                   
    m = 1
    p = 1
    n = pr%Lnose  
    frontnode = pr%prnode(front(1))
! m, P and n are starting,increment and ending index of the the next loop,respectively.                                                   
! In the case that new front distance (x) > potential nose (due to the mild slope of the overhang)
! then assume a slipe surface parallel to the overhang's face OR compute the intersection point.
    
    if ( (x-potnose%dist) >= -0.01 ) then
  
      if (refined ) then
       
       CALL cantilever_failure( TENSILE_PR, PR , PR%WATER_ELEV , critical_slope , EffectVolume , fenode ,SF,potnose_coincid_newfront)
      
       sumsource  = EffectVolume(1)
       PR = TENSILE_PR
       lost_nodes = -999
       
       ALLOCATE (newnodes(1),stat=STATUS)  
       if (status > 0) then
        write (*,*)' the memory allocation for new nodes after tensile failure, was unsuccessful ...'
        pause
       end if

       RETURN
      
      else
      
       x = potnose%dist
      
      end if 
    end if  

! HN06March09: correction for probable relative positions of potential nose and old front.  

! if the old front distance is smaller than potential nose distance
     if (dis < potnose%dist) then                                          
       trans1 = dis
       trans2 = potnose%dist
! if the old front distance is greater than potential nose distance
     ELSEIF (dis > potnose%dist) then                                      
       trans1 = potnose%dist
       trans2 = dis
       TwistFlag = .TRUE.
! if the old front distance is equal to the potential nose distance
     ELSEIF (ABS(dis - potnose%dist)< 0.001) then                          
       trans1 = dis
       trans2 = trans1
     end if

 elseif (side =='right') then

       dis = pr%prnode(front(2))%distance
       z   = pr%prnode(front(2))%elevation
! the distance component of the new front
       x   = dis + 1./TAN(radian(critical_slope)) * (newwater_elev - z)  
       m = pr%max_nodes
       p = -1
       n = pr%Rnose
       frontnode = pr%prnode(front(2))

! In the case that new front distance (x) > potential nose (due to the mild slope of the overhang
! then assume a slipe surface parallel to the overhang's face OR compute the intersection point.
  
       if ( (potnose%dist - x) >= -0.01 ) then
          
        if  (refined ) then
          
          CALL cantilever_failure( TENSILE_PR, PR , PR%WATER_ELEV , critical_slope , EffectVolume , fenode,SF,potnose_coincid_newfront  )
          
          sumsource  = EffectVolume(2)
          PR = TENSILE_PR
          lost_nodes = -999

          ALLOCATE (newnodes(1),stat=STATUS)  
          if (status > 0) then
           write (*,*)' the memory allocation for new nodes after tensile failure, was unsuccessful ...'
           pause
          end if
 
          RETURN
        else
          x = potnose%dist
        end if
          
       end if  

! if the old front distance is smaller than potential nose distance
       if (dis < potnose%dist) then                                          
          trans1 = potnose%dist
          trans2 = dis
          TwistFlag = .TRUE.
! if the old front distance is greater than potential nose distance
       ELSEIF (dis > potnose%dist) then                                      
          trans1 = dis
          trans2 = potnose%dist
! if the old front distance is equal to the potential nose distance
       ELSEIF (ABS(dis - potnose%dist)< 0.001) then                          
          trans1 = dis
          trans2 = trans1
       end if

 end if

! at the beginning it is assumed that there is no conjugate fe_node, however, it will be later determined if it is correct or not.
 new_front%fe_nodenumber = 0                               
 new_front%distance = x
 new_front%elevation =  newwater_elev
 new_front%water_elev = newwater_elev
 new_front%attribute = 'front'
   
!*******************************************************************************************************************************
! Initialization of counters, arrays and varibles.
s  = 0
s1 = 0
s2 = 0
d1 = x
h1 = newwater_elev
d2 = dis
! elevation of the old front
h2 = z                 
t  = 0
q  = 0
k  = 0
l  = 0
!RHOBS=1000.*(1-porosity)* RHOS   !conversion of density[Kg/m^3] to concentration [gr/m^3]
buffer = 0

sourceterm = 0.
sourceterm1 = 0.

!**************    Start of compuatation of tensile failure for the left bank   ***************************************************!
! for each slice (between two subsequent profile node) the wasted mass is allocated to the corresponding FE nodes beneath the overhang.
! it computes also the number of profile nodes lost due to tensile failure and computes the coordiante of the new projected nodes
! on the extrapolation line between old and new front (only needed for lost volume computation)and assign the corresponding FE node to them.
! This projection on extrapolation line is done, since it is possible that the current (updated) FE nodes on this extrapolation line
! have changed their elevation due to erosion of FE nodes, computed in Exner.
! However, after computation of the lost volume due to the tensile failure, the actual elevation of profile nodes in this region is updated
! using their conjugate FE nodes.
!**********************************************************************************************************************************

!HN0309.  improved for the case where q = 0, meaning that there is no fe node associated to the profile node (for the case of front ad nose

main: do i = m,n,p

  d= pr%prnode(i)%distance
  u= ABS(pr%prnode(i)%fe_nodenumber)

LR:   if (side =='left') then

!        if (ABS(d-x)<0.001) then                                  ! if the current node distance coincides with new front, then assign the fe_node number to it.
! if the current node distance coincides with new front, then assign the fe_node number to it.
        if (ABS(d-x)<=0.001) then                                  
           new_front%fe_nodenumber= u
           new_front%distance = pr%prnode(i)%distance
! although those nodes in overhang zone (above extrapolation line) have already, in avalanche subroutine, the value of -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
           pr%prnode(i)%fe_nodenumber = -u        
           pr%prnode(i)%attribute = 'overhang'
        end if

! when the profile nodes are located between new_front and old front         
 L1:     if ((d-x > 0.001) .AND. (d<= trans1)) then                       
! if the current node has already a projection on the old profile no need to create double node (once more of it).
            if ( pr%prnode(i)%fe_nodenumber < 0 ) cycle main          
! The fe node number of the profile nodes, on the top of the profile, turns to negative signalising its projection on extrapolation line(under water).
            pr%prnode(i)%fe_nodenumber = -u                            
! Projection of the current profile's node on extrapolation line (the elevation), 
            h = newwater_elev - TAN(radian(critical_slope))* (d - x)   
! this elevation is only to be used for calculation of mass lost (area loss) in overhang zone.                                                                       
! the actual elevation of the profile node is equal to that of fenode, which has been computed by Exner, which may have not be identical with that of the extrapolation line (which is the original initial position).
            pr%prnode(i)%attribute = 'overhang'                        
!start computing the lost mass through tensile failure.
! since the elevation of the profile nodes on lower edge are all the same and equal to front(1).
            del_h1 = newwater_elev - h1            
            del_h2 = newwater_elev - h

            sourceterm =1./2. * (d - d1) * (del_h1 + del_h2)
            d1 = pr%prnode(i)%distance
            h1 = h
            Sumsource = Sumsource + sourceterm
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [mg/m^2.s].
! since the node which receives the mass is directly beneath profile node therefore the TRIBAREA (which is the contributing area of the elements connected to each node)
! is the same for  denoting node(source) as well as receiving node.Hence the ratio of this two areas is equal to 1.           
          
!      sourceterm = del_h2 * RHOBS *1000./DELT      
            
! assiagning the lost mass to the corresponding Fe- node as source term for suspension
            q = ABS(pr%prnode(i)%fe_nodenumber)    
!            fenode(q)%sed_source = sourceterm + fenode(q)%sed_source 
            
! if the current node is conjugated node of front, no need to account it in added nodes (buffer).
            if (trim(ADJUSTL(pr%prnode(i)%attribute))=='front') cycle main   
! in the case that the node is an old intersection point or base point with q=0, do not include it as new node projection. 
            if (q == 0) cycle main                               
! Number of the new nodes on left bank on extrapolation line.
            l=l+1                                                
! The associated fe_node number to the new profile node on the extrapolation line.
            buffer(l,1) = q                                      
! The distance of the new profile node on the extrapolation line.
            buffer(l,2) = d                                      
! The elevation of the new profile node on the overhang is not the one on the extrapolation line
            buffer(l,3) = fenode(q)%elevation                   
! but the one in fenode array, because the node might have been eroded due to the erosion resulting from flooding.                                                                 
               
        
         elseif ((d> trans1) .AND. (d<=trans2)) then   L1
!            pr%prnode(i)%fe_nodenumber = -u       ! 04.05.2009 11:10 .
            s = s + 1
  

! this if statement computes the case of crossing over old front.
CO:         if (s==1) then                                       
! It should be noted that by the first entrance into this if statement, h1 and d1                                                                 
! have the values of the last node from previous if statement, which is the one before
              del_h1 = newwater_elev - h1                        
! old front node or old fornt node itself, and h2 = z, d2 =dis alreading defined over the loop: main.
              del_h2 = newwater_elev - h2                        
! this area correspondes to the area between the last node before old front and old front itself
              sourceterm1 = 1./2. * (d2 - d1) * (del_h1 + del_h2)
!,( the area left to the old-front node).                                                                 
! sustitute the first area-computing node with old front node Distance
              d1 = trans1                                           
! sustitute the first area-computing node with old front node elevation
              h1 = z                                             
! sustitute the second area-computing node with the current node Distance
              d2 = pr%prnode(i)%distance          
! sustitute the second area-computing node with the elevation of lower edge of teh overhang
              h2 = pr%prnode(front(1))%elevation  
! which are equal to the elevation of the old front. (since current node lies on upper edge of the overhang)                                                                 
              del_h1 = newwater_elev - h1
              del_h2 = newwater_elev - h2
! the new area correspondes to the area between the old front the node following it
              sourceterm1 = sourceterm1 + 1./2. * (d2 - d1) * (del_h1 + del_h2) 
! (the area right to the old front-node).The sum of righ and left to the old.front make the total                                                                                
! source term pssing to the Fe-node beneath the current profile's node.                                                                                
             Sumsource = Sumsource + sourceterm1

! to test if q1 = q in the program so that the findnode algorithm is omitted later.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!            RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65  
!            sourceterm1 = del_h2 * RHOBS * 1000./(DELT)   
!           fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1   
!--------------------------------------------------------------------------------------------------------------------------------------
!cross over case
            else  CO        

              d1 = d2
              h1 = h2
! Although h2 will remain constant = old front elevation, but to make the follow-up the program easier it will be repeated.
              d2 = pr%prnode(i)%distance         
! since the current profile's node lies on the upper edge of the overhang,
              h2 = pr%prnode(front(1))%elevation 
! but we need its correspong image node on the lower edge of the overhang,                                                                
! which are equal to the elevation of the old front's elevation.
              del_h1 = newwater_elev - h1        
              del_h2 = newwater_elev - h2

! as a matter of fact Del_h1 and del_he are equal
              sourceterm =1./2. * (d2 - d1) * (del_h1 + del_h2) 
              Sumsource = Sumsource + sourceterm
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!          RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65    
!          sourceterm = del_h2 * RHOBS * 1000./(DELT)     
!         fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm     
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO
            
            if (TwistFlag) then
            
! assiagning the lost mass to the corresponding Fe- node as source term for suspension
             q = ABS(pr%prnode(i)%fe_nodenumber)    
             s2 = s2 + 1
! in the case that the node is an old intersection point or base point with q=0, do not include it as new node projection. 
             if (q == 0) cycle main                               
            
! Projection of the current profile's node on line element between new front and old front.
             h = newwater_elev + delz/deld* (dis - d)   
             fenode(q)%elevation = h
! Number of the new nodes on left bank on extrapolation line.
             l=l+1                                                
! The associated fe_node number to the new profile node on the extrapolation line.
             buffer(l,1) = q                                      
! The distance of the new profile node on the extrapolation line.
             buffer(l,2) = d                                      
! The elevation of the new profile node on the overhang is not the one on the extrapolation line
             buffer(l,3) = fenode(q)%elevation                   
            
            end if
            
         ELSEIF (d > trans2) then  L1

! number of nodes, which will be deleted in the new profile due to tensile failure
             s1 = s1 + 1                                         

! this if statement computes the case of crossing over potential nose.
 CO1:        if (s1==1) then                                     

! substitute the distance and elevation of the last node from the previous if statement
              d1 = d2                                            
! to d1 and h1. As a matter of fact h1 and h2 remain constant for d > front%distance.
              h1 = h2                                            
              d2 = trans2
              h2 = pr%prnode(front(1))%elevation

              del_h1 = newwater_elev - h1
              del_h2 = newwater_elev - h2
! this area correspondes to the area between the last node before potential nose and potential nose itself
              sourceterm1 = 1./2. * (d2 - d1) * (del_h1 + del_h2)
!,( the area left to the potential nose node).                                                                 
! sustitute the first area-computing node with potential nose Distance
              d1 = trans2                              
! sustitute the second area-computing node with the current node Distance
              d2 = pr%prnode(i)%distance          
! d2 is equal to d, in fact it was not necessary to make a new variable 'd2'                                                                 
! from here on, the water elevation, will not be the refrence , but the elevation of upper edge of overhang
              del_h1 = newwater_elev - h1         
! should be subtracted from that of the lower edge , since the nodes following potential nose are fully submerged.
              del_h2 = abs(pr%prnode(i)%elevation - h2) 
! the new area correspondes to the area between the potential nose and the node following it
              sourceterm1 = sourceterm1 + 1./2. * (d2 - d1) * (del_h1 + del_h2) 
! (the area right to the potential nose).The sum of righ and left to the potential nose make the total                                                                                
! source term passing to the Fe-node beneath the current profile's node.                                                                                
              Sumsource = Sumsource + sourceterm1
              z1 = pr%prnode(i)%elevation
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
              q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!        RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65      
!        sourceterm1 = del_h2 * RHOBS * 1000./(DELT)       
!       fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1       
!--------------------------------------------------------------------------------------------------------------------------------------
!after cross over case
            else  CO1        

              d1 = d2
! h2 and h1 have the same value and they are located on the lower edge of the overhang.
              h1 = h2                                    
              d2 = pr%prnode(i)%distance

              del_h1 = z1 - h1
              del_h2 = pr%prnode(i)%elevation - h2

              sourceterm =1./2. * (d2 - d1) * (del_h1 + del_h2)
              Sumsource = Sumsource + sourceterm
              z1 = pr%prnode(i)%elevation
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
              q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!      RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65        
!      sourceterm = del_h2 * RHOBS * 1000./(DELT)         
!        fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm      
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO1

         end if     L1
!**************    End of compuatation of tensile failure for the left bank   ********************************!

!**************   Start of compuatation of tensile failure for the right bank   ********************************!

! the right overhang
      else    LR         
!          if (ABS(d-x)<0.001) then 
           if (ABS(d-x)<=0.001) then
!             u= ABS(pr%prnode(i)%fe_nodenumber)    ! it is computed in the main loop!
             new_front%fe_nodenumber= u
             new_front%distance = pr%prnode(i)%distance
! although those nodes in overhang zone (above extrapolation line) have already in avalanche subroutine -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
             pr%prnode(i)%fe_nodenumber = -u        
             pr%prnode(i)%attribute = 'overhang'
           end if

!L2:        if ((d<x) .AND. (d>=trans1)) then
L2:        if ((d- x<-0.001) .AND. (d>=trans1)) then
! if the current node has already a projection on the old profile no need to create double node (once more of it).
            if ( pr%prnode(i)%fe_nodenumber < 0 ) cycle main          
            pr%prnode(i)%attribute = 'overhang'
! The fe node number of the profile nodes, on the top of the profile, turns to negative signalising its projection on extrapolation line(under water).
            pr%prnode(i)%fe_nodenumber = -u     
! Image of the current profile's node on extrapolation line (the elevation). The equation is based on old front (right).
            h = z + TAN(radian(critical_slope)) * (d - dis)                                  
! since the elevation of the profile nodes on lower edge are all the same and equal to front(1).
            del_h1= newwater_elev - h1            
            del_h2 = newwater_elev - h

            sourceterm =1./2. * (d1 - d) * (del_h1 + del_h2)
            d1 = pr%prnode(i)%distance
            h1 = h
            Sumsource = Sumsource + sourceterm
! assiagning the lost mass to the corresponding Fe- node as source term for suspension
            q = ABS(pr%prnode(i)%fe_nodenumber)         
    
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.    
! in the following it is directly assigned to the nodes below overhang. The source term is in [mg/m^2.s]               
!  RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65          
! sourceterm = del_h2 * RHOBS * 1000./DELT           
    
!      fenode(q)%sed_source = sourceterm + fenode(q)%sed_source      
          
! if the current node is conjugated node of front, no need to account it in added nodes (buffer).
            if (trim(ADJUSTL(pr%prnode(i)%attribute))=='front') cycle main   
! in the case that the node is an old intersection point or base point with q=0, do not include it as new node projection. 
            if (q == 0) cycle main                               
! Number of the new nodes on right bank.
            l=l+1                                                
! The associated fe_node number to the new profile node on the overhang.
            buffer(l,1) = q                                      
! The distance of the new profile node on the overhang.
            buffer(l,2) = d                                      
! The elevation of the new profile node on the overhang is not the one on the extrapolation line but the one in fenode array, because the node may have been eroded due to the flooding.
            buffer(l,3) = fenode(q)%elevation                   

            if (abs(d-trans1) <= 0.001) FrontCounted = .TRUE.
            
         elseif ((d<trans1) .AND. (d>=trans2)) then   L2
!            pr%prnode(i)%fe_nodenumber = -u       ! 04.05.2009 11:10 .

            s = s + 1

! this if statement computes the case of crossing over old front.
CO2:         if (s==1) then                                      
! It should be noted that by the first entrance into this if statement, h1 and d1                                                                 
! have the values of the last node from previous if statement, which is the one before
              del_h1 = newwater_elev - h1         
! old front node and h2 = z, d2 =dis alreading defined over the loop: main.
              del_h2 = newwater_elev - h2         
! this area correspondes to the area between the last node before old front and old front itself
              sourceterm1 = 1./2. * (d1 - d2) * (del_h1 + del_h2)
!,( the area left to the old-front node).                                                                 
! sustitute the first area-computing node with old front node Distance
              d1 = trans1                                           
! sustitute the first area-computing node with old front node elevation
              h1 = z                                             
! sustitute the second area-computing node with the current node Distance
              d2 = pr%prnode(i)%distance          
! sustitute the second area-computing node with the elevation of lower edge of teh overhang
              h2 = h1                                            
! which are equal to the elevation of the old front. (since current node lies on upper edge of the overhang)                                                                 
              del_h1 = newwater_elev - h1
              del_h2 = del_h1
! the new area correspondes to the rea between the old front the node following it
              sourceterm1 = sourceterm1 + 1./2. * (d1 - d2) * (del_h1 + del_h2) 
! (the area right to the old front-node).The sum of righ and left to the old.front make the total                                                                                
! source term pssing to the Fe-node beneath the current profile's node.                                                                                
             Sumsource = Sumsource + sourceterm1
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------

!            if (front(2) == 0) then    
!              k= front(1)     
!          else      
!           k= front(2)        
!         end if       
   
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!  RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65            
              
!  sourceterm1 = del_h2 * RHOBS * 1000./(DELT)             
!         fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1     
!--------------------------------------------------------------------------------------------------------------------------------------
!cross over case
            else  CO2        

              d1 = d2
              h1 = h2
! Although h2 will remain constant = old front elevation, but to make the follow-up the program easier it will be repeated.
              d2 = pr%prnode(i)%distance         
! since the current profile's node lies on the upper edge of the overhang,
              h2 = z                                            
! but we need its correspong projecte node on the lower edge of the overhang,                                                                
! which are equal to the elevation of the old front's elevation.
              del_h1 = newwater_elev - h1        
              del_h2 = del_h1

! as a matter of fact Del_h1 and del_he are equal
              sourceterm =1./2. * (d1 - d2) * (del_h1 + del_h2) 
              Sumsource = Sumsource + sourceterm
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
! RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65             
              
! sourceterm = del_h2 * RHOBS * 1000./(DELT)              
!         fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm     
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO2
         
            if (TwistFlag) then
             s2 = s2 + 1
! assiagning the lost mass to the corresponding Fe- node as source term for suspension
             q = ABS(pr%prnode(i)%fe_nodenumber)    
            
! in the case that the node is an old intersection point or base point with q=0, do not include it as new node projection. 
             if (q == 0) cycle main                               
            
! Projection of the current profile's node on line element between new front and old front.
             h = newwater_elev - delz/deld * (x - d)   
             fenode(q)%elevation = h
! Number of the new nodes on left bank on extrapolation line.
             l=l+1                                                
! The associated fe_node number to the new profile node on the extrapolation line.
             buffer(l,1) = q                                      
! The distance of the new profile node on the extrapolation line.
             buffer(l,2) = d                                      
! The elevation of the new profile node on the overhang is not the one on the extrapolation line
             buffer(l,3) = fenode(q)%elevation                   
            
            end if

!
         ELSEIF (d < trans2) then  L2      

! number of nodes, which will be deleted in the new profile due to tensile failure
             s1 = s1 + 1                                         

! this if statement computes the case of crossing over potential nose.
 CO3:        if (s1==1) then                                     

! substitute the distance and elevation of the last node from the previous if statement
              d1 = d2                                            
! to d1 and h1. As a matter of fact h1 and h2 remoldain constant for d < front%distance.
              h1 = h2                                            
              d2 = trans2
              h2 = z

              del_h1 = newwater_elev - h1
              del_h2 = del_h1
! this area correspondes to the area between the last node before potential nose and potential nose itself
              sourceterm1 = 1./2. * (d1 - d2) * (del_h1 + del_h2)
!,( the area left to the potential nose node).                                                                 
! sustitute the first area-computing node with potential nose Distance
              d1 = trans2                              
! sustitute the second area-computing node with the current node Distance
              d2 = pr%prnode(i)%distance          
! d2 is equal to d, in fact it was not necessary to make a new variable 'd2'                                                                 
! from here on, the water elevation, will not be the refrence , but the elevation of upper edge of overhang
              del_h1 = newwater_elev - h1         
! should be subtracted from that of the lower edge , since the nodes following potential nose are fully submerged.
              del_h2 = pr%prnode(i)%elevation - h2 
! the new area correspondes to the area between the potential nose and the node following it
              sourceterm1 = sourceterm1 + 1./2. * (d1 - d2) * (del_h1 + del_h2) 
! (the area right to the potential nose).The sum of righ and left to the potential nose make the total                                                                                
! source term passing to the Fe-node beneath the current profile's node.                                                                                
              Sumsource = Sumsource + sourceterm1
              z1 = pr%prnode(i)%elevation
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!    RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65          
           
!     sourceterm1 = del_h2 * RHOBS * 1000./(DELT)          
!      fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1        
!--------------------------------------------------------------------------------------------------------------------------------------
!after cross over case
            else  CO3        

              d1 = d2
! h2 and h1 have the same value and they are located on th elower edge of the overhang.
              h1 = h2                                    
              d2 = pr%prnode(i)%distance

              del_h1 = z1 - h1
              del_h2 = pr%prnode(i)%elevation - h2

              sourceterm =1./2. * (d1 - d2) * (del_h1 + del_h2)
              Sumsource = Sumsource + sourceterm
              z1 = pr%prnode(i)%elevation
! to test if q1 = q in the program so that the findnode algorithm is omitted later.
              q1 = ABS(pr%prnode(i)%fe_nodenumber)                 
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
! HN. 11June2009. Computation of source volume directly assigned to the nodes below it. 
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
!   RHOBS=1000.*(1-porosity)* SGSAND (q_fenode(q1))  ![kg/m^3], i.e. SGSAND = 2.65           
             
!   sourceterm = del_h2 * RHOBS * 1000./(DELT)            
!   fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm           
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO3

         end if     L2

      end if   LR

     end do  main

! minus 1 since the overhang's old nose is counted in l.
ALLOCATE (newnodes(l),stat=STATUS)  
if (status > 0) then
 write (*,*)' the memory allocation for new nodes after tensile failure, was unsuccessful ...'
 pause
end if
! initalize the array newnodes

do i = 1,l
   newnodes(i)%fe_nodenumber = INT(buffer(i,1))
   newnodes(i)%distance = buffer(i,2)
   newnodes(i)%elevation = buffer(i,3)
   newnodes(i)%water_elev = fenode(newnodes(i)%fe_nodenumber)%water_level
   newnodes(i)%attribute = 'profile'
end do

! minus 1 since the overhang's old nose is counted in l.
 number_newnodes = l          
 lost_nodes = s1 + s2
!------------------------ Internal function ------------------------------
CONTAINS
INTEGER function q_fenode(conjugate_node)
! the function q is to compute the nearest FE node under the profile node, which has no conjugate FE-node beneath it..
INTEGER :: conjugate_node
INTEGER :: t, start, endd,inc
REAL (KIND = 8) :: a, b, c

 if (side == 'left') then
    b = pr%prnode(i)%distance
    c = 0.
    start = front(1)
    endd =  pr%max_nodes
    inc = 1

 ELSE IF (side == 'right') then
    b = pr%prnode(i)%distance
    c = pr%prnode(pr%max_nodes)%distance
    endd  = 1
    start = front(2) 
    inc   = -1
 end if
               if (conjugate_node == 0) then
findnod1:         do j= start,endd,inc

                    if (abs (pr%prnode(j)%distance-c) < abs(b-c) )  then
                       CYCLE findnod1
                    else
                       t=j
                       EXIT findnod1
                    end if

                  end do findnod1

                   q_fenode = ABS(pr%prnode(t)%fe_nodenumber)
               else
                   q_fenode = conjugate_node
               end if
end function q_fenode
!------------------- The end of the internal function --------------------------------

end subroutine tensile_failure
!------------------- The end of the explicit interface subroutine --------------------------------
END MODULE tensile
