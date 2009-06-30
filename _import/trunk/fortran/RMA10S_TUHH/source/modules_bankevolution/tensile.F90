!     Last change:  HN    9 Apr 2009    8:46 pm
! The following depicts left and right bank of a cross section in the river with existing two overhangs
! the Fe-node number corresponding to the profile nodes in overhang region are equal
! to the negative value of the corresponding fe-node number
! pr(i)%prnode(j)%Fe_nodenumber = - Fe_node%number
!      |<-	here is    the overhang zone   <-------------------------------------|
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
USE BLK10MOD  , ONLY : DELT
USE BLKSANMOD , ONLY : TRIBAREA, EXTLD, SGSAND

implicit none

TYPE(profile_node) , allocatable ,DIMENSION (:), save :: newnodes                   ! the save statment make the array available to the main program, since in Fortran95 allocatable arrays are automatically deallocated after ending the subroutine.

CONTAINS
subroutine tensile_failure(pr,fenode,side,newwater_elev, pot_nose_dist ,new_front,number_newnodes,lost_nodes)

 implicit none

 TYPE (finite_element_node), DIMENSION (:), intent (INOUT) :: fenode
 type ( profile )                         , intent (INOUT) :: pr
 Real (kind = 8), INTENT (IN) :: pot_nose_dist, newwater_elev
 CHARACTER (LEN=5), INTENT (IN) :: side

 TYPE (profile_node) , INTENT (OUT) :: new_front        ! is the distance of the new front to the origin of the profile
 integer , intent (OUT) :: number_newnodes, lost_nodes
 INTEGER , parameter :: DOUBL = 8
 INTEGER , DIMENSION (20,3) :: buffer    ! the variable holding THE fe_ node NUMBER, distance and elevation of THE new nodes projected ON extrapolation line.
 REAL (KIND=8) :: dis,z,z1,x,d,d1,d2,h,h1,h2,del_h1,del_h2, sourceterm,sourceterm1
 REAL (KIND=8) :: trans1, trans2          ! these two variables define the borders defined for the computation of the lost volume due to tensile failure.
 REAL (KIND=8) :: RHOBS
 real          :: radian
 
 INTEGER , dimension (2) :: front         ! this critical slope is consolidated unsaturated critical slope (unsubmerged area)
 INTEGER :: i,j,k,l,m,n,p,q,r,s,s1,t,u,b,bb,counter
 INTEGER :: status, q1


Write (*,*) ' Entering to Tensile failure Modelling ....'

 if (ALLOCATED(newnodes)) then               ! To make the array available for the the next call, make it deallocated if it is allocated.
  deallocate (newnodes)
 end if
 ! the profile's node number corresponding to fronts (left and/or) right , if available, if not, they are equal to zero.
 front(1) = pr%Lfront
 front(2) = pr%Rfront
!*******************************************************************************************************************************
! In the following if-clause the local coordiante of the new front is computed, where elevation is equal to the water elevation*
! and the only unkwon is the distance to the origin of the profile. That is here the variable x.                               *
!*******************************************************************************************************************************
 if (side =='left') then

    dis = pr%prnode(front(1))%distance                        ! the distance component of the old front
    z = pr%prnode(front(1))%elevation
    x = dis - 1./TAN(radian(critical_slope)) * (newwater_elev - z)                   ! the distance component of the new front
    m = 1
    p = 1
    n = pr%Lnose                                                     ! m, P and n are starting,increment and ending index of the the next loop,respectively.

  ! HN06March09: correction for probable relative positions of potential nose and old front.

     if (dis < pot_nose_dist) then                                          ! if the old front distance is smaller than potential nose distance
       trans1 = dis
       trans2 = pot_nose_dist
     ELSEIF (dis > pot_nose_dist) then                                      ! if the old front distance is greater than potential nose distance
       trans1 = pot_nose_dist
       trans2 = dis
     ELSEIF (ABS(dis - pot_nose_dist)< 0.001) then                          ! if the old front distance is equal to the potential nose distance
       trans1 = dis
       trans2 = trans1
     end if

 elseif (side =='right') then
! if the input is side = right , the following if statemnet is not needed, since
! it is claer that a front is existing, and front(2) is equal to the front of right %Rfront
 !    if (front(2)==0 ) then                                  ! if there is no second front. then the first front is either right or left, if side=right then
                                                            ! the first front is right. However if there are two fronts then the secnod one is right.
  !     dis = pr%prnode(front(1))%distnace                        ! the distance component of the old front
   !    z = pr%prnode(front(1))%elevation
    !   x = dis - 1./TAN(critical_slope) * (newwater_elev - z)    ! the distance component of the new front
     !else

       dis = pr%prnode(front(2))%distance
       z   = pr%prnode(front(2))%elevation
       x   = dis + 1./TAN(radian(critical_slope)) * (newwater_elev - z)  ! the distance component of the new front
       m = pr%max_nodes
       p = -1
       n = pr%Rnose

     if (dis < pot_nose_dist) then                                          ! if the old front distance is smaller than potential nose distance
       trans1 = pot_nose_dist
       trans2 = dis
     ELSEIF (dis > pot_nose_dist) then                                      ! if the old front distance is greater than potential nose distance
       trans1 = dis
       trans2 = pot_nose_dist
     ELSEIF (ABS(dis - pot_nose_dist)< 0.001) then                          ! if the old front distance is equal to the potential nose distance
       trans1 = dis
       trans2 = trans1
     end if


 end if

 new_front%fe_nodenumber = 0                               ! at the beginning it is assumed that there is no conjugate fe_node, however, it will be later determined if it is correct or not.
 new_front%distance = x
! new_front%elevation = pr%water_elev
 new_front%elevation =  newwater_elev
 new_front%water_elev = newwater_elev
 !new_front%water_elev = pr%water_elev
 new_front%attribute = 'front'
!*******************************************************************************************************************************
! Initialization of counters, arrays and varibles.
s = 0
s1 = 0
d1 = x
h1 = newwater_elev
d2 = dis
h2 = z                 ! elevation of the old front
t = 0
q = 0
k = 0
l = 0
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
!?????????????? check in the following, if there were no node available between d=x upto d = dis or d= pot_nose_dis or even d= nose
! the code works properly???????????????????????????????????
!HN0309.  improved for the case where q = 0, meaning that there is no fe node associated to the profile node (for the case of front ad nose
main: do i = m,n,p

  d= pr%prnode(i)%distance
  u= ABS(pr%prnode(i)%fe_nodenumber)
LR:   if (side =='left') then

        if (ABS(d-x)<0.001) then                                  ! if the current node distance coincides with new front, then assign the fe_node number to it.
           new_front%fe_nodenumber= u
           pr%prnode(i)%fe_nodenumber = -u        ! although those nodes in overhang zone (above extrapolation line) have already, in avalanche subroutine, the value of -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
        end if

 L1:     if ((d > x).and.(d<= trans1)) then                    ! when the profile nodes are located between new_front and old front
            pr%prnode(i)%fe_nodenumber = -u     ! The fe node number of the profile nodes, on the top of the profile, turns to negative signalising its projection on extrapolation line(under water).
            h = newwater_elev - TAN(radian(critical_slope))* (d - x)   ! Projection of the current profile's node on extrapolation line (the elevation), The equation is based on new front (left)
                                                               ! this elevation is only to be used for calculation of mass lost (area loss) in overhang zone.
                                                               ! the actual elevation of the profile node is equal to that of fenode, which has been computed by Exner, which may have not be identical with that of the extrapolation line (which is the original initial position).
!start computing the lost mass through tensile failure.
            del_h1 = newwater_elev - h1            ! since the elevation of the profile nodes on lower edge are all the same and equal to front(1).
            del_h2 = newwater_elev - h

            sourceterm =1./2. * (d - d1) * (del_h1 + del_h2)
            d1 = pr%prnode(i)%distance
            h1 = h

! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [mg/m^2.s].
! since the node which receives the mass is directly beneath profile node therefore the TRIBAREA (which is the contributing area of the elements connected to each node)
! is the same for  denoting node(source) as well as receiving node.Hence the ratio of this two areas is equal to 1.           
          
            sourceterm = del_h2 * RHOBS *1000./(DELT*3600.)
            
            q = ABS(pr%prnode(i)%fe_nodenumber)    ! assiagning the lost mass to the corresponding Fe- node as source term for suspension
            fenode(q)%sed_source = sourceterm

            l=l+1                                                ! Number of the new nodes on left bank on extrapolation line.
            buffer(l,1) = q                                      ! The associated fe_node number to the new profile node on the extrapolation line.
            buffer(l,2) = d                                      ! The distance of the new profile node on the extrapolation line.
            buffer(l,3) = fenode(q)%elevation                   ! The elevation of the new profile node on the overhang is not the one on the extrapolation line
                                                                 ! but the one in fenode array, because the node might have been eroded due to the erosion resulting from flooding.
         elseif ((d> trans1).and.(d<=trans2)) then   L1
!            pr%prnode(i)%fe_nodenumber = -u       ! 04.05.2009 11:10 .
            s = s + 1

CO:         if (s==1) then                                       ! this if statement computes the case of crossing over old front.
                                                                 ! It should be noted that by the first entrance into this if statement, h1 and d1
              del_h1 = newwater_elev - h1                        ! have the values of the last node from previous if statement, which is the one before
              del_h2 = newwater_elev - h2                        ! old front node or old fornt node itself, and h2 = z, d2 =dis alreading defined over the loop: main.
              sourceterm1 = 1./2. * (d2 - d1) * (del_h1 + del_h2)! this area correspondes to the area between the last node before old front and old front itself
                                                                 !,( the area left to the old-front node).
              d1 = trans1                                           ! sustitute the first area-computing node with old front node Distance
              h1 = z                                             ! sustitute the first area-computing node with old front node elevation
              d2 = pr%prnode(i)%distance          ! sustitute the second area-computing node with the current node Distance
              h2 = pr%prnode(front(1))%elevation  ! sustitute the second area-computing node with the elevation of lower edge of teh overhang
                                                                 ! which are equal to the elevation of the old front. (since current node lies on upper edge of the overhang)
              del_h1 = newwater_elev - h1
              del_h2 = newwater_elev - h2
              sourceterm1 = sourceterm1 + 1./2. * (d2 - d1) * (del_h1 + del_h2) ! the new area correspondes to the area between the old front the node following it
                                                                                ! (the area right to the old front-node).The sum of righ and left to the old.front make the total
                                                                                ! source term pssing to the Fe-node beneath the current profile's node.
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1
!--------------------------------------------------------------------------------------------------------------------------------------
            else  CO        !cross over case

              d1 = d2
              h1 = h2
              d2 = pr%prnode(i)%distance         ! Although h2 will remain constant = old front elevation, but to make the follow-up the program easier it will be repeated.
              h2 = pr%prnode(front(1))%elevation ! since the current profile's node lies on the upper edge of the overhang,
                                                                ! but we need its correspong image node on the lower edge of the overhang,
              del_h1 = newwater_elev - h1        ! which are equal to the elevation of the old front's elevation.
              del_h2 = newwater_elev - h2

              sourceterm =1./2. * (d2 - d1) * (del_h1 + del_h2) ! as a matter of fact Del_h1 and del_he are equal
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO

         ELSEIF (d > trans2) then  L1

             s1 = s1 + 1                                         ! number of nodes, which will be deleted in the new profile due to tensile failure

 CO1:        if (s1==1) then                                     ! this if statement computes the case of crossing over potential nose.

              d1 = d2                                            ! substitute the distance and elevation of the last node from the previous if statement
              h1 = h2                                            ! to d1 and h1. As a matter of fact h1 and h2 remain constant for d > front%distance.
              d2 = trans2
              h2 = pr%prnode(front(1))%elevation

              del_h1 = newwater_elev - h1
              del_h2 = newwater_elev - h2
              sourceterm1 = 1./2. * (d2 - d1) * (del_h1 + del_h2)! this area correspondes to the area between the last node before potential nose and potential nose itself
                                                                 !,( the area left to the potential nose node).
              d1 = trans2                              ! sustitute the first area-computing node with potential nose Distance
              d2 = pr%prnode(i)%distance          ! sustitute the second area-computing node with the current node Distance
                                                                 ! d2 is equal to d, in fact it was not necessary to make a new variable 'd2'
              del_h1 = newwater_elev - h1         ! from here on, the water elevation, will not be the refrence , but the elevation of upper edge of overhang
              del_h2 = pr%prnode(i)%elevation - h2 ! should be subtracted from that of the lower edge , since the nodes following potential nose are fully submerged.
              sourceterm1 = sourceterm1 + 1./2. * (d2 - d1) * (del_h1 + del_h2) ! the new area correspondes to the area between the potential nose and the node following it
                                                                                ! (the area right to the potential nose).The sum of righ and left to the potential nose make the total
                                                                                ! source term passing to the Fe-node beneath the current profile's node.
              z1 = pr%prnode(i)%elevation
              q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1
!--------------------------------------------------------------------------------------------------------------------------------------
            else  CO1        !after cross over case

              d1 = d2
              h1 = h2                                    ! h2 and h1 have the same value and they are located on the lower edge of the overhang.
              d2 = pr%prnode(i)%distance

              del_h1 = z1 - h1
              del_h2 = pr%prnode(i)%elevation - h2

              sourceterm =1./2. * (d2 - d1) * (del_h1 + del_h2)

              z1 = pr%prnode(i)%elevation
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO1

         end if     L1
!**************    End of compuatation of tensile failure for the left bank   ********************************!

!**************   Start of compuatation of tensile failure for the right bank   ********************************!

      else    LR         ! the right overhang
           if (ABS(d-x)<0.001) then
!             u= ABS(pr%prnode(i)%fe_nodenumber)    ! it is computed in the main loop!
             new_front%fe_nodenumber= u
             pr%prnode(i)%fe_nodenumber = -u        ! although those nodes in overhang zone (above extrapolation line) have already in avalanche subroutine -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
           end if

L2:        if ((d<x).and.(d>=trans1)) then
            pr%prnode(i)%fe_nodenumber = -u     ! The fe node number of the profile nodes, on the top of the profile, turns to negative signalising its projection on extrapolation line(under water).
            h = z + TAN(radian(critical_slope)) * (d - dis)   				       ! Image of the current profile's node on extrapolation line (the elevation). The equation is based on old front (right).
            del_h1= newwater_elev - h1            ! since the elevation of the profile nodes on lower edge are all the same and equal to front(1).
            del_h2 = newwater_elev - h

            sourceterm =1./2. * (d1 - d) * (del_h1 + del_h2)
            d1 = pr%prnode(i)%distance
            h1 = h

            q = ABS(pr%prnode(i)%fe_nodenumber)         ! assiagning the lost mass to the corresponding Fe- node as source term for suspension
    
    ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
    ! in the following it is directly assigned to the nodes below overhang. The source term is in [mg/m^2.s]           
            RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
            sourceterm = del_h2 * RHOBS * 1000./DELT
            !sourceterm = del_h2 * RHOBS * 1000./(DELT*3600.)
    
            fenode(q)%sed_source = sourceterm

            l=l+1                                                ! Number of the new nodes on right bank.
            buffer(l,1) = q                                      ! The associated fe_node number to the new profile node on the overhang.
            buffer(l,2) = d                                      ! The distance of the new profile node on the overhang.
            buffer(l,3) = fenode(q)%elevation                   ! The elevation of the new profile node on the overhang is not the one on the extrapolation line but the one in fenode array, because the node may have been eroded due to the flooding.


         elseif ((d<trans1).and.(d>=trans2)) then   L2
!            pr%prnode(i)%fe_nodenumber = -u       ! 04.05.2009 11:10 .

            s = s + 1

CO2:         if (s==1) then                                      ! this if statement computes the case of crossing over old front.
                                                                 ! It should be noted that by the first entrance into this if statement, h1 and d1
              del_h1 = newwater_elev - h1         ! have the values of the last node from previous if statement, which is the one before
              del_h2 = newwater_elev - h2         ! old front node and h2 = z, d2 =dis alreading defined over the loop: main.
              sourceterm1 = 1./2. * (d1 - d2) * (del_h1 + del_h2)! this area correspondes to the area between the last node before old front and old front itself
                                                                 !,( the area left to the old-front node).
              d1 = trans1                                           ! sustitute the first area-computing node with old front node Distance
              h1 = z                                             ! sustitute the first area-computing node with old front node elevation
              d2 = pr%prnode(i)%distance          ! sustitute the second area-computing node with the current node Distance
              h2 = h1                                            ! sustitute the second area-computing node with the elevation of lower edge of teh overhang
                                                                 ! which are equal to the elevation of the old front. (since current node lies on upper edge of the overhang)
              del_h1 = newwater_elev - h1
              del_h2 = del_h1
              sourceterm1 = sourceterm1 + 1./2. * (d1 - d2) * (del_h1 + del_h2) ! the new area correspondes to the rea between the old front the node following it
                                                                                ! (the area right to the old front-node).The sum of righ and left to the old.front make the total
                                                                                ! source term pssing to the Fe-node beneath the current profile's node.

             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
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
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1
!--------------------------------------------------------------------------------------------------------------------------------------
            else  CO2        !cross over case

              d1 = d2
              h1 = h2
              d2 = pr%prnode(i)%distance         ! Although h2 will remain constant = old front elevation, but to make the follow-up the program easier it will be repeated.
              h2 = z                                            ! since the current profile's node lies on the upper edge of the overhang,
                                                                ! but we need its correspong projecte node on the lower edge of the overhang,
              del_h1 = newwater_elev - h1        ! which are equal to the elevation of the old front's elevation.
              del_h2 = del_h1

              sourceterm =1./2. * (d1 - d2) * (del_h1 + del_h2) ! as a matter of fact Del_h1 and del_he are equal
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO2

         ELSEIF (d < trans2) then  L2      !

             s1 = s1 + 1                                         ! number of nodes, which will be deleted in the new profile due to tensile failure

 CO3:        if (s1==1) then                                     ! this if statement computes the case of crossing over potential nose.

              d1 = d2                                            ! substitute the distance and elevation of the last node from the previous if statement
              h1 = h2                                            ! to d1 and h1. As a matter of fact h1 and h2 remoldain constant for d < front%distance.
              d2 = trans2
              h2 = z

              del_h1 = newwater_elev - h1
              del_h2 = del_h1
              sourceterm1 = 1./2. * (d1 - d2) * (del_h1 + del_h2)! this area correspondes to the area between the last node before potential nose and potential nose itself
                                                                 !,( the area left to the potential nose node).
              d1 = trans2                              ! sustitute the first area-computing node with potential nose Distance
              d2 = pr%prnode(i)%distance          ! sustitute the second area-computing node with the current node Distance
                                                                 ! d2 is equal to d, in fact it was not necessary to make a new variable 'd2'
              del_h1 = newwater_elev - h1         ! from here on, the water elevation, will not be the refrence , but the elevation of upper edge of overhang
              del_h2 = pr%prnode(i)%elevation - h2 ! should be subtracted from that of the lower edge , since the nodes following potential nose are fully submerged.
              sourceterm1 = sourceterm1 + 1./2. * (d1 - d2) * (del_h1 + del_h2) ! the new area correspondes to the area between the potential nose and the node following it
                                                                                ! (the area right to the potential nose).The sum of righ and left to the potential nose make the total
                                                                                ! source term passing to the Fe-node beneath the current profile's node.
              z1 = pr%prnode(i)%elevation
             q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the current profile's node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm1
!--------------------------------------------------------------------------------------------------------------------------------------
            else  CO3        !after cross over case

              d1 = d2
              h1 = h2                                    ! h2 and h1 have the same value and they are located on th elower edge of the overhang.
              d2 = pr%prnode(i)%distance

              del_h1 = z1 - h1
              del_h2 = pr%prnode(i)%elevation - h2

              sourceterm =1./2. * (d1 - d2) * (del_h1 + del_h2)

              z1 = pr%prnode(i)%elevation
              q1 = ABS(pr%prnode(i)%fe_nodenumber)                 ! to test if q1 = q in the program so that the findnode algorithm is omitted later.
! finding the corresponding Fe-node, which receives the wasted mass as source term, this node lies directly beneath the profile node.
! in the case that q1 = 0, the INTERNAL FUNCTION q compute this FE node.
!--------------------------------------------------------------------------------------------------------------------------------------
 ! HN. 11June2009. Computation of source volume directly assigned to the nodes below it.
! in the following it is directly assigned to the nodes below overhang. The source term is in [g/m^2.s]           
              RHOBS=1000.*(1-porosity)* SGSAND (q1)  ![kg/m^3], i.e. SGSAND = 2.65
              !sourceterm1 = del_h2 * RHOBS * 1000./(DELT*3600.)
               sourceterm1 = del_h2 * RHOBS * 1000./(DELT)
              fenode(q_fenode(q1))%sed_source = fenode(q_fenode(q1))%sed_source + sourceterm
!--------------------------------------------------------------------------------------------------------------------------------------
            end if  CO3

         end if     L2

      end if   LR

     end do  main

ALLOCATE (newnodes(l),stat=STATUS)  ! minus 1 since the overhang's old nose is counted in l.
if (status > 0) then
 write (*,*)' the memory allocation for new nodes after tensile failure, was unsuccessful ...'
 pause
end if

do i = 1,l
   newnodes(i)%fe_nodenumber = buffer(i,1)
   newnodes(i)%distance = buffer(i,2)
   newnodes(i)%elevation = buffer(i,3)
   newnodes(i)%water_elev = fenode(newnodes(i)%fe_nodenumber)%water_level
   newnodes(i)%attribute = 'profile'
end do
number_newnodes = l                      ! minus 1 since the overhang's old nose is counted in l.
lost_nodes = s1
!------------------------ Internal function ------------------------------
CONTAINS
INTEGER function q_fenode(conjugate_node)
! the function q is to compute the nearest FE node under the profile node, which has no conjugate FE-node beneath it..
INTEGER :: conjugate_node
INTEGER :: t, start, endd,inc
REAL (KIND = 8) :: a, b

 if (side == 'left') then
    a = d2
    b= pr%prnode(i)%distance
    start = front(1)
    endd =  pr%max_nodes
    inc = 1
 ELSE IF (side == 'right') then
    a =pr%prnode(i)%distance
    b  =d2
    start = pr%max_nodes
    endd =  front(2)
    inc = -1
 end if
               if (conjugate_node == 0) then
findnod1:         do j= start,endd,inc

                    if (a < b)  then
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
