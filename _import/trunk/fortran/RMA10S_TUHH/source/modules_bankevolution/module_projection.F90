!     Last change:  HN    9 Apr 2009    8:46 pm
! This program is particularly written for simulation of progression of undercutting in avalanche algorithm,
! in which the profile nodes between old and new Front are projected on an extrapolation line with unsaturated angle of repose
! and the new profile is sorted from bank-toe to the top of the bank in a temporary arry "newprofile". The Fe nodes are projected
! on the  extension of the same line between new front and the top of the bank.  The program works for
!  left as well as right bank.
! this program also works, for the case that new Front remains submerged (equal to the water elevation (the case z1 and z2 <= water elevation in subroutine Avalanche)
! To acheive this goal , an optional argument "critical_slope"(saturated angle of repose) has been added to th eargument list, so that in the mentioned case in Avalanche,
! this subroutine is called with the optional argument, and the probable new nodes are projected on the saturated angle of repose.

module projectn

CONTAINS

subroutine projection(fenode,pointA_distance,pointB,angle,start,endd,newprofile,current_index,side,oldprofile,critical_slop,nose,&
                     & front,oldpr_index , lost)

USE types
USE share_profile
use param

implicit none

TYPE (finite_element_node) , DIMENSION (:), INTENT (INOUT):: fenode
TYPE (profile)     , INTENT (INOUT)        :: newprofile               ! newprofile is the profile storing projected nodes
TYPE (profile)     , INTENT (INOUT)        :: oldprofile
INTEGER            , INTENT (INOUT)        :: current_index            ! is the starting or current index of the profile node in newprofile.
REAL (kind = 8)    , INTENT (IN)           :: pointA_distance
TYPE (profile_node), INTENT (IN), OPTIONAL :: nose
TYPE (profile_node), INTENT (IN)           :: pointB             ! pointA is for example old front , or in the case of first time generating overhang it is the new nose.
                                                                 ! pointB is for example new front. In this case pointA is the lower point and pointB is the higher point on an extrapolation line with angle of "angle".
                                                                 ! However , in the case of first time generating overhang, PointA and pointB have the same elevation.
REAL               , INTENT (IN)           :: angle              ! angle is the unsaturated angle of repose IN RADIAN
REAL               , INTENT (IN), OPTIONAL :: critical_slop     ! it is the saturated angle of repose. It is for the case, where the FE and Profile nodes should be projected on the submerged area
                                                                 ! the case where the new front remains submerged = Water level
INTEGER            , INTENT (IN)           :: start,endd, side   ! side is equal j in main program and sdie = 1 is left side and equal 2 is right side.
LOGICAL            , INTENT (IN), OPTIONAL :: front

REAL (KIND = 8)    , INTENT (out),OPTIONAL :: lost               ! the lost area due to failure of the bank over water surface as a result of multiple undercutting assumption.
LOGICAL            , INTENT (IN),OPTIONAL  :: oldpr_index         ! 

REAL(KIND = 8)                             :: a,b,c,d,h, z
REAL(KIND = 8)                             :: origin, rel_dd, rel_d, rel_dist_oldfront
REAL(KIND = 8)                             :: dd, elev, del_d1, del_d2, rel_b
REAL(KIND = 8)                             :: SLOP, deltaElev, Ref_height, Sumvolume, Eff_area

INTEGER                                    :: r,j,u,i,m, en,p,t,s,ind, begin, save_index, k
LOGICAL                                    :: flag, flag_newfront , intsec, NewFrontCoBase 

! dont forget to update corresponding fe_nodes (conjugate fe-nodes)
!----------------------  SORTING ALGORITHM AND PROJECTION OF NEW NODES ON EXTRAPOLATION LINE ------------------------------------------

save_index = current_index
! initialize 
elev = 0.
if (present(lost) ) lost = 0.

if (present (front)) then
   flag = front
else
   flag = .FALSE.
end if

flag_newfront  = .FALSE.
NewFrontCoBase = .FALSE.
intsec         = .FALSE.

r = (-1)** side                            ! for left bank, side= 1 then r = -1 and for right bank side = 2 and r = 1
j = 0

      if (side == 1) then                  ! for left bank
         p      = oldprofile%Lfront
         origin = oldprofile%prnode(1)%distance

          IF (start == endd) then
             en = 1                        ! for the case that endd = lfront and start is also lfront. It is the case where,
          else                             ! the existing front is eroded due to the avalanche.
             en = endd
          END if

      ELSE IF (side == 2) then              ! for right bank

         p      = oldprofile%Rfront
         origin = oldprofile%prnode(oldprofile%max_nodes)%distance

          IF (start == endd) then
             en = oldprofile%max_nodes     ! for the case that endd = rfront and start is also rfront. It is the case where, the existing front is eroded due to the avalanche.
          else
             en = endd
          END if

      end if

         b = ABS(pointB%distance - origin)                        ! pointB is new front with higher elevation than pointA (lower front=old_front)
         c = ABS(pointA_distance - origin)

 ! calculation of the new front under condition of undercutting the old front by the basepoint.
      if (present (oldpr_index)) then

        dd = pointB%distance + r / TAN(angle)* abs(oldprofile%prnode(p)%elevation - pointB%elevation)      ! distance coordiante of the new front resulting form undercutting below overhang.
                if (dd < 0.) then
                  write (*,*)
                  write (*,*) ' WARNING , Module Projection...'
                  write (*,*) ' The distance coordinate of the new undercutting front is negative in ContLine: ',oldprofile%CL_number
                  pause
                 end if  

        
        rel_dd = ABS(dd- origin)
        rel_dist_oldfront = ABS(oldprofile%prnode(p)%distance - origin)                             ! relativ distance of the old front with respect to the origin.
       
        if (abs( rel_dd - b) <= 0.001 )  NewFrontCoBase = .TRUE.                                   ! Singalaize that newfront coincides with base, It means water level is equal to the front elevation.
 
 ! check if the computed new front undrcuts the old one or lies on the other side of the front. In the latter case, it meeans the
 ! extrapolation line meets the profile of the bank below the old front. This case, introduces a new profile node on intersection point
 ! which should be correctly sorted into the new profile.

        if( (rel_dd - rel_dist_oldfront )> 0.01 )then                     ! it means the extrapolation line from base point intersects with the profile before reaching to the elevation of the old front.
           call intersect(oldprofile, pointB,angle/pi*180.,start,p,r,dd,elev,ind, 0.001)                            ! therefore, the intersection point should be computed and substituted with dd.
           rel_dd = ABS(dd- origin)
           intsec = .TRUE.
        elseif ( ABS(rel_dd - rel_dist_oldfront )<= 0.01) then
           rel_dd = rel_dist_oldfront   
           dd     = oldprofile%prnode(p)%distance 
        end if

            if (present (lost).AND. (abs (oldprofile%prnode(p)%elevation - oldprofile%prnode(p)%water_elev)> 0.01)) then  ! if it is required the lost area be computed and the water level is not equal to front elevation.
                                                                                                                         ! since the lost area below water level is computed always in avalanche subroutine.
               del_d1 = ABS(pointB%distance - pointA_distance)       ! the base of trinagular made by potebtial nose(pointA) and new progressing front(pointB).

               select case (intsec)

                   case (.true.)
                    lost = del_d1 * (elev - pointB%elevation)/2.     ! the triangular area lost betweenpotential nose, front (pointB = basepoint) and intersection of extrapolation line with bank profile.

                   case default
                    del_d2 = ABS(oldprofile%prnode(p)%distance - dd)
                    elev = oldprofile%prnode(p)%elevation
                    lost = (del_d1 + del_d2) * (elev - pointB%elevation)/2.       ! the triangular area lost betweenpotential nose, front (pointB = basepoint) and intersection of extrapolation line with bank profile.

               end select

            end if

      end if
! ****************************************************************************************************
! The following do structure computes the projection of the upper profile only on the extrapolation
! line between old front/nose and new front/basepoint and sort these points in the newprofile array and adjust
! their conjugate fenode. The array newprofile sorts the profile nodes from bank-toe towards top
! of the bank. It means its last node is the first profile's node for the left profile and vice versa
! for the right bank.
! ****************************************************************************************************

!------------------------  projection between old front (or nose) and new front ----------------------------

proj:do i = start + r, en ,r

       d     = oldprofile%prnode(i)%distance
       rel_d = ABS(d - origin)                           ! distance of the current point to the origin (left or right end)
      
       u     = oldprofile%prnode(i)%fe_nodenumber
       
 l1:    IF ( ABS(rel_d - b) <= 0.001 ) THEN              ! check if the new front(or basepoint) coincides with a profile node(in the case of basepoint, it can be any node even old front)
                                                         ! on the upper profile, resulting to have a conjugate FE node (in the case old front is projecting and it has no fe_conjugate, the new
                                                         ! just generated node will have also no conjugate fe_node.
           if (flag) cycle proj                          ! in the case that the current node has a conjugate in the overhnag and has been projected already once on base point(usuallay it happens when the current node is Front). 
                                                         ! then igonre th eprojection.
           j = j +1                                      
                                                         
           flag = .TRUE.                                 ! signaling that front(or basepoint) has been already added to the new profile(important for sorting the nodes).
           oldprofile%prnode(i)%fe_nodenumber   = - abs(u)    ! although those nodes in overhang zone (above extrapolation line) have already in avalanche subroutine -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
           newprofile%prnode(current_index + j) = pointB ! projection /equating pointB as a new node in newprofile.
           newprofile%prnode(current_index + j)%fe_nodenumber = abs(u)
           newprofile%prnode(current_index + j)%distance  =  oldprofile%prnode(i)%distance   !30APRIL2009;17:57  IT IS DONE TO ENSURE THAT THE COMPUTED DISTANCE IS EXACTLY EQUAL TO THAT OF THE CONICDED PROFILE NODE TO PROHIBIT ERROR IN SUBROUTINE SIMPLE_PROJECTION IN SUBROUTINE CANTILEVER.
           oldprofile%prnode(i)%attribute = 'overhang'
           
            if (u>0) then
             fenode(u)%elevation = pointB%elevation       ! update the fenode's elevation accordingly.
             fenode(u)%statuss = 'deactivate'             ! reset the fe_ndoe status for fututre computation in Exner subroutine and evaluation in Bankevolution subroutine.
            end if

             if ((trim(adjustL(pointB%attribute)) == 'front').AND. (.NOT. PRESENT (oldpr_index) ) ) then          ! if the pointB is the new front and it is not going to be undercut by base point.
               if (side == 1) then
                 newprofile%Lfront = current_index + j
               ELSE IF (side == 2) THEN
                 newprofile%Rfront = current_index + j
               end if
             end if
          
             if ((trim(adjustL(pointB%attribute)) == 'basepoint').AND. (NewFrontCoBase ) ) then          ! if the pointB is the base and coincides with new front. 
               newprofile%prnode(current_index + j)%attribute = 'front'
               flag_newfront= .TRUE.
               
               if (side == 1) then
                 newprofile%Lfront = current_index + j
               ELSE IF (side == 2) THEN
                 newprofile%Rfront = current_index + j
               end if
             end if
          

        ELSE IF ( ( ABS (rel_d - c) <=0.001 ) .AND. (trim(oldprofile%prnode(i)%attribute) == 'front') ) THEN    l1       ! in the case that current profile node is an old front and coincides with potential nose (wsl1[water surface] = front's elevation).
                                                                                                          ! otherwise, rel_d = c has been already considered in Avalanche module.
           if (u>0) then
            j = j +1

            oldprofile%prnode(i)%fe_nodenumber = 0                       ! The old front will be deleted from the old profile by putting its fenumber to zero (it will be ignored in the rest of program = deleted)
            oldprofile%prnode(i)%attribute = 'overhang'

            newprofile%prnode(current_index + j) = oldprofile%prnode(i)
            newprofile%prnode(current_index + j)%fe_nodenumber = abs(u)       ! to make sure that a positive value is assaigned to new profile's Fe-nodenumber.
            newprofile%prnode(current_index + j)%attribute = 'profile'
 
            z = pointB%elevation + r * TAN(critical_slop)* (d - pointB%distance)
            newprofile%prnode(current_index + j)%elevation = z
            fenode(u)%elevation = z
            fenode(u)%statuss = 'deactivate'                             ! reset the fe_ndoe status for fututre computation in Exner subroutine and evaluation in Bankevolution subroutine.
           end if

     !   else if ( (rel_d > b).and.(rel_d < c).AND. (u>0) ) then   l1      ! when the profile nodes are located between new_front/nose and old front/basepoint and have not been already projected (u>0, u=0 will not be projected)
     !       j = j +1                                                     ! if rel_d = c has been already considered in Avalanche module, in case it coincides with a profile node.
        ELSE IF ( (rel_d > b).and.(rel_d < c) ) THEN   l1 
           
            oldprofile%prnode(i)%fe_nodenumber = -abs(u)                      ! although those nodes in overhang zone (above extrapolation line) have already in avalanche subroutine -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
            oldprofile%prnode(i)%attribute = 'overhang'
           
           if (u>0) then
            j = j +1
            newprofile%prnode(current_index + j) = oldprofile%prnode(i)
            newprofile%prnode(current_index + j)%fe_nodenumber = abs(u)       ! to make sure that a positive value is assaigned to new profile's Fe-nodenumber.
            newprofile%prnode(current_index + j)%attribute = 'profile'

             if (present (critical_slop)) then
               z = pointB%elevation + r * TAN(critical_slop)* (d - pointB%distance)   ! Projection of the current profile's node on extrapolation line-saturated slope (the elevation), The equation is based on new front (left)
             else
               z = pointB%elevation + r * TAN(angle)* (d - pointB%distance)            ! Projection of the current profile's node on extrapolation line-unsaturated slope (the elevation), The equation is based on new front (left)
             end if

            newprofile%prnode(current_index + j)%elevation = z
            fenode(u)%elevation = z
            fenode(u)%statuss = 'deactivate'                            ! reset the fe_ndoe status for fututre computation in Exner subroutine and evaluation in Bankevolution subroutine.
           end if
           
        ELSE IF (rel_d < b) THEN       l1

prs:       if ((PRESENT(oldpr_index)).AND.(rel_d-rel_dd>0.001))  then    ! the point is between basepoint and the new front.

             if (.not. flag) then                                 ! if none of the profile nodes coincides with the basepoint, then include the base point seperately
              j = j + 1                                           ! in the temporary sorting array newprofile.
              newprofile%prnode(current_index + j) = pointB       ! the new basepoint has then no conjugate Fe-node.
              flag = .TRUE.
             ENDIF
             
             if (u>0) then                                         ! 04.05.2009 17:39  . To prevent the nodes in overhangs with negative Fe_nodenumbers to be re-projected on the extrapolation line beyond basepoint.
              j = j + 1
              newprofile%prnode(current_index + j) = oldprofile%prnode(i)
             
               if (.NOT.intsec) then
                oldprofile%prnode(i)%fe_nodenumber = -ABS(u)
                oldprofile.prnode(i).attribute = 'overhang'

                newprofile%prnode(current_index + j)%fe_nodenumber = ABS(u)               ! to make sure that a positive value is assaigned to new profile's Fe-nodenumber.
                newprofile%prnode(current_index + j)%attribute = 'profile'
                z = pointB%elevation + ((-1)**side) * TAN(angle)* (d - pointB%distance)   ! Projection of the current profile's node on extrapolation line-unsaturated slope (the elevation), The equation is based on new front (left)
               else
                z = elev + (rel_d - rel_dd)/(b - rel_dd)* (pointB%elevation - elev)
               end if

              newprofile%prnode(current_index + j)%elevation = z

              fenode(u)%elevation = z
              fenode(u)%statuss = 'deactivate'                   ! reset the fe_ndoe status for fututre computation in Exner subroutine and evaluation in Bankevolution subroutine.
             
             elseif (u == 0.AND..NOT. intsec ) then
             
              oldprofile.prnode(i).attribute = 'overhang'
             
             end if

           else if ( ( PRESENT(oldpr_index) ) .AND. ( ABS (rel_d-rel_dd) <=0.001 ) )  then    prs

             IF ( flag_newfront) CYCLE proj
             j = j + 1
             newprofile%prnode(current_index + j) = oldprofile%prnode(i)
             flag_newfront= .TRUE.
             flag = .TRUE.                                                              ! It means: in the case that the point i is old front and coincides
                                                                                        ! with the new front then donot include base point anymore. 
    sec:     if (intsec) then

              newprofile%prnode(current_index + j)%attribute = 'profile'

             else             sec
   
              oldprofile%prnode(i)%fe_nodenumber = -ABS(u)
              oldprofile.prnode(i).attribute = 'overhang'

              newprofile%prnode(current_index + j)%fe_nodenumber = ABS(u)                 ! to make sure that a positive value is assaigned to new profile's Fe-nodenumber.
              newprofile%prnode(current_index + j)%elevation = oldprofile%prnode(p)%elevation    ! the new front's elevation is equal to the old fornt's elevation (p determines if it was on left or right bank)
  
  !           z = pointB%elevation + ((-1)**side) * TAN(angle)* (dd - pointB%distance)   ! Projection of the current profile's node on  point dd(new front) with unsaturated slope, The equation is based on new front (left)
   !          newprofile%prnode(current_index + j)%elevation = z
!             flag_newfront= .TRUE.

!    sec:     if (intsec) then
 !             newprofile%prnode(current_index + j)%attribute = 'profile'

  !           else             sec

              newprofile%prnode(current_index + j)%attribute = 'front'

               if (side == 1) then
                 newprofile%Lfront = current_index + j
               ELSE IF (side == 2) THEN
                 newprofile%Rfront = current_index + j
               end if

             end if   sec

           else        prs
            exit proj
           END if      prs
        
        END if l1

     END do  proj
!------------------------------- The end of projection between control points [old front/nose/potential nose and new front/front/basepoint] -----------------------------------------

!------------------------ Inclusion of the new front (or basepoint/intersection), in the case it is not already determined/included above   ---------------
        if ((.not.flag_newfront).AND. (PRESENT(oldpr_index)))  then
          j = j + 1
          newprofile%prnode(current_index + j)%distance = dd
          newprofile%prnode(current_index + j)%fe_nodenumber = 0
          newprofile%prnode(current_index + j)%water_elev = newprofile%water_elev  !?????

    sec2: if (intsec) then

           newprofile%prnode(current_index + j)%attribute = 'profile'
           newprofile%prnode(current_index + j)%elevation = elev

          else    sec2

           newprofile%prnode(current_index + j)%attribute = 'front'
           newprofile%prnode(current_index + j)%elevation = oldprofile%prnode(p)%elevation

            if (side == 1) then
              newprofile%Lfront = current_index + j
            ELSE IF (side == 2) THEN
              newprofile%Rfront = current_index + j
            end if

          ENDIF sec2

        ENDIF

        if ((.not.flag ).AND. (.NOT.PRESENT(oldpr_index)))  then  ! if none of the profile nodes coincides with the new front, then include the new front seperately
          j = j + 1                                               ! in the temporary sorting array newprofile.
          newprofile%prnode(current_index + j) = pointB           ! the new front has then no conjugate Fe-node

             if (trim(pointB%attribute) == 'front') then
               if (side == 1) then
                 newprofile%Lfront = current_index + j
               ELSE IF (side == 2) THEN
                 newprofile%Rfront = current_index + j
               end if
             end if

        end if

!--------------------------------------------------------------------------------------------------------------------------------------
! **********************************************************************************************************
! in the following do statement the rest of the profile nodes (overhang zone and the rest beyond new front)
! are sorted in the temporary newprofile array. So that this array ,which had already included the new
! nodes after avalanche from the bank-toe upto the old front after avalanche, now extend its content
! to include the profile nodes between old front to newfront and new front to the start or ending node
!  of the profile, for left and right bank respectively.  This means that the newprofile includes all
! profile nodes from bank-toe to top of the profile after computation of avalanche.
! **********************************************************************************************************
fr:if (trim(pointB%attribute) == 'front') then                               ! in the case of computation for a basepoint (a front apart from the first front), the following block is not executed.

    if ( ( present (nose) ) .AND.(trim(oldprofile%prnode(start)%attribute) =='overhang')) then   ! if the overhang is just being generated, and the nose is between two profile nodes
          j = j + 1                                                                 ! then put the nose as the next node after the new front.
          newprofile%prnode(current_index + j)= nose

             if (side == 1) then
              newprofile%Lnose = current_index + j
             ELSEIF (side == 2) THEN
              newprofile%Rnose = current_index + j
             end if

          j = j + 1
          newprofile%prnode(current_index + j) = oldprofile%prnode(start)            ! the old profile node i+p = start point, which is now in the overhang.
  
          if (oldprofile%prnode(start)%fe_nodenumber > 0 ) &                         ! in the case that the START node is beynod projection zone (between nose and front)
&            newprofile%prnode(current_index + j)%attribute = 'profile'              ! don't include it as an overhang node.
 !         if ( (ABS(oldprofile%prnode(start)%distance - origin) - ABS(pointB%distance - origin) )
    else if ((present (nose)).AND.(trim(oldprofile%prnode(start)%attribute) =='nose')) then    ! if the overhang is just being generated, and the node with the index "start" is the nose then assiagn it as a nose to the new profile.
          j = j + 1
          newprofile%prnode(current_index + j)= nose

             if (side == 1) then
              newprofile%Lnose = current_index + j
             ELSE IF (side == 2) THEN
              newprofile%Rnose = current_index + j
             end if
    end if
ENDif fr

!----------------------------- Start sorting the rest of the profile above front /nose (overhang and top of the profile)------------------------------------------------------------------

  if (present (oldpr_index) .and. (.not.intsec) ) then                ! the case that basepoint has been created, however there is no intersection of extrapolation line with the profile.

   if (side == 1) then        ! for left bank
     begin = oldprofile%lnose
   ELSE IF (side == 2) then    ! for right bank
     begin = oldprofile%Rnose
   end if

  ELSE IF ( present (oldpr_index) .and. (intsec) ) THEN                ! the case that basepoint has been created, however there is AN intersection of extrapolation line from this point with the profile below old front.

   begin = ind + r

  else
                                                                      ! for the case where no base point ( undercut below overhang) has occured/ created.
   begin = start + r

  end if

        do i = begin, endd ,r                         ! begin = start + r is the node after old front, which is nose and endd is the first node in oldprofile or last node for left and right banks, respectively.
          j = j + 1                                   ! (or in case of early phase of creation of overhang it is the node above nose,if start is nose, otherwise the second node after )
          newprofile%prnode(current_index + j) =  oldprofile%prnode(i)
          
           if ( (side == 1).and. (i == oldprofile%Lnose) ) then 
            newprofile%Lnose = current_index + j
           else if ((side == 2).and. (i == oldprofile%Rnose) ) then
            newprofile%Rnose = current_index + j
           end if 

          d = newprofile%prnode(current_index + j)%distance
          h = newprofile%prnode(current_index + j)%elevation

  sec3:   if (.not.intsec) then

            if (present (oldpr_index)) then
             z     = oldprofile%prnode(p)%elevation + r * TAN(angle)* (d - dd)         ! projection of the elevation of profile nodes on th eextrapolation line beyond new front, for updating FE nodes.
             rel_b = ABS (dd - origin)
            else
             z     = pointB%elevation + r * TAN(angle)* (d - pointB%distance)   ! Projection of the current profile's node on extrapolation line (the elevation) for updating FE nodes., The equation is based on new front (left)
             rel_b = ABS (pointB%distance - origin)
            ENDIF

            rel_d = abs ( d - origin)

            if ((h >= z).AND. ( (rel_d - rel_b)< -0.01 )) then    ! create new Fe-nodes on slip surface by projecting the profile nodes on extrapolation line between new front and top of the bank.
                                                        ! for the profile nodes above the projection line.
             u= ABS(oldprofile%prnode(i)%fe_nodenumber)
             newprofile%prnode(current_index + j)%fe_nodenumber = abs(u)        ! although those nodes in overhang zone (above extrapolation line) have already in avalanche subroutine -1 x fe_nodenumber as their conjugate fe_node, but to be on safe side, it is done here again.
             newprofile%prnode(current_index + j)%attribute     ='profile'
             if ( u /= 0 ) then
             fenode(u)%elevation = z
             fenode(u)%statuss = 'deactivate'
             end if
            end if

          end if   sec3

        end do


      current_index = current_index + j
      newprofile%max_nodes = current_index

! compute the failure volume due to undercutting by falling water level in the river (production and propagation of base point).      
!********************************************************************************************
! The following algorithm should be actived only in the case,  volume should be distributed
! based on connecting FE element's area to each node. Otherwise, the exact eroded area
! has been already calculated above and assigned to LOST!
!******************************************************************************************** 
   !if (present (lost).AND. (abs (oldprofile%prnode(p)%elevation - oldprofile%prnode(p)%water_elev)> 0.01)) then  ! if it is required the lost area be computed and the water level is not equal to front elevation.
 
 ! DEACTIVATE DUE TO SIMPLER ALGORITHM
!    if (present (lost)) then
!       Sumvolume = 0.
!       k =0                                                                                                           ! since the lost area below water level is computed always in avalanche subroutine.
 
! volume: do i = start + r, en ,r
        
 !        d     = oldprofile%prnode(i)%distance
 !        rel_d = ABS(d - origin)                           ! distance of the current point to the origin (left or right end)
        
 !        k = k + 1
 !        m         = abs (newprofile%prnode(save_index + k)%fe_nodenumber )
         ! if there is no conjugate Fe node for this prnode cycle
         
 !        if ( (Oldprofile%prnode(i)%fe_nodenumber == 0 ).OR.(m == 0) ) cycle volume
         
  !       if (newprofile%prnode(save_index + k)%attribute == 'front' ) exit
         
         ! if the node is beyond ( towards the bank) old front then the use the difference between the height of old front
         ! and the node of newprofile below it as deltaElev. 
         
  !       if ( p /= 0) then  ! case of alreeady exiting overhang
         
  !        if ( rel_d <= abs(d - oldprofile%prnode(p)%distance) ) then
  !         ref_height = oldprofile%prnode(p)%elevation
  !        else 
  !         ref_height = oldprofile%prnode(i)%elevation
  !        end if
         
  !       else              ! case of just generating overhang
  !         ref_height = newprofile%water_elev
  !       end if 
  !       deltaElev = abs ( ref_height - newprofile%prnode(save_index + k)%elevation )
         
  !       call EffectiveArea(Eff_area,m)
         
  !       Sumvolume = SumVolume + Eff_area * deltaElev
         
  !       end do volume
         
 !        lost = SumVolume
 !  end if    

end subroutine  projection
end module   projectn

!-----------------------------------------------------------------------------------------------------------------------------------------
! this subroutine computes the intersection coordiante of the extrapolation line from base point with the profile under the old front.
subroutine intersect(profil,pivotnode,angle,start,p,r,distance,elevation,indx, ips)
  USE types
  use share_profile
  USE PARAM
  implicit none

  TYPE (profile)     , INTENT (IN)   :: profil
  TYPE (profile_node), INTENT (IN)   :: pivotnode
  REAL               , INTENT (IN)   :: angle
  REAl(kind = 8)     , INTENT (IN)   :: ips  
  INTEGER            , INTENT (IN)   :: start,p,r

  REAL (KIND = 8)    , INTENT (OUT)  :: distance, elevation
  INTEGER            , INTENT (OUT)  :: indx

  REAL (KIND = 8)                    :: y,y1,y2, x1, x2,ypsilon
  real (KIND = 8)                    :: y1_projected , y2_projected
  REAL (KIND = 8)                    :: slopeofelement, vector_dis1,vector_dis2
  INTEGER                            :: i

  indx = 0
  !if (present(ips)) then
   ypsilon = ips
 ! else
 !  ypsilon = 0.001
 ! end if 
  
  do i  = start,p-r,r               ! look for the nodes between starting node and front with increment r, all defined in the implicit interface subroutine above.

     y1 = profil%prnode(i)%elevation
     x1 = profil%prnode(i)%distance
     x2 = profil%prnode(i + r )%distance
     y2 = profil%prnode(i + r)%elevation

     y1_projected  = pivotnode%elevation + r * TAN (angle*pi/180.) * ( x1 - pivotnode%distance)
     y2_projected  = pivotnode%elevation + r * TAN (angle*pi/180.) * ( x2 - pivotnode%distance)
   
     if (  ( (y1_projected - y1) <=0.001 ) .and. ( y2_projected > y2 ) ) then

        distance  = (y1 - pivotnode%elevation - slopeofelement(x1,y1,x2,y2) * x1 + r * TAN (angle*pi/180.) * pivotnode%distance ) /&       ! d coordiane of intersection point
        & ( r * TAN (angle*pi/180.) - slopeofelement(x1,y1,x2,y2))
        elevation = pivotnode%elevation + r * TAN (angle*pi/180.) * (distance - pivotnode%distance)                              ! elevation coordiante of the intersection point
        vector_dis1 = sqrt ( (distance - X1)**2 + (elevation - y1) **2 )
        vector_dis2 = sqrt ( (distance - X2)**2 + (elevation - y2) **2 )
!-----------------------------------      

        IF ( vector_dis2  <= ypsilon ) THEN
        indx = i + R
        distance  = profil%prnode(indx)%distance ! 30APRIL2009; 16:20
        elevation = profil%prnode(indx)%elevation ! 30APRIL2009; 16:20

        ELSEIF ( vector_dis1 <= ypsilon ) THEN 
        indx = i                                     ! 30APRIL2009; 16:20
        distance  = profil%prnode(indx)%distance      ! 30APRIL2009; 16:20
        elevation = profil%prnode(indx)%elevation     ! 30APRIL2009; 16:20
        ELSE
        indx = i
        END IF 

!-----------------------------------      
        EXIT

     end if
     

  end do

if (indx == 0) then
indx = p
distance  = profil%prnode(indx)%distance
elevation = profil%prnode(indx)%elevation
end if 

end subroutine intersect
!-----------------------------------------------------------------------------------------------------------------------------------------
real (KIND=8) function slopeofelement(x0,z0,x1,z1)
     REAL (KIND = 8), INTENT (IN)  :: x1,x0,z0,z1

       slopeofelement = (z1-z0)/(x1-x0)
      RETURN
end function slopeofelement
