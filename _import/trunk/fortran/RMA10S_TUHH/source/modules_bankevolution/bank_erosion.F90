!     Last change:  MD    7 Jul 2009    5:39 pm
! Bank_erosion Version 1.03
! HN,Dec.2008.
! This program is for calculation of bank evolution process in steady and unsteady cases
! ,which includes undercutting and  cantilever failure of the non-cohesive sandy riverbanks.
! in the main program after computation of flow and sediment transport and bed evolution
! using Exner equation new bed elevation data for each profile is passed to this subroutine.
! Therefore, the main loop over profiles have already been done in the main program, which calls
! this subroutine.
! The variable Profiles (old_pr) and number_of_profiles are global variable, the first defined as data type in 'types module' and both initialized
! in subroutine getgeo (getgemos.for) in RMA-Kalypso. The number of profiles and their number of Fe_nodes remain constant through out the computations.
! While the number of profile nodes may change in the process of bank evolution.


!***************************************************************************************************************************************
! The following tasks are acheived in this subroutine directly:                                                                        *
!                                                                                                                                      *
! 1- Finding the potential nose (needed for Tensile Failure subroutine)                                                                *
! 2- Updating the Profile nodes corresponding to active FE nodes (whose elevation has changed by Exner,Fe_node(j)%statuss='active')    *
! 3- Copying the updated profile (including th eunchanged node) into profile: "pr"                                                     *
! 4- Calls the subroutine tensile_failure, after which sorts the profile into: "profil" by adding new and removing old Profile nodes   *
!    in the failed zone.                                                                                                               *
! 5- Pass Profil or pr to avalnche to compute the bank collapse and new profile, in which (in avalanche) FE-nodes are updated.         *
! 6- Pass the new profile to cantilever_failure subroutine, and a new updated updated profile is computed.                             *
! 7- The updated profile after cantilever failure is written back to old-pr at the end of the program.                                 *
!                                                                                                                                      *
!***************************************************************************************************************************************

!subroutine bank_evolution(fenodes,numberofnodes,number_of_profiles,exner_pr, tens_pr, Avalanch_pr, Cantilever_pr)
subroutine bank_evolution (CallCounter)
USE types
!USE INIT_TYPE
use tensile
USE avalanch_modul
use share_profile  ! the BANKPROFILES containing the profile data is within this module
use cantilever
use param
USE BLK10MOD       ! it contins water level of nodes and their elevation (ao), MAXE,MAXP and ......
USE ASSIGNROFILES  ! the size of profile SIZ is defined here.
USE INIT_TYPE
USE BLKSANMOD      ! the source term variable EXTLD has been defined there.
USE BLKDRMOD

      implicit none
    !  INTEGER , INTENT (IN) :: numberofnodes
   !   TYPE(finite_element_node), DIMENSION (numberofnodes), INTENT (INOUT) :: Fenodes    ! it is better to define Fenodes in a module by reading the input data file of continuity lines.
     ! integer :: number_of_profiles                                                                                   !   fenodes has been defined in share_module

!      integer , intent (IN),optional :: number_of_profiles
      
      INTEGER , INTENT (IN) :: CallCounter
! Local variables
!      TYPE(profile_node), ALLOCATABLE,DIMENSION (:),TARGET  :: Pr_node
     
      TYPE(profile_node) , ALLOCATABLE,DIMENSION (:) :: Ladded_nodes,Radded_nodes !,pr_node
      TYPE(profile_node) :: Lnew_front, Rnew_front                                   ! newprofile  front node number in case of submerged overhang

      TYPE(potential_nose),DIMENSION (2) :: potentialnose
      TYPE(profile)       ,DIMENSION (1) :: temp_prof
      TYPE(profile)                      :: Profil , pr                              ! no need to define original profile here(old_pr), it has been already defined as global array
                                                                                     ! in subroutne getgeo. pr is the profile after adoption to Exner calculations and profil the one after adoption to tensile failure calculations.
   !   TYPE(finite_element_node), DIMENSION (:), allocatable :: Fenodes

      TYPE(finite_element_node), DIMENSION (MAXP) :: Fenode
      ! maxp is a global array saving the number of finite element nodes including midside nodes.
 !     integer , parameter :: single=4
  !    integer , parameter :: DOUBLE=8
            
      Real , parameter :: ypsilon = 0.01
      Real (kind = DOUBLE) , DIMENSION (MAXE)     :: ElementSource                   ! maxe is a global array saving the number of elements.
      Real (kind = DOUBLE) , DIMENSION (MAXP)     :: diffbed 
      Real (kind = DOUBLE) , DIMENSION (2) :: failure_source , EffectiveWidth_Overhang, avalanche_source
           
      REAL(KIND=DOUBLE) :: z1,z2,zz2, realtemp
     ! REAL(KIND=DOUBLE) :: average_water_level,distfromwater
      REAL(KIND=DOUBLE) :: distfromwater
      REAL(KIND=doublE) :: HTP , DIFF1, D1, D2
    !  REAL(KIND=DOUBLE), (IN) :: wsll                                               	! no need to define it here, since included in Fenodes data type
     
      INTEGER , DIMENSION (2) ::  front                                              ! front is an array that stores the profile node number of overhang's front.
      INTEGER , DIMENSION (2) ::  last_submerged_node , Banktoe_node   ! for left and right banks.
     
      INTEGER :: Lnumber_newnodes,Rnumber_newnodes,Lnumber_lostnodes,Rnumber_lostnodes ! Lnumber_lostnodes  and Rnumber_lostnodes are the number (Anzahl) of deleted profile nodes in overhang due to submergence (tensile failure)
      INTEGER :: Ltotal, Rtotal, Total ,totalprnode                                    ! number_of_profiles is also a global variable like profile
      INTEGER :: h,hh,i,j,k,kk,l,m,n,b,t,g, u , j1, j2
      integer :: integertemp
      integer :: deleted_nodesL ,deleted_nodesR , pr_number
      INTEGER :: status, lstatus , rstatus
      INTEGER :: numberofnodes , number_of_profiles

      CHARACTER (LEN = 35):: exner_pr, tens_pr, Avalanch_pr, Cantilever_pr 
      CHARACTER (LEN = 1) :: seign,sgn
      CHARACTER (LEN = 5) :: side                                                     ! it is a tag that is either 'left' or 'right', and it signals tensile failure subroutine
      CHARACTER (LEN = 3) :: digit                                                                             ! with which overhang it deals.
      CHARACTER (LEN = 9):: distribution
      LOGICAL :: Lsubmerged,Rsubmerged

!******* Initialization of variables ****************************************
Write (*,*) ' Entering to Bank Evolution Modelling ....'

Lnumber_newnodes = 0
Rnumber_newnodes = 0
Lnumber_lostnodes = 0
Rnumber_lostnodes = 0
H = 1

DiffBed = 0.  ! array initialization
EXTLD = 0.0   ! array initialization

!allocate ( ElementSource(MAXE), stat = lstatus)
 !  if (lstatus /= 0) then
  !    write (*,*) ' failure by allocating array "ElementSource" in subroutine BankErosion '
      !STOP ' Program will be terminated!'
   !  end if 

! call Counter is the number of times that this subroutine is called from RMA10s.f90
write (digit,'(I3)' )CallCounter

if ( CallCounter < 10 ) then
digit = ADJUSTL(digit)
digit = TRIM (digit)
digit = '00'//digit
elseif ( ( CallCounter >= 10 ).and.( CallCounter < 100 ) ) then
digit = ADJUSTL(digit)
digit = TRIM (digit)
digit = '0'//digit
endif

! at this moment the name of output file after each stage of bank evolution is fixed
exner_pr      = 'afterexner' // digit // '.txt'
tens_pr       = 'aftertensile' // digit // '.txt'
Avalanch_pr   = 'afteravalanche' // digit // '.txt'
Cantilever_pr = 'aftercantilever' // digit // '.txt'


numberofnodes = maxp

if (SIZ/= 0) then
number_of_profiles = SIZ
else
number_of_profiles = SIZE (BANKPROFILES)
end if
!???? is needed to allocate old_pr every time this subroutine is called, or it is needed to 
! preserve its value between calls by save statement.????????????????
if (.NOT. allocated(old_pr) ) then
   allocate (old_pr (number_of_profiles) , stat = lstatus)
   if (lstatus /= 0) then
      write (*,*) ' failure by allocation array "old_pr" '
      pause ' Program will be terminated!'
      stop
     end if 
 end if                                                                                     !?is it needed here if it is already  No when definNed as global in getgeo subroutine

call UpdateFenodes (FENODES , numberofnodes) 

! old_pr and fenode are local variables in bank evolution PACKAGE, shared within its modules.
old_pr = BANKPROFILES        ! old_pr is a global variable defined in share_profile module.
FENODES.sed_source =0.0
fenode = FENODES             ! FENODES IS DEFINED IN MODULE SHARE_PROFILE AND ASSIGNED IN SUBROUTINE INPUT.FOR

! Each profile is checked first wether they are active for further computation.
prof: do i= 1, number_of_profiles

        pr = old_pr(i)  ! initialize pr with the values of old profile
        call init_profile(temp_prof,1)
        profil = temp_prof(1)
        k=0
        h=1             ! index for potential nose, max=2
        t=1
        potentialnose%node = 0
        potentialnose%nextnode = 0
        potentialnose%dist = 0.
        front=0

       if (.not.old_pr(i)%activation) then 
         CYCLE                                                                         ! so if activation is .false. then .not..false. is true and it cycles if th eprofile is inactive.
       end if                                                                              ! if the profile is active then the average new water elevation is computed across the profile
! HN June2009. It is  better to use local water depth over the nodes instead of 
!    average water level, since in a large river or riverbend, there is a traverse 
!    water level slope. However for example for computation of suction head in riverbank
! it seems that this average of water level should be enough. The main idea was to damp
! the marsh effect on water slope close to the riverbank.
        pr%water_elev = average_water_level(i)                    ! average of water level in new profile (new time step)

profnod: do j=1, old_pr(i)%max_nodes-1

      ! in the following block only the profile nodes belonging to profile and front types are updated using new Fenodes elevations
      ! regardless of unsteadiness of flow. Howerver the rest of nodes(overhang and nose) will be later updated afterwhich, it is checked if
      ! flow is unsteady. In this case the volume of overhang , which sinks under water in the case of water rise in the river
      ! should be computed first(tensile failure), before the nodes in the region of overhang are diminished or moved.
      ! and the stability of bank failure and the corresponding bank deformation is computed in avalanche and cantilever subroutines

           distfromwater = old_pr(i)%prnode(j)%elevation-pr%water_elev
           ZZ2           = old_pr(i)%prnode(j+1)%elevation-pr%water_elev       

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the block to determine potential nose position over left and right bank.
! this is required by subroutine Tensile failure if an existing overhang
! is submerged.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    IF( ( ABS ( DISTFROMWATER) <= 0.001 ).AND. (TRIM(OLD_PR(I)%PRNODE(J)%ATTRIBUTE) /='front')  )THEN    !IN THE CASE THE WATER LEVEL COINCIDES WITH A NODE, WHICH IS NOT A FRONT. SINCE THE POTENTIAL NOSE DISTANCE IS REQUIRED IN TENSILE FAILURE                         ! THE CASE IN WHICH WATER LEVEL HAS NOT BEEN CHANGED AND IS EQUAL TO THE NOSE ELEVATION OF THE OLD PROFILE: STEADY STATE.
                                                                                                                                         ! THE POTENTIAL NOSE DISTANCE SHOULD NOT BE MISTAKEN WITH THE DISTANCE OF FRONT; WHEN WATER LEVEL IS EQUAL TO FRONT ELEVATION.     
      POTENTIALNOSE(H)%node     = J
      POTENTIALNOSE(H)%nextnode = -(J + 1)
      POTENTIALNOSE(H)%dist     = OLD_PR(I)%PRNODE(J)%DISTANCE
      H = H + 1
    ELSEIF ( ( (distfromwater > 0.001) .AND. (ZZ2 < -0.001) ).OR.( (distfromwater < -0.001) .AND. (ZZ2 > 0.001) ) ) THEN     ! TRANSITAION THROUGH WATER SURFACE FOR THE LEFT AND RIGHT BANKS; RESPECTIVELY.
      potentialnose(H)%nextnode = (j + 1)           ! if the node doesnot  coincide with water then potential nose should be computed.
      CALL mk_potentialnose(pr,potentialnose(H))
      H = H + 1
    END IF  

           pr%prnode(j)            = old_pr(i)%prnode(j)                                    ! the elevation of the new nodes will be overwritten if necessary in the following.
           b                       = abs(old_pr(i)%prnode(j)%fe_nodenumber)
           if (b /= 0 ) then                                                
           pr%prnode(j)%water_elev = fenode(b)%water_level
           else
           pr%prnode(j)%water_elev = pr%water_elev
           end if
                                                                                     ! this if-statement cares for the case of water rise(unsteady) over overhang, so that it doesn't deal with
                                                                                     ! the submerged zone in the area of overhang (except front) at this moment, but in stability analysis
           m=old_pr(i)%prnode(j)%Fe_nodenumber                                       !The front should be dealt as fluvial erosion process in case of water rise, only if it is a Fe node
                                                                                     ! otherwise, it is not needed, since its evolution is not computed in Exner equation.
           if( m <= 0 )then                                                          ! only Fe nodes, which have changed are accounted for chaning the corresponding Prnodes.
              cycle  profnod 
           elseif (TRIM(fenode(m)%STATUSs)=='deactivate' )then       
              cycle  profnod 
           ELSEIF (TRIM(OLD_PR(I)%PRNODE(J)%ATTRIBUTE) =='front') THEN
              cycle  profnod 
           ELSEIF ((distfromwater > 0.001) .AND. (ZZ2 >= 0.0)) THEN
              cycle  profnod
           end if
           
           z1= fenode(m)%elevation                                                  ! and in the case of 'front' if it correspondes to a FE node (m>0). m= - old_pr(i)%prnode(j)%Fe_nodenumber for the nodes in overhang zone.
!---------------------  ??????????????????????????????????????                       ! and zero for those who have to conjugate fe_node (probably like nose or front).
! new elevation of profile nodes are updated
           z2=old_pr(i)%prnode(j)%elevation                                      !??????? here in the case of unsteady flow, a flood plain node can be displaced
           if (ABS(z2-z1)>ypsilon) then                                              ! wrongly, before finding its image Pr_node.
                  pr%prnode(j)%elevation=z1                                      ! Note that the old profile nodes are not displaced but it is the new profile
           end if                                                                    !so that at the moment old data is still retrievable(it is necessary for the case
                                                                                     !that Front point is linked to a submerged Fe node and is eroded. The original
         end do profnod                                      		                      !position of the Front is needed for calculation of tensile failure volume/area.

! at this stage the updated profile of old_pr(i) is pr which identical to old_profile except
! the updated elevation of the nodes (..pr%elevation), due to erosion or deposition computed
! by Exner equation. In the following a new profile variable called profil will save the changes made
! to the profile old_pr(i) due to the probable tensile stress.

!--------------------- Computation of tensile failure of overhang (submerged) -------------------
     lsubmerged=.false.
     rsubmerged=.false.
                                                                                    ! in the case that overhang (nose) already exists(produced in avalanch subroutine)
        if (old_pr(i)%lnose/=0) then                                                ! and it is submerged then tensile subroutine should compute the tensile failure area in overhang.
            integertemp=old_pr(i)%lnose                                             ! %lnose and %rnose store the prnode number of the nose on the left and right bank. Zero means no nose
            side='left'                                                             ! and no overhang respectively.
             IF ( (old_pr(i)%prnode(integertemp)%elevation - pr%water_elev) <= -0.01 )then   ! if new water stage is over nose, then the old nose is submerged and tensile failure should be run.
               Lsubmerged=.true.
               call tensile_failure(pr,fenode, side, pr%water_elev,potentialnose(1)%dist,Lnew_front,Lnumber_newnodes, &
                &                   Lnumber_lostnodes)

               ALLOCATE (Ladded_nodes(Lnumber_newnodes),stat=lstatus)
                if (lstatus>0) then
                  write (*,*) ' The "Ladded_nodes" array was not successfully allocated for profile: ',i
                  pause
                end if

               Ladded_nodes = newnodes
               deallocate (newnodes)

             END if
        ENDif

        if ((old_pr(i)%rnose/=0)) then
              integertemp=old_pr(i)%rnose
              side='right'
              IF ( (old_pr(i)%prnode(integertemp)%elevation - pr%water_elev) <= -0.01 )then  ! ifnew water stage is over nose, then the old nose is submerged and tensile failure should be run.
                 Rsubmerged =.true.
                 call tensile_failure(pr,fenode,side,pr%water_elev,potentialnose(2)%dist,Rnew_front,Rnumber_newnodes, &
                      &              Rnumber_lostnodes)

                 ALLOCATE (Radded_nodes(Lnumber_newnodes),stat=rstatus)
                   if (rstatus>0) then
                     write (*,*) ' The "Radded_nodes" array was not successfully allocated for profile: ',i
                     PAUSE
                   end if

                 Radded_nodes = newnodes
                 deallocate (newnodes)

               END if
        end if
! transfer source term to global variable EXTLD, which is global external source term for sand computation in total load trasport.
       EXTLD = fenode.Sed_source
! If either of the overhang's noses is submerged then the following block is executed
submerg:if (lsubmerged.or.rsubmerged) then

           Ltotal = Lnumber_newnodes - Lnumber_lostnodes
           Rtotal = Rnumber_newnodes - Rnumber_lostnodes
           total = Ltotal + Rtotal
           totalprnode = total + pr%max_nodes

           profil%max_nodes = totalprnode
           profil%cl_number = pr%cl_number
           profil%water_elev = pr%water_elev
           profil%activation =.TRUE.

!  Initialize the array of prnode to zero!
           profil%prnode%elevation =0.0
  ! In the following the profile node number from which on new created profile nodes should be substituted into, TO GET sortednew profile will be determined.
 kk=0
 hh=0
          if (lsubmerged) then
               if (potentialnose(1)%node == 0) then                                 ! It means the potential nose is between two profile nodes.
                  kk = potentialnose(1)%nextnode                                    ! The profile node NUMBER from which THE new nodes are TO be substituted.
                  deleted_nodesL = old_pr(i)%lnose - kk                ! number OF deleted nodes IN left overhang due TO  tensile failure
                                                                               ! it is not needed, since s1 already computes the number of deleted nodes, but it will be remained to be used a scheck value.
               else                                                            ! It means the potential nose is a profile node.
                  kk = potentialnose(1)%node
                  deleted_nodesL = old_pr(i)%lnose - kk
               end if
          end if

            if (rsubmerged) then
               if (potentialnose(2)%node == 0) then
                  hh = potentialnose(2)%nextnode
                  deleted_nodesR = old_pr(i)%Rnose - hh
               else                                                              ! It means the potential nose is a profile node.
                  hh = potentialnose(2)%node
                  deleted_nodesR = old_pr(i)%Rnose - hh
               end if
            end if


        j = 1
        g = 0
! This part of code substitute the new nodes resulting from tensile failure in the profile and remove the old ones in the right places.
!___________________________________________________________________________________
sort:     do  
             g = g + 1
             if ( g > pr%max_nodes) then 
               exit sort
             end if  
             
             
            
sortnode:    if ( (g == kk).AND.(lsubmerged) ) then

leftbank:       if (potentialnose(1)%node == 0) then                      ! the potential nose is between two fe-nodes.

                  profil%prnode(j)%distance = potentialnose(1)%dist
                  profil%prnode(j)%elevation = pr%water_elev
                  profil%prnode(j)%fe_nodenumber = 0
                  profil%prnode(j)%water_elev =  pr%water_elev
                  profil%prnode(j)%attribute = 'nose'
                  profil%lnose = j
                  j = j + 1
                  profil%prnode(j) = Lnew_front
                  profil%lfront = j
                  m = 1
                  j = j + 1

                    do l = j , j+Lnumber_newnodes - 2          ! Since the old front is counted in added nodes, therefor, if added nose is equal to 1, it means the old front, which should not be added here at this moment,
                     profil%prnode(l) = Ladded_nodes(m)        !before checking if it has a conjugate fenode. However, if the loop executes up to j + added_nodes - 1 and added_nose = 1, then the loop executes from l = j to j+1-1= j
                     m= m + 1                                  ! However, l = j,j, executes the loop once before it increses its index to j+1, which should not be the case. Therefore, the last index is j + added_nodes - 2
                    end do

                  j= j + Lnumber_newnodes - 1                                 ! the index of the next node of the new profile, if add_node = 1(meaning only old front, it should be subtracted from added nodes, because it will be dealt with in the following.Therefore: -1
                  u = g + Lnumber_lostnodes                                   !from the current node g , which is the node after potential nose, plus deleted nodes (which include also the current node, meaning the current node has been considered twice)then u should be the node after old nose that is old front.

                    if (pr%prnode(u)%fe_nodenumber > 0) then   ! then if the old front has a conjugate fe_node it should be the next node in new profile
                       profil%prnode(j)=pr%prnode(u)
                       profil%prnode(j)%attribute = 'profile'
                       j = j +1
                       u = Lnumber_lostnodes 
                    else
                       u = Lnumber_lostnodes 
                    end if
                       
                       g = g + u  
                   
                   !    do l = 1,u                           ! since the current old profile node is among the counted deleted nodes, therefore
                    !     CYCLE sort                         ! the loop must extend to Lnumber_lostnodes - 1 + 2 (2 for the old node and front)
                     !  end do

                ELSE                          leftbank   ! potential nose is a profile node.

                  profil%prnode(j) = pr%prnode(g)
                  profil%prnode(j)%attribute = 'nose'
                  profil%lnose = j
                  j = j + 1
                  profil%prnode(j) = Lnew_front
                  profil%lfront = j
                  m = 1
                  j = j + 1

                    do l = j , j+Lnumber_newnodes - 2
                     profil%prnode(l) = Ladded_nodes(m)
                     m= m + 1
                    end do

                  j= j + Lnumber_newnodes - 1

                  u = g + Lnumber_lostnodes + 1                               !from the current node g , which is the potential nose plus deleted nodes , plus 1 (for front), then u should be old front
                                                                              ! then if the old front has a conjugate fe_node it should be the next node in new profile
                    if (pr%prnode(u)%fe_nodenumber > 0) then
                       profil%prnode(j)=pr%prnode(u)
                       profil%prnode(j)%attribute = 'profile'
                       j = j +1
                       u = Lnumber_lostnodes + 1 !2
                    else
                       u = Lnumber_lostnodes + 1 !2
                    end if
                      g = g + u
                      
                      ! do l = 1,u                           ! since the old profile's deleted nodes start from current old profile node, then
                       !  CYCLE sort                         ! the loop must extend to Lnumber_lostnodes + 2 (2 for the old node and front)
                       !end do

                ENDIF                         leftbank

  ! the same procedure of sorting new nodes for the right bank
             ELSEIF ((trim(pr%prnode(g)%attribute) =='front').AND.(Rsubmerged) ) then     sortnode

rightbank:      if (pr%prnode(g)%fe_nodenumber > 0) then                    ! if the right front has a conjugate fe_node.
!04.05.2009 11:26
!                  profil%prnode(j) = pr%prnode(g)
 !                 profil%prnode(j)%attribute = 'profile'
  !                j= j + 1
   !04.05.2009 11:26
                  m = Rnumber_newnodes

                    do l = j , j + Rnumber_newnodes - 1 !2  !04.05.2009 11:26                                  
                     profil%prnode(l) = Radded_nodes(m)
                     m= m - 1
                    end do
                   j= j+Rnumber_newnodes ! - 1 !04.05.2009 11:26

                  profil%prnode(j) = Rnew_front
                  profil%Rfront = j
                  j = j + 1

                  profil%prnode(j)%distance = potentialnose(2)%dist
                  profil%prnode(j)%elevation = pr%water_elev
                  profil%prnode(j)%fe_nodenumber = 0 
                  profil%prnode(j)%water_elev =  pr%water_elev
                  profil%prnode(j)%attribute = 'nose'
                  profil%Rnose = j
                  j = j + 1

                   if (potentialnose(2)%node == 0) then
                     u = Rnumber_lostnodes !+ 1
                   else
                     u = Rnumber_lostnodes + 1 !2
                     profil%prnode(profil%Rnose)%fe_nodenumber = - ABS (pr%prnode(potentialnose(2)%node)%fe_nodenumber)
                   end if
                      
                      g = g + u

                ELSE                          rightbank
                       
                     m = Rnumber_newnodes
  
                      do l = 1,m
                       if(trim(Radded_nodes(m)%attribute) =='front') then
                        cycle
                       end if
                         profil%prnode(j) = Radded_nodes(m)
                         j = j + 1
                      end do   
  !                     do l = j , j + Rnumber_newnodes - 2                        ! since the first new added node was substituted to the current j, then the next nodes should be from this node + added nodes -1
   !                     profil%prnode(l) = Radded_nodes(m)
    !                    m= m - 1
     !                  end do

       !            j= j+Rnumber_newnodes -1
                   profil%prnode(j) = Rnew_front
                   profil%Rfront = j
                   j = j + 1
                  profil%prnode(j)%distance = potentialnose(2)%dist
                  profil%prnode(j)%elevation =pr%water_elev
                  profil%prnode(j)%fe_nodenumber = 0    
                  profil%prnode(j)%water_elev =  pr%water_elev
                  profil%prnode(j)%attribute = 'nose'
                  profil%Rnose = j
                  j = j + 1

                   if (potentialnose(2)%node == 0) then
                     u = Rnumber_lostnodes !+ 1
                   else
                     u = Rnumber_lostnodes +1 ! 2
                     profil%prnode(profil%Rnose)%fe_nodenumber = - ABS (pr%prnode(potentialnose(2)%node)%fe_nodenumber)
                   end if
                    g = g + u

                ENDIF                         rightbank


             else                             sortnode

                profil%prnode(j) = pr%prnode(g)
                j = j + 1

             end if                           sortnode

          end do                   sort
        end if    submerg
   profil%max_nodes = j - 1

           call output1 (pr, exner_pr, 11, i)

           if (Lsubmerged .or. Rsubmerged ) call output1 (profil,tens_pr, 21, i)




! I thinkt the right thing for definition of temporary profile data is to define a temporary variable: "pr" (type:profile) for saving
! the updated profile according to Exner equation with the same size prnode numbers as old_pr(pr_number). then define another
! temporary variable "profil"  (type:profile) to store the updated prnode of pr after probable tensile failure. then send the  latter
! variable to avalanche to compute new fe and profile node in the case of avalanching (first stored within a local temporary variable in avalanche
! but given back to "profil" in the current subroutine (profil as inout variable for avalanche subroutine). Then it will be passed to catilever_failure
! subroutine, which produce new updated profile after cantilever failure and send it back to another new Temporary variable in the current program.
! this new variable is saved back to the old_pr(pr_number), BUT IF IT IS POSSIBLE to change the size of the pr_node array within old_pr without
! destroying the rest of prnodes belonging to other profiles than old_pr(pr_number). Then this process can be acheived within the main loop.
! Otherwise a new temporaray array of profile should be created to hold all new profiles and then deallocated and reallocate old_profile
! and assign new updated profile to it.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!--------------------- Computation of AVALANCHE of submerged sandy bank -------------------
          ! initialize variables
          last_submerged_node     = 0
          Banktoe_node            = 0
          EffectiveWidth_Overhang = 0.0
          avalanche_source        = 0.0
          
          if (lsubmerged.or.rsubmerged) then
             ! ??? call suspended sediment transport routine to update bed and bank-toe due to tensile failure
             ! ??? call recurssively the current subroutine to update profile nodes
             call avalanche (profil,totalprnode, fenode, last_submerged_node, Banktoe_node,EffectiveWidth_Overhang,avalanche_source)
          else
             call avalanche (pr,pr%max_nodes, fenode, last_submerged_node, Banktoe_node,EffectiveWidth_Overhang,avalanche_source)
          end if
          
           call output1 (ava_pr,Avalanch_pr, 31, i)

!--------------------- Computation of Cantilever failure of overhang (not submerged)-------------------
  ! ava_pr is the profile after computation of avalanche, it is defined as a global profile in interface of the module , which includes avalanche subroutine.
           
           failure_source = 0.0
           
           call cantilever_failure (ava_pr,pr%water_elev,critical_slope,failure_source , fenode)
         
           call output1 (canti_pr,Cantilever_pr, 41, i)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   DISTRIBUTATION OF MASS WASTE AT BANK-TOE  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! find the current profile node number of left an dright bank-toe and last submerged node.
    ! These IDs could have been changed due to addition or deletion of profile nodes due to
    ! avalanche and failure of the bank.
    
    do kk = 1,canti_pr%max_nodes
    do j = 1,2
      if (canti_pr%prnode(kk)%fe_nodenumber == last_submerged_node(j))then
          last_submerged_node(j) = kk
      elseif (canti_pr%prnode(kk)%fe_nodenumber == Banktoe_node(j))then 
          Banktoe_node(j)  = kk
      end if      
    end do  
    end do
  
  ! cant_pr is the profile after computation of probable mass failure,it is defined as a global profile in interface of the module , which includes cantilever_failure subroutine.
    
    if ((failure_source(1) /=0) .or.(failure_source(2) /=0) ) then
     Avalanche_source = Avalanche_source + failure_source
    end if
    
    distribution = 'linear' 
    call DistributeWastedMass(Avalanche_source , canti_pr, last_submerged_node , Banktoe_node, &
    &          Distribution, EffectiveWidth_Overhang ,fenode, numberofnodes ,ElementSource)
    
        !   fenode(abs(last_submerged_node(1)))%sed_source =  fenode(abs(last_submerged_node(1)))%sed_source + failure_source(1)
         !  fenode(abs(last_submerged_node(2)))%sed_source =  fenode(abs(last_submerged_node(2)))%sed_source + failure_source(2)
          
           old_pr(i)=canti_pr
      
       if (lsubmerged ) then
           deallocate (Ladded_nodes)
       endif
       if (Rsubmerged) then
           deallocate ( Radded_nodes)
       end if

      end do  prof
  ! array manipulation. Ersoion is (+) and deposition is (-)
  ! in the following the application of (-) sign will reverse their sign
  ! for updating bed features.
  
     DiffBed = FENODES.elevation - fenode.elevation         
     
      ELEVB  = ELEVB  - DiffBed
      TTHICK = TTHICK - DiffBed
	  DELBED = DELBED - DiffBed
 
 ! THE FOLLOWING HAS BEEN COPIED FROM SLUMP.FOR                                  
 
  !     set distribution linear

      ncn=0
      do n=1,ne             ! ne is maximum number of elements, defined as global variable in module BLK10MOD
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              delbed(nop(n,j))=(delbed(nop(n,j1))+delbed(nop(n,j2)))/2.
              elevb(nop(n,j))=(elevb(nop(n,j1))+elevb(nop(n,j2)))/2.
              tthick(nop(n,j))=(tthick(nop(n,j1))+tthick(nop(n,j2)))/2.
              dgn(nop(n,j))=(dgn(nop(n,j1))+dgn(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 

 ! CIPK MAY02 UPDATE BED ELEVATION
	DO N=1,NP

!        IF(N .EQ. 141) THEN
!	  AAA=0
!	ENDIF

        !TODO: Differ between Marsh and not-Marsh as well as dimensions; it shouldn't be applied for 1D Polynomial
	DIFF1=ELEVB(N)-AO(N)
	IF(DIFF1 .NE. 0.) THEN
  	  AO(N)=ELEVB(N)
	  ADO(N)=ADO(N)+DIFF1
          HEL(N)=WSLL(N)-ADO(N)
          CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)
	  VEL(3,N)=HTP
	ENDIF

	ENDDO

      BANKPROFILES = old_pr
      FENODES = fenode
            
   !   deallocate (fenodes)
      
       deallocate (old_pr)
contains 
!-----------------------------------------------------------------------------------------------------------------------------------------
real (kind = DOUBLE) function average_water_level(k)
!real function ave_water_level(fe_nodes,n,k)
! Ave-water elevation across a profile is computed based on water level in active fe-nodes
!USE types

!implicit none
INTEGER , intent (IN):: k
!INTEGER , intent (IN)::n,k                                                           ! k is the index of the current profile, whose water level is being computed
!TYPE (finite_element_node), DIMENSION (n), INTENT (IN) :: Fe_nodes

REAL :: summ
INTEGER :: l,j,b,t

summ=0.
t=0
l=old_pr(k)%max_nodes

do j=1,l

  b=abs(old_pr(k)%prnode(j)%Fe_nodenumber)
  if (b==0) cycle
  if (fenodes(b)%elevation >= fenodes(b)%water_level) THEN                           ! only water elevation of submerged nodes are averaged, the equall sign is for the case if the RMA-Kalypso the dry nodes retain the same WSLL as wet nodes.
   cycle
  end if
  summ=summ+fenodes(b)%water_level
  t=t+1

end do

if (t==0) then
  average_water_level= old_pr(k)%water_elev
 else
  average_water_level=summ/(t*1.)
end if
!return
end function average_water_level

end subroutine bank_evolution

!-----------------------------------------------------------------------------------------------------------------------------------------

subroutine output1(profileIN, filename, unitt, profilenumber)

use types

implicit none

type (profile)       , intent (in) :: profileIN
character (len = 35) , intent (in) :: filename
integer              , intent (in) :: unitt, profilenumber
integer                            :: i,istat
logical                            :: exists
   
   inquire (file = filename, EXIST= exists)

! The following control is necessary, when more than one timestep is available.
! to ensure all of the profiles in each time steps are written to the output file.
    
    if (exists) then
     open (UNIT=unitt, file = filename , STATUS='old', ACTION='write',POSITION = 'append', IOSTAT=istat)
     call file_error (filename, istat)
    else
     open (UNIT=unitt, file = filename , STATUS='new', ACTION='write', IOSTAT=istat)
     call file_error (filename, istat)
    endif 
   
     write (unitt, *) 'profile number: ' , profilenumber
   
     write (unitt, *) 'Contiline number: ', profileIN%cl_number
   
     write (unitt, 110) profileIN%lnose, profileIN%Rnose, profileIN%lfront, profileIN%Rfront
    110 format (4(2x, I4))
   
    write (unitt, '( 2x,a6,6x,a1,7x,a1,4x,a9)')'fenode','D','Z','attribute'
    
    do i = 1, profileIN%max_nodes
     write (unitt,120) profileIN%prnode(i)%fe_nodenumber , profileIN%prnode(i)%distance, profileIN%prnode(i)%elevation &
     &                       , profileIN%prnode(i)%attribute   
    end do
    
    120 format (1x,I7,3x,F7.3,1x,f7.4, 1x,a9)
    close (unit = unitt) 
   return 
end subroutine output1
!-----------------------------------------------------------------
subroutine file_error (filename, ierror)

implicit none 

character (len = 35) , intent (in) :: filename
integer              , intent (in) :: ierror

     if (ierror /= 0) then
      write (*,*) ' failure by opening the file:', filename, ' it does not exist!'
      STOP ' Program will be terminated!'
     end if
     
 end subroutine file_error
 
 !---------------------------------------------
 subroutine DistributeWastedMass(WastedVolume , CurrentProfile, FirstPoint,&
 &                    LastPoint, Distribution, EffectiveWidth, fenode, nn, ElementSource)
 
 USE Types
 USE Param
 USE parakalyps, ONLY : IsNodeOfElement
 USE BLK10MOD  , ONLY : MAXE , AREA , DELT
 USE BLKSANMOD , ONLY : EXTLD, TRIBAREA, SGSAND
 implicit none 
 
 Integer          , intent (in) :: nn
 type (Profile)   , intent (in) :: CurrentProfile
 type (Finite_element_node) , intent (inout) :: fenode(nn)
  
 
 Real (Kind= 8)   , intent (in) :: WastedVolume (2)
 Real (Kind= 8)   , intent (in) :: EffectiveWidth(2)    ! It is for later consideration of the effective width
                                                        ! of the overhnag and its relation with berm width to 
                                                        ! compute the EffectiveBanktoeWidth. However, firstly
                                                        ! Berm attribute should be added to the profiles and control
                                                        ! against probable failure in the rest of code, when facing
                                                        ! with the new attribute.
 Integer          , intent (in) :: FirstPoint (2) , LastPoint(2) 
 Character(Len= 9), intent (in) :: Distribution        
 Real (Kind= 8)   , intent (out) :: ElementSource (MAXE)
 
 Real (Kind= 8)   :: StartDistance , EndDistance , RelativeDistance
 Real (Kind= 8)   :: EffectiveBanktoeWidth , PreviousDistance, origin
 Real (Kind= 8)   :: DistributingRate , WastedSourceTerm 
 Real (Kind= 8)   :: PreviousDistributingRate ,TrapezoidRule
 Real (Kind= 8)   :: SumConnectedElementArea
 Real (Kind= 8)   :: NodalSource, DryTRIBAREA   ! DryTRIBAREA: the contributing area to a node with connected dry elements 
 integer          :: Increment(2)
 integer          :: i , m , j, k, ELEMNO , FirstNode, LastNode, LR
 
! determine if there are one bank or two banks available.
! if the right bank is totally submerged, it implies there is no rightbnak morphological evolution
! meant.

if (CurrentProfile%water_elev > CurrentProfile%prnode(CurrentProfile%max_nodes)%elevation ) then
   LR = 1                  ! only left bank is available.
else 
   LR = 2                  ! Left and rigtht banks area available.
end if      

 LeftRightBank: do  j = 1, LR
 
  if ( j == 1) then
  origin = CurrentProfile.Prnode(1).Distance
  Increment(1) = 1
  elseif (j == 2) then
  origin = CurrentProfile.Prnode(CurrentProfile.max_nodes).Distance
  Increment(2) = -1
  endif 
  
  FirstNode = FirstPoint (j)
  LastNode  = LastPoint  (j)
  ! at the moment deactivated. For Reasons, refer to the description in subroutine
  ! HasPrnodeFenode
  
  !call HasPrnodeFenode (CurrentProfile, FirstNode , Increment(j))
  !call HasPrnodeFenode (CurrentProfile, LastNode  , Increment(j))

 if( (FirstNode == 0).OR.(LastNode == 0) ) cycle  LeftRightBank     ! in the case that no bank erosion has occured on either of bank side cycle to the next one. 
 StartDistance = ABS (CurrentProfile.Prnode(FirstNode).Distance - origin)
 EndDistance   = ABS (CurrentProfile.Prnode(LastNode).Distance  - origin)
 
 EffectiveBanktoeWidth = EndDistance - StartDistance
 
 PreviousDistributingRate  = 0.0
 PreviousDistance          = 0.0
 
 FirstNode = FirstNode + Increment(j)
 
NodDistr: do i = FirstNode, LastNode , Increment(j)
 
           RelativeDistance = ABS (CurrentProfile.Prnode(i).Distance - origin )   ! relative distance to the origin
           RelativeDistance = ABS (RelativeDistance - StartDistance )   ! relative distance to the starting point of distribution (first wet node from each bank)
 
           if (trim(distribution) == 'linear') then
              DistributingRate = 2.0 * RelativeDistance / EffectiveBanktoeWidth ** 2 
           elseif (trim(distribution) == 'binominal') then
              DistributingRate = 3.0 * RelativeDistance ** 2 / EffectiveBanktoeWidth ** 3 
           end if
 
           TrapezoidRule    = 0.5 * (DistributingRate + PreviousDistributingRate)* &
           &                        ( RelativeDistance - PreviousDistance )  
         
           m                = CurrentProfile.Prnode(i).Fe_NodeNumber
   ! wasted source term in m^3 x kg/m^3
           WastedSourceTerm = TrapezoidRule * WastedVolume(j) *(1- POROSITY)* SGSAND(m)* 1000.  
 ! check if it is correct to multiply wasted volume by rhos and porosity, since it has been already done in avalanche
 

           PreviousDistributingRate  = DistributingRate
           PreviousDistance          = RelativeDistance
 
  !         SumConnectedElementArea = 0.0
          ElementSource           = 0.0
 ! get the elements connencted to this node
 ! Get the area of each element
 ! distribute the mass among the elements based on their area
 
 ! this block is commented out, because the total effective vloume of overhang has been already computed in 
 ! WastedVolume and it has been distributed over fenodes at banktoe using WastedSourceTerm (mass[g]).
 ! Now it is only needed to divide this nodal mass by nodal effective area of receiving nodes.
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !sumarea:   do k = 1,IsnodeOfElement (m,0) ! total number of elements connected to the node "m"
  !           SumConnectedElementArea = SumConnectedElementArea + AREA (IsnodeOfElement (m,k))
   !         end do sumarea
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! TODO the source term should be distributed over nodes (EXTLD) and not elements
  ! so that later in COEF subroutine it will be multiplied by weighing function XM
  ! and summed over all nodes in the element and assigned as EXTL.
  ! therefoe it might be better to distribute the wasted mass to the elements 
  ! connected to that node, and then redistribute the mass of each element to its nodes
  ! and then summ all of cotributing masses from sourounding elements of each node to that node.
 
 
 ! this block is commented out, because the total effective vloume of overhang has been already computed in 
 ! WastedVolume and it has been distributed over fenodes at banktoe using WastedSourceTerm (mass[g]).
 ! Now it is only needed to divide this nodal mass by nodal effective area of receiving nodes.
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !ElemDistr: do k = 1,IsnodeOfElement (m,0)
  !            ELEMNO    = IsnodeOfElement (m,k)
   !           if (DELT > 0 ) then
    !          ElementSource(ELEMNO) = ElementSource(ELEMNO) + (AREA (ELEMNO) / &
     !       &                         SumConnectedElementArea )* fenode(m).Sed_source * &
      !      &                        1000. /(DELT * 3600.)
!             ElementSource is now in g/m.s            
       !      end if            
        !    end do ElemDistr
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
             ! TRIBAREA = 0 for a node connecting to wet or partly wet elements.
             if (TRIBAREA (m)/= 0. ) then
          !   NodalSource = NodalSource +  WastedSourceTerm * 1000. / (TRIBAREA (m) * DELT ) !* 3600.)
               NodalSource = WastedSourceTerm * 1000. / (TRIBAREA (m) * DELT ) !   [kg] * [1000 g/kg] /[m^2.s]
             ! Nodal Source term is in [g]/[m^2.s]
             else 
               call EffectiveArea (DryTRIBAREA,m)
               NodalSource = WastedSourceTerm * 1000. / (TRIBAREA (m) * DELT ) !   [kg] * [1000 g/kg] /[m^2.s]
               fenode(m).Sed_source = fenode(m).Sed_source + NodalSource 
               EXTLD (m)            = EXTLD (m) + NodalSource        !   The fe_node_sed_source is tottaly identical to the EXTLD.
             ! EXTLD (m)            = EXTLD (m) + fenode(m).Sed_source
             end if
 end do NodDistr
 end do LeftRightBank
 
end subroutine DistributeWastedMass
!-----------------------------------------------------------------
subroutine HasPrnodeFenode (ThisProfile, ProfileNode , increment)
! this subroutine skips the non-conjugate profile nodes and find the required profile nodes
! with conjugate Fe node.
! Since it is not decided yet to retain non-conjugate profile nodes (due to the problem of 
! later updating of their elevation, which have no conjugate fe-node to do that), 
! this subroutine will remain at the moment inactive.!!!
 USE Types
 
 implicit none 
 
 type (Profile)   , intent (in)    :: ThisProfile
 integer          , intent (inout) :: ProfileNode
 integer          , intent (in)    :: increment
 
 integer          :: i , j, nxt,temp
 
 nxt = 1                !mutiplication factor to find the next profile node with conjugated Fe node in finding bank-toe.

    if (ThisProfile.Prnode(ProfileNode).fe_nodenumber == 0) then     
    temp = ProfileNode
  
  ffn:do 
     
       if (ThisProfile.prnode(temp + increment*nxt).fe_nodenumber /= 0 )then 
       ProfileNode = temp + increment*nxt
       exit ffn
       end if
       nxt = nxt + 1
     
      end do ffn
     
    end if
end subroutine   HasPrnodeFenode  

!------------------------------------
subroutine MakeFenodes (fenod, no, callcount)

USE BLK10MOD , only      :ao, wsll,maxe, ncorn, nop
USE Types
USE INIT_TYPE
USE share_profile , only : BANKPROFILES
USE ASSIGNROFILES , only : SIZ
implicit none
     INTEGER , INTENT (in) :: no ,callcount 
     TYPE(finite_element_node), DIMENSION (no), INTENT (OUT) :: Fenod    

integer :: i, istat, j, k,nn

SELECT CASE ( callcount)
 
 CASE(1)         ! initilize the fenod array to zero.
 
 CALL INIT_PROFILE(SIZE = no,fenode = FeNod)

 CASE (2)        ! initialize the fenode array with the model data of input.for

fenod.typ ='corner'
fenod.sed_source = 0.0
fenod.Profile_Number = 0
!fenod.x_cor = 0.0
!fenod.y_cor = 0.0
 
fenod.statuss ='deactivate'
  
  do i = 1,no
     fenod(i).Node_Number = i
     fenod(i).elevation = ao(i)
     fenod(i).water_level = wsll(i)          
  
  ! find each fenod to which profile belongs.
 !    do j = 1,SIZ
 !     do k = 1,200
 !       nn = BANKPROFILES (j).Prnode(k).fe_nodenumber
 !       if (i == nn) then
 !        fenod(i).Profile_Number = j
 !        BANKPROFILES (j).Prnode(k).Water_elev = fenod(i).water_level
 !       end if
 !     end do   
 !    end do
     
  end do
   
   ! Assign profileID of each fenode.
     do j = 1,SIZ
      do k = 1,BANKPROFILES (j).max_nodes
        nn = BANKPROFILES (j).Prnode(k).fe_nodenumber
        if (nn /= 0) then
         fenod(nn).Profile_Number = j
         BANKPROFILES (j).Prnode(k).Water_elev = fenod(nn).water_level
        end if
      end do   
     end do
  ! find the midside nodes
  DO i = 1, maxe
   DO j =  2,ncorn(i),2
     fenod ( NOP(i,j) ).typ='midside'
   end do
  end do   
 
 ! HN JUNE 2009
 ! THE FOLLOWING PART (UPDATING FENODES) HAS BEEN MOVED TO A SEPARATE SUBROUTINE CALLED UpdateFenodes.   

!case (3)               ! Update fenod in dynamic (unsteady) simulation
! check if the node elevation has changed, activate the stattus of fenode and activate the corresponding bankprofile.
 ! DO I = 1, NO
  
  ! if ( abs (fenod(i).elevation - ao(i) ) > 0.005) then
!         fenod(i).statuss ='activate'
 !   if ( fenod(i).Profile_Number /= 0) then
  !   nn = fenod(i).Profile_Number
   !  BankProfiles(nn).Activation = .TRUE.
 !   end if
  ! end if
    
 ! END DO

END SELECT
end subroutine  MakeFenodes 
 
 !----------------------------------------------------------      
subroutine UpdateFenodes (fenod, no)

USE BLK10MOD      , only : ao, wsll
USE Types
USE share_profile , only : BANKPROFILES

implicit none
     
     INTEGER , INTENT (in) :: no  
     TYPE(finite_element_node), DIMENSION (no), INTENT (INOUT) :: Fenod    

     integer :: i, nn


! check if the node elevation has changed, activate the stattus of fenode and activate the corresponding bankprofile.
  DO I = 1, NO
  fenod(i).water_level = wsll(i)
  
   if ( abs (fenod(i).elevation - ao(i) ) > 0.005) then
     fenod(i).statuss ='activate'
     fenod(i).elevation = ao(i)
    if ( fenod(i).Profile_Number /= 0) then
     nn = fenod(i).Profile_Number
     BankProfiles(nn).Activation = .TRUE.
    end if
   end if
    
  END DO

end subroutine  UpdateFenodes 
       

     