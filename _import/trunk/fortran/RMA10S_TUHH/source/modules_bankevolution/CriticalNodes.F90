! Subroutine to find the deepest point of a side of a profile, and optional the last sub
! -merged node. They are needed for calculation of the bank-toe width, needed for wasted
!  mass distribution at bank-toe.

Module ControlNodes
USE TYPES

contains

subroutine criticalnodes(prof, start, endd, increment, DeepestNode, LastSubmergedNode)



TYPE(profile)  , INTENT (IN)            :: prof
INTEGER        , INTENT (IN)            :: start, endd, increment
INTEGER        , INTENT (out)           :: DeepestNode
INTEGER        , INTENT (out), optional :: LastSubmergedNode

REAL          :: temp
INTEGER       :: i, p 

!?????????????????????   the following algorithm to find local minima should be developed further ?????????????????
!                        since if there is a berm then the local minima, might not be the starting point
!                        where the collapsed bank deposits, but it might be deposited on berm.
!                        or if there is a deep point on the top of the bank, it may assume it as bank-toe.


temp = 1000.

Inn:do i = start,endd,increment

   if (prof%prnode(i)%elevation==0.) cycle
   if( (prof%prnode(i)%attribute =='front').OR.(prof%prnode(i)%attribute =='nose') ) cycle
 
   if (prof%prnode(i)%elevation< temp) then

    temp= prof%prnode(i)%elevation
    DeepestNode = i                             ! local minima, bank-toe
 
   ELSE IF(prof%prnode(i)%elevation>= temp) then

     if (temp > prof%water_elev) then             !HN20April09 if the found local minima is greater than water elevation then it still locates on bank or top of the bank.
       cycle inn                                      ! it is for the case that a deep point is on the top of the bank and create an unwanted local minima.
     else
         EXIT inn
     end if

   end if

  end do inn
  
  if (PRESENT (LastSubmergedNode) )then
    p = -increment

      do i = DeepestNode, start, p


        if ( (prof%prnode(i)%elevation  - prof%water_elev)<-0.001 .AND. &
   &         (prof%prnode(i+p)%elevation - prof%water_elev)<-0.001 )then                   ! In the submerged area, the element is totally submerged

              LastSubmergedNode = i + p
        else
              exit
        end if
      
      end do
   end if


end subroutine  criticalnodes  
End Module controlnodes    
