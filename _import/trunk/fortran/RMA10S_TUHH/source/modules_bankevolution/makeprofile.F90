module make_profile

USE TYPES
USE mod_ContiLines
USE INIT_TYPE
USE MOD_NODES
USE MOD_ARCs
USE BLK10MOD , only : ao

CONTAINS 

SUBROUTINE MAKEPROFILE (CCLine, NumberFenodes, bankprofile,WithMideSide)

IMPLICIT NONE 

TYPE (contiLine)  , pointer     :: CCLine
type (profile)    , INTENT (out):: BankProfile
INTEGER           , INTENT (IN) :: NumberFenodes 
LOGICAL           , INTENT (IN) :: WithMideSide 

TYPE (PROFILE)    , DIMENSION(1):: tmp_Profile
type (linkedNode) , pointer     :: nextNode
type (arc)        , pointer     :: nextarc
INTEGER                         :: j

! Inintilaise the new profile
!---------------------------------
CALL INIT_PROFILE (tmp_Profile,1)
BankProfile = tmp_profile(1)

!  BUILD PROFILE OUT OF CONTILINE
!------------------------------
 
 write (*,*) 'is CCLine associated ?', associated (CCLine)
 
 BankProfile.cl_number = CCLine.ID
 
 BankProfile.max_nodes = NumberFenodes

 write (*,*) ' NumberFenodes  ',NumberFenodes
 
if ( .NOT. associated (CCLine.firstSegment)) then

! if there is only side nodes the following will be executed, otherwise (midside nodes includes
! then the statement after else is executed.

  NextNode => CCLine.FirstNode
  
 write (*,*) 'NodeNumber     distance    elevation       attribute '

 do j = 1,NumberFenodes
 
  BankProfile.prnode(j).fe_nodenumber = NextNode.ThisNode.ID
  BankProfile.prnode(j).distance = sqrt( ( NextNode.ThisNode.cord (1) -           &
&                                          CCLine.FirstNode.ThisNode.cord (1) ) **2 &
&                                       +  ( NextNode.ThisNode.cord (2) -           &
&                                          CCLine.FirstNode.ThisNode.cord (2) )**2 )
  BankProfile.prnode(j).elevation = NextNode.ThisNode.ao
 
  BankProfile.prnode(j).attribute = 'profile'
 
 write (*,*) BankProfile.prnode(j).fe_nodenumber, '    ' , &
 &           BankProfile.prnode(j).distance, '    ' , &
 &           BankProfile.prnode(j).elevation, '    ' , &
 &           BankProfile.prnode(j).attribute
 
  NextNode => NextNode.next
 
 end do
 
 else 

 nextarc => CCLine.firstsegment
 
!!!!!!!!!  FIRST NODE OF THE SEGMENT   
  j = 1
  
  BankProfile.prnode(j).fe_nodenumber = NextArc.first.ID
  BankProfile.prnode(j).distance = sqrt( ( NextArc.first.cord (1) -           &
&                                          CCLine.FirstNode.ThisNode.cord (1) ) **2 &
&                                       +  ( NextArc.first.cord (2) -           &
&                                          CCLine.FirstNode.ThisNode.cord (2) )**2 )
  BankProfile.prnode(j).elevation = NextArc.first.ao
 
  BankProfile.prnode(j).attribute = 'profile'
 
 write (*,*) BankProfile.prnode(j).fe_nodenumber, '    ' , &
 &           BankProfile.prnode(j).distance, '    ' , &
 &           BankProfile.prnode(j).elevation, '    ' , &
 &           BankProfile.prnode(j).attribute

  do
 
!  if (j >= NumberFenodes -1) exit
 if ( .NOT. associated(nextarc) ) exit
 
 midside: IF ( WithMideSide ) then 
 j = j + 1
 
!!!!!!!!!  MIDSIDE NODE OF THE SEGMENT  
 
  BankProfile.prnode(j).fe_nodenumber = NextArc.midside.ID
  BankProfile.prnode(j).distance = sqrt( ( NextArc.midside.cord (1) -           &
&                                          CCLine.FirstNode.ThisNode.cord (1) ) **2 &
&                                       +  ( NextArc.midside.cord (2) -           &
&                                          CCLine.FirstNode.ThisNode.cord (2) )**2 )
!BankProfile.prnode(j).elevation = NextArc.midside.ao  
  BankProfile.prnode(j).elevation = ao (NextArc.midside.ID)
  BankProfile.prnode(j).attribute = 'profile'
 
 write (*,*) BankProfile.prnode(j).fe_nodenumber, '    ' , &
 &           BankProfile.prnode(j).distance, '    ' , &
 &           BankProfile.prnode(j).elevation, '    ' , &
 &           BankProfile.prnode(j).attribute
 
 end if midside
 j = j + 1

!!!!!!!!!  LAST NODE OF THE SEGMENT   

  BankProfile.prnode(j).fe_nodenumber = NextArc.last.ID
  BankProfile.prnode(j).distance = sqrt( ( NextArc.last.cord (1) -           &
&                                          CCLine.FirstNode.ThisNode.cord (1) ) **2 &
&                                       +  ( NextArc.last.cord (2) -           &
&                                          CCLine.FirstNode.ThisNode.cord (2) )**2 )
  BankProfile.prnode(j).elevation = NextArc.last.ao
  BankProfile.prnode(j).attribute = 'profile'
 
 write (*,*) BankProfile.prnode(j).fe_nodenumber, '    ' , &
 &           BankProfile.prnode(j).distance, '    ' , &
 &           BankProfile.prnode(j).elevation, '    ' , &
 &           BankProfile.prnode(j).attribute
 
 NextArc => NextArc.nextseg
 
 end do 
 end if
 
END SUBROUTINE MAKEPROFILE

end module make_profile