C     Last change:  JAJ  18 May 2006   10:00 pm
      MODULE BLK2
!NiS,mar06: 	This module replaces common block definitions in the program code of Kalypso2D.
!               The reason is to make the allocatable option for the arrays possible, because the
!               old way of declaring did not work. This is oriented at future.
!               This module is used in the subroutines:
!                       reord_Kalyps, order_Kalyps, adjpt
!               that are capable to read Kalypso-2D geometry-data.
!               The arrays are allocated and deallocated in the reorder-subroutine.

      INTEGER     , ALLOCATABLE :: iel(:), list(:), ihold(:)
      REAL(KIND=4), ALLOCATABLE :: wd(:), paxis(:)

      END MODULE 