C     Last change:  JAJ  18 May 2006   10:00 pm
      MODULE BLKASTEPH
!NiS,mar06: 	This module replaces common block definitions in the program code of Kalypso2D.
!               The reason is to make the allocatable option for the arrays possible, because the
!               old way of declaring did not work. This is oriented at future.
!               This module is used in the subroutines:
!                       reord_Kalyps, order_Kalyps, adjpt
!               that are capable to read Kalypso-2D geometry-data.
!               The arrays are allocated and deallocated in the reorder-subroutine.

      INTEGER 			:: mpq, nepem, nr, nc, nprt, mist
      INTEGER, ALLOCATABLE      :: mlist(:), msn(:), icol(:,:), nadm(:),
     +                             inum(:)
!NiS,mar06:     The array alpha exists in the original RMA10S-code, so it can not be defined twice. Additionally
!               the array is defined in Kalypso2D but never used. Consequently it is totally deactivated in those
!               subroutine to reorder the mesh:
!     +				   alpha(:)

      END MODULE 