!     Last change:  WP   31 Aug 2007    6:31 pm
      MODULE BLKASTEPH
!NiS,mar06: This module replaces common block definitions in the program code of Kalypso2D.
!           The reason is to make the allocatable option for the arrays possible, because the
!           old way of declaring did not work. This is oriented at future.
!           This module is used in the subroutines:
!                       reord_Kalyps, order_Kalyps, adjpt
!           that are capable to read Kalypso-2D geometry-data.
!           The arrays are allocated and deallocated in the reorder-subroutine.
!
      INTEGER              :: nepem, nr, nc, nprt, mist
      integer (kind = 8) :: mpq
      INTEGER, ALLOCATABLE :: mlist(:), msn(:), icol(:,:), nadm(:),     &
     &                        inum(:)
!
!
      END MODULE 