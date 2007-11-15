!     Last change:  WP    9 Nov 2007    7:58 am
!function to get the valid polynom to apply
!TODO: Replace it by binary search
FUNCTION FindPolynom (rangearray, depth, entries)

implicit none

INTEGER :: FindPolynom
INTEGER :: i
INTEGER :: entries
REAL (KIND = 8) :: rangearray(1:*)
REAL (KIND = 8) :: depth


findit: DO i = 1, entries
  IF (depth <= rangearray(i)) THEN
    FindPolynom = i
    RETURN
  ENDIF
ENDDO findit

END
