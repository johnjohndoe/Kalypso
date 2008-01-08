!     Last change:  WP   17 Dec 2007    7:50 pm
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

if (rangearray (entries) /= rangearray (entries-1)) then
  FindPolynom = entries
else
  FindPolynom = entries - 1
endif
RETURN

END
