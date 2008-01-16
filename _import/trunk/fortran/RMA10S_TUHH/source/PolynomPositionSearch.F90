!     Last change:  WP   10 Jan 2008    7:06 pm
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

if (entries == 1) then
  FindPolynom = entries
elseif (rangearray (entries) /= rangearray (entries-1)) then
  FindPolynom = entries
else
  FindPolynom = entries - 1
endif
RETURN

END
