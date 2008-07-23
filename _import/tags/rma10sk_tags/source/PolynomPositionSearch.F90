!     Last change:  NIS   5 Jun 2008    8:50 am
!function to get the valid polynom to apply
!TODO: Replace it by binary search
FUNCTION FindPolynom (rangearray, depth, entries)

implicit none

!output/ function
!****************
INTEGER :: FindPolynom
!input
!*****
INTEGER, INTENT (IN) :: entries
REAL (KIND = 8), INTENT (IN) :: rangearray(1:*)
REAL (KIND = 8), INTENT (IN) :: depth
!locals
!******
INTEGER :: i


findit: DO i = 1, entries
  IF (depth <= rangearray(i)) THEN
    FindPolynom = i
    RETURN

  if (depth > rangearray (i) .and. i == entries) then
    FindPolynom = i + 1
    return
  end if
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
