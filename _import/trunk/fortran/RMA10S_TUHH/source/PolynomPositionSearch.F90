!     Last change:  NIS   5 Jun 2008    8:50 am
!function to get the valid polynom to apply
!TODO: Replace it by binary search
FUNCTION FindPolynom (rangearray, depth, entries, xcord, ycord, node)

implicit none

!output/ function
!****************
INTEGER :: FindPolynom
!input
!*****
INTEGER (kind = 4), INTENT (IN) :: entries, node
REAL (KIND = 8), INTENT (IN) :: rangearray(1:*)
REAL (KIND = 8), INTENT (IN) :: depth
real (kind = 8), intent (in) :: xcord, ycord
!locals
!******
INTEGER :: i

findit: DO i = 1, entries
  IF (depth <= rangearray(i)) THEN
    FindPolynom = i
    RETURN
  elseif (depth > rangearray (i) .AND. i == entries) then
    !nis,aug08: If no entry found, value range too small, stop!
    !weirs and bridges
    if (node == 0) then
      call errormessageandstop (4004, 0, xcord, ycord)
    else
      !no error at the moment, but there will be a warning
      !call errormessageandstop (4005, node, xcord, ycord)
      findPolynom = i
      return
    endif
  end if
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
