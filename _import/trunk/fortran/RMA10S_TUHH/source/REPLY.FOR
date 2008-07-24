      SUBROUTINE REPLY (ANAM, ISWT)
      
      !Declarations
      CHARACTER (len = 32) :: ANAM
      !Switch to be as reply from anam
      integer :: iswt

      ISWT = 0

      IF(ANAM == 'NO' .or. anam == 'no' .or.
     +   anam == 'NONE' .or. anam == 'none') then
        ISWT = 1
      elseif (ANAM == 'NOMORE' .or. ANAM == 'nomore' .or.
     +        ANAM == ' ') then
        ISWT=2
      endif 
      
      RETURN
      END
