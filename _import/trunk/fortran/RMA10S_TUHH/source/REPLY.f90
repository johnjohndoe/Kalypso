      SUBROUTINE REPLY (ANAM, ISWT)
      
      !Declarations
      CHARACTER (len = 32) :: ANAM
      !Switch to be as reply from anam
      integer :: iswt

      ISWT = 0

      IF(ANAM == 'NO' .OR. anam == 'no' .OR. 
     +   anam == 'NONE' .OR. anam == 'none') then
        ISWT = 1
      elseif (ANAM == 'NOMORE' .OR. ANAM == 'nomore' .OR. 
     +        ANAM == ' ') then
        ISWT=2
      endif 
      
      RETURN
      END
