!     Last change:  NIS  28 Apr 2008    8:14 pm
!IPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
!IPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
SUBROUTINE FLDIR

USE BLK10MOD
SAVE

!nis,apr08,com : There is a problem with TF(ALFA). What does it mean? Is it an intrinsic INTEL function?
!implicit none

!global variables with read access
!*********************************
!NOPS (M, N) = nodes (N = 1, 20) of element M stored in sequence (corner node - midisde node - corner node - ...; although not clear for 3D-elts)
!IMAT (M)    = material type of the element M
!CORD (M, I) = coordinates of a node M, (for x: I = 1; for y: I = 2)
!NDEP (M)    = number of depedending nodes above one node M (for 3D and width averaged applications)
!ALFA (M)    = global flow direction related to global x-axis at the node M
!NP          = number of nodes in the mesh
!NPM         = number of 3D nodes in the mesh (NP plus 3D-referenced nodes)
!NEM         = number of 3D elements in the mesh

!global variables with write access
!**********************************
!DIR (N)  = direction factor (-1.0 or 1.0) of a node N
!ITAB (N) = stores an element, where a node N belongs to

!local definitions
!*****************
INTEGER :: K, M, N
INTEGER :: NRF, N1
INTEGER :: L, L2, L3
REAL (KIND = 8) :: DX, DY

!-
!...... Routine to establish flow direcions for one- dimensional elements
!-
!IPK AUG05      DIMENSION ITAB(MNP)
!-
!...... Analyse the elements for connections
!-
!NiS,may06m,com: initialize the ITAB-array
DO N = 1, NPM
  ITAB (N) = 0
enddo

!run through all elements to remember the 1D elements, a node is connected to
DO M = 1, NEM
!if element is not deactivated  
  IF (IMAT (M) > 0) THEN

!IPK DEC00 ALLOW FOR GATE STRUCTURE

!if element is either    
!  imat < 900  : normal element    
!  imat > 5000 : normal 3D-element    
!  IGTP /= 0   : gate type element    
    IF (IMAT (M) < 900 .OR. IMAT (M) > 5000 .OR. IGTP (M) /= 0) THEN

!if the sixth node of the element, which would be the third corner node is 0, then it      
!is a 1D-element and it has to be processed      
      IF (NOPS (M, 6) == 0) THEN

!check only for the two corner nodes of the 1D element        
        DO K = 1, 3, 2
!get the node number          
          N1 = NOPS (M, K)
!remember for the node N1 the element it is connected with          
          ITAB (N1) = M
        ENDDO
      ENDIF
    ENDIF
  ENDIF
enddo

!run through all elements
ForAllElts: DO N = 1, NEM
!consider only control structure elements and junction elements  
  IF (IMAT (N) > 900 .AND. IMAT(N) < 1000) THEN
!-
!...... Select a node to establish direction
!-
    forAllIntNodes: DO K = 1, 8

!get node K in element N      
      NRF = NOPS (N, K)

!if node is defined (NRF == 0) then cycle the loop to get the next node      
      IF (NRF == 0) exit forAllIntNodes

!-
!...... Search other elements for junction nodes
!-
        M = ITAB (NRF)

!IPK DEC00 ALLOW STRUCTURE ELEMENTS
        IF (M == 0 .AND. (IMAT(N) > 903 .AND. IMAT(N) < 1000)) CYCLE forAllIntNodes

          IF (M == 0 .OR. M > NE) then
!IPK SEP04 CREATE ERROR FILE
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
            WRITE (75, 720) NRF, M, (L, ITAB (L), L = 1, NP)
            WRITE ( *, 720) NRF, M, (L, ITAB (L), L = 1, NP)
  720 FORMAT ('  ERROR IN ELEMENT CONNECTIONS DETECTED IN FLDIR'/ '  MATCHING NODE',I6,'  ELEMENT APPARENTLY',I5/ &
     &        '  TABLE OF ELEMENTS CONNECTED TO NODES AS FOLLOWS'/ 5 (I8, I6))

            STOP

          ELSE
            DO L = 1, 3, 2
              IF (NOPS (M, L) == NRF) THEN
!-
!...... When match is found get slope with respect to 's'
!-
            L2 = NOPS (M, 2)
            IF (L == 1) THEN
              L3 = NOPS (M, 3)
            ELSE
              L3 = NOPS (M, 1)
            ENDIF

            DX = (CORD (L3, 1) - CORD (NRF, 1)) / 2.
            DY = (CORD (L3, 2) - CORD (NRF, 2)) / 2.
            IF (ABS (DX) > ABS (DY)) THEN
              IF (DX < 0.) THEN
                DIR (NRF) = -1.
              ELSE
                DIR (NRF) = 1.
              ENDIF
            ELSE
              IF (DY < 0.) THEN
                DIR (NRF) = -1.
              ELSE
                DIR (NRF) = 1.
              ENDIF
!What is TF (ALFA (NRF))???              
              IF (TF (ALFA (NRF)) < 0.) DIR (NRF) = -DIR (NRF)
            ENDIF
          ENDIF
        enddo
      endif
    ENDDO forAllIntNodes
  ENDIF
enddo ForAllElts

!NiS,may06,com: für jeden 2D-horizontal averaged elements; alle Knoten mit mehr als einem Layer (NDEP(N) > 1)
DO N = 1, NPM
  IF (NDEP (N) > 1) THEN
    DO K = 2, NDEP (N)
      L = NREF (N) + K - 1
      DIR (L) = DIR (N)
    enddo
  ENDIF
enddo

RETURN
END
