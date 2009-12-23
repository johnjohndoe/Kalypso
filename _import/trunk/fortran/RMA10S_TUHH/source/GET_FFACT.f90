! Last change:  MD   15 May 2009    9:41 am

!--------------------------------------------------------------
!MD: 28.07.2008
!
!    Subroutine zur Berechnung des Friction Factors FFACT(ELE)
!    an jedem Element aus dem Restart (Kalypso.2d-file)
!
!    Transformation for elementwise Friction Factor to
!    a Friction Factor at each node
! --------------------------------------------------------------
 SUBROUTINE GET_FFACT
 USE BLK10MOD, only : maxp, ne, imat, ncn, ncorn, nop, fact, np, wsll, ao, area, lout
 USE BLKSANMOD, only : ffact_el, elton, tribarea, ffact_kn  
 USE ParaKalyps, only : lambdatot
 implicit none
 
! REAL(KIND=8) :: FACT 
 REAL(KIND=8), allocatable :: FFACT_TEMP(:,:)
 REAL(KIND=8), allocatable :: AREA_PART(:,:)
 INTEGER (KIND=4) :: I,N,M
! -------------------------------------------------------------
!MD:  Preparing Restart FFACT for each Element
allocate (FFACT_TEMP (1:maxp,1:12), AREA_PART(1:maxp,1:12))
DO N=1,NE
  IF (lambdaTot(N) >= 0.0) THEN
    FFACT_EL(N) = lambdaTot(N)/8.0
  Else
    WRITE(75,*) 'STOP!  NO LambdTot availabe at ELEMENT Nummer',N
    WRITE(*,*)  'STOP!  NO LambdTot availabe at ELEMENT Nummer',N
    STOP
  END IF
END DO


!    Routine to get elements connected to nodes and allocate areas
!MD: Setting ELTON(Node,neighboured element) to Elementnumber
DO N=1,NE
  IF (IMAT(N) < 900 .OR. IMAT(N) > 999) then
    NCN=NCORN(N)
    DO M=1,NCN
      DO I=1,12
        IF(ELTON(NOP(N,M),I) == N) THEN
          GO TO 250
        ENDIF
        IF(ELTON(NOP(N,M),I) == 0) THEN
          ELTON(NOP(N,M),I) = N
          GO TO 250
        ENDIF
      ENDDO
      WRITE(75,*) 'STOP!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
      WRITE(75,*) (ELTON(NOP(N,M),I),I=1,12)
      WRITE(*,*) 'STO!!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
      STOP
      250  CONTINUE
    ENDDO
  ENDIF
ENDDO


!  Next assign areas based on straight side assumption
!  Weighting the friction factors FFACT_EL of neighboured Elements
!    over their influence Area to the node N: AREA_PART(N,I)
!    Summ over all area-weighted the friction factors is diveded by
!    whole area TRIBAREA(N)
FACT=0.0
! over all nodes N
DO N=1,NP         
  TRIBAREA(N)=0.
  FFACT_KN(N)=0.0
! over all neighboured elements I to node N
  DO I=1,12         
    M=ELTON(N,I)
    IF(M > 0) THEN
! for quadrangle elements
      IF(NCORN(M) == 8) THEN    
        FACT=4.
! for triangle elements
      ELSE                        
        FACT=3.
      ENDIF

!Nur wenn knoten Nass!
      if((wsll(N) >= ao(N)) .AND. (AREA(M) > 0.))then 
        AREA_PART(N,I)=AREA(M)/FACT
! Teilflaeche je Element M zum Knoten N        
        TRIBAREA(N)=TRIBAREA(N)+AREA(M)/FACT
! Gesamte-Einflussflaeche je Knoten N        
        FFACT_TEMP(N,I)=FFACT_EL(M)
! Lambda-Wert je Element M        
        FFACT_KN(N)= FFACT_KN(N) + (FFACT_TEMP(N,I)*AREA_PART(N,I))
! Summation der Teilwerte lambda * Teilfläche        
      endif
    ENDIF
  ENDDO

  IF (TRIBAREA(N) > 0.) THEN
    FFACT_KN(N)= FFACT_KN(N) / TRIBAREA(N)
  END IF

  IF (TRIBAREA(N) <= 0. .AND. FFACT_KN(N) > 0.0) THEN
!Nur wenn knoten nass!
    if(wsll(N) >= ao(N)) then 
      WRITE(*,*) 'STOP!  NO Area was found for NODE: ',N
      STOP
!wenn knoten trocken
    else 
      FFACT_KN(N)= 0.00
    endif
  END IF

  IF (FFACT_KN(N) <= 0.0 .AND. TRIBAREA(N) > 0.) THEN
!Nur wenn knoten Nass!
    if(wsll(N) >= ao(N)) then 
      WRITE(*,*) 'STOP!  NO friction factor was found for NODE: ',N
      STOP
    endif
  END IF

  IF (FFACT_KN(N) == 0.0 .AND. TRIBAREA(N) == 0.) THEN
!Nur wenn knoten Nass!
    if(wsll(N) >= ao(N)) then 
      WRITE(LOUT,*) 'ACHTUNG: NO friction factor and NO Area was found for NODE: ',N
    endif
  END IF

  IF (FFACT_KN(N) < 0.0) THEN
    FFACT_KN(N)= 0.00
  END IF

  IF (N == NP) THEN
    WRITE(75,*) 'All Friction factors are calculated till NODE ',N
  END IF
! WRITE(75,*) ' Friction factor at NODE ',N, ' is:',FFACT_KN(N)  
ENDDO

deallocate (FFACT_TEMP , AREA_PART)
RETURN
END