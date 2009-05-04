! Last change:  MD   13 Jan 2009    6:08 pm

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
  IF (lambdaTot(N).ge.0.0) THEN
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
  IF (IMAT(N).LT.900 .OR. IMAT(N).GT.999) then
    NCN=NCORN(N)
    DO M=1,NCN
      DO I=1,12
        IF(ELTON(NOP(N,M),I) .EQ. N) THEN
          GO TO 250
        ENDIF
        IF(ELTON(NOP(N,M),I) .EQ. 0) THEN
          ELTON(NOP(N,M),I) = N
          GO TO 250
        ENDIF
      ENDDO
      WRITE(75,*) 'STOP!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
      WRITE(75,*) (ELTON(NOP(N,M),I),I=1,12)
      WRITE(*,*) 'STOP!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
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
DO N=1,NP         ! over all nodes N
  TRIBAREA(N)=0.
  FFACT_KN(N)=0.0
  DO I=1,12         ! over all neighboured elements I to node N
    M=ELTON(N,I)
    IF(M .GT. 0) THEN
      IF(NCORN(M) .EQ. 8) THEN    ! for quadrangle elements
        FACT=4.
      ELSE                        ! for triangle elements
        FACT=3.
      ENDIF

      if((wsll(N) .ge. ao(N)) .and. (AREA(M).gt.0.))then !Nur wenn knoten Nass!
        AREA_PART(N,I)=AREA(M)/FACT
        ! Teilflaeche je Element M zum Knoten N
        TRIBAREA(N)=TRIBAREA(N)+AREA(M)/FACT
        ! Gesamte-Einflussflaeche je Knoten N
        FFACT_TEMP(N,I)=FFACT_EL(M)
        ! Lambda-Wert je Element M
        FFACT_KN(N)= FFACT_KN(N) + (FFACT_TEMP(N,I)*AREA_PART(N,I))
        ! Summation der Teilwerte lambda * Teilfl�che
      endif
    ENDIF
  ENDDO

  IF (TRIBAREA(N).gt.0.) THEN
    FFACT_KN(N)= FFACT_KN(N) / TRIBAREA(N)
  END IF

  IF (TRIBAREA(N).le.0. .and. FFACT_KN(N).gt.0.0) THEN
    if(wsll(N) .ge. ao(N)) then !Nur wenn knoten nass!
      WRITE(*,*) 'STOP!  NO Area was found for NODE: ',N
      STOP
    else !wenn knoten trocken
      FFACT_KN(N)= 0.00
    endif
  END IF

  IF (FFACT_KN(N).le.0.0 .and. TRIBAREA(N).gt.0.) THEN
    if(wsll(N) .ge. ao(N)) then !Nur wenn knoten Nass!
      WRITE(*,*) 'STOP!  NO friction factor was found for NODE: ',N
      STOP
    endif
  END IF

  IF (FFACT_KN(N).eq.0.0 .and. TRIBAREA(N).eq.0.) THEN
    if(wsll(N) .ge. ao(N)) then !Nur wenn knoten Nass!
      WRITE(LOUT,*) 'ACHTUNG: NO friction factor and NO Area was found for NODE: ',N
    endif
  END IF

  IF (FFACT_KN(N).lt.0.0) THEN
    FFACT_KN(N)= 0.00
  END IF

  IF (N.eq.NP) THEN
    WRITE(75,*) 'All Friction factors are calculated till NODE ',N
  END IF
  ! WRITE(75,*) ' Friction factor at NODE ',N, ' is:',FFACT_KN(N)
ENDDO

deallocate (FFACT_TEMP , AREA_PART)
RETURN
END