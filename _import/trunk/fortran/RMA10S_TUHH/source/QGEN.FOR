C     Last change:  K     2 Mar 2007    7:00 pm
cipk  last update feb 09 2001 allow for zero length cc lines

      SUBROUTINE QGEN(J,QREQ,THET,QQAL)
      !nis,sep06,com: Overgiven variables:
      ! J    = number of CCL
      ! QREQ = total discharge across the CCL J
      ! THET = direction of total discharge crossing the CCL; measured from the principal x-axis in ANTICLOCKWISE RADIANS)
      ! QQAL = 3 values (array) for the three concentrations of salinity, temperature and sediment
      !-
      USE BLK10MOD
      USE BLK11MOD
!nis,sep06: Usage of module necessary for using amf-subroutine
      USE BLKDRMOD
!-
!nis,nov06: Usage of ParaKalyps for Darcy-Subroutine
      USE ParaKalyps
!-
      USE PARAFlow1dFE !neues Modul für Teschke-1D-Elemente
      SAVE
C-
C-..... Generate specified total flow boundary conditions
C-
      DIMENSION QQAL(3)

!nis,sep06: new variables
      REAL     :: amec (350), teilline (350), dxl (350)

!nis,sep06: changes for good variable passing
!      REAL     :: lambda, betspeck, betd, fliesstiefe
      REAL         :: lambda, betspeck, betd
      !nis,nov06: cwr_line for intentinout-property of Darcy-subroutine
      REAL         :: cwr_line
      !-
      REAL(KIND=8) :: fliesstiefe, waspi
      !nis,nov06: For proper passing d2 must be KIND=4
      !REAL(KIND=8) :: d1,d2,d3,d1v,d3v
      REAL(KIND=8) :: d1,d3,d1v,d3v,d2_kind8
      REAL         :: d2
      !-
!-

      REAL     :: cos_in_grad, sin_in_grad

      INTEGER max
!-

!nis,sep06: controloutputfile iostat, controloutput declarations
      INTEGER             :: iostaterror
      INTEGER             :: zzz, yy
      CHARACTER (LEN=27)  :: filename

!nis,oct06: For reading the geometry file
      CHARACTER (LEN = 2) :: LineID
      INTEGER             :: tmpnode
      real (KIND=8)       :: tmpvx,tmpvy,tmpdepth,tmpwl
!-

cipk aug05      INCLUDE 'BLK10.COM'
C-
      DATA VOID/-1.E20/

      !nis,sep06,testfileouput-------------------------------------
      iostaterror = 0
      WRITE(filename,'(a,i2.2,a)')  'Randbedingungen_Linie',J,'.txt'
      open (999,filename,iostat = iostaterror)
      if (iostaterror.ne.0) STOP 'file does not work'
      !------------------------------------------------------------

C-
C...... Calculate total projected area
C-
      !nis,sep06,com: MAX matches the number of corner-nodes of the CCL J
      MAX=LMT(J)
CIPK FEB01
        !nis,sep06,com: if there is no node for the CCL, stop the program
	IF(MAX .EQ. 0) THEN
	  WRITE(75,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
	  WRITE(75,*) 'EXECUTION TERMINATED'
	  WRITE(*,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
	  WRITE(*,*) 'EXECUTION TERMINATED'
	ENDIF
      !nis,sep06,com: Use the global direction of the inflow direction
      STQT(J)=THET

      !nis,sep06,com: If the CCL has only 1 node, that means 1D-element
      IF(MAX .EQ. 1) THEN
C-
C...... This is for 1-D element
C-
        K=0
        DO 120 M=1,NE
          K=K+1
          IF(NOP(K,3) .EQ. LINE(J,1)) THEN
            NA=NOP(K,3)
            NC=NOP(K,1)
            IF(NC .GT. NPM) GO TO 120
            GO TO 127
          ENDIF
          IF(NOP(K,1) .EQ. LINE(J,1)) THEN
            NA=NOP(K,1)
            NC=NOP(K,3)
            IF(NC .GT. NPM) GO TO 120
            GO TO 127
          ENDIF
  120   CONTINUE
  127   NM=NOP(K,2)
        !EFa Nov06, Sonderfall für Teschke-1D-Elemente
        !nis,feb07: Allow for numbered FFF midsides
        !if (nm.EQ.-9999) then
        if (nm < -1000) then
        !-
          dx=cord(nc,1)-cord(na,1)
          dy=cord(nc,2)-cord(na,2)
        else
          IF(CORD(NM,1) .GT. VOID) THEN
            DX=2.*(CORD(NM,1)-CORD(NA,1))-0.5*(CORD(NC,1)-CORD(NA,1))
            DY=2.*(CORD(NM,2)-CORD(NA,2))-0.5*(CORD(NC,2)-CORD(NA,2))
          ELSE
            DX=CORD(NC,1)-CORD(NA,1)
            DY=CORD(NC,2)-CORD(NA,2)
          ENDIF
        endif
        BEETA=ATAN2(DY,DX)
        IF(ABS(BEETA-THET) .GT. 1.570796  .AND.
     +     ABS(BEETA-THET) .LT. 4.712394) BEETA=BEETA+3.141596
        SPEC(NA,1)=QREQ*COS(BEETA)
        SPEC(NA,2)=QREQ*SIN(BEETA)
        NFIX(NA)=31000
        IF(QQAL(1) .GE. 0.) THEN
          NFIX(NA)=NFIX(NA)+10
          SPEC(NA,4)=QQAL(1)
          VEL(4,NA)=QQAL(1)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+20
          SPEC(NA,4)=-QQAL(1)
        ENDIF
        IF(QQAL(2) .GE. 0.) THEN
          NFIX(NA)=NFIX(NA)+1
          SPEC(NA,5)=QQAL(2)
          VEL(5,NA)=QQAL(2)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+2
          SPEC(NA,5)=-QQAL(2)
        ENDIF
        IF(QQAL(3) .GE. 0.) THEN
          NFIX1(NA)=1
          SPEC(NA,6)=QQAL(3)
          VEL(6,NA)=QQAL(3)
CIPK OCT01
        ELSE
          NFIX1(NA)=2
          SPEC(NA,6)=-QQAL(3)
        ENDIF

        !nis,feb07,testing
        !WRITE(*,*) 'Vergebe Randbedingung'
        !WRITE(*,*) spec(na,1), spec(na,2)
        !pause
        !-

!nis,sep06: In the 2D-part an additional if control was entered, so
!           that the following if control could not be reached by
!           a 1D-boundary. This is preserved by copying it to this place.
!           The if control number 500 is replaced by actual way of statement.
        IF(ICYC .GT. 0) THEN
          DO K=1,MAX
            NA=LINE(J,K)
            CALL BFORM(NA)
          ENDDO
        ENDIF
!-


!nis,com:**********************************************************
! 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D
!nis,com:**********************************************************


!nis,sep06,com: If CCL has more than 1 node, that means 2D
!nis,jan07,com: condition to come here is: max.ne.1

cipk oc98        VM=SQRT(VEL(1,NA)**2+VEL(2,NA)**2)
C       VEL(1,NA)=VM*COS(BEETA)
C       VEL(2,NA)=VM*SIN(BEETA)
      ELSE
C-
C...... IBN contains counter for number of references to a node
C-

        !nis,sep06,testfileoutput---------------------------------------
        WRITE(999,*) 'Control Output file for checking',
     +               'boundary conditions'
        WRITE(999,*) 'Linie CCL: ', J
        WRITE(999,*) 'Anzahl Knoten: ', lmt(J)
        WRITE(999,*)
        !---------------------------------------------------------------


        !nis,sep06,com: Set every number of elements-reference-counter for every surface node to 0
        DO 103 N=1,NPM
          IBN(N)=0
  103   CONTINUE

        !nis,sep06,com: Objective - get the number IBN for every node of all elements; why is that repeated here; it was done in BLINE.sub
        DO 105 N=1,NE
          !nis,sep06,com: Jump over every deactivated
          IF(IMAT(N) .LT. 1) GO TO 105

          !nis,sep06,com: Jump over control structure elements
          !               Here an overloaded messages is in because, if the value is .lt.1 the code can not reach this code part.
          !               It would be enough to have if(NM.gt.900) with NM=IMAT(N) without ABS()-function
          !-
cipk oct98 update to f90
          NM=ABS(IMAT(N))
          IF(NM .GT. 900) GO TO 105

          !nis,sep06,com: Jump over element, that has no turbulence value
          IF(ORT(NM,1) .EQ. 0.) GO TO 105
          !nis,sep06,com: Get node number (might be 3, 5, 6, 8); but how can a 1D-element reach this code part, because if there was
          !               only one node as boundary condition this part of the subroutine is not used
          NCN=NCORN(N)
          !nis,sep06,com: Treat 1D-2D-transition element as 1D-element; corner node of 2D-transition-element part is not touched; those nodes still
          !               act as boundary nodes.
          IF(NCN .EQ. 5) NCN=3
          !nis,sep06,com: increase IBN by one for every occurance in an element.
          DO 104 M=1,NCN
cipk oct98 update to f90
            !EFa Nov06, Sonderfall für Teschke-1D-Elemente
            !nis,feb07: Allow for numbered FFF midsides
            !if (nop(n,2).NE.-9999) then
            if (nop(n,2) > -1000) then
              K=ABS(NOP(N,M))
              IBN(K)=IBN(K)+1
            else
              !if (nop(n,m).NE.-9999) then
              if (nop(n,m) > -1000) then
                K=ABS(NOP(N,M))
                IBN(K)=IBN(K)+1
              end if
            end if
            !-
  104     CONTINUE
  105   CONTINUE

!nis,jan07,com: BSWIT.eq.1 means original method of rma10s
!NiS,sep06: Activating possibility of chosing the way of discharge boundary condition transformation
        IF (BSWIT.eq.1) THEN

          !nis,sep06,com: Calculate the discharge crosssectional area (wetted area)
          !nis,sep06,com: initialize summed area SUMA
          SUMA=0.

          MAX = LMT(J)-2
          DO 150 K = 1, MAX, 2
            NA = LINE(J,K)
            NC = LINE(J,K+2)
            DX=CORD(NC,1)-CORD(NA,1)
            DY=-(CORD(NC,2)-CORD(NA,2))
            !EFa Nov06, Sonderfall für Teschke-1D-Elemente mit Kilometrierung
            if (kmx(na).NE.-1.and.kmx(nc).NE.-1) then
              !nis,feb07: Scaling kilometers for using meters
              !xl=ABS(kmx(na)-kmx(nc))
              xl=ABS(kmx(na)-kmx(nc)) * 1000
              !-
            else
              XL=SQRT(DX**2+DY**2)
            end if
            ALP=ATAN2(DX,DY)
            D1=VEL(3,NA)
            D3=VEL(3,NC)
            D2=(D1+D3)/2.
            SUMA = SUMA+XL*COS(ALP-THET)*D2
  150     CONTINUE
C-
C...... Compute velocity required
C-
          SUMA=ABS(SUMA)
          !nis,testing
          !  WRITE(*,*) 'Abfluss ist: ',qreq
          !-
          VEST=QREQ/SUMA
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
          MAX=MAX+2
          DO 300 K=1,MAX
            NA=LINE(J,K)
            D1=VEL(3,NA)
            SPEC(NA,1)=VEST*D1*COS(THET)
            SPEC(NA,2)=VEST*D1*SIN(THET)
            IF(SPEC(NA,1) .NE. 0.) THEN
              NFIX(NA)=31000
            ELSE
              NFIX(NA)=13000
            ENDIF

            !NiS,sep06,com: This is just for water-quality calculation
            IF(QQAL(1) .GE. 0.) THEN
              NFIX(NA)=NFIX(NA)+10
              SPEC(NA,4)=QQAL(1)
              VEL(4,NA)=QQAL(1)
CIPK OCT01
            ELSE
              NFIX(NA)=NFIX(NA)+20
              SPEC(NA,4)=-QQAL(1)
            ENDIF
            IF(QQAL(2) .GE. 0.) THEN
              NFIX(NA)=NFIX(NA)+1
              SPEC(NA,5)=QQAL(2)
              VEL(5,NA)=QQAL(2)
CIPK OCT01
            ELSE
              NFIX(NA)=NFIX(NA)+2
              SPEC(NA,5)=-QQAL(2)
            ENDIF
            IF(QQAL(3) .GE. 0.) THEN
              NFIX1(NA)=1
              SPEC(NA,6)=QQAL(3)
              VEL(6,NA)=QQAL(3)
CIPK OCT01
            ELSE
              NFIX1(NA)=2
              SPEC(NA,6)=-QQAL(3)
            ENDIF
  300     CONTINUE
C-
C...... Find correct components for boundary nodes
C-
!NiS,sep06,com: Objective is to find the right direction of the node inflow, parallel to the boundary at the inflow
          DO 400 K=1,MAX,MAX-1
            NA=LINE(J,K)
C-
C...... Locate element with this node
C-
            DO 340 N=1,NE
              IF(IMAT(N) .EQ. 0) GO TO 340
              NCN=NCORN(N)
              DO 310 MM=1,NCN
                M=MM
cipk oct98 update to f90
                IF(ABS(NOP(N,M)) .EQ. NA) GO TO 315
  310         CONTINUE
              GO TO 340
C-
C....... Found a match node. Now determine side for parallel flow
C-
  315         IF(K .EQ. 1) THEN
                KK=2
              ELSE
                KK=MAX-1
              ENDIF
              MLW=M-1
              IF(MM .EQ. 1) THEN
                MLW=NCN
              ELSE
                MLW=M-1
              ENDIF
cipk oct98 update to f90

cipk oct98            MLX=ABS(NOP(N,MLW))
              MSL=MOD(MLW+2,NCN)
              IF(MSL .EQ. 0) MSL=NCN
              MFR=MOD(MLW+3,NCN)
cipk oct98 update to f90
              MSL=ABS(NOP(N,MSL))
              IF(MSL .EQ. LINE(J,KK)) GO TO 317
              IF(IBN(MSL) .EQ. 1) GO TO 330
  317         CONTINUE
              MLW=MOD(M+1,NCN)
              IF(MLW .EQ. 0) MLW=NCN
cipk oct98 update to f90
cipk oct98            MLX=ABS(NOP(N,MLW))
              IF(MLW .GT. 2) THEN
                MSL=MLW-2
                MFR=MLW-3
              ELSE
                MSL=NCN
                MFR=NCN-1
              ENDIF
cipk oct98 update to f90
              MSL=ABS(NOP(N,MSL))
              IF(MSL .EQ. LINE(J,KK)) GO TO 340
              IF(IBN(MSL) .EQ. 1) GO TO 330
              GO TO 340
  330         CONTINUE
cipk oct98 update to f90
              MFR=ABS(NOP(N,MFR))
C-
C...... Now compute boundary angle
C-
              DX=2.*(CORD(MSL,1)-CORD(NA,1))
     +           -0.5*(CORD(MFR,1)-CORD(NA,1))
              DY=2.*(CORD(MSL,2)-CORD(NA,2))
     +           -0.5*(CORD(MFR,2)-CORD(NA,2))
              BEETA=ATAN2(DY,DX)
C-
C...... Find angle of side that flow crosses
C-
              IF(K .EQ. 1) THEN
                NAD=LINE(J,3)
              ELSE
                NAD=LINE(J,MAX-2)
              ENDIF
              DXA=CORD(NAD,1)-CORD(NA,1)
              DYA=-(CORD(NAD,2)-CORD(NA,2))
              AALFA=ATAN2(DXA,DYA)
              IF(ABS(AALFA-THET) .GT. 1.570796  .AND.
     +           ABS(AALFA-THET) .LT. 4.712394) THEN
                AALFA=AALFA+3.141596
              ENDIF
              IF(ABS(AALFA-BEETA) .GT. 1.570796  .AND.
     +           ABS(AALFA-BEETA) .LT. 4.712394) THEN
                BEETA=BEETA+3.141596
              ENDIF
C-
C...... Finally adjust flows
C-
              IF(COS(THET) .NE. 0.) THEN
              QMAG=SPEC(NA,1)*COS(AALFA-THET)/
     +             (COS(THET)*COS(AALFA-BEETA))
              ELSE
              QMAG=SPEC(NA,2)*COS(AALFA-THET)/
     +             (SIN(THET)*COS(AALFA-BEETA))
              ENDIF
              SPEC(NA,1)=QMAG*COS(BEETA)
              SPEC(NA,2)=QMAG*SIN(BEETA)
              GO TO 400
  340       CONTINUE
  400     CONTINUE

!nis,sep06: This occurs at the end now
!           Because of deactivating, the following part has to be inserted
!           above in the 1D-part.
!        ENDIF
!-
        IF(ICYC .GT. 0) THEN
          DO 500 K=1,MAX
            NA=LINE(J,K)
            CALL BFORM(NA)
  500     CONTINUE
        ENDIF


      !NiS,sep06: Using the Kalypso-2D-option for Q-boundary condition transformation
      ELSEIF (BSWIT.eq.2) then

        !Nis,oct06: Problem is, that in Kalypso2D, version 1.2.4 (Ocotber 2006), the boundary conditions are later in assigned as
        !           the initial conditions. In RMA10S it behaves the other way round. Because of that the algorithm in Kalypso2D for setting the
        !           BCs can not directly be applied in RMA10S. Therefore the rdKalypso-subroutine has to be changed in a new way,
        !           that it can get the initial conditions for the boundary lines from the input file.
        !
        !Getting the initial solution for first iteration, if restarting
        do i = 1, lmt(j)
          iostaterror = 0
          !Nis,oct06: Only possible if it is a restart input file in Kalypso2D-format
          if (nb.ne.0 .and. igeo.eq.2) then
            REWIND(nb)
            reading: do WHILE (iostaterror.eq.0)
              read (nb,'(a2)') LineID
              if (LineID.eq.'VA') then
                BACKSPACE(nb)
                read (nb,*) LineID, tmpnode,tmpvx,tmpvy,tmpdepth,tmpwl
                if (tmpnode.eq.line(j,i)) then
                  VEL(1,tmpnode)=tmpvx
                  VEL(2,tmpnode)=tmpvy
                  VEL(3,tmpnode)=tmpdepth

                  EXIT reading
                endif
              endif
            enddo reading
            REWIND(nb)
          endif
        enddo
        !-

        !nis,jan07,com: starting node, ending node, chord length from starting to ending node
        na = line (j, 1)
        nc = line (j, lmt (j) )
        dx = cord (nc, 1) - cord (na, 1)
        !nis,nov06: for vector usage, the coordinates have to be from the same sign
        !dy = - (cord (nc, 2) - cord (na, 2) )
        dy = (cord (nc, 2) - cord (na, 2) )
        !-

!nis,dec06: deactivating, confuses!
!        !pl     Anfang
!        !       260199
!        IF (dx.lt.0.00001.and.dx.gt. - 0.00001.and.
!     +    dy.lt.0.00001.and.dy.gt. - 0.00001) then
!          WRITE (Lout, * ) 'Nr.', 'Knoten m', 'Knoten c', 'Knoten a'
!          WRITE (Lout, * ) '6.', nm, nc, na
!        ENDIF
!        !pl   Ende
!-

        alp = atan2 (dx, dy)
        ! alp ist der Winkel zwischen x-Achse und Liniensehne
        ! thet=thet+alp
        ! thet ist jetzt der Winkel zwischen x-Achse und der Senkrechten
        ! der Einstroemgeschwindigkeiten

        !nis,nov06,testing: test for irregular angles
        !if (ABS(thet).gt.2*3.141596.or.ABS(alp).gt.2*3.141596) then
        !  WRITE(*,*) 'Einstroemwinkel in QGEN, BSWIT.eq.2 zu hoch!!!'
        !end if

        !nis,nov06: Scalarproduct of continuity line chord's normal vector and
        !           Q-flow direction
        !testing:
        !WRITE(*,*) 'line:', j
        !WRITE(*,*) 'alp:', alp
        !WRITE(*,*) 'thet:',thet
        !thet = (dx*COS(thet)+dy*SIN(thet))/(SQRT(dx**2+dy**2))
        !WRITE(*,*) 'thet:', thet
        !-

        !nis,nov06: Setting thet to zero, if angle to small
        !if (thet.lt.1e-3) thet = 0
        !WRITE(*,*) 'thet:', thet
        !-


        ! um Wellen laengs des Einstroemrandes zu vermeiden, die sich bei
        ! wiederholtem durchlaufen dieser Option einstellen koennen, wird
        ! hier erst einmal ein mittlerer Wasserspiegel am Einstroemrand
        ! berechnet:

        !nis,sep06,com: Initializing the Summed waterlevel
        waspi = 0.0
        !nis,dec06: Getting the waterlevel at the continuity line
        call getLineAverageWaterlevel(j,waspi)
        !-

      !nis,sep06,testfileoutput:--------------------------------------------
        !calculation of real average waterlevel
        tmpwl = 0
        do zzz = 1,lmt(j)
          tmpwl = tmpwl + ao(line(j,zzz))+vel(3,line(j,zzz))
          WRITE(999,*)'tmpwl', tmpwl
        end do
        tmpwl = tmpwl/lmt(j)
        !-
       WRITE(999,*) 'average marsh-waterlevel at line',J,':',waspi
       WRITE(999,*) 'average  real waterlevel at line',J,':',tmpwl
       WRITE(999,'(1x,a6,4(1x,a10))')'Knoten','depth','diff.',
     +           'surf.elev', 'real WL'
      WRITE(999,'(1x,a6,4(1x,a10))') '------','----------','----------',
     +                               '----------','----------'
       do zzz = 1,lmt(J)
         yy = line(j,zzz)
         diff = vel(3,yy) - tmpwl + ao(yy)
         hoeh = vel(3,yy)+ao(yy)
         WRITE(999,'(1x,i6,4(1x,f10.6))') yy,vel(3,yy),diff,ao(yy),hoeh
         !nis,oct06,testing
         !WRITE(*,*) yy, vel(3,yy), diff
         !-
       end do
       WRITE(999,*) '------------------------------------------------'
       WRITE(999,*) 'positive sign means WL higher  than average'
       WRITE(999,*) 'negative sign means WL smaller than average'
       WRITE(999,*) '------------------------------------------------'
      !---------------------------------------------------------------------


          ! Zur Verteilung der Fliessgeschwindigkeiten laengs eines Q-Randes
          ! muss ein Proportionalitaetsfaktor 'suma' gebildet werden, der sich
          ! aus der Integration der spezifischen Abfluesse ueber die gesamte
          ! Linie ergibt:

          !nis,sep06,com: Initialize factor suma
          suma = 0.
          !nis,sep06,com: Run over all nodes of CCL
          DO k = 1, lmt (j)
            !nis,sep06,com: Initialize teillinie (Conveyance factors) and dxl (influenced segment length) for every node
            teilline (k) = 0.0
            dxl (k) = 0.0
          END DO

          !nis,sep06,com: For next loop the maximum value is the penultimate CORNER node
          max = lmt (j) - 2


      !nis,jan07,testfileoutput-----------------------------------------------
      do zzz = 1,lmt(j)
        WRITE(999,*) 'lineimat(',j,',',line(j,zzz),'): ',
     +               lineimat(j,zzz)
      end do
      WRITE(999,*) 'Testen der Geschwindigkeiten und des Wasserstandes'
      WRITE(999,'(4(5x,a2,4x))') 'vx','vy','h','WL'
      WRITE(999,'(4(1x,a10))') '----------','----------',
     +                         '----------','----------'
      do zzz = 1,lmt(j)
        yy = line(j,zzz)
        write(999,'(4(1x,f10.7))')(vel(ccc,yy),ccc=1,3),ao(yy)+vel(3,yy)
      end do
      !-----------------------------------------------------------------------

          !nis,sep06,com: Loop over all corner nodes till the last one of CCL to get the Conveyance factor of the single parts of line.
          DO 750 k = 1, max, 2
            !nis,sep06,com: Get the first (na), midside (ncc), and endnode (nc) of the actual line segment
            na  = line (j, k)
            ncc = line (j, k + 1)
            nc  = line (j, k + 2)

            !nis,sep06,com: Calculate the x- and y- length of the actual CCL-segment
            dx = cord (nc, 1) - cord (na, 1)
            dy = - (cord (nc, 2) - cord (na, 2) )
            !nis,sep06,com: Calculate the sixth part of the Euklidean length of the actual CCL-segment, it's used for the Simpson Integration rule
            xl = sqrt (dx**2 + dy**2) * 1 / 6

!pl         Anfang
!           260199
          IF (dx.lt.0.00001.and.dx.gt. -
     +      0.00001.and.dy.lt.0.00001.and.dy.gt. - 0.00001) then
            WRITE (Lout, * ) 'Nr.', 'Knoten m', 'Knoten c', 'Knoten a'
            WRITE (Lout, * ) '7.', nm, nc, na
          ENDIF
!pl         Ende
                                                                        
            !nis,sep06,com: Calculate the direction angle of the actual CCL-segment
            alp = atan2 (dx, dy)

! uralt   d1=vel(3,na)                                                  
! uralt   d3=vel(3,nc)                                                  
! alt     d1=waspi-ao(na)-amec(k)                                       
! alt     d3=waspi-ao(nc)-amec(k+2)                                     
                                                                        
            !nis,sep06,com: IDNOPT.eq.0, if only wetting and drying is applied
            IF (idnopt.eq.0) then
              !nis,sep06,com: get the depth of the corner nodes of the actual CCL-segment
              d1 = waspi - ao (na)
              d3 = waspi - ao (nc)
            !nis,sep06,com: IDNOPT.ne.0, if marsh-algorithm is applied
            ELSE
              !nis,sep06,com: Get the depth of the corner nodes of the actual CCL-segment
              d1v = waspi - ao (na)
              d3v = waspi - ao (nc)

              !nis,sep06,com: Transform the depths to the marsh-algorithm-depths

!nis,sep06: zeigmarsh-parameter is not working in RMA10S yet
!            CALL amf (d1v, d1, akp (na), adt (na), adb (na), amec (k),
!     +       d2, 1, zeigmarsh)
!            CALL amf (d3v, d3, akp (nc), adt (nc), adb (nc), amec (k),
!     +       d2, 1, zeigmarsh)
              CALL amf (d1v, d1, akp (na), adt (na), adb (na), amec (k),
     +         d2, 1)
              CALL amf (d3v, d3, akp (nc), adt (nc), adb (nc), amec (k),
     +         d2, 1)
!-
            ENDIF

            !nis,sep06,com: Set depth to 0, if neglectable
            IF (d1.le.0.0) d1 = 0.0
            IF (d3.le.0.0) d3 = 0.0
            !nis,sep06,com: Average the water depth for the midside node
            d2 = (d1 + d3) / 2.0

            !nis,sep06,com: If roghness-value.gt.0 applied Darcy-Weisbach-law (ks-value)
            !nis,jan07: Some problems with cniku, so that origin ort(nn,15) is used
            !IF (cniku (lineimat (j, k + 1)) .gt.0.) then
            IF (ort (lineimat (j, k + 1), 15) .gt.0.) then
            !-
!             Darcy-Weisbach
                                                                        
              !nis,sep06,com: Beginning corner node of the actual CCL-segment (1/6 of the element width)
              !*****************************************************************************************
              !nis,sep06,com: calculate the absolute flow-velocity of beginning corner node of the actual CCL-segment
              vecq = sqrt (vel (1, na) **2 + vel (2, na) **2)
              !nis,sep06,com: Calculate the flow resistance of the beginning corner node of the actual CCL-segment
              !    darcy (lambda, vecq, h,  ks,                           a,  dp, nn, morph, bedform, mel, c_wr)
              !nis,nov06: Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before
              !           usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
              !           calculations for that yet.
              cwr_line = 1.0
!              CALL darcy (lambda, vecq, d1, ort (lineimat(j, k + 1), 5),
!     +                    0., 0., 0,  0,     bedform, mel, cwr_line)
              !nis,jan07: Some problems with cniku-array, so using the origin ort(nn,15) for sand roughness
              !CALL darcy (lambda, vecq, d1, cniku (lineimat(j, k + 1)),

                !nis,jan07,testing
                !WRITE(*,*) 'IN qgen', ort(lineimat(j,k+1),15)
                !-
              CALL darcy (lambda, vecq, d1, ort(lineimat(j, k + 1), 15),
              !-
     +                    0., 0., 0,  0,     gl_bedform, mel, cwr_line)
              !-
              !nis,sep06,com: teilline represents the Conveyance factor of the element part.
              teilline(k) = teilline(k) + xl*(d1** (3./2.)) *
     +                      ((78.48/lambda)**0.5)

              !nis,dec06,testfileoutput-------------------------------------
              WRITE(999,*) 'Corner node ', k,':', line(j,k)
              WRITE(999,*) 'Conveyance: ', teilline(k)
              WRITE(999,*) 'Resistance: ', lambda
              !-------------------------------------------------------------

              !nis,sep06,com: Initialize the total flow velocity
              vecq = 0.0
                                                                        
              !nis,sep06,com: midside node of the actual CCL-segment (4/6 of the element width)
              !********************************************************************************
              !nis,sep06,com: Calculate the flow velocity at the midside node of the actual CCL-segment
              IF (ncc.gt.0) vecq = sqrt (vel(1,ncc)**2 + vel(2,ncc)**2)
              !nis,sep06,com: Calculate the flow resistance of the midside node of the actual CCL-segment
              !nis,nov06: Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before,
              !           usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
              !           calculations for that yet.
              !           d2 must be KIND=4, so it is set by a temporary variable.
              cwr_line = 1.0
              d2_kind8 = d2
              !nis,nov06,testing
              !WRITE(*,*) 'd2_kind8:', d2_kind8
              !WRITE(*,*) 'd2:      ', d2
              !STOP 'testing finished'
              !-
!              CALL darcy (lambda, vecq, d2, ort (lineimat (j, k+1), 5),
!     +                    0., 0., 0,  0, bedform, mel, 1.0)

              !nis,jan07: Some problems with cniku, so origin ort(nn,15) is used
              !CALL darcy(lambda, vecq, d2_kind8, cniku(lineimat(j,k+1)),
                !nis,jan07,testing
                !WRITE(*,*) 'IN qgen', ort(lineimat(j,k+1),15)
                !-
              CALL darcy(lambda, vecq,d2_kind8, ort(lineimat(j,k+1),15),
              !-
     +                    0., 0., 0,  0,    gl_bedform, mel, cwr_line)
              d2 = d2_kind8
              !-
              !nis,sep06,com: Calculate the Conveyance factor of the midside node of the actual CCL-segment
              teilline (k + 1) = xl * 4 * (d2** (3. / 2.) ) *
     +                           ((78.48/lambda)**0.5)

              !nis,dec06,testfileoutput--------------------------------------
              WRITE(999,*) 'Midside node ', k+1,':', line(j,k+1)
              WRITE(999,*) 'Conveyance: ', teilline(k+1)
              WRITE(999,*) 'Resistance: ', lambda
              !--------------------------------------------------------------


              !nis,sep06,com: Ending corner node of the actual CCL-segment (1/6 of the element width)
              !**************************************************************************************
              !nis,sep06,com: Calculate the total velocity of the ending corner node of the actual CCL-segment
              vecq = sqrt (vel (1, nc) **2 + vel (2, nc) **2)
              !nis,sep06,com: Calculate the flow resistance of the ending corner node of the actual CCL-segment
              !nis,nov06: Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before,
              !           usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
              !           calculations for that yet.
              cwr_line = 1.0
!              CALL darcy (lambda, vecq, d3, ort (lineimat (j, k+1),5),
!     +                    0., 0., 0,  0, bedform, mel, 1.0)
              !nis,jan07: Some Problems with cniku, so origin ort(nn,15) is used
              !CALL darcy (lambda, vecq, d3, cniku(lineimat (j, k+1)),
                !nis,jan07,testing
                !WRITE(*,*) 'IN qgen', ort(lineimat(j,k+1),15)
                !-
              CALL darcy (lambda, vecq, d3, ort(lineimat (j, k+1),15),
              !-
     +                    0., 0., 0,  0, gl_bedform, mel, cwr_line)
!-
              !nis,sep06,com: Calculate the Connyance factor of the ending cornder node of the actual CCL-segment
              teilline (k + 2) = teilline (k + 2) + xl * (d3**(3./2.)) *
     +                           ((78.48/lambda)**0.5)
                                                                        
              !nis,dec06,testfileoutput------------------------------------------
              if (k.eq.max) then
                WRITE(999,*) 'End node ', k+2,':', line(j,k+2)
                WRITE(999,*) 'Conveyance: ', teilline(k+2)
                WRITE(999,*) 'Resistance: ', lambda
              endif
              !------------------------------------------------------------------

            ELSE
!             Manning-Strickler:

              !nis,sep06,com: Calculating the Conveyance factors for beginning, midside and ending nodes of the actual elements
              !               with the law of Manning Strickler
              teilline (k)     = teilline (k) + xl * (d1** (5. / 3.) ) *
     +                           ( - 1. * ort (lineimat (j, k + 1), 5) )
              teilline (k + 1) = xl * 4. * (d2** (5. / 3.) ) * ( - 1. *
     +                           ort (lineimat (j, k + 1), 5) )
              teilline (k + 2) = teilline (k + 2) + xl * (d3** (5./3.))*
     +                           ( - 1. * ort (lineimat (j, k + 1), 5) )
                                                                        
            ENDIF
                                                                        
            !nis,sep06,com: Calculate the influence width of the nodes of the segment. Corner nodes are a combination of the
            !               two adjacent elemtns
            dxl (k) = dxl (k) + xl
            dxl (k + 1) = xl * 4
            dxl (k + 2) = dxl (k + 2) + xl
                                                                        
!           if (k.eq.1)print*,' vorruebergehend eingebaute randstoerung:'
!           q=const
!           teilline(k)=teilline(k)+xl
!           teilline(k+1)=xl*4
!           teilline(k+2)=teilline(k+2)+xl
                                                                        
  750     END DO

          !nis,sep06,com: Summing of the Conveyance factors
          DO 303 k = 1, lmt (j)
            suma = suma + teilline (k)
  303     END DO
                                                                        
          !nis,sep06,com: Testing of the Conveyance factors
          suma = abs (suma)
          IF (abs (suma / qreq) .lt.0.0000001) then
            WRITE (Lout, * ) 'Volumenstromvorgabe an trockener Linie'
            WRITE (Lout, * ) 'keine fliesztiefe an q-linie: ', j
            STOP 'Volumenstromvorgabe an trockener Linie'
          ENDIF

          !nis,sep06,com: Calculation of estimated flow velocity
          !(question remark: Due to the formula of the Conveyance factor vest is the slope??? Q=sqrt(I)*K; K=Conveyance factor)
          vest = qreq / suma

          !nis,feb07: Writing root of slope into output file
          WRITE(999,*) 'Gefaelle ist: ', vest, vest**2
          !-

!...... insert values into spec  and  nfix arrays                       
!-        Randbedingung wird als spezifischer Durchfluss
!         angegeben in BFORM in Betrag und Richtung umgewandelt
!         und in COEFS als Randbedingung beruecksichtigt.
                                                                        
          !nis,sep06,com: Refresh the max-value with the number of nodes of the actual CCL
          max = lmt (j)
                                                                        
          !nis,sep06,com: Run through every node of the CCL for applying the specified discharge
          DO 900 k = 1, max
            !nis,sep06,com: Get the actual node number
            na = line (j, k)
            !nis,sep06,com: Skip non existing point
            IF (na.le.0) goto 900

            !nis,sep06,com: Initialize values
            dx1 = 0.0
            dy1 = 0.0
            dx2 = 0.0
            dy2 = 0.0

            !nis,sep06,com: Testing whether
            !nis,sep06,com: node is the first of the actual CCL
            IF (k.eq.1) then
              !nis,sep06,com: Calculate the x-distance to the second node of the actual CCL
              dx1 = cord (line (j, k + 1), 1) - cord (line (j, k), 1)
              !nis,sep06,com: Calculate the y-distance to the second node of the actual CCL
              dy1 = cord (line (j, k + 1), 2) - cord (line (j, k), 2)
              !nis,sep06,com: Copy the values
              dx2 = dx1
              dy2 = dy1

            !nis,sep06,com: node is the last of the actual CCL
            ELSEIF (k.eq.max) then
              !nis,sep06,com: Calculate the x-distance to the penultimate node of the actual CCL
              dx1 = cord (line (j, k), 1) - cord (line (j, k - 1), 1)
              !nis,sep06,com: Calculate the y-distance to the penultimate node of the actual CCL
              dy1 = cord (line (j, k), 2) - cord (line (j, k - 1), 2)
              !nis,sep06,com: Copy the values
              dx2 = dx1
              dy2 = dy1

            !nis,sep06,com: node is in between the first and last of the actual CCL
            ELSE
              !nis,sep06,com: Calculate the x- and y- distances to the node before the actual one of the actual CCL
              IF (line (j, k - 1) .gt.0) then
                dx1 = cord (line (j, k), 1) - cord (line (j, k - 1), 1)
                dy1 = cord (line (j, k), 2) - cord (line (j, k - 1), 2)
              ENDIF
              !nis,sep06,com: Calculate the x- and y- distances to the node after the actual one of the actual CCL
              IF (line (j, k + 1) .gt.0) then
                dx2 = cord (line (j, k + 1), 1) - cord (line (j, k), 1)
                dy2 = cord (line (j, k + 1), 2) - cord (line (j, k), 2)
              ENDIF
            ENDIF
                                                                        
!           Vektorkomponenten des Einheitsvektors parallel zur Elementseite
!           hinter(1) und vor(2) dem Knoten:
                                                                        
            !nis,sep06,com: Calculate the squared Euclidic distance to the node before the actual one
            wurz = (dx1**2 + dy1**2) ![m²]
            !nis,sep06,com: If the squared length is not neglectable, calculate the normalized values of the x- and y- distances.
            !               The result is the divided normalized vector parallel to the actual segment direction
            IF (wurz.gt.0.00001) then
              wurz = wurz**0.5       ![m]
              dx1 = dx1 / wurz       ![m/m] = [-]
              dy1 = dy1 / wurz       ![m/m] = [-]
            ENDIF
                                                                        
            !nis,sep06,com: Calculate the squared Euclidic distance to the node after the actual one
            wurz = (dx2**2 + dy2**2)
            !nis,sep06,com: If the squared length is not neglectable, calculate the normalized values of the x- and y- distances.
            !               The result is the divided normalized vector parallel to the actual segment direction
            IF (wurz.gt.0.00001) then
              wurz = wurz**0.5
              dx2 = dx2 / wurz
              dy2 = dy2 / wurz
            ENDIF
                                                                        
! uralt   d1=vel(3,na)                                                  
! alt     d1=waspi-ao(na)-amec(k)                                       
                                                                        
!nis,sep06,com: Process on midsidenodes
            IF (mod (k, 2) .eq.0) then
!             Mittseitenknoten
              !nis,sep06,com: Get the adjacent corner nodes
              na1 = line (j, k - 1)
              na2 = line (j, k + 1)

              !nis,sep06,com: If wetting/drying is activated, just calculate the waterdepths of the two adjacent corner nodes of the actual midside node
              IF (idnopt.eq.0) then
                d1 = waspi - ao (na1)
                d3 = waspi - ao (na2)
              !nis,sep06,com: If marsh-algorithm is applied, calculate the waterdepths of the two adjacent corner nodes of the actual midsied node and
              !               transform them afterwards
              ELSE
                d1v = waspi - ao (na1)
                d3v = waspi - ao (na2)

!nis,sep06: Zeigmarsh not active in RMA10S yet
!              CALL amf (d1v, d1, akp (na1), adt (na1), adb (na1),
!     +               amec (k - 1), d2, 1, zeigmarsh)
!              CALL amf (d3v, d3, akp (na2), adt (na2), adb (na2),
!     +               amec (k + 1), d2, 1, zeigmarsh)
                CALL amf (d1v, d1, akp (na1), adt (na1), adb (na1),
     +                 amec (k - 1), d2, 1)
                CALL amf (d3v, d3, akp (na2), adt (na2), adb (na2),
     +                 amec (k + 1), d2, 1)
!-

              ENDIF

              !nis,sep06,com: Set to 0, if neglectable depth
              IF (d1.le.0.0) d1 = 0.0
              IF (d3.le.0.0) d3 = 0.0
              !nis,sep06,com: Average the values (question remark: Shouldn't that be the waterdepth of the midside node? e.g. d2)
              d1 = (d1 + d3) / 2.0

!nis,sep06,com: Process on corner nodes
            ELSE
!             Eckknoten
              !nis,sep06,com: Calculate the water depth of the actual node of the actual node of actual CCL
              !               due to the drying/wetting algorithm or marsh-algorithm
              !               na is still the actual node number of actual node of actual CCL
              IF (idnopt.eq.0) then
                d1 = waspi - ao (na)
              ELSE
                d1v = waspi - ao (na)
!nis,sep06: Zeigmarsh not active in RMA10S
!                CALL amf (d1v, d1, akp (na), adt (na), adb (na),
!       +                 amec (k), d2, 1, zeigmarsh)
                CALL amf (d1v, d1, akp (na), adt (na), adb (na),
     +                   amec (k), d2, 1)
!-

              ENDIF
            ENDIF
                                                                        
            !nis,sep06,com: Set depth 0, if ngelectable
            IF (d1.le.0.0) d1 = 0.0

          !nis,sep06,com: Thet defines the inflow angle of the discharge, 0 means it is perpendicular to the chord of the line-segment
          !nis,jan07: machineaccuracy is less than a value 1e-4; value can not be equal zero
          !IF (thet.eq.0.0) then
          IF (ABS(thet).lt.1.0e-4) then
          !-
!             Stroemungsrichtung senkrecht zum jeweiligen Liniensegment



              !nis,sep06,com: Checking the angle between the segment(part)s at the actual node. The question comes from the relation:
              !   dx1   dx2
              !   --- = --- => dx1 * dy2 - dx2 * dy1 = 0, if the angle is (nearly .lt. 0.0001.) the same.
              !   dy1   dy2
            IF (abs (dy2 * dx1 - dy1 * dx2) .lt.0.0001) then

              !nis,dec06,testing
              !WRITE(*,*) 'Berechne Teilabfluss, Sort1'
              !WRITE(*,*) 'an Knoten',k
              !-

              !nis,sep06,com: Not clear yet (25092006)
              IF ( (dx1**2 + dy1**2) .gt.0.9) then
                spec (na,1) = vest * (teilline (k) / dxl (k) ) * (-dy1)
                spec (na,2) = vest * (teilline (k) / dxl (k) ) * (dx1)
              ELSEIF ( (dx2**2 + dy2**2) .gt.0.9) then
                spec (na,1) = vest * (teilline (k) / dxl (k) ) * (-dy2)
                spec (na,2) = vest * (teilline (k) / dxl (k) ) * (dx2)
              ENDIF


              !nis,sep06,com: If the angle is not the same, the discharge direction is specified with the direction of the bisector
            ELSE
!               im Knick zwischen den Elementseiten wird der Durchfluss
!               so gesetzt, dass senkrecht ueber jede Seite der verlangte
!               Abfluss eintritt. Die Richtung ergibt sich somit zur Winkel-
!               halbierenden, der Betrag des gesetzten Abflusses ist groesser
!               als der vorgegebenen Abfluss.

              !nis,dec06,testing
              !WRITE(*,*) 'Berechne Teilabfluss, Sort2'
              !WRITE(*,*) 'an Knoten',k
              !WRITE(*,*) 'Winkel zwischen Segmenten:',
      !+                    abs (dy2 * dx1 - dy1 * dx2)
              !-


                spec (na, 1) = vest * (teilline (k) / dxl (k) ) *
     +            ((dx2 - dx1) / (dx1 * dy2 - dx2 * dy1) )
                spec (na, 2) = vest * (teilline (k) / dxl (k) ) *
     +            ((dy2 - dy1) / (dx1 * dy2 - dx2 * dy1) )

            ENDIF
          ELSE
                                                                        
              !nis,dec06,testing
              !WRITE(*,*) 'Berechne Teilabfluss, Sort3'
              !WRITE(*,*) 'an Knoten',k
              !-

!             Stroemungsrichtung im Winkel ',thet,' Grad zur X-Richtung
                                                                        
!             29.01.98 cosd/sind unbekannt ?
!             cosd/sind - liefert Vinkel in Grad
!             cos/sin   - liefert Vinkel in Radian
!             Umrechnung vom Radian zu Grad:  cos(alpha)*180¹/pi
                                                                        
!             spec(na,1)=vest*(teilline(k)/dxl(k))*cosd(thet)
!             spec(na,2)=vest*(teilline(k)/dxl(k))*sind(thet)
                                                                        
              !nis,sep06,com: Calculate the Cosinus of the inflow angle relative to the inflow line
              !               and then evaluate the specific inflow at the node, vest seems to be the averaged slope
              cos_in_grad = (cos (thet) * 180) / 3.141592654
              spec (na,1) = vest * (teilline(k) / dxl(k)) * cos_in_grad
              !nis,sep06,com: Calculate the Sinus of the inflow angle relative to the inflow line
              !               and then evaluate the specific inflow at the node, vest seems to be the averaged slope
              sin_in_grad = (sin (thet) * 180) / 3.141592654
              spec (na,2) = vest * (teilline(k) / dxl(k)) * sin_in_grad

                                                                        
            ENDIF

            !NiS,sep06,com: This is just for water-quality calculation
              !NiS,sep06,com: salinity
              IF(QQAL(1) .GE. 0.) THEN
                NFIX(NA)=NFIX(NA)+10
                SPEC(NA,4)=QQAL(1)
                VEL(4,NA)=QQAL(1)
              ELSE
                NFIX(NA)=NFIX(NA)+20
                SPEC(NA,4)=-QQAL(1)
              ENDIF
              !NiS,sep06,com: temperature
              IF(QQAL(2) .GE. 0.) THEN
                NFIX(NA)=NFIX(NA)+1
                SPEC(NA,5)=QQAL(2)
                VEL(5,NA)=QQAL(2)
              ELSE
                NFIX(NA)=NFIX(NA)+2
                SPEC(NA,5)=-QQAL(2)
              ENDIF
              !NiS,sep06,com: sediment-concentration
              IF(QQAL(3) .GE. 0.) THEN
                NFIX1(NA)=1
                SPEC(NA,6)=QQAL(3)
                VEL(6,NA)=QQAL(3)
              ELSE
                NFIX1(NA)=2
                SPEC(NA,6)=-QQAL(3)
              ENDIF
            !-
                                                                        
  333       CONTINUE

  900     END DO
                                                                        
          !nis,sep06,com: Give every node the controlling specifier nfix and set the value of inflow to minimum 0.0001, if nothing is assigned
          DO 334 k = 1, max
            na = line (j, k)
            IF (na.gt.0) then
              IF (spec (na, 1) .eq.0.) spec (na, 1) = 0.0001
              nfix (na) = 31000
            ENDIF
  334     END DO
                                                                        
!         an den Enden der Durchflusslinien wird angeneommen,dass sie
!         auf einen Rand stossen. Deshalb wird im Folgenden noch an den
!         Enden der Durchflusslinie eine randparallele Stroemungssrichtung
!         gesetzt:
                                                                        
          !nis,sep06,com: Processing on the outer CCL-nodes
          DO 335 k = 1, max, max - 1
            neck = line (j, k)
!           neck ist der erste/letzte Knoten in der Q-Linie
            !nis,sep06,com: Get the second or penultimate node of the line, depending on which one is processed at the actually
            IF (k.eq.1) then
              neckvor = line (j, k + 1)
            ELSE
              neckvor = line (j, k - 1)
            ENDIF

            !nis,sep06,com: Initialize the ieck-counter, which stands for number of element, can be corner-elements, only the number of ieck.eq.1
            !               is allowed for correct processing.
  341       ieck = 0
                                                                        
            !nis,sep06,com: Run through every element to find out which element determines the outer flow direction
            DO 336 n = 1, ne
              !nis,sep06,com: Get number of nodes
              ncn = ncorn (n)
              !nis,sep06,com: Switch for showing, that an element has node within definition list for further processing
              igefunden = 0
              !nis,sep06,com: Run through all nodes of element
              DO 337 m = 1, ncn, 2
                !nis,sep06,com: Testing whether one node is equal to the actual CCL-end-node
                IF (iabs (nop (n, m) ) .eq. neck) then

                  !nis,sep06,com: Getting the two adjacent midside nodes of the node that is equivalent to the CCL-end node
                  IF (m.eq.1) then
                    neck1 = nop (n, ncn)
                  ELSE
                    neck1 = nop (n, m - 1)
                  ENDIF
                  neck2 = nop (n, m + 1)

                  !nis,sep06,com: Showing, that the node is in the definition list of the element and that the processing on that
                  !               element should continue
                  igefunden = 1
                ENDIF
  337         END DO
                                                                        
              !nis,sep06,com: If node is found, but node is dry cycle loop, nothing happens to the direction of the outer boundary node direction.
              IF ( (igefunden.eq.1) .and. (imat (n).le.0) ) then
      WRITE(Lout,*) 'Ecke ', k, 'von Linie', j, 'an trockenem Element '
                GOTO 335
              ENDIF
                                                                        
              !nis,sep06,com: Check, whether the element represents the boundary of the inflow. This occurs, when
              !               1. The counter ibn of the midsidenode is 1, which means it has only one adjacent element
              !               2. The node is not equal to the penultimate node of the CCL, which means it does not lie on the CCL
              !
              !               The following control structure controls the number of elements, that have this property. Additionally the
              !               last processed node, that increased the ieck-counter is saved. In the correct case of ieck.eq.1 after processing
              !               the whole network also the correct node neckr is saved.
              IF (igefunden.eq.1) then
                IF ((ibn (neck1) .eq.1) .and. (neck1.ne.neckvor)) then
                  ieck = ieck + 1
                  neckr = neck1
                ENDIF
                IF ((ibn (neck2) .eq.1) .and. (neck2.ne.neckvor)) then
                  ieck = ieck + 1
                  neckr = neck2
                ENDIF
                                                                        
!               neckr ist Nachbarknoten von neck,liegt auf dem Rand und
!               gehoert nicht zur Q-Linie.
              ENDIF
  336       END DO
                                                                        

            !nis,sep06,com: If the counter of elements, that fit the CCL-end-node is .gt.1 and network error occurs and the execution is stopped
            IF (ieck.ne.1) then
              PRINT * , 'Anfang oder Ende der IQGEN-Linie', j
              PRINT * , 'ist keine Ecke'
              WRITE (Lout, * ) 'Anfang oder Ende der IQGEN-Linie', j
              WRITE (Lout, * ) 'ist keine Ecke'
              STOP
            ENDIF
                                                                        
            !nis,sep06,com: Get the squared length of CCL-corner-node to boundary-midside-node arc (betd)
            dx = cord (neck, 1) - cord (neckr, 1)
            dy = cord (neck, 2) - cord (neckr, 2)
            betd = (dx**2 + dy**2)
                                                                        
            !nis,sep06,com: This length should not be to small, because no angle can be calculated and the processing in the Newton-Raphson-Scheme will
            !               get unstable
            IF (betd.lt.0.00001) stop 'betd zu klein'
                                                                        
            !nis,sep06,com: Get the Euklidic length of that arc-part
            betd = betd**0.5
            !nis,sep06,com: Normalize the x- and y- component of that arc-part
            dx = dx / betd
            dy = dy / betd
                                                                        
            !nis,sep06,com: Get the nominal discharge of the CCL-end-node
            speck1 = spec (neck, 1)
            speck2 = spec (neck, 2)
            !nis,sep06,com: Get the squared absolute discharge of the CCL-end-node
            betspeck = (speck1**2 + speck2**2)
            !nis,sep06,com: If it is to small, set minimum values
            IF (betspeck.lt.0.00001) then
              spec (neck, 1) = 0.0001
              nfix (neck) = 31000
              GOTO 335
            ENDIF
            !nis,sep06,com: Get absolute discharge of the CCL-end-node
            betspeck = betspeck**0.5
            !nis,sep06,com: Normalize the discharge components
            speck1 = speck1 / betspeck
            speck2 = speck2 / betspeck
                                                                        
            !nis,sep06,com: Problems with the direction of the original flow and the direction of the boundary lead to abruption of the program
            IF (abs ( (dx * speck1 + dy * speck2) ) .lt.0.00001) then
        WRITE (Lout,  *) '>> Abbruch <<<  An der NCL-Linie ', j
        WRITE (Lout,  *) ' wurde ein Volumenstrom vorgegeben (IQGEN).'
        WRITE (Lout,  *) ' An den Linienenden wird die Geschwindigkeit '
        WRITE (Lout,  *)  ' randparallel ausgerichtet. '
        WRITE (Lout,  *) ' Im vorliegenden Fall ist dies nicht',
     +                    'sinnvoll moeglich.'
        WRITE ( * ,   *)  'IQGEN-Linie ', j, ' endet an unschoener Ecke'
              STOP
            ENDIF
                                                                        
!           q-Randbedingung wird parallel zur Verbindungslinie von
!           neck und neckr gesetzt. Diese Richtungsaenderung wird so
!           vollzogen, dass sie sich nicht auf den Abfluss ueber die
!           Q-Linie auswirkt.

            !nis,sep06,com: Projecting the specified discharge to the correct inflow direction, parallel to the boundary
            spec (neck, 1) = dx * betspeck / (dx * speck1 + dy * speck2)
            spec (neck, 2) = dy * betspeck / (dx * speck1 + dy * speck2)
  335     END DO

  402     GOTO 401

  401     CONTINUE
                                                                        
        ENDIF
        !nis,sep06,com: End of processing on the Q-boundary condition
                                                                        
!nis,sep06,com: Checking the continuity or rather the correct transformation of the boundary condition

!       Ermittlung des gesetzten gesamten Durchflusses
!       ( Kontrolle + Korrektur ):

        !nis,sep06,com: Initializing sumx and sumy
        sumx = 0.0
        sumy = 0.0

        !nis,sep06,com: Set the number of segment-starting-nodes in the CCL
        max = lmt (j) - 2
                                                                        
        !nis,sep06,com: Run through all Corner nodes of the the CCL
        DO 559 k = 1, max, 2
          !nis,sep06,com: Get the first corner node of segment
          na = line (j, k)
          !nis,sep06,com: Get the midside node of the segment
          nbb = line (j, k + 1)
          !nis,sep06,com: Cycling the loop, if segment is dry
          IF (ndry (nbb) .ne.1) goto 559
          IF (nbb.le.0) goto 559
          !nis,sep06,com: Get the segment-ending-node of the CCL
          nc = line (j, k + 2)
          !nis,sep06,com: Calculate the sixth of the segment length
          dx = (cord (nc, 1) - cord (na, 1) ) / 6.
          dy = (cord (nc, 2) - cord (na, 2) ) / 6.

          !nis,sep06,com: Calculate the discharge-part of the absolute CCL-discharge at the actual segment,
          !               using the Simpson-rule
!         Simpsonformel integriert Parabeln exakt
          sumx = sumx + dy * (spec (na, 1) + 4.0 * spec (nbb, 1)
     +                        + spec (nc, 1))
          sumy = sumy + dx * (spec (na, 2) + 4.0 * spec (nbb, 2)
     +                        + spec (nc, 2))

!         Bestimmung der Durchfluszkomponenten bei linearer Verteilung
!         der Fliesztiefe, geradem Elementrand und mittigem
!         Mittseitenknoten o.k.

  559   END DO
                                                                        

        !nis,sep06,com: Calculate Correction-factor total of discharge
!       korrekte Summation der Durchflusskomponenten auch fuer
!       nicht gerade (polygon) Raender:
        total = sumx - sumy
        !nis,feb07,testing
!       Korrektur:
        total = abs (qreq) / abs (total)
      !nis,nov06,testfileoutput------------------------------------------------
      write(999,*)
     +  ' gesetzter Durchfluss an Linie',j,' = ',qreq,
     +  'Korrektur:', total
      WRITE(999,*) 'Teilabfluesse: ', sumx, sumy
      !------------------------------------------------------------------------

        !nis,sep06,com: Correct the discharge at the nodes by scaling with correction-factor
        DO k = 1, max
          na = line (j, k)
          IF (na.gt.0) then
            spec (na, 1) = spec (na, 1) * total
            spec (na, 2) = spec (na, 2) * total
          ENDIF
        END DO
                                                                        
        !nis,sep06,testfileoutput-----------------------------------------------
        DO k = 1, max
          na = line (j, k)
          WRITE (999,9898)  j, na, spec(na,1), spec (na, 2),
     +                      SQRT((spec(na,2))**2+(spec(na,1))**2)
        END DO

        WRITE (Lout,*) ' gesetzter Durchfluss an Linie', j, ' ist: ',
     +                    qreq, ' Durchflussverhaeltnis  ist: ', total
 9898  format (1x,'Q-Linie',i6.6,' Knoten: ',i6.6,' Geschw.-X= ',f10.5,
     +         ' Geschw.-Y= ', f10.5,'Gesamtgeschw.:',f10.5)
        !-----------------------------------------------------------------------
                                                                        
        !nis,sep06,com: For unsteady calculation ???
        IF (icyc.gt.0) then

          DO 901 k = 1, lmt (j)
            na = line (j, k)
            IF (na.gt.0) call bform (na)
  901     END DO

        !nis,sep06,com: npr stands for the iteration number in steady case
        ELSEIF (npr.gt.0) then

          DO 555 k = 1, lmt (j)
           na = line (j, k)
            IF (na.gt.0) call bform (na)
  555     END DO
        ENDIF

      !write(*,*)'qgen gelaufen mit Linie ',j


        !NiS,sep06: Ending BSWIT
        ENDIF
!      !nis,sep06: Ending 1D or 2D differentiation
!      ENDIF

      !nis,sep06,testing:
      !write (*,*) 'Testausgabe'
      !do testi = 1, 6
      !  WRITE(*,*) spec(line(j,1),testi)
      !enddo
      !-

      !nis,sep06: Closing testfile-------
      CLOSE(999,STATUS = 'keep')
      !----------------------------------
      RETURN
      END
