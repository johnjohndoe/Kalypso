C     Last change:  WP   17 Apr 2008    1:02 pm
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
      SAVE
C-
C-..... Generate specified total flow boundary conditions
C-
      DIMENSION QQAL(3)

!nis,sep06: new variables
      REAL     :: amec (3535), Conveyance (3535), dxl (3535)
      !nis,aug07: dummy parameters for passing
      REAL (KIND = 8) :: dummy (1:3), NullVal
      real            :: dum1

!nis,sep06: changes for good variable passing
      REAL (KIND = 8) :: lambda
      real            :: betspeck, betd
      !nis,nov06: cwr_line for intentinout-property of Darcy-subroutine
      REAL            :: cwr_line
      !-
      REAL (KIND = 8) :: fliesstiefe, waspi
      !nis,nov06: For proper passing d2 must be KIND=4
      !REAL(KIND=8) :: d1,d2,d3,d1v,d3v
      REAL (KIND = 8) :: d1,d3,d1v,d3v,d2_kind8
      REAL            :: d2
      !-
!-

      REAL     :: cos_in_grad, sin_in_grad

      INTEGER  :: maxL
      REAL (KIND = 8) :: xl, xlstrike
      REAL (KIND = 8), DIMENSION(0:2) :: weight, di
      real (KIND = 8) :: LineCniku
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

      DATA VOID/-1.E20/

      NullVal = 0.0

C-
C...... Calculate total projected area
C-
      !nis,sep06,com: maxL matches the number of corner-nodes of the CCL J
      maxL=LMT(J)
CIPK FEB01
        !nis,sep06,com: if there is no node for the CCL, stop the program
      IF(maxL == 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF
      !nis,sep06,com: Use the global direction of the inflow direction
      STQT(J)=THET

      !nis,sep06,com: If the CCL has only 1 node, that means 1D-element
      IF(maxL == 1) THEN
C-
C...... This is for 1-D element
C-
        K=0
        DO 120 M=1,NE
          K=K+1
          IF(NOP(K,3) == LINE(J,1)) THEN
            NA=NOP(K,3)
            NC=NOP(K,1)
            IF(NC > NPM) GO TO 120
            GO TO 127
          ENDIF
          IF(NOP(K,1) == LINE(J,1)) THEN
            NA=NOP(K,1)
            NC=NOP(K,3)
            IF(NC > NPM) GO TO 120
            GO TO 127
          ENDIF
  120   CONTINUE
  127   NM=NOP(K,2)
          IF(CORD(NM,1) > VOID) THEN
            DX=2.*(CORD(NM,1)-CORD(NA,1))-0.5*(CORD(NC,1)-CORD(NA,1))
            DY=2.*(CORD(NM,2)-CORD(NA,2))-0.5*(CORD(NC,2)-CORD(NA,2))
          ELSE
            DX=CORD(NC,1)-CORD(NA,1)
            DY=CORD(NC,2)-CORD(NA,2)
          ENDIF
        BEETA=ATAN2(DY,DX)
        IF(ABS(BEETA-THET) > 1.570796 .AND. 
     +     ABS(BEETA-THET) < 4.712394) BEETA=BEETA+3.141596
        SPEC(NA,1)=QREQ*COS(BEETA)
        SPEC(NA,2)=QREQ*SIN(BEETA)
        NFIX(NA)=31000
        IF(QQAL(1) >= 0.) THEN
          NFIX(NA)=NFIX(NA)+10
          SPEC(NA,4)=QQAL(1)
          VEL(4,NA)=QQAL(1)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+20
          SPEC(NA,4)=-QQAL(1)
        ENDIF
        IF(QQAL(2) >= 0.) THEN
          NFIX(NA)=NFIX(NA)+1
          SPEC(NA,5)=QQAL(2)
          VEL(5,NA)=QQAL(2)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+2
          SPEC(NA,5)=-QQAL(2)
        ENDIF
        IF(QQAL(3) >= 0.) THEN
          NFIX1(NA)=1
          SPEC(NA,6)=QQAL(3)
          VEL(6,NA)=QQAL(3)
CIPK OCT01
        ELSE
          NFIX1(NA)=2
          SPEC(NA,6)=-QQAL(3)
        ENDIF

!nis,sep06: In the 2D-part an additional if control was entered, so
!           that the following if control could not be reached by
!           a 1D-boundary. This is preserved by copying it to this place.
!           The if control number 500 is replaced by actual way of statement.
        IF(ICYC > 0) THEN
          DO K=1,maxL
            NA=LINE(J,K)
            CALL BFORM(NA)
          ENDDO
        ENDIF
!-


!nis,com:**********************************************************
! 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D 2D
!nis,com:**********************************************************


!nis,sep06,com: If CCL has more than 1 node, that means 2D
!nis,jan07,com: condition to come here is: maxL /= 1

cipk oc98        VM=SQRT(VEL(1,NA)**2+VEL(2,NA)**2)
C       VEL(1,NA)=VM*COS(BEETA)
C       VEL(2,NA)=VM*SIN(BEETA)
      ELSE
C-
C...... IBN contains counter for number of references to a node
C-

        !nis,sep06,testfileoutput---------------------------------------
        WRITE(*,*) 'Control Output file for checking',
     +               'boundary conditions'
        WRITE(*,*) 'Linie CCL: ', J
        WRITE(*,*) 'Anzahl Knoten: ', lmt(J)
        WRITE(*,*) 'required Q: ', QREQ
        WRITE(*,*)
        !---------------------------------------------------------------


        !nis,sep06,com: Set every number of elements-reference-counter for every surface node to 0
        DO 103 N=1,NPM
          IBN(N)=0
  103   CONTINUE

        !nis,sep06,com: Objective - get the number IBN for every node of all elements; why is that repeated here; it was done in BLINE.sub
        DO 105 N=1,NE
          !nis,sep06,com: Jump over every deactivated
          IF(IMAT(N) < 1) GO TO 105

          !nis,sep06,com: Jump over control structure elements
          !               Here overloading messages are in because, if the value is < 1 the code can not reach this code part.
          !               It would be enough to have if(NM > 900) with NM=IMAT(N) without ABS()-function
          !-
cipk oct98 update to f90
          NM=ABS(IMAT(N))
          IF(NM > 900) GO TO 105

          !nis,sep06,com: Jump over element, that has no turbulence value
          IF(ORT(NM,1) == 0.) GO TO 105
          !nis,sep06,com: Get node number (might be 3, 5, 6, 8); but how can a 1D-element reach this code part, because if there was
          !               only one node as boundary condition this part of the subroutine is not used
          NCN=NCORN(N)
          !nis,sep06,com: Treat 1D-2D-transition element as 1D-element; corner node of 2D-transition-element part is not touched; those nodes still
          !               act as boundary nodes.
          IF(NCN == 5) NCN=3
          !nis,sep06,com: increase IBN by one for every occurance in an element.
          DO 104 M=1,NCN
cipk oct98 update to f90

            K=ABS(NOP(N,M))
            IBN(K)=IBN(K)+1

  104     CONTINUE
  105   CONTINUE

!nis,jan07,com: BSWIT == 1 means original method of rma10s
!NiS,sep06: Activating possibility of chosing the way of discharge boundary condition transformation
        IF (BSWIT == 1) THEN

          !nis,sep06,com: Calculate the discharge crosssectional area (wetted area)
          !nis,sep06,com: initialize summed area SUMA
          SUMA=0.

          maxL = LMT(J)-2
          DO 150 K = 1, maxL, 2
            NA = LINE(J,K)
            NC = LINE(J,K+2)
            DX=CORD(NC,1)-CORD(NA,1)
            DY=-(CORD(NC,2)-CORD(NA,2))
            XL=SQRT(DX**2+DY**2)
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
          VEST=QREQ/SUMA
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
          maxL=maxL+2
          DO 300 K=1,maxL
            NA=LINE(J,K)
            D1=VEL(3,NA)
            SPEC(NA,1)=VEST*D1*COS(THET)
            SPEC(NA,2)=VEST*D1*SIN(THET)
            IF(SPEC(NA,1) /= 0.) THEN
              NFIX(NA)=31000
            ELSE
              NFIX(NA)=13000
            ENDIF

            !NiS,sep06,com: This is just for water-quality calculation
            IF(QQAL(1) >= 0.) THEN
              NFIX(NA)=NFIX(NA)+10
              SPEC(NA,4)=QQAL(1)
              VEL(4,NA)=QQAL(1)
CIPK OCT01
            ELSE
              NFIX(NA)=NFIX(NA)+20
              SPEC(NA,4)=-QQAL(1)
            ENDIF
            IF(QQAL(2) >= 0.) THEN
              NFIX(NA)=NFIX(NA)+1
              SPEC(NA,5)=QQAL(2)
              VEL(5,NA)=QQAL(2)
CIPK OCT01
            ELSE
              NFIX(NA)=NFIX(NA)+2
              SPEC(NA,5)=-QQAL(2)
            ENDIF
            IF(QQAL(3) >= 0.) THEN
              NFIX1(NA)=1
              SPEC(NA,6)=QQAL(3)
              VEL(6,NA)=QQAL(3)
CIPK OCT01
            ELSE
              NFIX1(NA)=2
              SPEC(NA,6)=-QQAL(3)
            ENDIF
  300     CONTINUE


      !NiS,sep06: Using the Kalypso-2D-option for Q-boundary condition transformation
        ELSEIF (BSWIT == 2) then

          !check, whether Q-Line is valid. Flow should cross every segment in same direction
          vnomx = COS (thet)
          vnomy = SIN (thet)

          do k = 1, lmt(j)-2, 2
            dxi = cord (line (j, k + 2), 1) - cord (line (j, k), 1)
            dyi = cord (line (j, k + 2), 2) - cord (line (j, k), 2)
            QI = vnomx * dyi - vnomy * dxi
            if (k == 1) Q1 = QI

            if (QI * Q1 < 0) then
              WRITE(*,*) 'ERROR - flow crosses boundary line'
              WRITE(*,*) 'segmentwise in different directions.'
              WRITE(*,*) 'change model!'
              WRITE(*,*) 'execution of program terminated - STOP'
              stop
            end if
          end do

          !Calculation of the average water level:
          !Initializing
          waspi = 0.0

          !subroutine gets average water level waspi for line j
          call getLineAverageWaterlevel(j,waspi)

          !nis,sep06,testfileoutput:--------------------------------------------
          !calculation of real average waterlevel
          tmpwl = 0
          do zzz = 1,lmt(j)
            tmpwl = tmpwl + ado(line(j,zzz))+vel(3,line(j,zzz))
          end do
          tmpwl = tmpwl/lmt(j)

          WRITE(*,*) 'average marsh-waterlevel at line',J,':',waspi
          WRITE(*,*) 'average  real waterlevel at line',J,':',tmpwl
          WRITE(*,'(1x,a6,4(1x,a10))')'Knoten','depth','diff.',
     +             'surf.elev', 'real WL'
          WRITE(*,'(1x,a6,4(1x,a10))')
     +              '------','----------','----------',
     +              '----------','----------'
          do zzz = 1,lmt(J)
            yy = line(j,zzz)
            diff = vel(3,yy) - tmpwl + ao(yy)
            hoeh = vel(3,yy)+ao(yy)
            WRITE(*,'(1x,i6,4(1x,f10.6))')
     +            yy,vel(3,yy),diff,ao(yy),hoeh
          end do
          WRITE(*,*) '-----------------------------------------------'
          WRITE(*,*) 'positive sign means WL higher  than average'
          WRITE(*,*) 'negative sign means WL smaller than average'
          WRITE(*,*) '-----------------------------------------------'
          !---------------------------------------------------------------------

          !length of line
          maxL = lmt (j)

          !Initializations
          !sum of Conveyance factors
          suma = 0.
          DO k = 1, maxL
            !Conveyance factors
            Conveyance (k) = 0.0
            !Influence ranges
            dxl (k) = 0.0
          END DO
          !weights for simpson rule
          weight(0) = 1.0
          weight(1) = 4.0
          weight(2) = 1.0

          !starting node, ending node
          na = line (j, 1)
          nc = line (j, maxL)

          !x- and y- chord length of transition line
          dxline = cord (nc, 1) - cord (na, 1)
          dyline = cord (nc, 2) - cord (na, 2)

          !calculate the reduction of xl-length because of inflow angle (theorem of intersecting lines; Strahlensatz)
          !must be: xlstrike = xl * reductionfactor
          reductionfactor = (COS(thet) * dyline - SIN(thet) * dxline)
          reductionfactor = reductionfactor / SQRT(dxline**2+dyline**2) !*sqrt(cos(thet)**2 + sin(thet)**2)

          maxL = lmt(j) - 2

          !nis,sep06,com: Loop over all corner nodes till the last one of CCL to get the Conveyance factor of the single parts of line.
          ThroughNodesOfLine: DO k = 1, maxL, 2
            !Get the first (na), midside (ncc), and endnode (nc) of the actual line segment
            na  = line (j, k)
            ncc = line (j, k + 1)
            nc  = line (j, k + 2)

            !Calculate the x- and y- lengths of the actual CCL-segment
            dx = cord (nc, 1) - cord (na, 1)
            dy = cord (nc, 2) - cord (na, 2)

            !counting sixth length of segment projected on the chord of the transition length (it's vector product for angle calculation)
            xl = ABS((dx * dxline + dy * dyline) /
     +               (6 * SQRT (dxline**2 + dyline**2)))
            xlstrike = xl * reductionfactor

            !nis,jun07: switched off Marsh-Algorithm:
            IF (idnopt == 0) then
              !waterdepth
              di(0) = waspi - ao (na)
              di(2) = waspi - ao (nc)
            !nis,jun07: Marsh-Algorithm:
            ELSE
              !waterdepth
              d1v = waspi - ado (na)
              d3v = waspi - ado (nc)
              !Transform to Marsh-depth
              CALL amf (d1v, di(0), akp (na), adt (na), adb (na),
     +                  amec (k), dum1, 1)
              CALL amf (d3v, di(2), akp (nc), adt (nc), adb (nc),
     +                  amec (k), dum1, 1)
            ENDIF
            !Set depth to 0.0, if it was smaller zero
            IF (di(0) <= 0.0) di(0) = 0.0
            IF (di(2) <= 0.0) di(2) = 0.0
            !Average the water depth for the midside node
            di(1) = (di(0) + di(2)) / 2.0

            do no = 0, 2
              !Darcy-Weisbach:
              IF (ort (lineimat (j, k + 1), 15) > 0.) then
                !actual node
                nod = line (j, k)
                !calculate the absolute flow-velocity of beginning corner node of the actual CCL-segment
                vecq = sqrt (vel (1, na)**2 + vel (2, na) **2)
                !default vegetation cwr-value
                cwr_line = 1.0
                !temporary roughness in correct format
                LineCniku = ort(lineimat(j, k+1), 15)
                LineCniku = LineCniku * LineCorrectionKS(j, k+1)
                !get lambda
                CALL darcy (lambda, vecq, di(0),
     +            LineCniku,
     +            NullVal, NullVal, 0,  0, gl_bedform, mel, cwr_line, 2,
     +            dummy(1), dummy(2), dummy(3),dset)
                !Correct roughness, if there is a material (imat) factor (when marsh-option is active)
                if (idnopt /= 0 .AND. d1 < akp(na) * adb(na)) then
                  lambda = lambda
     +              * (ort (lineimat (j, k + 1), 12)**2 - 1.)
     +              * (akp(nod) * adb(nod) - d1)
     +              / (akp(nod) * adb(nod)) + 1.0
                end if
                !Conveyance represents the Conveyance factor of the element part.
                Conveyance(k + no) =
     +            Conveyance(k + no) + xlstrike * weight(no)
     +            * di(no)**(3./2.) * ((78.48/lambda)**0.5)
                !Calculate the influence width of the nodes of the segment. Corner nodes are a combination of the two adjacent elemtns
                dxl (k + no) = dxl (k + no) + xlstrike * weight(no)

              !Manning-Strickler:
              ELSE
                !Calculating the Conveyance factors with the law of Manning Strickler
                Conveyance (k+no) =
     +            Conveyance (k+no)
     +            + xlstrike * weight(no) * (di(no)** (5. / 3.) )
     +            * ( - 1. * ort (lineimat (j, k + 1), 5))

              ENDIF
            enddo
          END DO ThroughNodesOfLine

          maxL = lmt (j)

          !nis,sep06,com: Summing of the Conveyance factors
          DO k = 1, maxL
            suma = suma + Conveyance (k)
          END DO
          !nis,sep06,com: Testing of the Conveyance factors
          suma = abs (suma)
          IF (abs (suma / qreq) < 0.0000001) then
            WRITE (Lout, * ) 'Volumenstromvorgabe an trockener Linie'
            WRITE (Lout, * ) 'keine fliesztiefe an q-linie: ', j
            STOP 'Volumenstromvorgabe an trockener Linie'
          ENDIF

          !nis,sep06,com: Calculation of estimated flow velocity
          !(question remark: Due to the formula of the Conveyance factor vest is the slope??? Q=sqrt(I)*K; K=Conveyance factor)
          vest = qreq / suma

          !nis,sep06,com: Run through every node of the CCL for applying the specified discharge
          AssginSpecificDischarge: DO k = 1, maxL
            !nis,sep06,com: Get the actual node number
            na = line (j, k)
            !nis,sep06,com: Skip non existing point
            IF (na <= 0) cycle AssginSpecificDischarge

            !Process on midsidenodes to get their depths
            IF (mod (k, 2) == 0) then
              !Get the adjacent corner nodes
              na1 = line (j, k - 1)
              na2 = line (j, k + 1)

              !If wetting/drying is activated, just calculate the waterdepths of the two adjacent corner nodes of the actual midside node
              IF (idnopt == 0) then
                d1 = waspi - ao (na1)
                d3 = waspi - ao (na2)
              !If marsh-algorithm is applied, calculate the waterdepths of the two adjacent corner nodes of the actual midsied node and
              !  transform them afterwards
              ELSE
                d1v = waspi - ado (na1)
                d3v = waspi - ado (na2)
                CALL amf (d1v, d1, akp (na1), adt (na1), adb (na1),
     +                    amec (k - 1), d2, 1)
                CALL amf (d3v, d3, akp (na2), adt (na2), adb (na2),
     +                    amec (k + 1), d2, 1)
              ENDIF
              !Set to 0, if neglectable depth
              IF (d1 <= 0.0) d1 = 0.0
              IF (d3 <= 0.0) d3 = 0.0
              !Average the values (question remark: Shouldn't that be the waterdepth of the midside node? e.g. d2)
              d1 = (d1 + d3) / 2.0
            !Process on corner nodes to get their flow depth
            ELSE
              !no special drying/wetting algorithm
              IF (idnopt == 0) then
                d1  = waspi - ao (na)
              !marsh active
              ELSE
                d1v = waspi - ado (na)
                CALL amf (d1v, d1, akp (na), adt (na), adb (na),
     +                    amec (k), d2, 1)
              ENDIF
            ENDIF

            !Set depth 0, if ngelectable
            IF (d1 <= 0.0) d1 = 0.0


            !Set the absolute velocities at 2D transition nodes and copy specific discharges to global arrays
            if (d1 > 0.0) then
              spec (na, 1) = vest * Conveyance (k) / dxl (k) * COS(thet)
              spec (na, 2) = vest * Conveyance (k) / dxl (k) * SIN(thet)
            else
              spec (na,1) = 0.0
            end if

          ENDDO AssginSpecificDischarge

          !nis,sep06,com: Give every node the controlling specifier nfix and set the value of inflow to minimum 0.0001, if nothing is assigned
          DO k = 1, maxL
            na = line (j, k)
            IF (na > 0) then
              IF (spec (na, 1) == 0.) spec (na, 1) = 0.0001
              nfix (na) = 31000
            ENDIF

          !NiS,sep06,com: This is just for water-quality calculation
          !NiS,sep06,com: salinity
          IF(QQAL(1) >= 0.) THEN
            NFIX(NA)=NFIX(NA)+10
            SPEC(NA,4)=QQAL(1)
            VEL(4,NA)=QQAL(1)
          ELSE
            NFIX(NA)=NFIX(NA)+20
            SPEC(NA,4)=-QQAL(1)
          ENDIF
          !NiS,sep06,com: temperature
          IF(QQAL(2) >= 0.) THEN
            NFIX(NA)=NFIX(NA)+1
            SPEC(NA,5)=QQAL(2)
            VEL(5,NA)=QQAL(2)
          ELSE
            NFIX(NA)=NFIX(NA)+2
            SPEC(NA,5)=-QQAL(2)
          ENDIF
          !NiS,sep06,com: sediment-concentration
          IF(QQAL(3) >= 0.) THEN
            NFIX1(NA)=1
            SPEC(NA,6)=QQAL(3)
            VEL(6,NA)=QQAL(3)
          ELSE
            NFIX1(NA)=2
            SPEC(NA,6)=-QQAL(3)
          ENDIF
          Enddo

        !NiS,sep06: Ending BSWIT
        ENDIF

C-
C...... Find correct components for boundary nodes
C-
        DO K=1,maxL,maxL-1
          !node
          NA=LINE(j,K)
          IF(K == 1) THEN
            neighbourMidside = line (j, 2)
            neighbourCorner = line (j, 3)
          ELSE
            neighbourMidside = line (j, maxL-1)
            neighbourCorner = line (j, maxL-2)
          ENDIF
          
         call fixDirection (na, neighbourMidside, NeighbourCorner, thet)
        enddo

          !set usable boundary conditions only for unsteady,
          !  steady update is done somewhere else
          if (icyc > 0) then
            DO k = 1, lmt (j)
              na = line (j, k)
              IF (na > 0) call bform (na)
            END DO
          endif


      !nis,sep06: Ending 1D or 2D differentiation
      ENDIF


      !nis,sep06: Closing testfile-------
      CLOSE(999,STATUS = 'keep')
      !----------------------------------
      RETURN
      END


      subroutine fixDirection (thisNode, NeighbourMidsideOnLine, 
     +                          NeighbourCornerOnLine, thet)
        use blk10mod, only: ncn, ncorn, nop, ibn, cord, spec
        use parakalyps, only: isNodeOfElement
        implicit none
        !arguments
        integer (kind = 4), intent (in) :: thisNode
        integer (kind = 4), intent (in) :: NeighbourCornerOnLine
        integer (kind = 4), intent (in) :: NeighbourMidsideOnLine
        real (kind = 8), intent (in) :: thet
        !local variables
        integer (kind = 4) :: mlw, msl, mfr
        integer (kind = 4) :: i, k, m, n
        integer (kind = 4) :: kk
        real (kind = 8) :: dx, dy, dxa, dya
        real (kind = 8) :: beeta, aalfa
        real (kind = 8) :: qmag
!NiS,sep06,com: Objective is to find the right direction of the node inflow, parallel to the boundary at the inflow
            findCorrectElement: do i = 1, IsNodeOfElement(thisNode, 0)
              !get element
              N = IsNodeOfElement(thisNode, i)
              !find entry in node list of element
              ncn = ncorn(n)
              findentry: do M = 1, ncn, 2
                if (nop(N, M) == thisNode) EXIT findentry
              end do findentry
C-
C....... Found a match node. Now determine side for parallel flow
C-
              MLW = M - 1
              IF (m == 1) MLW = NCN

cipk oct98 update to f90

cipk oct98            MLX=ABS(NOP(N,MLW))
              MSL=MOD(MLW+2,NCN)
              IF(MSL == 0) MSL=NCN
              MFR=MOD(MLW+3,NCN)

cipk oct98 update to f90
              MSL=ABS(NOP(N,MSL))
              IF(MSL /= NeighbourMidsideOnLine) then
                IF(IBN(MSL) == 1) EXIT findCorrectElement
              endif

              MSL = MLW
              MFR = MLW - 1

!cipk oct98 update to f90
              MSL=ABS(NOP(N,MSL))
              IF(MSL == NeighbourMidsideOnLine) CYCLE findCorrectElement
              IF(IBN(MSL) == 1) EXIT findCorrectElement
            ENDDO findCorrectElement

cipk oct98 update to f90
              MFR=ABS(NOP(N,MFR))
C-
C...... Now compute boundary angle
C-
              DX=2.*(CORD(MSL,1)-CORD(thisNode,1))
     +           -0.5*(CORD(MFR,1)-CORD(thisNode,1))
              DY=2.*(CORD(MSL,2)-CORD(thisNode,2))
     +           -0.5*(CORD(MFR,2)-CORD(thisNode,2))
              BEETA=ATAN2(DY,DX)
C-
C...... Find angle of side that flow crosses
C-
              DXA=CORD(NeighbourCornerOnLine,1)-CORD(thisNode,1)
              DYA=-(CORD(NeighbourCornerOnLine,2)-CORD(thisNode,2))
              AALFA=ATAN2(DXA,DYA)
              IF(ABS(AALFA-THET) > 1.570796 .AND. 
     +           ABS(AALFA-THET) < 4.712394) THEN
                AALFA=AALFA+3.141596
              ENDIF
              IF(ABS(AALFA-BEETA) > 1.570796 .AND. 
     +           ABS(AALFA-BEETA) < 4.712394) THEN
                BEETA=BEETA+3.141596
              ENDIF
C-
C...... Finally adjust flows
C-
              IF(COS(THET) /= 0.) THEN
              QMAG=SPEC(thisNode,1)*COS(AALFA-THET)/
     +             (COS(THET)*COS(AALFA-BEETA))
              ELSE
              QMAG=SPEC(thisNode,2)*COS(AALFA-THET)/
     +             (SIN(THET)*COS(AALFA-BEETA))
              ENDIF
              SPEC(thisNode,1)=QMAG*COS(BEETA)
              SPEC(thisNode,2)=QMAG*SIN(BEETA)
      end subroutine