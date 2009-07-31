C     Last change:  WP   17 Apr 2008   10:05 am
CIPK  LAST UPDATE sEP 10 2001 FIX BUG FOR MULTIPLE TRIANGLES
cipk  last update apr 17 2001 fix case for laterally averaged flows
cipk  last update apr 12 2001 fix averages for zero flow case
cipk  LAST UPDATE mar 18 2001 add constituents for average concentration 
cipk  last update feb 09 2001 allow for zero length cc lines
cipk  last update jan 12 1999  add ss1 terms
cipk  last update Jan 21 1998
      SUBROUTINE CHECK

      use mod_storageElt
      use mod_ContiLines
      use mod_Arcs
      use mod_Nodes

      USE BLK10MOD
      USE BLKDRMOD
      USE BLK11MOD
      !nis,feb07: Using module for polynomial approach
      USE ParaKalyps
      USE Para1DPoly
      !-
      SAVE
      
      
      type (contiLine), pointer :: tmpContiLine => null()
      type (arc), pointer :: tmpSeg => null()
      type (node), pointer :: tmpNode => null()
      real (kind = 8), dimension (1:2) :: tmpVec
      real (kind = 8) :: dirScaling
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK JUL01
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK MAR01
      INCLUDE 'BLKH.COM'
      INCLUDE 'BLKS.COM'
C-
cipk jan98 increment to 200
      DIMENSION ITEMP(200),FLW(2),AREAC(2),FLX1(2,3),FLX2(2,3),
     +FLX3(2,3),FLX4(2,3),FLX5(2,3),FLX6(2,3),FLX7(2,3),FLX8(2,3)
     +  ,TOTAL(200)
cipk jul01 add line above

!NiS,jul06: Adding WAITTH and WAITRH for consistency in common block defintion
!      REAL*8 WAITX,WAITT,WAITR
      REAL*8 WAITX,WAITT,WAITR,WAITTH,WAITRH

      REAL (kind = 8) :: TempArea, calcPolynomial
!-
      INTEGER :: PolyPos, findpolynom
!nis,nov06: For line connections
      INTEGER :: linecheck, LineLength
!-
      type (StorageElement), pointer :: SElt => null()

      COMMON /WATP/ WAITT(7),WAITR(9),WAITTH(16),WAITRH(16)
C-
      COMMON 
     1 XN(8),WAITX(16)

      DATA NCALL/0/
      IF( NCL .LE. 0 ) RETURN

      !initializations
      LineLength = 0

      !nis,sep06,com: Jump over, if not called the first time
      IF( NCALL .GT. 0 ) GO TO 140
C-
C-..... Augment continuity lists.....
C-
      !nis,sep06,com: Run through every CCL to overgive the real number of nodes
      DO 125 J = 1, NCL
        !assign continuity line
        tmpContiLine => ccls(j)
        NULlIFY (tmpSeg)
        M = LMT(J)
cipk feb01  skip out for zero lines
        !nis,sep06,com: Cycle loop, if CCL has no node
        IF(M .EQ. 0) GO TO 125
        !nis,sep06,com: ITEMP(K) temporaryly stores the CCL J
        DO 113 K = 1, M
          ITEMP(K) = LINE(J,K)
  113   CONTINUE
        !nis,sep06,com: Calculate the real number of nodes of CCL J
        LMT(J) = 2*LMT(J)-1
        !nis,sep06,com: Copy of real number of nodes of CCL J
        LineLength = LMT(J)
        !nis,sep06,com: Counter for corner nodes
        N = 0
        !nis,sep06,com: Jump over, if the CCL consist only of one node
        IF(M .EQ. 1) GO TO 125

        !nis,sep06: New, taken from Kalypso-2D-code. Initializing the fields
        !           line(J,L) and lineimat (J,L) for the CCL J. The values of line(J,L) are
        !           temporaryly stored in ITEMP(L), so nothing can happen.
        !           lineimat(J,L) is a new field for the storage of the material
        !           type to improve the calculation of boundary conditions at inflows/outflows
        do l=1,LineLength
          line(j,l) = 0
          lineimat(j,l) = 0
        end do
        !-


        !nis,sep06,com: Loop for restructuring saved CCL J;
        !               Originally only the corner nodes were saved
        !               Afterwards the midside nodes are stored in between the corneer nodes
        !nis,sep06: the CCL has only to be runned through all segments. The segments always begin
        !           with one of the nodes. The last node starts no segment, so it can be taken out
        !           of the loop.
        !DO 124 L = 1, LineLength, 2
        
        DO 124 L = 1, LineLength-2, 2
        !-
  
          if (.not. (associated (tmpSeg))) then
            tmpSeg => tmpContiLine.firstSegment
          else
            tmpSeg => tmpSeg.nextSeg
          endif
          !nis,sep06,com: Count the number of segments
          N = N + 1
          !nis,sep06,com: Get the two actual corner nodes in CCL J
          NA = ITEMP(N)
          NC = ITEMP(N+1)
          !nis,sep06,com: Rearrange the storage of those nodes with a space in between
          LINE(J,L) = NA
          LINE(J,L+2) = NC
          !nis,sep06,com: Run through all elements to find the midside node between the two actual nodes
          DO 123 JJ = 1, NE

            !nis,sep06: If the material type of the adjacent element is 0, no flow
            !           can occur there: Jump over that segment
            if (imat(jj).eq.0) GO TO 123
            !-

            !nis,sep06,com: Get number of corner nodes (element arcs)
            NCN=NCORN(JJ)
            !nis,sep06,com: Run through all arcs of element to find the midside node
            DO 117 KK = 1, NCN, 2
              !nis,sep06,com: Get the two corner nodes of actual arc
              KKK = MOD(KK+2,NCN)
              IF(KKK .EQ. 0) KKK=3
CIPK OCT98 CONVERT TO F90
              N1 = ABS(NOP(JJ,KK))
              N2 = ABS(NOP(JJ,KKK))
              !nis,sep06,com: Compare corner nodes with actual nodes of CCL J
              !               If it fits at one element process the midside
              !               node at marker 115
              IF( NA .EQ. N1 .AND. NC .EQ. N2 ) GO TO 115
              IF( NC .EQ. N1 .AND. NA .EQ. N2 ) GO TO 115
              !nis,sep06,com: If the comparison doesn't fit, cycle loop
              GO TO 117
CIPK OCT98 CONVERT TO F90
              !nis,sep06,com: enter the correct midside node reference into the CCL J
  115         LINE(J,L+1) = ABS(NOP(JJ,KK+1))
              !Assing midside node to arc
              
              allocate (tmpNode)
              tmpNode = newNode (line(j,l+1), 
     +                    cord(line(j,l+1), 1), cord(line(j,l+1), 2))
              call addMidside (tmpSeg, tmpNode)
              nullify (tmpNode)

              !nis,sep06: Note the roughness class of the of the adjacent element, if it is a boundary node
              lineimat(j,l+1) = ABS(imat(jj))
              lineelement (j, l+1) = jj
              LineCorrectionKS(j, l+1)   = CorrectionKS(jj)
              LineCorrectionAxAy(j, l+1) = CorrectionAxAy(jj)
              LineCorrectionDp(j, l+1)   = CorrectionDp(jj)

              possibleLines: do i = 1, maxlt
                if (TransLines (i, 2) == j) then
                  TransitionElement (jj) = .true.
                  TransLinePart (jj) = i
                endif
              end do possibleLines

              !nis,sep06,com: end loop, if the correct midside node is found and go back
              !               to cycle loop over CCL segments
              GO TO 124
  117       CONTINUE
  123     CONTINUE
CIPK JUL02 TEST FOR FAILURE TO FIND CONTINUITY LINE
          !TODO: ErrorMessage
          IF(LINE(J,L+1) .EQ. 0) THEN
CIPK SEP04
            IF(IERR .NE. 1) THEN
              CLOSE(75)
              OPEN(75,file='ERROR.OUT')
            ENDIF
            WRITE(*,*) 'ERROR PROCESSING CONTINUITY LINE ',J
            WRITE(75,*) 'ERROR PROCESSING CONTINUITY LINE ',J
            WRITE(*,*) ' UNABLE TO FIND NODE BETWEEN NODES ',
     +      LINE(J,L),LINE(J,L+2)
            WRITE(75,*) ' UNABLE TO FIND NODE BETWEEN NODES ',
     +      LINE(J,L),LINE(J,L+2)
            IERR=1
          ENDIF
  124   CONTINUE

      !nis,nov06: Here the Continuity lines are complete. Now the midside nodes at 1D-2D-line-transitions can be set with alfak. It is the
      !           continuation of the parts in the GETGEO.subroutine. It will lead to problems, if there was only 1 element, that defines
      !           the transition. For that case it is already tested in the subroutine GETGEO. Thus at this point, there is no possibility
      !           that the transition line is shorter than 2 elements.
        if (MaxLT.ne.0) then

          !for every transition
          Transitiontest: do i=1,MaxLT

            !test, whether actual Continuityline is a Transitionline
            if (J.eq.TransLines(i,2)) then

              !LMT of the Line must be .ge.5, because the minimum element number is 2 (tested in GETGEO.subroutine)
              DO nodeno=2,LMT(J),2
                !all midside nodes get the direction angle of the 3rd node in the line. There are a combination of reasons
                !for chosing the 3rd node
                !1.    The third node is always present. At the minimum Transitionline with 5 nodes, the 1st and the 5th describe
                !      the corner nodes, the 2nd and 4th describe the midside node without alfak value and the 3rd describes a
                !      corner node within the the Transitionline.
                !2.    The 1st and the last corner node don't have the direction of the flow angle alfak. Their direction
                !      is dependent on the parallel flow boundary direction. The governing boundaries are connected at those
                !      nodes.
                !Concluding these things, that first the 3rd node is always present and always has the correct alfak-direction,
                !leads to the taken choice.
                alfak(LINE(J,nodeno)) = alfak(LINE(J,3))

              END DO
              !jump out of loop over all transitions
              EXIT Transitiontest
            end if
          end do Transitiontest
        end if

  125 CONTINUE

      !nis,jun07: deactivating line check appeareance
      ! !nis,nov06: Check, whether all 1D-2D-line-Transitions are connected. The Question ist, whether connecting node is part of the transition line:
      if (MaxLT.ne.0) then
        call check_linetransition
      end if
      ! !-
      !-

CIPK JUL02
      !TODO: ErrorMessage
      IF(IERR .EQ. 1) THEN
        WRITE(*,*)
     +   'EXECUTION TERMINATED - FULL LIST OF ERRORS IN ERROR.OUT'
        STOP
      ENDIF

      !EFa jul07, initialization for stage-flow boundaries
      CALL STFLTAB (M, SRFEL, DFDH, FF, 0)
      !-

CIPK JUL01
      IF(IOCON .GT. 0) THEN
        WRITE(IOCON,6200) (J,J=1,NCL)
 6200	  FORMAT('      TOTAL CONTINUITY LINE FLOWS BY CONTINUITY LINE NUM
     +BER'//'  DAY      HOUR',I9,19I11)
      ENDIF


      IF(NB .EQ. 0) then
        ncall = 1
        RETURN
      endif
  140 CONTINUE
      WRITE(LOUT,6030)

      ND = NB
      DO 180 J = 1, NCL
        !get current continuity line
        tmpContiLine => ccls (j)
CIPK OCT98        NTL=1
        SUMX = 0.0
        SUMY = 0.0
cipk jul01
        TOTAL(J) =0.0
        SUMXS= 0.0
        SUMXT= 0.0
        SUMXSD=0.0
        SUMYS= 0.0
        SUMYT= 0.0
        SUMYSD=0.0
cipk feb01  skip out for zero lines
        IF(LMT(j) .EQ. 0) THEN
          GO TO 180
        !---------------------------------------------------------------------
        !calculate discharge through 1D or 2D horizontally averaged boundaries
        !---------------------------------------------------------------------
        ELSEIF(LMT(J) .EQ. 1) THEN
          NA=LINE(J,1)
          !1D 1D 1D 1D 1D 1D 1D 1D 
          !-----------------------
          IF(NDEP(NA) .LT. 2) THEN
            !nis,feb07: Allow for continuity checks at nodes with polynomial approach
            !Proceed on nodes of polynomial approach
            if (width (na) == 0.0) then

              PolyPos = findPolynom (polyRangeA (NA, :), vel(3, NA),
     +                               PolySplitsA (na), cord (na, 1),
     +                               cord (na, 2), na)
              TempArea = calcPolynomial (apoly(PolyPos, NA, 0:12),
     +                                   vel(3, NA), ubound (apoly, 3))

              if (vel(1,na) /= 0.0d0 .and. vel(2,na) /= 0.0d0) then
                dirScaling = projectionDirPointer
     +          (tmpContiLine.posNormal, vel(1:2,na))
              else
                dirScaling = 1.0d0
              endif
              SUMX = SQRT(vel(1,na)**2 + vel(2,NA)**2) * TempArea
     +               * dirScaling

            !nis,feb07,com: Original trapezoidal cross sections
            else
              SUMX=SQRT(VEL(1,NA)**2+VEL(2,NA)**2)*VEL(3,NA)*
     +           (2.*WIDTH(NA)+(SS1(NA)+SS2(NA))*VEL(3,NA))/2.
cipk mar01 add constituents
            endif
            SUMXS=SUMX*VEL(4,NA)
            SUMXT=SUMX*VEL(5,NA)
            SUMXSD=SUMX*VEL(6,NA)
          !2D horizontally averaged
          !------------------------
          ELSE
            K=NREF(NA)
            L=K+NDEP(NA)-3
            SUMX=0.0
            XHT=ELEV-AO(NA)
            DO 160 M=K,L,2
              IF(M .EQ. K) THEN
                M1=NA
              ELSE
                M1=M
              ENDIF
              FL=(CORD(M1,3)-CORD(M+2,3))/XHT
              CAS=COS(ALFA(NA))
              SN=SIN(ALFA(NA))
              V1=VEL(1,M1)*CAS+VEL(2,M1)*SN
              V2=4.*(VEL(1,M+1)*CAS+VEL(2,M+1)*SN)
              V3=VEL(1,M+2)*CAS+VEL(2,M+2)*SN
CIPK JAN99
              XFC = VEL(3,NA)/XHT 
              V1 = V1*( WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                 (CORD(M1,3)-AO(M1) - xht*fl/6.) )

              V2 = V2*(WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                     (CORD(M+1,3)-AO(M+1)) )

              V3 = V3*(WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                     (CORD(M+2,3)-AO(M+2)  + xht*fl/6.) )

CIPK JAN99              SUMX=SUMX+(V1+V2+V3)/6.*VEL(3,NA)*FL*WIDTH(NA)
CIPK MAR01  ADD OTHER CONSTITUENTS
              S1=V1*VEL(4,M1)/6.*VEL(3,NA)*FL
              T1=V1*VEL(5,M1)/6.*VEL(3,NA)*FL
              SD1=V1*VEL(6,M1)/6.*VEL(3,NA)*FL
cip apr01 fix error using V1 for lines below
              S2=V2*VEL(4,M+1)/6.*VEL(3,NA)*FL
              T2=V2*VEL(5,M+1)/6.*VEL(3,NA)*FL
              SD2=V2*VEL(6,M+1)/6.*VEL(3,NA)*FL
              S3=V3*VEL(4,M+2)/6.*VEL(3,NA)*FL
              T3=V3*VEL(5,M+2)/6.*VEL(3,NA)*FL
              SD3=V3*VEL(6,M+2)/6.*VEL(3,NA)*FL
              TEMP=(V1+V2+V3)/6.*VEL(3,NA)*FL
              SUMX=SUMX+TEMP
              SUMXS=SUMXS+(S1+S2+S3)
              SUMXT=SUMXT+(T1+T2+T3)
              SUMXSD=SUMXSD+(SD1+SD2+SD3)
  160       CONTINUE
          ENDIF
        !---------------------------------------------------------------------------------
        !calculate discharge through continuity line at 2D depth averaged or 3D boundaries
        !---------------------------------------------------------------------------------
        ELSE
          MAX = LMT(J)-2
          if (associated (tmpSeg)) nullify (tmpSeg)
          DO 700 K = 1, MAX, 2
            if (.not. (associated (tmpSeg))) then
              tmpSeg => tmpContiLine.firstSegment
            else
              tmpSeg => tmpSeg.nextSeg
            endif

            NA = tmpSeg.first.ID
            NB = tmpSeg.midside.ID
            NC = tmpSeg.last.ID
            
            tmpVec = arcVector (tmpSeg)
            
            DX=(CORD(NC,1)-CORD(NA,1))
            DY=(CORD(NC,2)-CORD(NA,2))

            !3D applications
            !---------------
            IF(NDEP(NB) .GT. 1) THEN
              !temporary
              continue
              !temporary-
!--------------------------------------------------------------------------------------
!3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D
!--------------------------------------------------------------------------------------
!              IDPI=NDEP(NA)
!              IDPK=NDEP(NC)
!              I1=NA
!              J1=NB
!              K1=NC
!C-
!C...... Count elements downward
!C-
!              NEL=NDEP(NA)-2
!              IF(NDEP(NC)-2 .GT. NEL) NEL=NDEP(NC)-2
!              DO 500 M=1,NEL,2
!C-
!C...... Test for triangle
!C-
!                IF(IDPK .GT. 1) THEN
!                  IF(IDPI .GT. 1) THEN
!C-
!C...... This is quadrilateral
!C-
!                    I2=NREF(NA)+M
!                    I3=I2+1
!                    J2=NREF(NB)+(M+1)/2
!                    K2=NREF(NC)+M
!                    K3=K2+1
!                    VF1=VEL(3,NA)/(ELEV-AO(NA))
!                    VF2=VEL(3,NC)/(ELEV-AO(NC))
!                    VDIST=((CORD(I1,3)-CORD(I3,3))*VF1
!     +                    +(CORD(K1,3)-CORD(K3,3))*VF2)/2.
!                    AREAC(1)=VDIST*DY
!                    AREAC(2)=VDIST*DX
!                    DO  LA=1,2
!                      FLW(LA)=((VEL(LA,I2)+VEL(LA,J2)+VEL(LA,K2)
!     +                +VEL(LA,J1))/3.-(VEL(LA,I1)+VEL(LA,I3)+VEL(LA,K3)
!     +                +VEL(LA,K1))/12.)*AREAC(LA)
!CIPK MAR01   ADD OTHER CONSTITUENTS
!
!C-
!C-.....COPY PROPER WEIGHTING FUNCTIONS.....
!C-
!                      NGP = 9
!                      NCN = 8
!C-
!C-.....COPY SHAPE FUNCTIONS
!C-
!                      CALL SB2(NCN,NGP)
!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
!                      DO I = 1, NGP
!                        WAITX(I) = WAITR(I)/4.
!                        DO JJ = 1, NCN
!                          XN(JJ) = XNX(JJ,I)
!                        ENDDO
!
!                  VX=VEL(LA,I1)*XN(1)+VEL(LA,I2)*XN(2)+VEL(LA,I3)*XN(3)
!     +              +VEL(LA,J2)*XN(4)+VEL(LA,K3)*XN(5)+VEL(LA,K2)*XN(6)
!     +              +VEL(LA,K1)*XN(7)+VEL(LA,J1)*XN(8)
!                  S1=VEL(4,I1)*XN(1)+VEL(4,I2)*XN(2)+VEL(4,I3)*XN(3)
!     +              +VEL(4,J2)*XN(4)+VEL(4,K3)*XN(5)+VEL(4,K2)*XN(6)
!     +              +VEL(4,K1)*XN(7)+VEL(4,J1)*XN(8)
!                  T1=VEL(5,I1)*XN(1)+VEL(5,I2)*XN(2)+VEL(5,I3)*XN(3)
!     +              +VEL(5,J2)*XN(4)+VEL(5,K3)*XN(5)+VEL(5,K2)*XN(6)
!     +              +VEL(5,K1)*XN(7)+VEL(5,J1)*XN(8)
!                  SD1=VEL(6,I1)*XN(1)+VEL(6,I2)*XN(2)+VEL(6,I3)*XN(3)
!     +              +VEL(6,J2)*XN(4)+VEL(6,K3)*XN(5)+VEL(6,K2)*XN(6)
!     +              +VEL(6,K1)*XN(7)+VEL(6,J1)*XN(8)
!                        IF(LA .EQ. 1) THEN
!                          SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
!                          SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
!                          SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
!                        ELSE
!                          SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
!                          SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
!                          SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
!                        ENDIF
!                      ENDDO
!
!                    ENDDO
!                    SUMX=SUMX+FLW(1)
!                    SUMY=SUMY+FLW(2)
!cipk jul01
!                    TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
!                    IDPI=IDPI-2
!                    IDPK=IDPK-2
!                    I1=I3
!                    J1=J2
!                    K1=K3
!                  ELSE
!C-
!C...... Triangle type 1
!C-
!                    J2=NREF(NB)+(M+1)/2
!                    K2=NREF(NC)+M
!                    K3=K2+1
!                    VDIST=(CORD(K1,3)-CORD(K3,3))*VEL(3,NC)/(2.*
!     +              (ELEV-AO(NC)))
!                    AREAC(1)=VDIST*DY
!                    AREAC(2)=VDIST*DX
!                    DO 425 LA=1,2
!                      FLW(LA)=(VEL(LA,J1)+VEL(LA,J2)+VEL(LA,K2))/3.
!     +                        *AREAC(LA)
!CIPK MAR01   ADD OTHER CONSTITUENTS
!
!C-
!C-.....COPY PROPER WEIGHTING FUNCTIONS.....
!C-
!                      NGP = 7
!	                NCN = 6
!C-
!C-.....COPY SHAPE FUNCTIONS
!C-
!                      CALL SB2(NCN,NGP)
!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
!                      DO I = 1, NGP
!                        WAITX(I) = WAITT(I)*2.
!                        DO JJ = 1, NCN
!                          XN(JJ) = XNX(JJ,I)
!                        ENDDO
!
!                  VX=VEL(LA,I3)*XN(1)+VEL(LA,J2)*XN(2)+VEL(LA,K3)*XN(3)
!     +              +VEL(LA,K2)*XN(4)+VEL(LA,K1)*XN(5)+VEL(LA,J1)*XN(6)
!                  S1=VEL(4,I3)*XN(1)+VEL(4,J2)*XN(2)+VEL(4,K3)*XN(3)
!     +              +VEL(4,K2)*XN(4)+VEL(4,K1)*XN(5)+VEL(4,J1)*XN(6)
!                  T1=VEL(5,I3)*XN(1)+VEL(5,J2)*XN(2)+VEL(5,K3)*XN(3)
!     +              +VEL(5,K2)*XN(4)+VEL(5,K1)*XN(5)+VEL(5,J1)*XN(6)
!                  SD1=VEL(6,I3)*XN(1)+VEL(6,J2)*XN(2)+VEL(6,K3)*XN(3)
!     +              +VEL(6,K2)*XN(4)+VEL(6,K1)*XN(5)+VEL(6,J1)*XN(6)
!                        IF(LA .EQ. 1) THEN
!                          SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
!                          SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
!                          SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
!                        ELSE
!                          SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
!                          SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
!                          SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
!                        ENDIF
!                      ENDDO
!
!  425               CONTINUE
!                    SUMX=SUMX+FLW(1)
!                    SUMY=SUMY+FLW(2)
!CIPK JUL01
!                    TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
!CIPK SEP01
!                    IDPK=IDPK-2
!                    J1=J2
!                    K1=K3
!
!CIPK SEP01                    GO TO 700
!                  ENDIF
!                ELSE
!C-
!C...... Triangle type 2
!C-
!                  J2=NREF(NB)+(M+1)/2
!                  I2=NREF(NA)+M
!                  I3=I2+1
!                  VDIST=(CORD(I1,3)-CORD(I3,3))*VEL(3,NA)/
!     +                  (2.*(ELEV-AO(NA)))
!                  AREAC(1)=VDIST*DY
!                  AREAC(2)=VDIST*DX
!                  DO 475 LA=1,2
!                    FLW(LA)=(VEL(LA,J1)+VEL(LA,J2)+VEL(LA,I2))/
!     +                      3.*AREAC(LA)
!CIPK MAR01   ADD OTHER CONSTITUENTS
!
!C-
!C-.....COPY PROPER WEIGHTING FUNCTIONS.....
!C-
!                    NGP = 7
!	              NCN = 6
!C-
!C-.....COPY SHAPE FUNCTIONS
!C-
!                    CALL SB2(NCN,NGP)
!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
!                    DO I = 1, NGP
!                      WAITX(I) = WAITT(I)*2.
!                      DO JJ = 1, NCN
!                        XN(JJ) = XNX(JJ,I)
!                      ENDDO
!
!                  VX=VEL(LA,I1)*XN(1)+VEL(LA,I2)*XN(2)+VEL(LA,I3)*XN(3)
!     +              +VEL(LA,J2)*XN(4)+VEL(LA,K3)*XN(5)+VEL(LA,J1)*XN(6)
!                  S1=VEL(4,I1)*XN(1)+VEL(4,I2)*XN(2)+VEL(4,I3)*XN(3)
!     +              +VEL(4,J2)*XN(4)+VEL(4,K3)*XN(5)+VEL(4,J1)*XN(6)
!                  T1=VEL(5,I1)*XN(1)+VEL(5,I2)*XN(2)+VEL(5,I3)*XN(3)
!     +              +VEL(5,J2)*XN(4)+VEL(5,K3)*XN(5)+VEL(5,J1)*XN(6)
!                  SD1=VEL(6,I1)*XN(1)+VEL(6,I2)*XN(2)+VEL(6,I3)*XN(3)
!     +              +VEL(6,J2)*XN(4)+VEL(6,K3)*XN(5)+VEL(6,J1)*XN(6)
!                      IF(LA .EQ. 1) THEN
!                        SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
!                        SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
!                        SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
!                      ELSE
!                        SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
!                        SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
!                        SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
!                      ENDIF
!                    ENDDO
!
!  475             CONTINUE
!                  SUMX=SUMX+FLW(1)
!                  SUMY=SUMY+FLW(2)
!CIPK JUL01
!                  TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
!CIPK SEP01
!                    IDPI=IDPI-2
!                    J1=J2
!                    I1=I3
!CIPK SEP01                  GO TO 700
!                ENDIF
!  500         CONTINUE
!--------------------------------------------------------------------------------------
!3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D 3D
!--------------------------------------------------------------------------------------
            ELSE
C-
C........ Two dimensional element
C-
              !get water levels of segment
              d1 = vel (3, na)
              d3 = vel (3, nc)
              if (d1 <= dset .or. d3 <= dset) go to 700
              d2 = (d1 + d3)/ 2.

CIPK MAR01  ADD OTHER CONSTITUENTS
              !calculate x-flow
              dirScaling = normalDirPointer (tmpSeg)
              dy = tmpVec(2) * dirScaling
              TEMPX=DY*(VEL(1,NA)*D1+4.0*VEL(1,NB)*D2+VEL(1,NC)*D3)
     1             /6.
              !calculate y-flow
              dx = tmpVec(1) * dirScaling
              TEMPY=DX*(VEL(2,NA)*D1+4.0*VEL(2,NB)*D2+VEL(2,NC)*D3)
     1             /6.
              NGP=4
              DO I = 1, NGP
C-
C......DEFINE SHAPE FUNCTIONS AND ACCUMULATE
C-
                XN(1)=(1.-AFACT(I))*(1.-2.*AFACT(I))
                XN(2)=(1.-AFACT(I))*4.*AFACT(I)
                XN(3)=(2.*AFACT(I)-1.)*AFACT(I)
                VX=VEL(1,NA)*XN(1)+VEL(1,NB)*XN(2)+VEL(1,NC)*XN(3)
                VY=VEL(2,NA)*XN(1)+VEL(2,NB)*XN(2)+VEL(2,NC)*XN(3)
                DP=VEL(3,NA)*XN(1)+VEL(3,NB)*XN(2)+VEL(3,NC)*XN(3)
                S1=VEL(4,NA)*XN(1)+VEL(4,NB)*XN(2)+VEL(4,NC)*XN(3)
                T1=VEL(5,NA)*XN(1)+VEL(5,NB)*XN(2)+VEL(5,NC)*XN(3)
                SD1=VEL(6,NA)*XN(1)+VEL(6,NB)*XN(2)+VEL(6,NC)*XN(3)
                SUMXS=SUMXS+DY*VX*DP*S1*HFACT(I)/2.
                SUMXT=SUMXT+DY*VX*DP*T1*HFACT(I)/2.
                SUMXSD=SUMXSD+DY*VX*DP*SD1*HFACT(I)/2.
                SUMYS=SUMYS+DX*VY*DP*S1*HFACT(I)/2.
                SUMYT=SUMYT+DX*VY*DP*T1*HFACT(I)/2.
                SUMYSD=SUMYSD+DX*VY*DP*SD1*HFACT(I)/2.
              ENDDO

              SUMX=SUMX+TEMPX
              SUMY=SUMY+TEMPY
            ENDIF
  700     CONTINUE
        ENDIF
CIPK JUL01
        TOTAL(J) = SUMX - SUMY
        !Save the water that's going through the line within the current step
        if (associated (ccls(j).storageElt)) then
          if (ncall == 0) then
            ccls(j).storageElt.currQ = Total (j)
          else
            if (maxn == 1) then
              ccls(j).storageElt.prevQ = ccls(j).storageElt.currQ 
            endif
            ccls(j).storageElt.currQ = Total (j)
            ccls(j).storageElt.storageAddition = 
     +        QAverage(ccls(j).storageElt) * delt
     
            SElt => ccls(j).storageElt
            WL = waterlevel (SElt, .true.)
          endif
        endif

cipk apr01
CIPK JUL01
        IF(TOTAL(J) .NE. 0.) THEN
        AVES(J) = (SUMXS - SUMYS)/TOTAL(J)
        AVET(J) = (SUMXT - SUMYT)/TOTAL(J)
        AVESD(J)= (SUMXSD - SUMYSD)/TOTAL(J)
        ELSE
          AVES(J)=0.
          AVET(J)=0.
          AVESD(J)=0.
        ENDIF
CIPK JUL01
        IF( J .EQ. 1 ) REF = TOTAL(J)
        IF(ABS(REF) .LT. 0.0001) REF=1.
CIPK JUL01
        PCT = 100.0*TOTAL(J)/REF
CIPK OCT98        MX = LMT(J)
        WRITE(LOUT,6035)
     +	  J,TOTAL(J),SUMX,SUMY,PCT,AVES(J),AVET(J),AVESD(J)
  180 CONTINUE
      NB = ND
CIPK JUL01
      IF(IOCON .GT. 0) THEN
        DO JJ=1,NCL,20
          JT=MIN(JJ+19,NCL)
          IF(JJ .EQ. 1) THEN
            WRITE(IOCON,'(I5,F10.2,1P20E11.3)')
     +		 DAYOFY,TET,(TOTAL(J),J=JJ,JT)
          ELSE
            WRITE(IOCON,'(15X,1P20E11.3)') (TOTAL(J),J=JJ,JT)
          ENDIF
        ENDDO
      ENDIF
      
      !remember that first call is over
      ncall = 1
      
      RETURN
      
 6030 FORMAT( // 10X, 'CONTINUITY CHECKS' //  10X, 'LINE          TOTAL
     1         X FLOW         Y FLOW   PERCENT  AVE-SALT  AVE-TEMP   AVE
     +-SED' )
 6035 FORMAT( 10X, I4, 1P3E15.3, 0PF10.1,3F10.2 )
      END
