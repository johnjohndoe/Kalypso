!     Last change:  NIS  16 Aug 2007    7:42 pm
subroutine TransVelDistribution

!description
!This subroutine calculates the Equation scaling factors for the solution scheme at 1D 2D line transitions. Therefore a mean velocity and cross
!sectional discharge area at the connected 1D element is calculated. From that a discharge is obtained. With that discharge the velocity distribution
!at the connected 2D elements of the transition can be calculated. The result is an Equation scaling factor for the degree of freedoms.
!The transitions collapses to 2 degrees of freedom. First is the velocity perpendicular to the chord of the transitioning line, the second is the
!maximum water depth at the coupling. Both values are referenced at the 1D part of the transition. Thus the equation scaling factors are related to
!the 1D degree of freedom

!declarations
!************

USE BLK10MOD
USE BLKDRMOD
USE PARAKalyps
USE paraFlow1dFE

!nis,jan07: Variables for the 1D-2D transition line
integer            :: TransLi, TransNo
real               :: TransVel, CSArea, Discharge, localVel
REAL (KIND = 8)    :: TransDep
CHARACTER (LEN=26) :: filename_out
!TransLi   :: number of Transition Line
!TransNo   :: number of Transition itself
!TransVel  :: average (1D) velocity at transition
!CSArea    :: cross sectional area at transition
!TransDep  :: maximum (1D) depth at line transition
!discharge :: discharge at line transition (input value for velocity distribution of 2D elements at transition)
!localVel  :: velocity at actual 2D node while processing the 2D part of the transition

!teststat  :: iostat variable for testfile
INTEGER            :: teststat
REAL (KIND = 8) :: Dh, dv

!transitioncheck
REAL (KIND = 8) :: DXtot, DYtot, dxi, dyi, Q1, QI, vnomx, vnomy, gamma, delta
REAL (KIND = 8) :: VelCrossA, SpecCrossA

!init
DXtot = 0.0
DYtot = 0.0
dxi   = 0.0
dyi   = 0.0
Q1    = 0.0
QI    = 0.0
vnomx = 0.0
vnomy = 0.0
gamma = 0.0
delta = 0.0


!testoutput file handling
teststat = 0
WRITE(filename_out,'(a16,i4.4,a4)') 'Transitionoutput',maxn,'.txt'
OPEN(999,filename_out, IOSTAT=teststat)
if (teststat.ne.0) STOP 'error with kopplungsoutput.txt'


!increments for numerical derivative
Dh = 0.0001
dv = 0.001

!for every line transition
transitionloop: do i = 1, MaxLT

  !if transition is empty, cycle loop.
  if (TransLines(i,1) == 0) CYCLE transitionloop

  !get the line number and the 1D-transitioning node number
  TransLi = TransLines(i,2)
  TransNo = nop(TransLines(i,1),3)

  !allocating the temporary velocity and specific discharge placeholder
  ALLOCATE (TransSpec (1:lmt (TransLi)))

  !initializing the temporary velocity and specific discharge placeholder
  do l = 1, lmt(TransLi)
    TransSpec(l) = 0.0
  end do

  !test transition: every segment has to be passed in the same direction (test with sign of cross product)
  !get total chord length of line DYtot und DXges
  DYtot = cord (line (TransLi, lmt(TransLi)), 2) - cord (line (TransLi, 1), 2)
  DXtot = cord (line (TransLi, lmt(TransLi)), 1) - cord (line (TransLi, 1), 1)
  !hole lagewinkel der kopplung (arctan gk/AK = delta)
  Delta = ATAN (DYtot / DXtot)
  !hole richtungswinkel der strömungsrichtung
  gamma = Delta + 3.14159 / 2.0

  !vnomx and vnomy are the components of the flow direction, Q crosses transition.
  !It's always perpendicular to border
  vnomx = COS(gamma)
  vnomy = SIN(gamma)

  do k = 1, lmt(TransLi)-2, 2
    dxi = cord (line (TransLi, k + 2), 1) - cord (line (TransLi, k), 1)
    dyi = cord (line (TransLi, k + 2), 2) - cord (line (TransLi, k), 2)
    QI = vnomx * dyi - vnomy * dxi
    if (k == 1) Q1 = QI

    if (QI * Q1 < 0) then
      WRITE(*,*) 'ERROR - Transition line is too curved'
      WRITE(*,*) 'flow direction through line is not unique defined'
      WRITE(*,*) 'change model!'
      WRITE(*,*) 'execution of program terminated - STOP'
      stop
    end if

  end do

  do j = 1, 2
    !j = 1: Calculate nominal value of specific discharge at nodes
    !j = 2: Calculate numeric derivative (difference quotient) due to LOWER water depth

!deactivated
!    !j = 3: Calculate numeric derivative (difference quotient) due to HIGHER water depth and average numeric derivative
!    !j = 4: Calculate numeric derivative (difference quotient) due to lower flow velocity at a particular node
!    !j = 5: Calculate numeric derivative (difference quotient) due to higher flow velocity at a particular node
!-

    !1D-values
    TransVel = vel (1, TransNo) * COS (alfa( TransNo)) + vel (2, TransNo) * SIN (alfa( TransNo))
    TransDep = vel (3, TransNo)

    !reset values for numerical derivative
    IF (j == 2) THEN
      TransDep = TransDep - Dh
    ELSEIF (j == 3) then
      TransDep = TransDep + 2 * Dh
    END IF

    !get the discharge area
    CSArea = 0.0
    !geometry-approach (RMA10S original)
    if (ABS(width(TransNo)) > 1.e-7) then
      CSArea = (width(TransNo) + 0.5 * (ss1(TransNo) + ss2(TransNo))*TransDep) * TransDep
    !polynom-approach
    else
      DO k = 0, 12
        CSArea = CSArea + apoly(TransNo,k) * TransDep**(k)
      ENDDO
    end if

    !Calculate discharge-value
    Discharge = ABS(CSArea * TransVel)

    !getting the 2D-specific discharges with the QGENTRANSITION-subroutine
    if (j < 4) then

      !Getting the specific discharges at the transition
      WRITE(*,*) 'Calculating velocity distribution for discharge: ', discharge, 'm³/s'
      call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)

      !input:
      !TransLi   = is line, where to set the velocities
      !TransNo   = is node, that couples
      !Discharge = is the discharge to determine the velocity distribution for
      !THET      = the angle related to the direction of the continuity line; defaults and unchangable thet = 0.0
      !output:
      !specific discharge at the nodes depending on water depth and velocity

      !copy values of the specific discharge and the values of the derivatives
      if (j == 1) then
        do k = 1, lmt(TransLi)
          na = line (TransLi, k)
          !specific discharge
          spec(na, 1) = TransSpec (k)
          !direction
          spec(na, 2) = alfa(na)
          !correct direction
          VelCrossA  = vel (1, TransNo) * DYtot - vel (2, TransNo) * DXtot
          SpecCrossA = spec (na, 1) * COS( alfa(na)) * DYtot - spec (na, 1) * SIN( alfa(na)) * DXtot
          !of nominal (1D)-discharge and spec-discharge through line have different signs then correct
          if (SpecCrossA * VelCrossA < 0.0) then
            spec(na, 1) = - spec(na, 1)
          end if
        end do

      !calculate numerical derivative over depth
      ELSEIF (j == 2) then
        do k = 1, lmt(TransLi)
          na = line(TransLi, k)
          !store it in derivative
          dspecdh(na) = TransSpec (k)
          !correct direction
          SpecCrossA = dspecdh (na) * COS( alfa(na)) * DYtot - dspecdh (na) * SIN( alfa(na)) * DXtot
          !of nominal (1D)-discharge and spec-discharge through line have different signs then correct
          if (SpecCrossA * VelCrossA < 0.0) then
            dspecdh(na) = - dspecdh(na)
          end if

          !calculate derivative
          !WRITE(*,*) dspecdh(na), spec(na,1), dh
          dspecdh (na) = (spec(na, 1) - dspecdh(na)) / Dh
        end do
!switched off for the moment
!      ELSEIF (j == 3) then
!        !do k = 1, lmt(TransLi)
!        !  na = line(TransLi, k)
!        !  dspecdh (na) = 0.5 * (dspecdh (na) + (SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2) - spec(na, 1)) / Dh)
!        !  !dspecdh (na) = 0.5 * (dspecdh (na) + (TransSpec(k, 1) * COS (alfa (na)) + TransSpec(k, 2) * sin (alfa (na))) / Dh)
!        !  WRITE(*,*) '3: ', dspecdh(na)
!        !end do
!-
      else
        WRITE(*,*) 'ERROR - this is not possible'
        STOP 'in LineTransitionCalc'
      end if
!switched off for the moment
!
!    !numerical derivative over velocities
!    ELSEIF (j == 4) then
!      !for every independent 2D-node
!      do k = 1, lmt(TransLi)
!        !node
!        na = line(TransLi, k)
!        !velocities
!        vel(1, na) = vel(1, na) + Dv * COS (alfa(na))
!        vel(2, na) = vel(2, na) + Dv * SIN (alfa(na))
!        call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)
!        !store value in derivative
!        dspecdv(na) = TransSpec (k)
!        !correct direction
!        if ( (TransVel < 0.0 .and. dspecdv(na) > 0.0) .or. (TransVel > 0.0 .and. dspecdv(na) < 0.0) ) then
!          dspecdv(na) = dspecdv(na) * (-1.0)
!        end if
!        !calculate derivative
!        dspecdv (na) = (spec(na, 1) - dspecdv(na)) / Dv
!        !reset velocity
!        vel(1, na) = vel(1, na) - Dv * COS (alfa(na))
!        vel(2, na) = vel(2, na) - Dv * SIN (alfa(na))
!      end do
!
!    ELSEIF (j == 5) then
!      !do k = 1, lmt(TransLi)
!      !  na = line(TransLi, k)
!      !  vel(1, na) = vel(1, na) + Dv * COS (alfa(na))
!      !  vel(2, na) = vel(2, na) + Dv * SIN (alfa(na))
!      !  call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)
!      !  WRITE(*,*) '2: ', transspec(k,1), transspec(k,2), spec(na,1), spec(na,2)
!      !  dspecdv (na) = 0.5 * (dspecdv (na) + (SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2) - spec(na, 1)) / Dv)
!      !  !dspecdv(na)=0.5*(dspecdv (na) + ((TransSpec(k, 1) * COS (alfa(na)) + TransSpec(k, 2) * SIN (alfa (na)) ) - spec(na, 1))/Dv)
!      !  vel(1, na) = vel(1, na) - Dv * COS (alfa(na))
!      !  vel(2, na) = vel(2, na) - Dv * SIN (alfa(na))
!      !  WRITE(*,*) '5: ', dspecdv(na)
!      !end do
!-
    ENDIF
  ENDDO

  DEALLOCATE (TransSpec)


  !Get actual 2D-discharge (algorithm out of check-subroutine)
  sumx = 0.0
  sumy = 0.0
  q2D(i) = 0.0

  GetDischarge: do k = 1, lmt(TransLi) - 2, 2
    !get nodes
    NA = LINE (TransLi, K)
    NB = LINE (TransLi, K + 1)
    NC = LINE (TransLi, K + 2)

    !get segment length
    DX = (CORD (NC, 1) - CORD (NA, 1))
    DY = (CORD (NC, 2) - CORD (NA, 2))

    !get nodal water depths
    D1 = VEL (3, NA)
    D3 = VEL (3, NC)
    IF (D1 <= DSET  .OR.  D3 <= DSET) CYCLE GetDischarge
    D2 = (D1 + D3) / 2.

    !calculate discharge components
    SUMX = SUMX + DY * (VEL (1, NA) * D1 + 4.0 * VEL (1, NB) * D2 + VEL (1, NC) * D3) / 6.
    SUMY = SUMY + DX * (VEL (2, NA) * D1 + 4.0 * VEL (2, NB) * D2 + VEL (2, NC) * D3) / 6.
  end do GetDischarge

  !calculate discharge be cross product ([V] x [L])
  q2D(i) = SUMX - SUMY

end do transitionloop

!Close testing file
close (999,STATUS='keep')

end subroutine
