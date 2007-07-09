!     Last change:  WP    9 Jul 2007   10:26 am

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
real               :: TransVel, CSArea, TransDep, Discharge, localVel
CHARACTER (LEN=25) :: filename_out
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


!nis,jan07: Getting the equation factors for the 1D-2D line transition
!steps to do:
!1. jumping over, if it was 1st iteration - Reason: In the first iteration, the water depth values at the coupling are zero, so no calculations are possible
!2. getting the discharge through transition by usage of the 1D-node's velocity and discharge area
!3. getting the velocities that should occur at a plane water surface with the subroutine for 2D-BC from Kalypso-2D approach
!4. calculating each velocity factor (relation between nodal velocity at 2D-part to 1D-velocity)
!5. Calculating each water depth factor (relation between nodal water depth at 2D-part to 1D-depth)

!step1: only process, if it was not the 1st iteration
!******

teststat = 0
WRITE(filename_out,*) 'Transitionoutput',maxn,'.txt'
OPEN(999,filename_out, IOSTAT=teststat)
if (teststat.ne.0) STOP 'error with kopplungsoutput.txt'

Dh = 0.0001
dv = 0.001

!if (maxn /= 1 .or. RESTARTSwitch /= 0) then
  !for every line transition
  transitionloop: do i = 1, MaxLT

    !step2: getting the discharge through transition with values from last iteration, differ between King-1D-elements and Teschke-1D-elements
    !******
    !if transition is empty, cycle loop.
    if (TransLines(i,1) == 0) CYCLE transitionloop

    !get the line number and the 1D-transitioning node number
    TransLi = TransLines(i,2)
    TransNo = nop(TransLines(i,1),3)

    !allocating the temporary velocity and specific discharge placeholder
    ALLOCATE (TransitionVels ( 1 : lmt(TransLi) ), TransSpec(1:lmt(TransLi), 1:2) )

    do j = 1, 5
      !j = 1: Calculate nominal value of specific discharge at nodes
      !j = 2: Calculate numeric derivative (difference quotient) due to LOWER water depth
      !j = 3: Calculate numeric derivative (difference quotient) due to HIGHER water depth and average numeric derivative
      !j = 4: Calculate numeric derivative (difference quotient) due to lower flow velocity at a particular node
      !j = 5: Calculate numeric derivative (difference quotient) due to higher flow velocity at a particular node


      !initializing the temporary velocity and specific discharge placeholder
      do l = 1, lmt(TransLi)
        TransitionVels(l) = 0.0
        TransSpec(l, 1) = 0.0
        TransSpec(l, 2) = 0.0
      end do

      !get the 1D-velocity; the transition node is always the 3rd one of the 1D-part definition nop(j,3)
      TransVel = SQRT((vel(1,TransNo))**2 + (vel(2,TransNo))**2)
      TransDep = vel(3,TransNo)


      IF (j == 2) THEN
        TransDep = TransDep - Dh
      ELSEIF (j == 3) then
        TransDep = TransDep + 2 * Dh
      END IF

      !get the discharge area
      !if width is not zero it is a King element (testing, the value should be less then machine accuracy)
      CSArea = 0.0

      !if width is not zero a trapezoidal profile of the original RMA10S code is used
      if (ABS(width(TransNo)) > 1.e-7) then
        CSArea = (width(TransNo) + 0.5 * (ss1(TransNo) + ss2(TransNo))*TransDep) * TransDep

      !if width is zero, but the element is 1D it can only be a Teschke element
      else
       !polynomial calculation
       DO k = 0, 12
         CSArea = CSArea + apoly(TransNo,k) * TransDep**(k)
       ENDDO
      end if

      !Calculate discharge
      Discharge = CSArea * TransVel

      !testing
      WRITE(999,*) 'Values from 1D-part'
      WRITE(999,*) 'Area:',CSArea
      WRITE(999,*) 'TransNo', ', width(TransNo)', ', TransDep'
      WRITE(999,*) TransNo, width(TransNo), TransDep
      WRITE(999,*) 'TransLi', ', discharge', ', TransVel'
      WRITE(999,*) TransLi, discharge, TransVel

      !step3: getting the 2D-specific discharges with the QGEN-subroutine part
      !******

    if (j < 4) then
      !some needed input values:
      !j    = is line, where to set the velocities;                        here it equals 'TransLi'
      !QREQ = is the discharge to determine the velocity distribution for; here it equals 'Discharge'
      !THET = the angle related to the direction of the continuity line; defaults and unchangable thet = 0.0

      !Getting the specific discharges at the transition
      call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)

      !Write test output lines
      WRITE(999,*) 'Applying factors'
      WRITE(999,*) '1D-water vel and h'
      WRITE(999,*) TransVel, TransDep
      WRITE(999,*) 'x-vel-factor, y-vel-factor, h-factor, ', 'xvel, yvel, h'

      !copy values of the specific discharge and the values of the derivatives
      if (j == 1) then
        do k = 1, lmt(TransLi)
          na = line (TransLi, k)
          spec(na, 1) = SQRT(TransSpec(k, 1)**2 + TransSpec(k, 2)**2)
          spec(na, 2) = alfa(na)
        end do
      ELSEIF (j == 2) then
        do k = 1, lmt(TransLi)
          na = line(TransLi, k)
          dspecdh (na) = (spec(na, 1) - SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2)) / Dh
        end do
      ELSEIF (j == 3) then
        do k = 1, lmt(TransLi)
          na = line(TransLi, k)
          dspecdh (na) = 0.5 * (dspecdh (na) + (SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2) - spec(na, 1)) / Dh)
        end do
      else
        WRITE(*,*) 'ERROR - this is not possible'
        STOP 'in LineTransitionCalc'
      end if
    ELSEIF (j == 4) then
      do k = 1, lmt(TransLi)
        na = line(TransLi, k)
        vel(1, na) = vel(1, na) - Dv * COS (alfa(na))
        vel(2, na) = vel(2, na) - Dv * SIN (alfa(na))
        call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)
        dspecdv (na) = (spec(na, 1) - SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2)) / Dv
        vel(1, na) = vel(1, na) + Dv * COS (alfa(na))
        vel(2, na) = vel(2, na) + Dv * SIN (alfa(na))
      end do
    ELSEIF (j == 5) then
      do k = 1, lmt(TransLi)
        na = line(TransLi, k)
        vel(1, na) = vel(1, na) + Dv * COS (alfa(na))
        vel(2, na) = vel(2, na) + Dv * SIN (alfa(na))
        call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)
        dspecdv (na) = 0.5 * (dspecdv (na) + (SQRT( TransSpec(k, 1)**2 + TransSpec(k, 2)**2) - spec(na, 1)) / Dv)
        vel(1, na) = vel(1, na) - Dv * COS (alfa(na))
        vel(2, na) = vel(2, na) - Dv * SIN (alfa(na))
      end do
    endif
    ENDDO

    DEALLOCATE (TransitionVels, TransSpec)

    sumx = 0.0
    sumy = 0.0
    GetDischarge: do k = 1, lmt(TransLi) - 2, 2
      NA = LINE (TransLi, K)
      NB = LINE (TransLi, K + 1)
      NC = LINE (TransLi, K + 2)
      DX = (CORD (NC, 1) - CORD (NA, 1))
      DY = (CORD (NC, 2) - CORD (NA, 2))
      D1 = VEL (3, NA)
      D3 = VEL (3, NC)
      IF (D1 <= DSET  .OR.  D3 <= DSET) CYCLE GetDischarge
      D2 = (D1 + D3) / 2.
      SUMX = SUMX + DY * (VEL (1, NA) * D1 + 4.0 * VEL (1, NB) * D2 + VEL (1, NC) * D3) / 6.
      SUMY = SUMY + DX * (VEL (2, NA) * D1 + 4.0 * VEL (2, NB) * D2 + VEL (2, NC) * D3) / 6.
    end do GetDischarge

    q2D(i) = (SUMX - SUMY)

    !WRITE(*,*) 'Linie: ', TransLi
    !do k = 1, lmt(TransLi)
    !  na = line(TransLi, k)
    !  WRITE(*,'(1x,a8,1x,i4,3(1x,f10.5))') 'Knoten: ', na, spec(na,1), dspecdh(na), dspecdv(na)
    !end do
    !pause

  end do transitionloop
  !-
!endif
!-
!nis,jan07: Close testing file
close (999,STATUS='keep')
!-

end subroutine
