!     Last change:  K    31 May 2007    4:38 pm

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
INTEGER            :: teststat
!teststat  :: iostat variable for testfile
!-


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

    !testing
    WRITE(*,*) transno, 'Iteration: ', maxn
    WRITE(*,*) vel(1,transno), vel(2,transno)
    pause
    !-

    !get the 1D-velocity; the transition node is always the 3rd one of the 1D-part definition nop(j,3)
    TransVel = SQRT((vel(1,TransNo))**2 + (vel(2,TransNo))**2)
    TransDep = vel(3,TransNo)
    !get the discharge area
    !if width is not zero it is a King element (testing, the value should be less then machine accuracy)
    CSArea = 0.0

    !if width is not zero a trapezoidal profile of the original RMA10S code is used
    if (ABS(width(TransNo)) > 1.e-7) then
      CSArea = (width(TransNo) + 0.5 * (ss1(TransNo) + ss2(TransNo))*TransDep) *TransDep

    !if width is zero, but the element is 1D it can only be a Teschke element
    else
     !polynomial calculation
     DO k = 0, 12
       CSArea = CSArea + apoly(TransNo,k) * TransDep**(k)
     ENDDO
    end if

    !Calculate discharge
    Discharge = CSArea * TransVel

    !nis,jan07,testing
    WRITE(999,*) 'Values from 1D-part'
    WRITE(999,*) 'Area:',CSArea
    WRITE(999,*) 'TransNo', ', width(TransNo)', ', TransDep'
    WRITE(999,*) TransNo, width(TransNo), TransDep
    WRITE(999,*) 'TransLi', ', discharge', ', TransVel'
    WRITE(999,*) TransLi, discharge, TransVel
    !-

    !step3: getting the 2D-velocities with the QGEN-subroutine part
    !******

    !allocating and initializing the temporary velocity saver
    ALLOCATE (TransitionVels(3,lmt(TransLi)))
    do l = 1, lmt(TransLi)
      do m = 1, 3
        TransitionVels(m,l) = 0.0
      end do
    end do

    !some needed input values:
    !j    = is line, where to set the velocities;                        here it equals 'TransLi'
    !QREQ = is the discharge to determine the velocity distribution for; here it equals 'Discharge'
    !THET = is the flow angle at boundaries;                             here the angle of the 3rd node of the TL can be taken
    !                                                                    'alfa(line(TransLi,3))'
    !QQAL = is a value for the quality constituent;                      here it is a dummy argument: 20 degrees

    !Getting the TransitionVels of the 1D-2D-line Transition
    call QGENtrans (TransLi, TransNo, Discharge, 0.0, TransDep)
    !              (TransLi, TransNo, Discharge, alfa(line(TransLi,3)), TransDep)

    !nis,jan07: Write test lines
    WRITE(999,*) 'Applying factors'
    WRITE(999,*) '1D-water vel and h'
    WRITE(999,*) TransVel, TransDep
    WRITE(999,*) 'x-vel-factor, y-vel-factor, h-factor, ', 'xvel, yvel, h'
    !-

    !step4 and step5: Calculation of the velocity factors and the depth factors
    !****************

    do l = 1, lmt(TransLi)

      !euclidic length of velocity vector
      localVel = SQRT(TransitionVels(1,l)**2 + TransitionVels(2,l)**2)

      !nis,may07: Update the velocity-values at the transition coming from the distribution calculation
      Vel(1, line(TransLi,l)) = TransitionVels(1,l)
      Vel(2, line(TransLi,l)) = TransitionVels(2,l)

      !x-component (parallel component)
      !EqScale(line(TransLi,l),1) = TransVel/ localVel
      EqScale(line(TransLi,l),1) = localVel / TransVel
      WRITE(999,*) 'lokale Absolutgeschwindigkeit:',localvel
      WRITE(*,*) 'Node: ', line(TransLi,l)
      !y-component (perpendicular component) (no value perpendicular to settled flow direction!)
      EqScale(line(TransLi,l),2) = EqScale(line(TransLi,l),1)
      !depth-component
      !EqScale(line(TransLi,l),3) = (TransDep + ao(TransNo) - ao(line(TransLi,l)) ) / TransitionVels(3,l)
      EqScale(line(TransLi,l),3) = TransitionVels(3,l)/ TransDep

      if (EqScale(line(Transli,l),3) < 0.0) then
        EqScale(line(Transli,l),3) =0.0
      end if

      !nis,jan07: Write testing lines
      WRITE(999,'(a7,i2,1x,a8,i2,1x,a7,i5,1x,a10,1x,3(f5.3,1x),a7,3(f7.3,1x),a9,1x,f7.3,a19,2(1x,f7.3))')                    &
      &                         'Linie: ', Transli, 'Knoten: ', l, 'global ',  line(Transli,l),                              &
      &                         'Faktoren: ', (EqScale(line(Transli,l),n), n=1,3),'Werte: ',  (TransitionVels(m,l), m=1, 3), &
      &                         'Spiegel: ', Vel(3,line(Transli,l)) + ao(line(Transli,l)),                                   &
      &                         'Geschwindigkeiten: ', (vel(o,line(Transli,l)),o=1,2)
      !-
    end do
    DEALLOCATE (TransitionVels)
  end do transitionloop
  !-
!endif
!-
!nis,jan07: Close testing file
close (999,STATUS='keep')
!-

end subroutine
