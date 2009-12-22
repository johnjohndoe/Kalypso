subroutine PipeSurfaceConnectionQs 

  use blk10mod, only: maxps, PipeSurfConn, ncorn, area, maxn
  use parakalyps, only: IntPolNo, IntPolElts
  use globalConstants
  use manholeDefinitions
  use EnergyElevations
  
  implicit none
  
  
  integer (kind = 4) :: i, j
  
  real (kind = 8) :: PipeSurfConnFlow
  real (kind = 8) :: flow
  integer (kind = 4) :: EltType, LocalElementType
  integer :: totalLoad
  
  type (energyLevel) :: heSurface, hePipe
  

  do i = 1, maxps
  
    !get the average energy elevations in the middle of an element
    EltType = LocalElementType (ncorn (PipeSurfConn(i).SurfElt))
    call AvgElementEnergyLevels (heSurface, PipeSurfConn(i).SurfElt, EltType)

    !get the average energy elevations in the middle of the pipe element
    EltType = LocalElementType (ncorn (PipeSurfConn(i).PipeElt))
    call AvgElementEnergyLevels (hePipe, PipeSurfConn(i).PipeElt, EltType)
    
    !total load
    totalLoad = 1

    !First Remove old specific flows
    !-------------------------------
    if (PipeSurfConn(i).SurfFlow /= 0.0d0 .AND. maxn /= 1) then
      !Calculate the specific outflow/inflow to the surface element
      CALL LNFLO (PipeSurfConn(i).SurfElt, - PipeSurfConn(i).SurfFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
    
      !Calculate the specific outflow/inflow for original element
      CALL LNFLO (PipeSurfConn(i).PipeElt, - PipeSurfConn(i).PipeFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
      !Do the same for the interpolated elements
      if (IntPolNo (PipeSurfConn(i).PipeElt) /= 0) then
        !for interpolated elements
        ForInterpolated1: do j = 1, IntPolNo (PipeSurfConn(i).PipeElt)
          if (IntPolElts (PipeSurfConn(i).PipeElt, j) == 0) EXIT ForInterpolated1
          call LNFLO (IntPolElts (PipeSurfConn(i).PipeElt, j), - PipeSurfConn(i).PipeFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
        end do ForInterpolated1
      end if
    endif

    !Calculate flow from surface to pipe
    PipeSurfConn(i).PipeFlow = PipeSurfConnFlow (heSurface, hePipe, PipeSurfConn(i).manholeDef)
    PipeSurfConn(i).SurfFlow = - PipeSurfConn(i).PipeFlow
    
    write(*,*) 'Surf outflow: ', PipeSurfConn(i).SurfFlow
    write(*,*) 'Pipe  inflow: ', PipeSurfConn(i).PipeFlow
    !pause
    
!    PipeSurfConn(i).DflowWRTv_upper = (PipeSurfConn(i).flow - PipeSurfConnFlow (heSurface - 0.01**2/(2*grav), hePipe, PipeSurfConn(i).manholeDef))/ 0.01
!    PipeSurfConn(i).DflowWRTh_upper = (PipeSurfConn(i).flow - PipeSurfConnFlow (heSurface - 0.01, hePipe, PipeSurfConn(i).manholeDef))/ 0.01
!    PipeSurfConn(i).DflowWRTv_lower = (PipeSurfConn(i).flow - PipeSurfConnFlow (heSurface, hePipe - 0.01**2/(2*grav), PipeSurfConn(i).manholeDef))/ 0.01
!    PipeSurfConn(i).DflowWRTh_lower = (PipeSurfConn(i).flow - PipeSurfConnFlow (heSurface, hePipe - 0.01, PipeSurfConn(i).manholeDef))/ 0.01
    
    
    !Now settle the new flow exchanges
    !---------------------------------
    !Calculate the specific outflow/inflow to the surface element
    CALL LNFLO (PipeSurfConn(i).SurfElt, PipeSurfConn(i).SurfFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
    
    !distribute total discharge over all interpolated elements.
    !If no interpolated elements are present, this command doesn't fail!
    PipeSurfConn(i).PipeFlow = PipeSurfConn(i).PipeFlow/ (IntPolNo (PipeSurfConn(i).PipeElt) + 1)
    !Calculate the specific outflow/inflow for original element
    CALL LNFLO (PipeSurfConn(i).PipeElt, PipeSurfConn(i).PipeFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
    !Do the same for the interpolated elements
    if (IntPolNo (PipeSurfConn(i).PipeElt) /= 0) then
      !for interpolated elements
      ForInterpolated2: do j = 1, IntPolNo (PipeSurfConn(i).PipeElt)
        if (IntPolElts (PipeSurfConn(i).PipeElt, j) == 0) EXIT ForInterpolated2
        call LNFLO (IntPolElts (PipeSurfConn(i).PipeElt, j), PipeSurfConn(i).PipeFlow, (/0.0,0.0,0.0/), totalLoad, 0, 0.0d0, 0.0d0)
      end do ForInterpolated2
    end if
  enddo

end subroutine

!calculate flow through pipe surface connection
!----------------------------------------------
function PipeSurfConnFlow (he_upper, he_lower, manh) result (flow)
  use manholeDefinitions
  use EnergyElevations
  use globalConstants
  implicit none

  real (kind = 8) :: flow, PS1, PS2
  real (kind = 8) :: pipeMaximum, weirDuBuat
  real (kind = 8) :: InflowLoss, OutflowLoss
  real (kind = 8) :: mueCorr

  type (energyLevel), intent (in) :: he_upper, he_lower
  !manhole
  type (pipeManhole), intent (in) :: manh
  !locals
  real (kind = 8) :: lambda, cole
  
  !cycle on equal energy elevations
  !--------------------------------
  if (he_upper.total == he_lower.total) then
    flow = 0.0d0
    return
  endif
  
  !initializations
  !---------------
  flow = 0.0d0
  InflowLoss = 0.0d0
  OutflowLoss = 0.0d0
  mueCorr = 0.0d0
  
  !get pipe's flow resistance lambda
  !---------------------------------
  lambda = cole (0.0d0, manh.diameter / 4, manh.ks)
  
  !determine loss direction
  !------------------------
  if (he_upper.total > he_lower.total) then
    InflowLoss = manh.ZetaInflowUpper
    OutflowLoss = manh.ZetaOutflowLower
  else
    InflowLoss = manh.ZetaOutflowLower
    OutflowLoss = manh.ZetaOutflowUpper
  endif
  
  
  !determinie maximum discharge through manhole via energy balance
  !---------------------------------------------------------------
  PS1 = pipeMaximum (he_upper.total, he_lower.total, he_upper.bedLevel, manh.length, lambda, manh.diameter, InflowLoss, OutflowLoss)
  
  if (he_upper.total < he_lower.total) then
    flow = PS1 * (-1.0)
  else
  
    !Calculate mue correction
    !------------------------
    if (he_lower.total >= he_upper.bedLevel) then
      mueCorr = sqrt (1-((he_lower.total - he_upper.bedLevel)/ he_upper.specific)**16)
    else
      mueCorr = 1.0d0
    endif
  
    !get theoretical flow from surface to pipe
    !-----------------------------------------  
    PS2 = weirDuBuat (manh.mue, mueCorr, he_upper.specific, manh.diameter*pi/4)
    flow = min (PS1, PS2)
  endif

  return
end function

!maximum pipe flow due to energy balance
!-----------------
function pipeMaximum (he_upper, he_lower, SurfElt, length, lambda, diameter, hLoss_in, hLoss_out) result (maxFlow)
  
  use globalConstants
  implicit none
  
  real (kind = 8) :: maxFlow
  real (kind = 8), intent (in) :: he_upper, he_lower, SurfElt, length, lambda, diameter, hLoss_in, hLoss_out
  real (kind = 8) :: he_in, he_out
  
  if (he_upper > he_lower) then
    he_in = he_upper
    he_out = max (he_lower, SurfElt - length)
  else
    he_in = he_lower
    he_out = he_upper
  endif
  
  maxFlow = 0.0d0
  
  maxFlow = sqrt (2.0 * grav * (he_in - he_out) / (lambda * length / diameter + hLoss_in + hLoss_out))
  maxFlow = maxFlow * (pi * diameter**2 / 4)
  
  return
end function


!normal pipe type
!----------------
function weirDuBuat (mue, mueCorr, he_ue, crestLength) result (flow)

  use globalConstants
  
  real (kind = 8) :: flow
  real (kind = 8), intent (in) :: mue, mueCorr, he_ue, crestLength
  
  flow = 0.0d0
  
  flow = crestLength * mue * mueCorr * 2.0/3.0 * (2.0 * grav)**0.5 * (he_ue**3.0)**0.5
  return
end function



!local Element Type Definition
!Normally a global type specifier should be invoked!
function LocalElementType(numberOfCornerNodes) result (Elttype)
  !function definition
  integer (kind = 4) :: Elttype
  !dummy arguments
  integer (kind = 4), intent (in) :: numberOfCornerNodes
  
  !Define the element type for the proper weighting function assessment
  if (numberOfCornerNodes > 6) then
    Elttype = 1
  elseif (numberOfCornerNodes > 5) then
    Elttype = 2
  !1D
  else
    Elttype = 0
  endif
end function
  
  


  