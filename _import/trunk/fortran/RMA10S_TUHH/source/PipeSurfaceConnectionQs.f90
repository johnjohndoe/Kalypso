subroutine PipeSurfaceConnectionQs 

  use blk10mod, only: maxps, PipeSurfConn, ncorn, area
  use globalConstants
  use manholeDefinitions
  
  implicit none
  
  
  integer (kind = 4) :: i
  
  real (kind = 8) :: PipeSurfConnFlow
  real (kind = 8) :: flow
  real (kind = 8) :: AvgKinEnergyLevelElt, AvgBottomLevelElt, AvgPiezoLevelElt
  integer (kind = 4) :: EltType, LocalElementType
  
  type (energyLevel) :: heSurface, hePipe
  

  do i = 1, maxps
  
    EltType = LocalElementType (ncorn (PipeSurfConn(i)%SurfElt))
    
    write(*,*) area(PipeSurfConn(i)%SurfElt)
    write(*,*) area(PipeSurfConn(i)%PipeElt)
    continue
    
    !get the average energy elevations in the middle of an element
    heSurface%bedLevel = AvgBottomLevelElt (PipeSurfConn(i)%SurfElt, Elttype)
    heSurface%veloHead = AvgKinEnergyLevelElt (PipeSurfConn(i)%SurfElt, Elttype)
    heSurface%pressHead = max (AvgPiezoLevelElt (PipeSurfConn(i)%SurfElt, Elttype) - heSurface%bedLevel, 0.0d0)
    heSurface%total = heSurface%veloHead + heSurface%bedLevel + heSurface%pressHead
    heSurface%specific = heSurface%veloHead + heSurface%pressHead
!
    EltType = LocalElementType (ncorn (PipeSurfConn(i)%PipeElt))
    hePipe%bedLevel = AvgBottomLevelElt (PipeSurfConn(i)%PipeElt, Elttype)
    hePipe%veloHead = AvgKinEnergyLevelElt (PipeSurfConn(i)%PipeElt, Elttype)
    hePipe%pressHead = max (AvgPiezoLevelElt (PipeSurfConn(i)%PipeElt, Elttype) - hePipe%bedLevel, 0.0d0)
    hePipe%total = hePipe%veloHead + hePipe%bedLevel + hePipe%pressHead
    hePipe%specific = hePipe%veloHead + hePipe%pressHead
    
    !Calculate flow from surface to pipe
    PipeSurfConn(i)%flow = PipeSurfConnFlow (heSurface, hePipe, PipeSurfConn(i)%manholeDef)
!    PipeSurfConn(i)%DflowWRTv_upper = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface - 0.01**2/(2*grav), hePipe, PipeSurfConn(i)%manholeDef))/ 0.01
!    PipeSurfConn(i)%DflowWRTh_upper = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface - 0.01, hePipe, PipeSurfConn(i)%manholeDef))/ 0.01
!    PipeSurfConn(i)%DflowWRTv_lower = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface, hePipe - 0.01**2/(2*grav), PipeSurfConn(i)%manholeDef))/ 0.01
!    PipeSurfConn(i)%DflowWRTh_lower = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface, hePipe - 0.01, PipeSurfConn(i)%manholeDef))/ 0.01
    
   
  enddo

end subroutine

function AvgBottomLevelElt (FEElt, Elttype) result (Bottom)
  use blk10mod, only: nop, ncorn, ao
  implicit none
  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: Bottom
  integer (kind = 4) :: node, i
  
  Bottom = 0.0d0
  do i = 1, ncorn(FEElt)
    node = nop (FEElt, i)
    if (mod (i,2) /= 0) then
      if (Elttype == 0) then
        Bottom = Bottom + ao (node) * 1/2
      elseif (Elttype == 1) then
        Bottom = Bottom + ao (node) * 1/4
      elseif (Elttype == 2) then
        Bottom = Bottom + ao (node) * 1/3
      endif
    endif
  enddo
  
  return
end function

function AvgPiezoLevelElt (FEElt, Elttype) result (Piezo)
  use blk10mod, only: nop, vel, ncorn, idnopt, ao
  use blkdrmod, only: akp, adt, adb, ado
  implicit none

  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: Piezo, RealDepth
  integer (kind = 4) :: node
  integer (kind = 4) :: i
  real (kind = 8) :: dummy1, dummy2
  
  piezo = 0.0d0

  do i = 1, ncorn(FEElt)
    node = nop (FEElt, i)
    if (mod (i,2) /= 0) then
      if (idnopt < 0) then
        call amf (RealDepth, vel (3, node), akp (node), adt (node), adb (node), dummy1, dummy2, 0)
        !TODO: use the linear function instead of hard coded weighting 1/3 or 1/4
        if (Elttype == 0) then
          Piezo = Piezo + (vel (3, node) + ao (node)) * 1/2
        elseif (Elttype == 1) then
          Piezo = Piezo + (RealDepth + ado(node)) * 1/4
        elseif (Elttype == 2) then
          Piezo = Piezo + (RealDepth + ado(node)) * 1/3
        !TODO: 1D elements
        !else 
        endif 
      else
        !TODO: use the linear function instead of hard coded weighting 1/3
        if (Elttype == 0) then
          Piezo = Piezo + (vel (3, node) + ao (node)) * 1/2
        elseif (Elttype == 1) then
          Piezo = Piezo + (vel (3, node) + ao(node)) * 1/4
        elseif (Elttype == 2) then
          Piezo = Piezo + (vel (3, node) + ao(node)) * 1/3
        !TODO: 1D elements
        !else 
        endif 
      endif
    endif
  enddo
  return
end function

function AvgKinEnergyLevelElt (FEElt, Elttype) result (kinEnergy)
  use blk10mod, only: nop, vel, ncorn
  implicit none

  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: kinEnergy
  real (kind = 8) :: velox, veloy, velo
  real (kind = 8) :: xn2, xn1d
  integer (kind = 4) :: node
  integer (kind = 4) :: i

  velox = 0.0d0
  veloy = 0.0d0
  velo = 0.0d0
  kinEnergy = 0.0d0
  do i = 1, ncorn(FEElt)
    node = nop (FEElt, i)
    velox = vel (1, node)
    veloy = vel (2, node)
    if (Elttype == 0) then
      velo = velo + sqrt(velox**2 + veloy**2) * xn1D (i, 0.5d0)
    elseif (Elttype == 1) then
      !for a quadrilateral elt the center point is at (0.0,0.0)
      velo  = velo + sqrt(velox**2 + veloy**2) * xn2(Elttype, i, 0.0d0, 0.0d0)
    elseif (Elttype == 2) then
      !for a triangle the center point is at (0.333,0.333)
      velo  = velo + sqrt(velox**2 + veloy**2) * xn2(Elttype, i, 1.0/3.0, 1.0/3.0)
    endif

  enddo
  kinEnergy = velo**2/(2.0*9.81)
  return
end function



function AvgEnergyLevelElt (FEElt) result (hEnergy)

  use blk10mod, only: ncorn
  implicit none

  real (kind = 8) :: hEnergy
  real (kind = 8) :: AvgPiezoLevelElt, AvgBottomLevelElt, SpezPiezo
  real (kind = 8) :: AvgKinEnergyLevelElt, kinEnergy
  integer (kind = 4) :: FEElt, Elttype, LocalElementType
  
  hEnergy = 0.0d0
  SpezPiezo = 0.0d0
  kinEnergy = 0.0d0
  
  Elttype = LocalElementType (ncorn(FEElt))
  
  SpezPiezo = max (AvgPiezoLevelElt (FEElt, Elttype) - AvgBottomLevelElt (FEElt, Elttype), 0.0d0)
  kinEnergy = AvgKinEnergyLevelElt (FEElt, Elttype)
  hEnergy =  kinEnergy + SpezPiezo
  
  return
  
end function

!calculate flow through pipe surface connection
!----------------------------------------------
function PipeSurfConnFlow (he_upper, he_lower, manh) result (flow)
  use manholeDefinitions
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
  if (he_upper%total == he_lower%total) then
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
  lambda = cole (0.0d0, manh%diameter / 4, manh%ks)
  
  !determine loss direction
  !------------------------
  if (he_upper%total > he_lower%total) then
    InflowLoss = manh%ZetaInflowUpper
    OutflowLoss = manh%ZetaOutflowLower
  else
    InflowLoss = manh%ZetaOutflowLower
    OutflowLoss = manh%ZetaOutflowUpper
  endif
  
  
  !determinie maximum discharge through manhole via energy balance
  !---------------------------------------------------------------
  PS1 = pipeMaximum (he_upper%total, he_lower%total, he_upper%bedLevel, manh%length, lambda, manh%diameter, InflowLoss, OutflowLoss)
  
  if (he_upper%total < he_lower%total) then
    flow = PS1 * -1
  else
  
    !Calculate mue correction
    !------------------------
    if (he_lower%total >= he_upper%bedLevel) then
      mueCorr = sqrt (1-((he_lower%total - he_upper%bedLevel)/ he_upper%specific)**16)
    else
      mueCorr = 1.0d0
    endif
  
    !get theoretical flow from surface to pipe
    !-----------------------------------------  
    PS2 = weirDuBuat (manh%mue, mueCorr, he_upper%specific, manh%diameter*pi/4)
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
  
  


  