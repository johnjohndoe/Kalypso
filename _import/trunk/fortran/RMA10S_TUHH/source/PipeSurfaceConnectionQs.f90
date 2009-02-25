subroutine PipeSurfaceConnectionQs 

  use blk10mod, only: maxps, PipeSurfConn
  use globalConstants
  
  implicit none
  
  
  integer (kind = 4) :: i, manholeType
  
  real (kind = 8) :: heSurface, hePipe
  real (kind = 8) :: PipeSurfConnFlow
  real (kind = 8) :: flow
  real (kind = 8) :: AvgEnergyLevelElt
  !heSurface     averaged energy elevation of surface element
  !hePipe        averaged energy elevation in pipe
  
  !initializations
  heSurface = 0.0d0
  hePipe = 0.0d0
  
  do i = 1, maxps
  
    !get the average energy elevations in the middle of an element
    heSurface = AvgEnergyLevelElt (PipeSurfConn(i)%SurfElt)
    hePipe = AvgEnergyLevelElt (PipeSurfConn(i)%PipeElt)
    
    !Calculate flow from surface to pipe
    ManholeType = 1
    PipeSurfConn(i)%flow = PipeSurfConnFlow (heSurface, hePipe, ManholeType)
    PipeSurfConn(i)%DflowWRTv_upper = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface - 0.01**2/(2*grav), hePipe, ManholeType))/ 0.01
    PipeSurfConn(i)%DflowWRTh_upper = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface - 0.01, hePipe, ManholeType))/ 0.01
    PipeSurfConn(i)%DflowWRTv_lower = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface, hePipe - 0.01**2/(2*grav), ManholeType))/ 0.01
    PipeSurfConn(i)%DflowWRTh_lower = (PipeSurfConn(i)%flow - PipeSurfConnFlow (heSurface, hePipe - 0.01, ManholeType))/ 0.01
    
   
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
  integer (kind = 4) :: FEElt, Elttype
  
  hEnergy = 0.0d0
  SpezPiezo = 0.0d0
  kinEnergy = 0.0d0
  
  !Define the element type for the proper weighting function assessment
  if (ncorn (FEElt) > 6) then
    Elttype = 1
  elseif (ncorn (FEElt) > 5) then
    Elttype = 2
  !1D
  else
    Elttype = 0
  endif
  
  SpezPiezo = max (AvgPiezoLevelElt (FEElt, Elttype) - AvgBottomLevelElt (FEElt, Elttype), 0.0d0)
  kinEnergy = AvgKinEnergyLevelElt (FEElt, Elttype)
  hEnergy =  kinEnergy + SpezPiezo
  
  return
  
end function

function PipeSurfConnFlow (he_upper, he_lower, ManholeType)
  use globalConstants
  implicit none
  real (kind = 8) :: PipeSurfConnFlow, PS1, PS2
  real (kind = 8) :: pipeMaximum, weirDuBuat
  real (kind = 8), intent (in) :: he_upper, he_lower
  integer (kind = 4), intent (in) :: ManholeType
  
  
  PipeSurfConnFlow = 0.0d0
  !different manhole types
  if (manholeType == 1) then
    PS1 = pipeMaximum (he_upper, 2.0, 0.005, 0.65, 0.0d0, 0.0d0)
    PS2 = weirDuBuat (0.5, he_upper, pi*0.65)
    PipeSurfConnFlow = min (PS1, PS2)
  endif 

  return
end function


function xn1D (node, xi)
  implicit none
  real (kind = 8) :: xn1D
  real (kind = 8) :: xi
  integer (kind = 4) :: node
  
  if (node == 1) then
    XN1D = (1.-xi)*(1.-2.*xi)
  elseif (node == 2) then
    XN1D = (1.-xi)*4.*xi
  elseif (node == 3) then
    XN1D = (2.*xi-1.)*xi
  endif
  
  return
end function


function weirDuBuat (mue, he_ue, width) result (flow)

  use globalConstants
  
  real (kind = 8) :: flow
  real (kind = 8), intent (in) :: mue, he_ue, width
  
  flow = 0.0d0
  
  flow = width * mue * 2.0/3.0 * (2.0 * grav)**0.5 * (he_ue**3.0)**0.5
  return
end function

function pipeMaximum (he_upper, length, lambda, diameter, hLoss_inflow, hLoss_outflow) result (maxFlow)
  
  use globalConstants
  implicit none
  
  real (kind = 8) :: maxFlow
  real (kind = 8), intent (in) :: he_upper, length, lambda, diameter, hLoss_inflow, hLoss_outflow
  
  maxFlow = 0.0d0
  
  maxFlow = sqrt(2.0 * grav / (1.0 + length * lambda / diameter) * (length + he_upper - hLoss_inflow - hLoss_outflow))
  maxFlow = maxFlow * (pi * diameter**2 / 4)
  
  return
end function

  