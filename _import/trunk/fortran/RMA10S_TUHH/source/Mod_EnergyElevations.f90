module EnergyElevations
  implicit none

  type energyLevel
    real (kind = 8) :: bedLevel = 0.0d0
    real (kind = 8) :: pressHead = 0.0d0
    real (kind = 8) :: veloHead = 0.0d0
    real (kind = 8) :: total = 0.0d0
    real (kind = 8) :: specific = 0.0d0
  end type
  
  private :: AvgBottomLevelElt, AvgKinEnergyLevelElt, AvgPiezoLevelElt
  
contains
!-------

subroutine AvgElementEnergyLevels (energyLevelValue, Element, ElementType)
  implicit none
  type (energyLevel), intent (inout) :: energyLevelValue
  integer (kind = 4), intent (in) :: Element
  integer (kind = 4), intent (in) :: ElementType
  
  energyLevelValue.bedLevel = AvgBottomLevelElt (Element, ElementType)
  energyLevelValue.veloHead = AvgKinEnergyLevelElt (Element, ElementType)
  energyLevelValue.pressHead = max (AvgPiezoLevelElt (Element, ElementType) - energyLevelValue.bedLevel, 0.0d0)
  energyLevelValue.total = energyLevelValue.veloHead + energyLevelValue.bedLevel + energyLevelValue.pressHead
  energyLevelValue.specific = energyLevelValue.veloHead + energyLevelValue.pressHead
end subroutine

function AvgBottomLevelElt (FEElt, Elttype)
  use blk10mod, only: nop, ncorn, ao
  implicit none
  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: AvgBottomLevelElt
  integer (kind = 4) :: node, i
  
  AvgBottomLevelElt = 0.0d0
  do i = 1, ncorn(FEElt)
    node = nop (FEElt, i)
    if (mod (i,2) /= 0) then
      if (Elttype == 0) then
        AvgBottomLevelElt = AvgBottomLevelElt + ao (node) * 1/2
      elseif (Elttype == 1) then
        AvgBottomLevelElt = AvgBottomLevelElt + ao (node) * 1/4
      elseif (Elttype == 2) then
        AvgBottomLevelElt = AvgBottomLevelElt + ao (node) * 1/3
      endif
    endif
  enddo
  
  return
end function

function AvgPiezoLevelElt (FEElt, Elttype)
  use blk10mod, only: nop, vel, ncorn, idnopt, ao
  use blkdrmod, only: akp, adt, adb, ado
  implicit none

  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: AvgPiezoLevelElt
  real (kind = 8) :: RealDepth
  integer (kind = 4) :: node
  integer (kind = 4) :: i
  real (kind = 8) :: dummy1, dummy2
  
  AvgPiezoLevelElt = 0.0d0

  do i = 1, ncorn(FEElt)
    node = nop (FEElt, i)
    if (mod (i,2) /= 0) then
      if (idnopt < 0) then
        call amf (RealDepth, vel (3, node), akp (node), adt (node), adb (node), dummy1, dummy2, 0)
        !TODO: use the linear function instead of hard coded weighting 1/3 or 1/4
        if (Elttype == 0) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (vel (3, node) + ao (node)) * 1/2
        elseif (Elttype == 1) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (RealDepth + ado(node)) * 1/4
        elseif (Elttype == 2) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (RealDepth + ado(node)) * 1/3
        !TODO: 1D elements
        !else 
        endif 
      else
        !TODO: use the linear function instead of hard coded weighting 1/3
        if (Elttype == 0) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (vel (3, node) + ao (node)) * 1/2
        elseif (Elttype == 1) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (vel (3, node) + ao(node)) * 1/4
        elseif (Elttype == 2) then
          AvgPiezoLevelElt = AvgPiezoLevelElt + (vel (3, node) + ao(node)) * 1/3
        !TODO: 1D elements
        !else 
        endif 
      endif
    endif
  enddo
  return
end function

function AvgKinEnergyLevelElt (FEElt, Elttype)
  use blk10mod, only: nop, vel, ncorn
  implicit none

  integer (kind = 4), intent (in) :: FEElt, Elttype
  real (kind = 8) :: AvgKinEnergyLevelElt
  real (kind = 8) :: velox, veloy, velo
  real (kind = 8) :: xn2, xn1d
  integer (kind = 4) :: node
  integer (kind = 4) :: i

  velox = 0.0d0
  veloy = 0.0d0
  velo = 0.0d0
  AvgKinEnergyLevelElt = 0.0d0
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
  AvgKinEnergyLevelElt = velo**2/(2.0*9.81)
  return
end function

end module
