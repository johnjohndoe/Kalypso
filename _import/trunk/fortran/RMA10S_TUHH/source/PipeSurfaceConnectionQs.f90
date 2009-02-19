subroutine PipeSurfaceConnectionQs 

  use blk10mod, only: maxps, PipeSurfConn
  
  implicit none
  
  
  integer (kind = 4) :: i
  
  real (kind = 8) :: heSurface, hePipe
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
    !PipeSurfConn(i)%flow = GetFlow (heSurface, hePipe)
   
  enddo

end subroutine


function AvgEnergyLevelElt (FEElt) result (hEnergy)

  use blk10mod, only: nop, vel, ncorn, idnopt, ao
  use blkdrmod, only: akp, adt, adb, ado
  implicit none

  real (kind = 8) :: hEnergy
  real (kind = 8) :: velox, veloy, velo, Piezo, RealDepth
  integer (kind = 4) :: FEElt, Elttype, node
  integer (kind = 4) :: i
  real (kind = 8) :: xn2, xn1d
  real (kind = 8) :: dummy1, dummy2
  
  hEnergy = 0.0d0
  velox = 0.0d0
  veloy = 0.0d0
  velo = 0.0d0
  Piezo = 0.0d0
  
  !Define the element type for the proper weighting function assessment
  if (ncorn (FEElt) > 6) then
    Elttype = 1
  elseif (ncorn (FEElt) > 5) then
    Elttype = 2
  !1D
  else
    Elttype = 0
  endif
  
  
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
  hEnergy = velo**2/(2.0*9.81) + piezo
  
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
