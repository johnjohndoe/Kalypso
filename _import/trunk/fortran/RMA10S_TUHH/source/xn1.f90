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
