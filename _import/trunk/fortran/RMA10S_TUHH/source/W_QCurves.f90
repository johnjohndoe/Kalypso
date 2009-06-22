function cstrcQ_fromQCurves (weirImat, HOWIN, HUWIN)

!this is just necessary to find the correct weir definition; it would be better to overgive the weir
!as dummy argument
use blk10mod, only: contrStructures, maxweir

use mod_ControlStructure
use mod_discreteFunction
implicit none

!function type
real (kind = 8) :: cstrcQ_fromQCurves
!arguments
integer (kind = 4), intent (in) :: weirImat
REAL (KIND = 8), INTENT (IN) :: huwin, howin
!local variables
real (kind = 8) :: Q, Qdir
type (discretefunction), pointer :: qFunction => null()
type (linkeddiscretefunction), pointer :: tmpfun => null()
type (controlstructure), pointer :: cstrc => null()
real (kind = 8) :: absz, ord
real (kind = 8) :: how, huw, huw_tmp
integer         :: i

QFunction => newDiscrFun ()

how = howin
huw = huwin

  !initialize with standard positive flow direction
  QDir = 1.0
  !if flow is turning around: introduce direction factor and switch water stages
  !this does not change the original data!
  if (how < huw) then
    QDir = -1.0
    huw_tmp = huw
    huw = how
    how = huw_tmp
  end if
  
  !find the corresponding weir definition
  do i = 1, maxWeir
    if (weirImat == contrStructures(i).typeID) cstrc => contrStructures (i)
  enddo

  !Calculate Q at the weir
  !-----------------------
  !
  !  how
  !  |
  !  |    |    Q1  Q2
  !  |    |    /   /
  !  |        /   /
  !  | ------/   /
  !  | ---------/
  !  |
  !  |    |
  !  |    | (Q on that vertical line, interpolated between single Q-curves)
  !  |    |
  !  |
  !  |-------------------->huw

  
  !generate the QFunction according 'vertical line' from picture above
  tmpFun => cstrc.QCurves.firstFun
  generateQFunction: do
    absz = functionValue (tmpFun.this, huw)
    ord = tmpFun.CurveValue
    call addPair (QFunction, absz, ord)
    if (associated (tmpFun.next)) then
      tmpFun => tmpFun.next
    else
      exit generateQFunction
    endif
  enddo generateQFunction

  !calculate flow over weir, taking account the direction fix
  Q = QDir * functionValue (QFunction, how)
  cstrcQ_fromQCurves = Q
  return
end