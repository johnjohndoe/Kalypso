!     Last change:  WP   29 Aug 2007   11:27 am
! __________________________________________________________________________________________________________


!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************
!
!         These subroutines compute the boundary conditions for new steps for steady and unsteady
!       flow for autoconverge.
!
!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************




subroutine autoboundaryh(hfin,hini,h2fin,h2ini,tfin,ttt,curve,hhh,hhh2)

! hfin    : finish waterstage                                                            
! hini    : initial waterstage                                                         
! h2fin   : finish waterstage 2                                                         
! h2ini   : initial waterstage 2                                                         
! tfin    : finish step /time                                                         
! tini    : initial step / time  (is set to zero)                                                         
! ttt     : step width (for unsteady: time step), for which the                                                         
!           boundary conditions will be calculatde                                                         
! curve  : option for the calculation of discharge and waterstage                                                         
!          0 means linear, 1 means logarithmic/power                                                         
! hhh     : calculated waterstage                                                         
! hhh2    : calculated waterstage 2                                                         

INTEGER,INTENT(IN) :: curve
REAL,INTENT(IN)    :: hfin,hini,h2fin,h2ini,tfin
REAL (KIND = 8)    :: ttt
REAL,INTENT (out)  :: hhh,hhh2

REAL :: a,b,hinii,hfini,h2fini,h2inii,tini,tttn

hinii = hini + 100.0

hfini = hfin + 100.0

h2inii = h2ini + 100.0

h2fini = h2fin + 100.0

if (curve == 0.) then

!y = a * x + b  

  a = (hfini - hinii) / (tfin-tini)

  b = hfini - a * tfin

  hhh = a * ttt + b - 100.

  a = (h2fini - h2inii) / (tfin-tini)

  b = h2fini - a * tfin

  hhh2 = a * ttt + b - 100.

else

  tini = 100.

  tfinn = tfin +100.

  tttn = ttt +100.

!y=a * x **  b  

  b = LOG(hinii / hfini) / LOG(tini / tfinn)

  a = hfini / (tfin ** b)

  hhh = a * tttn ** b - 100

  b = LOG(h2inii / h2fini) / LOG(tini / tfinn)

  a = h2fini / (tfinn ** b)

  hhh2 = a * tttn ** b - 100

end if

END subroutine



subroutine autoboundaryQ(qfin,qini,qdirfin,qdirini,tfin,ttt,curve,qqq,qqqdir)

! qfin    : finish discharge                                                         
! qini    : initial discharge                                                         
! qdirfin : finish direction of boundary discharge                                                         
! qdirini : initial direction of boundary discharge                                                         
! tfin    : finish step /time                                                         
! tini    : initial step / time                                                         
! ttt     : step width (for unsteady: time step), for which the                                                         
!         boundary conditions will be calculatde                                                         
! curve  : option for the calculation of discharge and waterstage                                                         
!         0 means linear, 1 means logarithmic/power                                                         
! qqq     : calculated discharge                                                         


INTEGER,INTENT(IN) :: curve
REAL,INTENT(IN)    :: qfin,qini,qdirfin,qdirini,tfin
REAL (KIND = 8)    :: ttt
REAL,INTENT (out)  :: qqq,qqqdir

REAL :: a,b,tini,tttn

if (curve == 0.) then

!y = a * x + b  

  a = (qfin - qini) / (tfin)

  b = qfin - a * tfin

  qqq = a * ttt + b

  a = (qdirfin - qdirini) / (tfin)

  b = qdirfin - a * tfin

  qqqdir = a * ttt + b

else

tini = 100.

tfinn = tfin + 100.

tttn = ttt +100.

!y = a * log(x) + b  

  a = (qfin - qini) / (LOG(tfinn / tini))

  b = qfin - a * LOG(tfinn)

  qqq = a * LOG(tttn) + b

  a = (qdirfin - qdirini) / (LOG(tfinn / tini))

  b = qdirfin - a * LOG(tfinn)

  qqqdir = a * LOG(tttn) + b

end if

END subroutine
