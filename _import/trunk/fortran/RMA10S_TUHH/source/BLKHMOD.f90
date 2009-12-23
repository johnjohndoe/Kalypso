module BLKHMOD

!nis,oct06,com: Gauss-Points for interval [0;1]  
  REAL, PARAMETER :: AFACT(4) = (/0.0694319, 0.3300095, 0.6699905, 0.9305682/)
  
!nis,oct06,com: Gauss-point-weights      
  REAL, PARAMETER :: HFACT(4) = (/0.3478548, 0.6521451, 0.6521451, 0.3478548/)
  REAL, PARAMETER :: SLOAD(2) = (/-1., +1./)
  
!nis,oct,com: dnal(3,4) is the definition  
!             the storage order is columnwise, so one row (i=1,2,3oder4) is equal to  
!             entry (dnal(1,i,);dnal(2,i);dnal(3,i))  
!             DNAL describes the values (unpraezise: geometrische Lagefaktoren auf einer Laenge)  
!  
!             column1:   
!             column2: Gauss-Points for interval [-4,;4]  
!             column3:  
  REAL, PARAMETER :: DNAL(3,4) =  (/-2.7222728, 3.4445456, -0.7222728, -1.6799620, &
                                 &   1.3599240, 0.3200380, -0.3200380, -1.3599240,  &
                                 &   1.679962,  0.7222728, -3.4445456,  2.7222728/)
  REAL, PARAMETER :: XNAL(3,4) =  (/0.80134615,  0.25844410, -0.05979025, 0.22778404, &
                                 &  0.88441292, -0.11219696, -0.11219696, 0.88441292, &
                                 &  0.22778404, -0.05979025,  0.25844410, 0.80134615/)
    
end module