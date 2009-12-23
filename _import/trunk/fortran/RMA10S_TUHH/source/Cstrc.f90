!     Last change:  WP   12 Jun 2008    2:05 pm
!     Last change:  NIS  21 Feb 2008    8:15 am
!ipk  last update nov 28 2006 allow for all 1-d control structure options
SUBROUTINE CSTRC(NN)
USE BLK10MOD
USE BLKSUBMOD
USE BLK11MOD
USE BLKECOM      
USE Para1DPoly
USE parakalyps

DIMENSION U(8)

!NiS,jul06: Declaring waterlevel H for proper call of amf-subroutine
REAL (KIND = 8) :: WH1, WH2, WS1, WS2
REAL (KIND = 8) :: calcPolynomial, calcPolynomial1stDerivative
REAL (KIND = 8) :: Q1t
INTEGER :: PolyPos(1:2), findpolynom
INTEGER :: NodA, NodB
real (kind = 8) :: qDh (1:2)

integer (kind = 4) :: cornerNode, n1
!nis,feb08
REAL (KIND = 8) :: DX, DY, CosinusAlpha

!-
!-
!...... Det up cutoff values
!-

IF(GRAV < 32.) THEN
  HCUT=0.001
ELSE
  HCUT=0.003
ENDIF
NCN=NCORN(NN)

!parameters for polynomial approach
if (width(nop(nn,1)) == 0.0) then

!Polnyomial positions  
  PolyPos(1) = findPolynom (PolyRangeA (nop (nn, 1), :), vel (3, nop(nn, 1)), PolySplitsA (nop (nn, 1)), cord (nop (nn,1), 1), cord (nop (nn,1), 2), nop (nn, 1))
  PolyPos(2) = findPolynom (PolyRangeA (nop (nn, 3), :), vel (3, nop(nn, 3)), PolySplitsA (nop (nn, 3)), cord (nop (nn,3), 1), cord (nop (nn,3), 2), nop (nn, 3))

!A(h)  
  ah (nop (nn, 1)) = calcPolynomial (apoly (PolyPos (1), nop (nn, 1), 0:4), vel (3, nop (nn, 1)), ubound (apoly, 3))
  ah (nop (nn, 3)) = calcPolynomial (apoly (PolyPos (2), nop (nn, 3), 0:4), vel (3, nop (nn, 3)), ubound (apoly, 3))

!dA(h)/dh  
  dahdh (nop (nn, 1)) = calcPolynomial1stDerivative (apoly (PolyPos (1), nop (nn, 1), 0:4), vel (3, nop (nn, 1)), ubound (apoly, 3))
  dahdh (nop (nn, 3)) = calcPolynomial1stDerivative (apoly (PolyPos (2), nop (nn, 3), 0:4), vel (3, nop (nn, 3)), ubound (apoly, 3))
else
  
!A(h)  
  ah (nop (nn,1)) = (width (nop (nn,1)) + (ss1 (nop (nn,1)) + ss2 (nop (nn,1))) / 2. * vel (3, nop (nn,1))) * vel (3, nop (nn,1))
  ah (nop (nn,3)) = (width (nop (nn,3)) + (ss1 (nop (nn,3)) + ss2 (nop (nn,3))) / 2. * vel (3, nop (nn,3))) * vel (3, nop (nn,3))
!dA(h)/dh  
  dahdh(nop (nn,1)) = (width (nop (nn,1)) + (ss1 (nop (nn,1)) + ss2 (nop (nn,1))) * vel (3, nop (nn,1)))
  dahdh(nop (nn,3)) = (width (nop (nn,3)) + (ss1 (nop (nn,3)) + ss2 (nop (nn,3))) * vel (3, nop (nn,3)))
endif

!form 1D-velocity at cstrc's corner nodes
DO N = 1, NCN
  NodA = NOP (NN, N)
  IF (NodA > 0) U(N) = VEL (1, NodA) * COS (ALFA (NodA)) + VEL (2, NodA) * SIN (ALFA (NodA))
ENDDO

!----------------------------------------------------------------------
!FLOW BALANCE: Discharge upstream must be equal to discharge downstream
!----------------------------------------------------------------------
!flow balance will be inserted into the first line of the local matrix estifm

!Don't consider midside nodes
FlowBalance: do cornerNode_index = 1, ncn, 2

!get current node in element  
  cornerNode = NOP (NN, cornerNode_index)
  
!Residual vector  
!---------------  
!  
! F = dir   * v   * A(h)    - dir   * v   * A(h)  
!        OW    OW    OW          UW    UW    UW  
!  
  F (1) = F (1) + dir(cornerNode) * u (cornerNode_index) * ah (cornerNode)

!Derivative with respect to velocities  
!-------------------------------------  
!  dF      |-            -|  
! ---- = - |  dir  * A(h) |  
!  dv      |_    i    i  _|  
!    i  
!Find the column to place derivative with respect to velocity in local Jacobian (estifm)  
  column = (cornerNode_index - 1) * NDF + 1
!Set derivative dF/dv  
  estifm (1, column) = - dir (cornerNode) * ah (cornerNode)

!Derivative with respect to velocities  
!-------------------------------------  
!          |-         dA (h)     -|  
!  dF      |            i         |    
! ---- = - |  dir  * ------- * v  |  
!  dh      |     i     dh       i |  
!    i     |_            i       _|  
!  
!Find the column to place derivative with respect to velocity in local Jacobian (estifm)  
  column = column + 2
!Set derivative dF/dh  
  estifm (1, column) = - dir(cornerNode) * dahdh (cornerNode) * u (cornerNode_index)

enddo FlowBalance
!Depending on the type of flow controller, an additional source might be present within the flow controller:
nm = imat (nn) - 900
if (njt (nm) <= 1) f (1) = f(1) + aj (nm)


!--------------------------------------------------------------------------------------
!Artificial consideration of the midside node; this equation is actually not necessary!
!--------------------------------------------------------------------------------------
f(5) = u(2) - (u(3) + u(1)) / 2.
estifm (5, 1) = 0.5
estifm (5, 9) = 0.5
estifm (5, 5) = -1.0


!--------------------------------------------
!FLOW CONDITION DUE TO CONTROL STRUCTURE TYPE
!--------------------------------------------
!By original there are several types of control structure definitions. The type is defined by njt:
!  * - no specification of njt leads to the usage of an energy balance
!  1 - 
!  2 -
!  3 -
!...
! 10 - Control structure (like bridge or weir) that is defined by data sets describing the Q-behaviour
!      due to upstream energy elevation and downstream energy elevation. The original implementation used
!      simple data tables, organized like a grid. It was found, that the definition of Q-functions in a group
!      leads to more reliable results. Both types can be still used by setting the switch UseEnergyCstrc
!      0 - original implementation (TO BE DELETED)
!      1 - approach using Q-curves (Kalypso approach!)


  if (njt(nm) == 10 .OR. njt(nm) == 11 .OR. njt(nm) == 12) then

!njt defines type of flow controller    
! 10 - Q-curve group    
! 11 - tabular data    
! 12 - weir formula    

!Residual function works like this    
!---------------------------------    
! F = (u1*h1+u2*h2) - f(u1,h1,h2) = 0    

!ITP displays weir crest structure    
!1 - paved    
!0 - gravel    
    ITP=1

!get nodal locations    
    N1 = NOP (NN, 1)
    N2 = NOP (NN, 3)

!ipk oct03  calculate overall embankment width    
    widem = sqrt ((cord (n2, 2) - cord (n1, 2))**2 + (cord (n2, 1) - cord (n1, 1))**2)
    NodA = 1
    NodB = 9

!if weir is submerged, then leave subroutine: Can only be used with njt(nm) == 12    
!if(isubm(n1) == 2) return    

!set increments for numerical derivatives    
    DH = 0.002
    DV = 0.01

!Check, if node is submerged    
!IF(WSLL(N1) < TRANSEL(N1)) return    

!Calculate the discharge based on the present values of both nodes    
    Q = (U(1) * ah(n1) + U(3) * ah(N2)) / 2.
     
!Direction fix of flow over/ through control structure    
    QFACT1 = 1.0
    DX = cord (nop (nn, 3), 1) - cord (nop (nn, 1), 1)
    DY = cord (nop (nn, 3), 2) - cord (nop (nn, 1), 1)
    CosinusAlpha = (DX * 1.0d0 + DY * 0.0d0) / (SQRT (DX**2 + DY**2) * SQRT (1.0d0**2 + 2.0d0**2))
    if (CosinusAlpha < 0.0d0) QFACT1 = -1.0

    
!Check if control structure is controlled time dependently; means cstrc might be inoperative    
    IF(NTMREF(IMAT(NN)) /= 0) THEN
      CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET,QFACT1)
      IF(ISWTOF == 1) THEN
        Q1T=0.0
        F(NodB)=Q-Q1T
        VEL(1,NOP(NN,1))=0.
        VEL(2,NOP(NN,1))=0.
        VEL(1,NOP(NN,2))=0.
        VEL(2,NOP(NN,2))=0.
        VEL(1,NOP(NN,3))=0.
        VEL(2,NOP(NN,3))=0.
        ESTIFM(1,1)=1.E6
        ESTIFM(9,9)=1.E6
!leave routine, if control structure is not operative in this time step        
        return
      ENDIF
    ENDIF

!Factorization for using metric or SI units    
    unitFac1 = 1.0d0
    unitFac2 = 1.0d0
    if (grav > 30.0 .AND. nctref (nn) == 0) then
      unitFac1 = 1.0/ 3.2808
      unitFac2 = 10.7636
    endif

!Prepare for call to get function    
    WH1 = unitFac1 * VEL(3,N1)
    WH2 = unitFac1 * VEL(3,N2)
!This might be problematic for polynom approach    
    WS1 = unitFac1 * WSLL(N1)
    WS2 = unitFac1 * WSLL(N2)
    WV1 = unitFac1 * SQRT(VEL(1,N1)**2+VEL(2,N1)**2)
    WV2 = unitFac1 * SQRT(VEL(1,N2)**2+VEL(2,N2)**2)
    WEC = unitFac1 * WHGT(N1)
    WLN = unitFac1 * WLEN(N1)


!RESIDUAL equation of flow condition
!-----------------------------------
!weir formula    
    if (njt (nm) == 12) then
      CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,widem)
      Q1T = Q1T * unitFac2
!QFunction relationship      
    elseif (njt (nm) == 10) then
      q1t = cstrcQ_fromQCurves (imat (nn), ws1 + wv1**2/(2.0*grav), ws2 + wv2**2/(2.0*grav))
!Q tabular relationship    
    elseif (njt (nm) == 11) then
      call wtform(q1t,nctref(imat(nn)),ws1,ws2, mcord(nn,1),mcord(nn,2))
      endif
!Factorize discharge by direction fix      
      Q1T = Q1T * QFACT1
!Form residual    
    F (nodB) = Q - Q1T

!Derivative with respect to h
!----------------------------
    ESTIFM (NodB, NodB + 2) = - 0.5 * U(3) * dahdh (n2)
    
    upstream_or_downstream: do j = 1, 2
      plus_minus_Dh: do i = 1, 2
        if (j == 1) then
              if (i == 1) then
            wh1 = wh1 + 0.5 * dh
            ws1 = ws1 + 0.5 * dh
          else
            wh1 = wh1 - dh
            ws1 = ws1 - dh
          endif
        else
          if (i == 1) then
            wh1 = wh1 + 0.5 * dh
            ws1 = ws1 + 0.5 * dh
            wh2 = wh2 + 0.5 * dh
            ws2 = ws2 + 0.5 * dh
          else
            wh2 = wh2 - dh
            ws2 = ws2 - dh
          endif
        endif

          if (njt (nm) == 12) then
          call wform (qDh (i), wh1, ws1, wv1, wh2, ws2, wv2, wec, wln, itp, widem)
          qDh(i) = qDh(i) * unitfac2
          elseif (njt (nm) == 11) then
            call wtform (qdh (i), imat (nn), ws1, ws2, mcord(nn,1), mcord(nn,2))
        elseif (njt (nm) == 10) then
          qdh(i) = cstrcQ_fromQCurves (imat (nn), ws1 + wv1**2/(2.0*grav), ws2 + wv2**2/(2.0*grav))
          endif
        qDh(i) = qDh(i) * QFACT1

!after looping up and down        
        if (i == 2) then
!total          
          dqdh = (qdh(1) - qdh(2)) / dh
!upstream          
          if (j == 1) then
            estifm (NodB, NodA + 2) = - 0.5 * U(1) * dahdh (n1) + dqdh
!downstream          
          else
!reset water levels and water depths            
            wh2 = wh2 + 0.5 * dh
            ws2 = ws2 + 0.5 * dh
            estifm (NodB, NodB + 2) = - 0.5 * u(3) * dahdh (n2) + dqdh
          endif
        endif
      enddo plus_minus_Dh
    enddo upstream_or_downstream

!-----------------------------------------
!Derivative with respect to u (first part)
!-----------------------------------------
    estifm (NodB, NodA) = -ah(n1) / 2.
    estifm (NodB, NodB) = -ah(n2) / 2.

    upstream_or_downstream_2: do j = 1, 2
      plus_minus_Dv: do i = 1, 2
        if (j == 1) then
              if (i == 1) then
            wv1 = wv1 + 0.5 * dv
          else
            wv1 = wv1 - dv
          endif
        else
          if (i == 1) then
            wv1 = wv1 + 0.5 * dv
            wv2 = wv2 + 0.5 * dv
          else
            wv2 = wv2 - dv
          endif
        endif
                
        if (njt (nm) == 12) then
          call wform (qDh (i), wh1, ws1, wv1, wh2, ws2, wv2, wec, wln, itp, widem)
          qDh(i) = qDh(i) * unitfac2
        elseif (njt (nm) == 11) then
            call wtform (qdh(i), imat (nn), ws1, ws2, mcord(nn,1),mcord(nn,2))
        elseif (njt (nm) == 10) then
          qdh(i) = cstrcQ_fromQCurves (imat (nn), ws1 + wv1**2/(2.0*grav), ws2 + wv2**2/(2.0*grav))
        endif

        qDh(i) = qDh(i) * QFACT1

!after looping up and down        
        if (i == 2) then
!total          
          dqdh = (qdh(1) - qdh(2)) / dh
!upstream          
          if (j == 1) then
            estifm (NodB, NodA) = estifm (NodB, NodA) + dqdh
!downstream          
          else
!reset water levels and water depths            
            wv2 = wv2 + 0.5 * dv
            estifm (NodB, NodB) = estifm (NodB, NodB)+dqdh
          endif
        endif
      enddo plus_minus_Dv
    enddo upstream_or_downstream_2

!-
!...... The other is some kind elevation relationship
!-
  elseIF(NJT(NM) == 1) THEN
!-
!...... Balance total head
!-
        NRX=NOP(NN,1)
        RX=VEL(1,NRX)*COS(ALFA(NRX))+VEL(2,NRX)*SIN(ALFA(NRX))
        THN=HEL(  NRX)+AO(NRX)+RX**2/(2.*GRAV)
        DO  KK=3,NCN
          N1=NOP(NN,KK)
          RY=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
          TH1=HEL(  N1)+AO(N1)+RY**2/(2.*GRAV)
          NodA=(KK-1)*NDF+1
          ESTIFM(NodA,1)=RX/GRAV
          ESTIFM(NodA,3)=1.0
          ESTIFM(NodA,NodA)=-RY/GRAV
          ESTIFM(NodA,NodA+2)=-1.0
          F(NodA)=TH1-THN
        ENDDO
!
      ELSEIF(NJT(NM) == 2) THEN
!-
!...... Reversible Q = function of head loss ( h1 -h2 -c)
!-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N1)-QD(NM)) < 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
!IPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N2)-QD(NM)) < 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
!ipk nov06 revise
        NodA=2*NDF+1
!dA/dh = average width        
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
!A        
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
!average current discharge in element        
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
!water surface elevations up- and downstream        
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
!elevation loss between upstream and downstream        
        HLOS=ABS(WS1-WS2)
!constant energy loss parameterized in element characteristic CJ        
        HLOS=HLOS-CJ(NM)
        HLD=SIGN(1.,WS1-WS2)
!residual  equation        
        F(NodA)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
        IF(HLOS < HCUT)   HLOS=HCUT
        ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
        ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.) -RX1*RSWT1*AWD1/2.
        ESTIFM(NodA,NodA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.) -RX2*RSWT2*AWD2/2.

!       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

!-
      ELSEIF(NJT(NM) == 3) THEN
!-
!...... Non-reversible Q = function of head loss ( h1 -h2 -c)
!-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N1)-QD(NM)) < 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
!IPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N2)-QD(NM)) < 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=WS1-WS2-CJ(NM)
        IF(HLOS < 0.) THEN
          F(NodA)=Q
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.

        ELSE
          F(NodA)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
          IF(HLOS < HCUT)   HLOS=HCUT
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.) -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.) -RX2*RSWT2*AWD2/2.

!       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

        ENDIF
!-
      ELSEIF(NJT(NM) == 4) THEN
!-
!......  Q = function of head  ( h1 )
!-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N1)-QD(NM)) < 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
!IPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N2)-QD(NM)) < 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
!ipk nov06 revise NodA
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)-CJ(NM)
        F(NodA)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
        ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
        ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.) -RX1*RSWT1*AWD1/2.
        ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.
!-
!       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

      ELSEIF(NJT(NM) == 5) THEN
!-
!...... Reversible   Head loss ( h1 -h2) = function of Q
!-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N1)-QD(NM)) < 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
!IPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N2)-QD(NM)) < 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
!ipk nov06 revise NodA
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        SQ=SIGN(1.,Q)
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=WS1-WS2
        HLD=SIGN(1.,WS1-WS2)
        F(NodA)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
        ESTIFM(NodA,1)= BJ(NM)*ACR1*RSWT1/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,NodA)= BJ(NM)*ACR2*RSWT2/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,3)=-1. +BJ(NM)*AWD1*RSWT1/2.*RX1*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,NodA+2)=1. +BJ(NM)*AWD2*RSWT2/2.*RX2*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
!-
!       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

      ELSEIF(NJT(NM) == 6) THEN
!-
!......  Q = function of head  ( h1 -c)
!-
        N1=NOP(NN,1)
!      RX1 is velocity in the 1-d direction at N1
!      RX2 is velocity in the 1-d direction at N2
!      RSWT1 and RSWT2 are direction multipliers
!       AWD1 and AWD2 are area/depth
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N1)-QD(NM)) < 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
!IPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) > 1.570796 .AND. ABS(ALFA(N2)-QD(NM)) < 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
!ipk nov06 revise NodA
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)-BJ(NM)
        WS2=HEL(N2)+AO(N2)-BJ(NM)
        WRITE(75,*) 'WS1,WS2',WS1,WS2
        IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN

!    No Flow case

          F(NodA)=Q
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)= -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.
        ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN

!    Flow controlled from upstream N1

          F(NodA)=Q-AJ(NM)*WS1**GAMJ(NM)
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.) -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.

!       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

        ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        

!    Flow controlled from upstream N2

          F(NodA)=-Q-AJ(NM)*WS2**GAMJ(NM)
          ESTIFM(NodA,1)=+(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=+(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=-AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.) +RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=+RX2*RSWT2*AWD2/2.

!C       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

        ELSE

!C    Flow controlled by H1-H2  Get new AJ

          IF(WS1 >= WS2) THEN
            AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
            F(NodA)=Q-AJN*(WS1-WS2)**CJ(NM)
            ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
            ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
            ESTIFM(NodA,3)=+AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.) -RX1*RSWT1*AWD1/2.
            ESTIFM(NodA,NodA+2)=-AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.) -RX2*RSWT2*AWD2/2.

!       Set temp/sal equation for equality

            F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
            ESTIFM(NodA+3,4)=-1.
            ESTIFM(NodA+3,NodA+3)=1.

          ELSE
            AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
            F(NodA)=-Q-AJN*(WS2-WS1)**CJ(NM)
            ESTIFM(NodA,1)=+(ACR1*RSWT1)/2.
            ESTIFM(NodA,NodA)=+(ACR2*RSWT2)/2.
            ESTIFM(NodA,3)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.) +RX1*RSWT1*AWD1/2.
            ESTIFM(NodA,NodA+2)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.) +RX2*RSWT2*AWD2/2.

!       Set temp/sal equation for equality

            F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
            ESTIFM(NodA+3,4)=-1.
            ESTIFM(NodA+3,NodA+3)=1.

          ENDIF
        ENDIF
!C-

!*********************
!something else
!|||||||||||||||||||||
!vvvvvvvvvvvvvvvvvvvvv
  ELSE

    WS1=WSLL(N1)
    WS2=WSLL(N2)
!ipk oct00
    if(isubm(n1) == 2) go to 500

    F(NodB)=WS1-WS2-(U(1)+U(3))**2/20.
!
!      Derivative with respect to u1 and u2
!
    ESTIFM(NodB,NodA)=(U(1)+U(3))/10.
    ESTIFM(NodB,NodB)=(U(1)+U(3))/10.
!
!      Derivative with respect to h1 and h2  (first term)
!
    ESTIFM(NodB,NodA+2)=-1.
    ESTIFM(NodB,NodB+2)=+1.

  ENDIF
  500   CONTINUE

RETURN
END
