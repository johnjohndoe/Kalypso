C     Last change:  MD    2 Sep 2008    1:44 pm
CIPK  NEWLY ADDED TO RMA10S FROM RMA2  JUNE 27 2005
CIPK  LAST UPDATE JAN 11 2005 ALLOW FOR NEG HLOS IN OPTION 2
CIPK  LAST UPDATE SEP 06 2004 ADD ERROR FILE
CIPK  LAST UPDATE JULY 5 2004 ALLOW FOR ENGLISH UNITS
cipk  last update oct 2 2003 compute embankment width
CIPK  LAST UPDATE SEP 23 2003 TEST FOR MISSING LEVEE DATA ADDED
CIPK  LAST UPDATE SEP 13 2002 ADJUST FOR DIST FUNCTION
CIPK  LAST UPDATE JUL 30 200 REVIE WEIR TESTING
cipk  last update may 3 2000 add submerged weir element
cipk  last update Apr 8 1997 fix integer*2 for msoft compiler
cipk  last update Mar 9 1997  add more control structures
Cipk  last update Sept 9 1996  fix lhs error for type 4
cipk  last update Apr 1 1996
cipk  last update nov 10 1995
      SUBROUTINE CSTRC2D(NN)
      USE COEF2MOD
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
      USE BLKECOM
      use paraKalyps, only: mcord
      
!NiS,jul06: Declaring waterlevel H for proper call of amf-subroutine
      REAL(KIND=8) :: H, WH1, WH2
      
      real (kind = 8) :: qdh (1:2)
!-

cipk may00 add cross velocity

      DIMENSION U(8),DAFL(8),V(8)
cipk apr97
cipk sep02      integer*2 n1,n2
      data pid2/1.5708/
      data pid32/4.7124/
      Dist(n1,n2)=sqrt((cord(n1,1)-cord(n2,1))**2 + 
     +                 (cord(n1,2)-cord(n2,2))**2)
cipk mar97 3 lines above modified

cipk mar00 initialize
      do j=1,32
        do k=1,32
          estifm(j,k)=0.
        enddo
        f(j)=0.
      enddo

cipk sep00
C-
C...... Determine type of control structure
C-
      NM=IMAT(NN)-900

C-
C...... Det up cutoff values
C-
      IF(GRAV < 32.) THEN
        HCUT=0.001
        QCUT=0.05
        ucut=0.01
      ELSE
        HCUT=0.003
        QCUT=0.15
        ucut=0.03
      ENDIF

!-----------------------------------------------------------
!Determine velocities at nodes based on fixed flow direction
!-----------------------------------------------------------
      NCN=NCORN(NN)
      DO 200 N=1,NCN
        NA=NOP(NN,N)
        IF(NA > 0) THEN
cipk sep00
          altmp=alfa(na)
          U(N)=VEL(1,NA)*COS(altmp)+VEL(2,NA)*SIN(altmp)
CIPK MAY00 ADD V COMPONENT
          V(N)=VEL(2,NA)*COS(altmp)-VEL(1,NA)*SIN(altmp)
        ENDIF
  200 CONTINUE

!--------------------------------------------------------------------------------
!FLOW BALANCE: Discharge upstream and downstream of a control structure are equal
!--------------------------------------------------------------------------------
      !This is a source term
      sinkSource = 0.0d0
      IF (NJT (NM) <= 1) sinkSource = -AJ (NM)

      !process only left corner, midside and right corner node of upstream arc of
      !cstrc element
      flowBalance: DO  nodeIndex = 1, 3      
        !UPSTREAM and DOWNSTREAM node for the following balance
        node_us = NOP (NN, nodeIndex)
        node_ds = NOP (NN, 8 - nodeIndex)

        !check for submergence
        if (isubm (node_us) == 2) cycle flowBalance

        !get equation number and columns for derivatives
        eqNo = (nodeIndex - 1) * ndf + 1
        column_us = eqNo
        column_ds = (7 - nodeIndex) * ndf + 1

        !get velocities and water depths
        v_us = u (nodeIndex)
        v_ds = u (8 - nodeIndex)
        if (nodeIndex /= 2) then
          h_us = vel (3, node_us)
          h_ds = vel (3, node_ds)
        else
          h_us = 0.5 * (vel (3, nop (nn, 1)) + vel (3, nop (nn, 3)))
          h_ds = 0.5 * (vel (3, nop (nn, 5)) + vel (3, nop (nn, 7)))
        endif

        !FLOW BALANCE between opposite corner nodes
        !------------------------------------------
        !
        !
        ! F = v   * h   - v   * h   + source/sink
        !      UW    UW    OW    OW
        !
        !
        !  dF      
        ! ---- = direction * h
        !  dv                 i
        !    i
        !
        !  dF
        ! ---- = direction * v
        !  dh                 i
        !    i
        IF(nodeIndex /= 2) THEN
          !form Residual vector
          f (eqNo) = sinkSource - h_us * v_us + h_ds * v_ds
          !from derivatives dF/dvi
          estifm (eqNo, column_us) = h_us
          estifm (eqNo, column_ds) = - h_ds
          !form derivatives dF/dhi
          estifm (eqNo, column_us + 2) = v_us
          estifm (eqNo, column_ds + 2) = - v_ds
        !FLOW BALANCE between opposite midside nodes
        !-------------------------------------------
        !                   |-             -|               |-             -|
        !                   | (i-1)   (i+1) |               | (i-1)   (i+1) |
        ! F = - v   * 0.5 * |h     + h      | - v   * 0.5 * |h     + h      | + sinkSource
        !        OW         | OW      OW    |    UW         | UW      UW    |
        !                   |_             _|               |_             _|
        ELSE
          !dF/dv (upstream)
          estifm (eqNo, column_us) = h_us
          !dF/dh (upstream left)
          estifm (eqNo, column_us - 2) = 0.5 * v_us
          !dF/dh (upstream right)
          estifm (eqNo, column_us + 6) = 0.5 * v_us
          !dF/dv (downstream)
          estifm (eqNo, column_ds) = - h_ds
          !dF/dh (downstream left)
          estifm (eqNo, column_ds - 2) = 0.5 * (-v_ds)
          !dF/dh (downstream right)
          estifm (eqNo, column_ds + 6) = 0.5 * (-v_ds)
          !Residual
          f (eqNo) = sinkSource - h_us * v_us + h_ds * v_ds
         ENDIF
      enddo flowBalance


!-------------------------------------------------------------------
!ARTIFICIAL EQUATION for midside nodes of side arcs of CSTRC element
!-------------------------------------------------------------------
         !Residual for velocities at left midside
         f (29) = u (8) - (u (7) + u (1)) / 2.
         f (30) = v (8) - (v (7) + v (1)) / 2.
         estifm (29, 1)  = 0.5
         estifm (29, 25) = 0.5
         estifm (29, 29) = -1.0
         estifm (30, 2)  = 0.5
         estifm (30, 26) = 0.5
         estifm (30, 30) = -1.0
         !Residual for velocities at right midside
         f (13) = u (4) - (u (3) + u (5)) / 2.
         f (14) = v (4) - (v (3) + v (5)) / 2.
         estifm (13, 9)  = 0.5
         estifm (13, 17) = 0.5
         estifm (13, 13) = -1.0
         estifm (14, 10) = 0.5
         estifm (14, 18) = 0.5
         estifm (14, 14) = -1.0



!---------------------------------------------------------
!FLOW CONDITION based on the type of the control structure
!---------------------------------------------------------

      if (njt (nm) == 10 .OR. njt (nm) == 11 .OR. njt (nm) == 12) then
      
        !njt defines type of flow controller
        ! 10 - Q-curve group
        ! 11 - tabular data
        ! 12 - weir formula


!Residual function works like this
!---------------------------------
! F = 0.5 * (u1*h1 + u2*h2) - q(u1,h1,v2,h2) = 0

        !ITP displays weir crest structure
        !1 - paved
        !0 - gravel
        ITP=1

        !do for all t
      flowCondition: DO nodeIndex = 1, 3      
        
        node_us = nop (nn, nodeIndex)
        node_ds = nop (nn, 8 - nodeIndex)

        !control structure length
        widem = sqrt ((cord (node_ds, 2) - cord (node_us, 2))**2
     +              + (cord (node_ds, 1) - cord (node_us, 1))**2)


!---------------
!direction stuff
!---------------
          altmp=atan2(cord(node_ds,2)-cord(node_us,2),
     +                    cord(node_ds,1)-cord(node_us,1))
          if(abs(alfa(node_us)) < pid2) then
            if(alfa(node_us) + pid2 < altmp .OR. 
     +         alfa(node_us) - pid2 > altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          elseif(alfa(node_us) > 0.) then
            if(alfa(node_us)-pid2 > altmp .AND. 
     +           alfa(node_us)-pid32 < altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          else
            if(alfa(node_us)+pid2 < altmp .AND. 
     +           alfa(node_us)+pid32 > altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          endif
!---------------
!direction stuff
!---------------

          eqNo = (7 - nodeIndex) * ndf + 1
          column_ds = eqNo
          column_us = (nodeIndex - 1) * ndf + 1

          !Set dh and dv for numerical derivative
          dh = 0.002
          dv = 0.01
      
          !for opposite corner nodes (holding velocities and depths
          if (nodeIndex /= 2) then
            !specific discharge
            q = 0.5 * (u(nodeindex) * vel(3, node_us) 
     +               + u(8 - nodeIndex) * vel(3, node_ds))

            !The following logic controls, whether flow controller is switched on or switched off;
            ! If control structure is switched off, the flow will be set to zero and the subroutine left
            QFACT1 = 1.0
            IF (NTMREF (IMAT(NN)) /= 0) THEN
              CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET
     +             ,QFACT1)
              IF(ISWTOF == 1) THEN
                Q1T = 0.0
                      F (eqNo) = Q - Q1T
                !Leave routine, if weir is not operative in this time step!
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

            !prepare variables
            WH1 = unitFac1 * VEL (3, node_us)
            WH2 = unitFac1 * VEL (3, node_ds)
            WS1 = unitFac1 * WSLL (node_us)
            WS2 = unitFac1 * WSLL (node_ds)
            WV1 = unitFac1 * SQRT (VEL (1, node_us)**2
     +                           + VEL (2, node_us)**2)
            WV2 = unitFac1 * SQRT (VEL (1, node_ds)**2
     +                           + VEL (2, node_ds)**2)
            WEC = unitFac1 * WHGT (node_us)
            WLN = unitFac1 * WLEN (node_us)

!RESIDUAL equation of flow condition
!-----------------------------------
            !weir formula
            if (njt (nm) == 12) then
              CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,widem)
              Q1T = Q1T * unitFac2
            !QFunction relationship
            elseif (njt (nm) == 10) then
              q1t = cstrcQ_fromQCurves (imat (nn), 
     +                                  ws1 + wv1**2/(2.0*grav),
     +                                  ws2 + wv2**2/(2.0*grav))
            !Q tabular relationship
            elseif (njt (nm) == 11) then
              call wtform(q1t,nctref(imat(nn)),ws1,ws2,
     +                    mcord(nn,1),mcord(nn,2))
            endif
            !Factorize discharge by direction fix
            Q1T = Q1T * QFACT1 * srevr
            !Form residual
            F (eqNo) = Q - Q1T


!Derivative with respect to h
!----------------------------
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
                  call wform (qDh (i), wh1, ws1, wv1,
     +                   wh2, ws2, wv2, wec, wln, itp, widem)
                  qDh(i) = qDh(i) * unitfac2
                elseif (njt (nm) == 11) then
                  call wtform (qdh (i), imat (nn), ws1, ws2,
     +                   mcord(nn,1), mcord(nn,2))
                elseif (njt (nm) == 10) then
                  qdh(i) = cstrcQ_fromQCurves (imat (nn), 
     +                                  ws1 + wv1**2/(2.0*grav),
     +                                  ws2 + wv2**2/(2.0*grav))
                endif
                qDh(i) = qDh(i) * QFACT1 * srevr

                !after looping up and down
                if (i == 2) then
                  !total
                  dqdh = (qdh(1) - qdh(2)) / dh
                  !upstream
                  if (j == 1) then
                    estifm (eqno, column_us+2) = 
     +                - 0.5 * u(nodeIndex) + dqdh
                  !downstream
                  else
                    !reset water levels and water depths
                    wh2 = wh2 + 0.5 * dh
                    ws2 = ws2 + 0.5 * dh
                    estifm (eqno, column_ds+2) =
     +                - 0.5 * u(8-nodeIndex) + dqdh
                  endif
                endif
              enddo plus_minus_Dh
            enddo upstream_or_downstream

!-----------------------------------------
!Derivative with respect to u (first part)
!-----------------------------------------
            estifm (eqno, column_us) = -vel (3, node_us) / 2.
            estifm (eqno, column_ds) = -vel (3, node_ds) / 2.

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
                  call wform (qDh (i), wh1, ws1, wv1,
     +                 wh2, ws2, wv2, wec, wln, itp, widem)
                  qDh (i) = qDh (i) * unitfac2
                elseif (njt (nm) == 11) then
                  call wtform (qdh(i), imat (nn), ws1, ws2,
     +                         mcord(nn,1),mcord(nn,2))
                elseif (njt (nm) == 10) then
                  qdh(i) = cstrcQ_fromQCurves (imat (nn), 
     +                       ws1 + wv1**2/(2.0*grav), 
     +                       ws2 + wv2**2/(2.0*grav))
                endif

                qDh(i) = qDh(i) * QFACT1 * srevr

                !after looping up and down
                if (i == 2) then
                  !total
                  dqdh = (qdh(1) - qdh(2)) / dh
                  !upstream
                  if (j == 1) then
                    estifm (eqno, column_us) =
     +                estifm (eqno, column_us)+dqdh
                  !downstream
                  else
                    !reset water levels and water depths
                    wv2 = wv2 + 0.5 * dv
                    estifm (eqno, column_ds) = 
     +                estifm (eqno, column_ds)+dqdh
                  endif
                endif
              enddo plus_minus_Dv
            enddo upstream_or_downstream_2
          ELSE
C
C     This is a mid-side node
C
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)

            Q=(U(nodeIndex)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-nodeIndex)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            QM=(U(1)*VEL(3,N3)+U(3)*VEL(3,N4))/4.
     +        +(U(7)*VEL(3,N6)+U(5)*VEL(3,N5))/4.
C DFDU2
            ESTIFM(eqno,column_us)=-(VEL(3,N3)+VEL(3,N4))/4.
C DFDU6
            ESTIFM(eqno,column_ds)=-(VEL(3,N5)+VEL(3,N6))/4.
C DFDH1
            ESTIFM(eqno,column_us-2)=-(U(nodeIndex)/4.-U(1)/4.)
C DFDH3
            ESTIFM(eqno,column_us+6)=-(U(nodeIndex)/4.-U(3)/4.)
C DFDH5
            ESTIFM(eqno,column_ds-2)=-(U(8-nodeIndex)/4.-U(5)/4.)
C DFDH7
            ESTIFM(eqno,column_ds+6)=-(U(8-nodeIndex)/4.-U(7)/4.)
C DFDU1
            ESTIFM(eqno,column_us-4)=VEL(3,N3)/4.
C DFDU3
            ESTIFM(eqno,column_us+4)=VEL(3,N4)/4.
C DFDU5
            ESTIFM(eqno,column_ds-4)=VEL(3,N5)/4.
C DFDU7
            ESTIFM(eqno,column_ds+4)=VEL(3,N6)/4.
            F(eqNo)=Q-QM
          ENDIF

        ENDDO flowCondition
C-

!----------------
!other cstrc type
!----------------
      elseIF(NJT(NM) == 1) THEN
C-
C...... Balance total head
C-
        DO 310 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
            Q1=U(KK)
            Q2=U(8-KK)
            IF(Q1 == 0.) THEN
              Q1=QCUT
            ELSEIF(ABS(Q1) < QCUT) THEN
              Q1=SIGN(QCUT,Q1)
            ENDIF
            IF(Q2 == 0.) THEN
              Q2=QCUT
            ELSEIF(ABS(Q2) < QCUT) THEN
              Q2=SIGN(QCUT,Q2)
            ENDIF
          IF(KK /= 2) THEN
            TH1=HEL(N1)+ADO(N1)+U(KK)**2/(2.*GRAV)
            TH2=HEL(N2)+ADO(N2)+U(8-KK)**2/(2.*GRAV)
            F(NB)=TH1-TH2
            ESTIFM(NB,NA)=-Q1/GRAV
            ESTIFM(NB,NB)=+Q2/GRAV
            ESTIFM(NB,NA+2)=-1.
            ESTIFM(NB,NB+2)=+1.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            TH1=WS1+U(KK)**2/(2.*GRAV)
            TH2=WS2+U(8-KK)**2/(2.*GRAV)
            F(NB)=TH1-TH2
            ESTIFM(NB,NA)=-Q1/GRAV
            ESTIFM(NB,NB)=+Q2/GRAV
            ESTIFM(NB,NA-2)=-0.5
            ESTIFM(NB,NA+6)=-0.5
            ESTIFM(NB,NB-2)=0.5
            ESTIFM(NB,NB+6)=0.5
          ENDIF
  310   CONTINUE
C
      ELSEIF(NJT(NM) == 2) THEN
C-
C...... Reversible Q = function of head loss ( h1 -h2 -c)
C-
        DO 320 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=ABS(WS1-WS2)
            HLOS=HLOS-CJ(NM)
            HLD=SIGN(1.,WS1-WS2)
cipk jan05
            IF(HLOS < 1.E-06) THEN
                F(NB)=Q-AJ(NM)
              ELSE              
              F(NB)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
            ENDIF
            IF(HLOS < HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(KK)/2.
            ESTIFM(NB,NB+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=ABS(WS1-WS2)
            HLOS=HLOS-CJ(NM)
            HLD=SIGN(1.,WS1-WS2)
cipk jan05
            IF(HLOS < 1.E-06) THEN
                F(NB)=Q-AJ(NM)
              ELSE              
              F(NB)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
            ENDIF
            IF(HLOS < HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
            ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
            ESTIFM(NB,NB-2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
            ESTIFM(NB,NB+6)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
          ENDIF
  320   CONTINUE
C-
      ELSEIF(NJT(NM) == 3) THEN
C-
C...... Non-reversible Q = function of head loss ( h1 -h2 -c)
C-
        DO 330 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=WS1-WS2-CJ(NM)
            IF(HLOS < 0.) THEN
              F(NB)=Q
            ELSE
              F(NB)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
              IF(HLOS < HCUT)   HLOS=HCUT
              ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -U(KK)/2.
              ESTIFM(NB,NB+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(8-KK)/2.
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=WS1-WS2-CJ(NM)
            IF(HLOS < 0.) THEN
              F(NB)=Q
            ELSE
              F(NB)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
              IF(HLOS < HCUT)   HLOS=HCUT
              ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
              ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
              ESTIFM(NB,NB-2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
              ESTIFM(NB,NB+6)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
            ENDIF
          ENDIF
  330   CONTINUE
C-
      ELSEIF(NJT(NM) == 4) THEN
C-
C......  Q = function of head  ( h1 )
C-
        DO 340 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)-CJ(NM)
            F(NB)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +               -U(KK)/2.
            ESTIFM(NB,NB+2)=
     +               -U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
cipk mar96 CJ is missing        WS1=(HEL(N3)+AO(N3)+HEL(N4)+AO(N4))/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-CJ(NM)
            F(NB)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
cipk sep96 fix actor of 2 error     +               -U(KK)/2.
            ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
cipk sep96 fix factor of 2 error     +               -U(KK)/2.
            ESTIFM(NB,NB-2)=
     +               -U(8-KK)/4.
            ESTIFM(NB,NB+6)=
     +               -U(8-KK)/4.
          ENDIF
  340   CONTINUE
C-
      ELSEIF(NJT(NM) == 5) THEN
C-
C...... Reversible   Head loss ( h1 -h2) = function of Q
C-

        DO 350 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            SQ=SIGN(1.,Q)
cipk jul94 add 2 line
            call amf(h,vel(3,n1),akp(n1),adt(n1),adb(n1),am1,dam1,0)
            call amf(h,vel(3,n2),akp(n2),adt(n2),adb(n2),am2,dam2,0)
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=WS1-WS2
            HLD=SIGN(1.,WS1-WS2)
            F(NB)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
            IF(Q == 0.) THEN
              Q=QCUT
            ELSEIF(ABS(Q) < QCUT) THEN
              Q=SIGN(QCUT,Q)
            ENDIF
            ESTIFM(NB,NA)=
     +    BJ(NM)*VEL(3,N1)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
            ESTIFM(NB,NB)=
     +    BJ(NM)*VEL(3,N2)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NA+2)=-1.
            ESTIFM(NB,NA+2)=-(1.+dam1)
     +   +BJ(NM)*U(KK)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB+2)=1.
            ESTIFM(NB,NB+2)=(1.+dam2)
     +   +BJ(NM)*U(8-KK)/2.*Q*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            SQ=SIGN(1.,Q)
cipk jul94 add 4 lines
            call amf(h,vel(3,n3),akp(n3),adt(n3),adb(n3),am3,dam3,0)
            call amf(h,vel(3,n4),akp(n4),adt(n4),adb(n4),am4,dam4,0)
            call amf(h,vel(3,n5),akp(n5),adt(n5),adb(n5),am5,dam5,0)
            call amf(h,vel(3,n6),akp(n6),adt(n6),adb(n6),am6,dam6,0)
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=WS1-WS2
            F(NB)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
            IF(Q == 0.) THEN
              Q=QCUT
            ELSEIF(ABS(Q) < QCUT) THEN
              Q=SIGN(QCUT,Q)
            ENDIF
            ESTIFM(NB,NA)=BJ(NM)*(VEL(3,N3)+VEL(3,N4))/4.
     +                  *GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
            ESTIFM(NB,NB)=BJ(NM)*(VEL(3,N5)+VEL(3,N6))/4.
     +                  *GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94           ESTIFM(NB,NA-1)=-0.5
            ESTIFM(NB,NA-2)=-0.5*(1.0+dam3)
     +   +BJ(NM)*U(KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NA+5)=-0.5
            ESTIFM(NB,NA+6)=-0.5*(1.0+dam4)
     +   +BJ(NM)*U(KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB-1)=0.5
            ESTIFM(NB,NB-2)=0.5*(1.0+dam5)
     +   +BJ(NM)*U(8-KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB+5)=0.5
            ESTIFM(NB,NB+6)=0.5*(1.0+dam6)
     +   +BJ(NM)*U(8-KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
          ENDIF
  350   CONTINUE
C-
      ELSEIF(NJT(NM) == 6) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 360 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              IF(WS1 > WS2) THEN
                AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
                F(NB)=Q-AJN*(WS1-WS2)**CJ(NM)
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
              ELSEIF(WS1 == WS2) THEN
                F(NB)=Q
                ajn=aj(nm)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)
              ELSE
                AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
                F(NB)=-Q-AJN*(WS2-WS1)**CJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
     +                          -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                          +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              IF(WS1 > WS2) THEN
                AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
                F(NB)=Q-AJN*(WS1-WS2)**CJ(NM)
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
              ELSEIF(WS1 == WS2) THEN
                F(NB)=Q
                ajn=aj(nm)
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
              ELSE
                AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
                F(NB)=-Q-AJN*(WS2-WS1)**CJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
     +                       -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
     +                       -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-1)=-ESTIFM(NB,NB-1)
     +                   +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                   +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
              ENDIF
            ENDIF
          ENDIF
cc          f(nb)=f(nb)/2.
  360   CONTINUE

C-
      ELSEIF(NJT(NM) == 7) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 370 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow submerged
              IF(WS1 > WS2) THEN
C    Flow controlled from upstream N1
                F(NB)=Q-AJ(NM)*WS1**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)*DJ(NM)
              ELSEIF(WS1 == WS2) THEN
                F(NB)=Q
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJ(NM)*DJ(NM)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJ(NM)*DJ(NM)
              ELSE
C    Flow controlled from upstream N2
                F(NB)=-Q-AJ(NM)*WS2**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
                ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*CJ(NM)*WS2**(CJ(NM)-1.)*DJ(NM)
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow submerged
              IF(WS1 > WS2) THEN
C    Flow controlled from upstream N1
                F(NB)=Q-AJ(NM)*WS1**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA-6)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)/2.*DJ(NM)
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)/2.*DJ(NM)
              ELSEIF(WS1 == WS2) THEN
                F(NB)=Q
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*DJ(NM)
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*DJ(NM)
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJ(NM)*DJ(NM)
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJ(NM)*DJ(NM)
              ELSE
C    Flow controlled from upstream N2
                F(NB)=-Q-AJ(NM)*WS2**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
                ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
                ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*CJ(NM)*(WS2**(CJ(NM)-1.))/2.*DJ(NM)
                ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*CJ(NM)*(WS2**(CJ(NM)-1.))/2.*DJ(NM)
              ENDIF
            ENDIF
          ENDIF
cc          f(nb)=f(nb)/2.
  370   CONTINUE
cipk mar97 add new loss function
C-
      ELSEIF(NJT(NM) == 8) THEN
C-
C.......If Depth < set value
C...... Reversible Q = C1*AFL*Sqrt( h1 -h2)
C......       or   q = C1*AFL/w *sqrt(h1-h2)
C......            A = function of depth
C-
C...... If Depth > set value 
C.      Reversible Q = C2*AFL*Sqrt(h1 -h2)
C.            or   q = C2*AFL/w*sqrt(h1-h2)
C. ...             A = constant at set value depth
C-
C.....  Get w first by averaging 1 to 3 and 5 to 7
        w=(dist(nop(nn,1),nop(nn,3))+dist(nop(nn,5),nop(nn,7)))/2.
C.......Get A based on average water depth
        avedep=(vel(3,nop(nn,1))+vel(3,nop(nn,3))+
     +          vel(3,nop(nn,5))+vel(3,nop(nn,7)))/4.
        wbas=cj(nm)
        ssl=gamj(nm)
        dnomin=dj(nm)
c        write(75,*) 'w,avedep,ssl,dnomin',w,avedep,ssl,dnomin
        if(avedep < dnomin ) then
          afl=avedep*(wbas+ avedep*ssl)
          dafl(1)=wbas/4.0+ssl*avedep/2.0
          dafl(3)=wbas/4.0+ssl*avedep/2.0
          dafl(5)=wbas/4.0+ssl*avedep/2.0
          dafl(7)=wbas/4.0+ssl*avedep/2.0
          cof1=aj(nm)
        else
          afl=dnomin*(wbas+ dnomin*ssl)
          dafl(1)=0.
          dafl(3)=0.
          dafl(5)=0
          dafl(7)=0.
          cof1=bj(nm)
        endif
c        write(75,*) 'afl,dafl(1),cof1',afl,dafl(1),cof1
        DO 390 KK=1,3
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=ABS(WS1-WS2)
            HLD=SIGN(1.,WS1-WS2)
c            write(75,*) 'hlos Q',hlos,Q
C            if(hlos == 0.) hlos=0.0001
            F(NB)=Q-cof1*AFL*hld*SQRT(HLOS)/W
            IF(HLOS < HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=+cof1*(AFL/SQRT(HLOS)*0.5
     +                        +SQRT(HLOS)*DAFL(KK))/W-U(KK)/2.
            ESTIFM(NB,NB+2)=-cof1*(AFL/SQRT(HLOS)*0.5
     +                        +SQRT(HLOS)*DAFL(8-KK))/W-U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=ABS(WS1-WS2)
            HLD=SIGN(1.,WS1-WS2)
c            write(75,*) 'hlos1 Q',hlos,Q
            F(NB)=Q-COF1*AFL*HLD*SQRT(HLOS)/W
            IF(HLOS < HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(KK-1))/W- U(KK)/4.
            ESTIFM(NB,NA+6)=cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(KK+1))/W- U(KK)/4.
            ESTIFM(NB,NB-2)=-cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(9-KK))/W- U(8-KK)/4.
            ESTIFM(NB,NB+6)=-cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(7-KK))/W- U(8-KK)/4.
          ENDIF
  390   CONTINUE
C-
C-
      ELSEIF(NJT(NM) == 9) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 420 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK /= 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            UM=(U(KK)+U(8-KK))/2.
            call amf(h,vel(3,n1),akp(n1),adt(n1),adb(n1),am1,dam1,0)
            call amf(h,vel(3,n2),akp(n2),adt(n2),adb(n2),am2,dam2,0)
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              if(q == 0) q=qcut
              IF(Q > 0.) THEN
                if(abs(ws1-ws2) < 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS1-WS2-AJN*UM**CJN
c                write(75,*) 'ajn,U,F(NB)',ajn,UM,F(NB)
                if(um == 0.) um=ucut
                ESTIFM(NB,NA)=+AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NB)=+AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NA+2)=-1.0
CC     +                          +AJN*CJN*UM**(CJN-1.)*U(KK)/2.
                ESTIFM(NB,NB+2)=+1.0
CC     +                          +AJN*CJN*UM**(CJN-1.)*U(8-KK)/2.
              ELSEIF(Q == 0.) THEN
                F(NB)=WS1-WS2
                ESTIFM(NB,NA)=0.
                ESTIFM(NB,NB)=0.
                ESTIFM(NB,NA+2)=-1.0
                ESTIFM(NB,NB+2)=+1.0
              ELSE
                if(abs(ws1-ws2) < 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS2-WS1-AJN*(-UM)**CJN
                ESTIFM(NB,NA)=-AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NB)=-AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NA+2)=+1.0
CC     +                        -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/2.
                ESTIFM(NB,NB+2)=-1.0
CCC    +                        -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/2.
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            UM=(U(KK)+U(8-KK))/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 <= 0.  .AND. WS2 <= 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 > 0. .AND. WS2 <= 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 <= 0. .AND. WS2 > 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              if(q == 0.) q=qcut
              IF(Q > 0.) THEN
                if(abs(ws1-ws2) < 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS1-WS2-AJN*UM**CJN
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                if(um == 0.) um=ucut
                ESTIFM(NB,NA)=
     +                 +AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NB)=
     +                 +AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NA-2)=-0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NA+6)=-0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NB-2)=0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(8-KK)/4.
                ESTIFM(NB,NB+6)=0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(8-KK)/4.
              ELSEIF(Q == 0.) THEN
                F(NB)=WS1-WS2
                ESTIFM(NB,NA)=0.1
                ESTIFM(NB,NB)=0.1
                ESTIFM(NB,NA-2)=-0.5
                ESTIFM(NB,NA+6)=-0.5
                ESTIFM(NB,NB-2)=0.5
                ESTIFM(NB,NB+6)=0.5
              ELSE
                if(abs(ws1-ws2) < 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS2-WS1-AJN*(-UM)**CJN
                ESTIFM(NB,NA)=
     +                 -AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NB)=
     +                 -AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NA-2)=+0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NA+6)=+0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NB-2)=-0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/4.
                ESTIFM(NB,NB+6)=-0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/4.
              ENDIF
            ENDIF
          ENDIF
          f(nb)=f(nb)/2.
  420   CONTINUE







      ENDIF

      RETURN
      END
