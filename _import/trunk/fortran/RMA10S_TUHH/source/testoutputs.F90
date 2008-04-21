!     Last change:  WP   15 Apr 2008   12:05 pm
!subroutines for testoutput from coef1DfE subroutine

subroutine MomOvVel (daintdt, areaint, vflowint, dbeiintdx, beiint, daintdx, dvintdx, grav, sfint, optin, icyc, sidft)
  implicit none
  REAL (KIND = 8), INTENT (IN) :: areaint, daintdt, daintdx
  REAL (KIND = 8), INTENT (IN) :: vflowint, dvintdx
  REAL (KIND = 8), INTENT (IN) :: beiint, dbeiintdx
  REAL (KIND = 8), INTENT (IN) :: grav, sfint, sidft
  INTEGER, INTENT (IN) :: optin, icyc

  WRITE(*,*) 'Momentum over velocity'
  WRITE(*,*) 'Direktterme FEEAN'

  if (icyc > 0) then
    if (optin == 1) then
      WRITE(*,*) 'Term A1: ', + daintdt
    end if
  end if

  IF     (optin == 1) then
    WRITE(*,*) ' Term B: ', + 2.0 * areaint* vflowint * dbeiintdx
    WRITE(*,*) ' Term C: ', + 2.0 * vflowint * beiint * daintdx
    WRITE(*,*) ' Term D: ', + 2.0 * beiint * areaint * dvintdx
  ELSEIF (optin == 2) then
    WRITE(*,*) ' Term D: ', + areaint * dvintdx
  ENDIF

  WRITE(*,*) ' Term F: ', + 2.0 * grav * sfint * areaint / vflowint
  WRITE(*,*) ' Term H: ', + sidft
  WRITE(*,*) 'Ableitungsterme FEEBN'
  IF     (optin == 1) then
    WRITE(*,*) ' Term D: ', + 2.0 * beiint * areaint * vflowint
  ELSEIF (optin == 2) then
    WRITE(*,*) ' Term D: ', + vflowint * areaint
  ENDIF
  pause
end subroutine

subroutine MomOvDep (vflowint, dvintdt, dvintdx, hhint, dhintdt, dhhintdx, beiint, dbeiintdh, &
         & d2beiintdhdx, areaint, daintdx, dareaintdh, d2areaintdh, d2aidhdx, sfint, dsfintdh1, yps, &
         & dypsdx, dypsdh, d2ypsdhdx, zsint, dzsintdh, grav, sbot, optin, icyc, byparts)
  implicit none
  REAL (KIND = 8), INTENT (IN) :: vflowint, dvintdt, dvintdx
  real (kind = 8), INTENT (IN) :: hhint, dhintdt, dhhintdx
  real (kind = 8), INTENT (IN) :: beiint, dbeiintdh, d2beiintdhdx
  real (kind = 8), INTENT (IN) :: areaint, daintdx, dareaintdh, d2areaintdh, d2aidhdx
  real (kind = 8), INTENT (IN) :: sfint, dsfintdh1
  real (kind = 8), INTENT (IN) :: yps, dypsdx, dypsdh, d2ypsdhdx
  real (kind = 8), INTENT (IN) :: zsint, dzsintdh
  real (kind = 8), INTENT (IN) :: grav, sbot
  integer, INTENT (IN) :: optin, icyc, byparts

  WRITE(*,*) 'Momentum over depth'
  WRITE(*,*) 'Direktterme FEEAN'

  if (icyc > 0) then
    if     (optin == 1) then
      WRITE(*,*) 'Term A1: ', + dareaintdh * dvintdt
      WRITE(*,*) 'Term A2: ', + dareaintdh * dhintdt
    ELSEIF (optin == 2) then
      WRITE(*,*) 'Term A1: ', + dareaintdh * dvintdt
    end if
  end if

  IF     (optin == 1) then
    WRITE(*,*) ' Term B: ', + vflowint**2 * (dareaintdh * dbeiintdh + areaint * d2beiintdhdx)
    WRITE(*,*) ' Term C: ', + vflowint**2 * (dbeiintdh * daintdx + beiint * d2aidhdx)
    WRITE(*,*) ' Term D: ', + 2.0 * vflowint * dvintdx * (beiint * dareaintdh + areaint * dbeiintdh)
  ELSEIF (optin == 2) then
    WRITE(*,*) ' Term D', + vflowint * dvintdx * dareaintdh
  ENDIF

  if     (byparts == 1) then
    WRITE(*,*) 'Term E1: ', + grav * dareaintdh * dhhintdx
  ELSEIF (byparts == 2) then
    WRITE(*,*) 'Term E1: ', - 0.5  * grav * hhint * d2areaintdh * dhhintdx
  ELSEIF (byparts == 3) then
    WRITE(*,*) 'Term E1: ', + grav *                                                                            &
    &     (hhint * dypsdx * dareaintdh + areaint * dypsdx + areaint * hhint * d2ypsdhdx &
    &      + yps * dhhintdx * dareaintdh + areaint * dhhintdx * dypsdh                        &
    &      - dzsintdh * daintdx - zsint * d2areaintdh)
  end if

  WRITE(*,*) ' Term F: ', + grav * (areaint * dsfintdh1 + sfint * dareaintdh)
  WRITE(*,*) ' Term G: ', - grav * sbot * dareaintdh
  WRITE(*,*) 'Ableitungsterme FEEBN'
  if (byparts == 1) then
    WRITE(*,*) 'Term E2: ', + grav * areaint
  ELSEIF (byparts == 2) then
    WRITE(*,*) 'Term E2: ', - grav * 0.5 * (hhint * dareaintdh - areaint)
  ELSEIF (byparts == 3) then
    WRITE(*,*) 'Term E2: ', + grav * (areaint * yps + areaint * hhint * dypsdh - zsint * dareaintdh )
  end if
  if (byparts == 2) then
    WRITE(*,*) 'Ableitungsterme FEECN'
    WRITE(*,*) 'Term E3: ', - 0.5 * grav * (hhint * dareaintdh + areaint)
  ELSEIF (byparts == 3) then
    WRITE(*,*) 'Ableitungsterme FEECN'
    WRITE(*,*) 'Term E3: ', - grav * (zsint * dareaintdh + areaint * dzsintdh)
  end if
  pause
end subroutine

subroutine Mom (nn, i, optin, icyc, byparts, areaint, daintdt, daintdx, dareaintdh, vflowint, dvintdt, &
       & dvintdx, hhint, dhhintdx, beiint, dbeiintdx, zsint, yps, dypsdx, sfint, sbot, xn, &
       & dnx, frn, frnx, ams, grav, sidft)


  implicit none
  INTEGER, INTENT (IN) :: nn, i, optin, icyc, byparts
  real (kind = 8), INTENT (IN) :: areaint, daintdt, daintdx, dareaintdh
  real (kind = 8), INTENT (IN) :: vflowint, dvintdt, dvintdx
  real (kind = 8), INTENT (IN) :: hhint, dhhintdx
  real (kind = 8), INTENT (IN) :: beiint, dbeiintdx
  real (kind = 8), INTENT (IN) :: zsint, yps, dypsdx
  real (kind = 8), INTENT (IN) :: sfint, sbot
  real (kind = 8), INTENT (IN) :: frn, frnx, ams, grav, sidft
  REAL (KIND = 8), DIMENSION (3), INTENT (IN) :: xn, dnx
  INTEGER :: l

  WRITE(*,*) 'Momentum equation, element: ', nn, 'GaussPt.: ', i
  WRITE(*,*) 'FRN'
  if (icyc > 0) then
    if (optin == 1) then
      WRITE(*,*) 'Term A1: ', + areaint * dvintdt
      WRITE(*,*) 'Term A2: ', + vflowint * daintdt
    ELSEIF (optin == 2) then
      WRITE(*,*) ' Term A: ', + areaint * dvintdt
    end if
  end if

  if (optin == 1) then
    WRITE(*,*) ' Term B: ', + vflowint**2 * areaint * dbeiintdx
    WRITE(*,*) ' Term C: ', + beiint * vflowint**2 * daintdx
    WRITE(*,*) ' Term D: ', + 2 * beiint * vflowint * areaint * dvintdx
  ELSEIF (optin == 2) then
    WRITE(*,*) ' Term D: ', + vflowint * areaint * dvintdx
  endif

  if     (byparts == 1) then
    WRITE(*,*) ' Term E: ',  + grav * areaint * dhhintdx
  ELSEIF (byparts == 2) then
    WRITE(*,*) 'Term E1: ', - grav * dhhintdx * (dareaintdh * hhint   - areaint)/ 2.0
  ELSEIF (byparts == 3) then
    WRITE(*,*) 'Term E1: ', - grav * (daintdx * zsint) &
                          & + grav * areaint * (hhint * dypsdx + yps * dhhintdx)
  end if

  WRITE(*,*) ' Term F: ', + grav * areaint * sfint
  WRITE(*,*) ' Term G: ', - grav * areaint * sbot
  WRITE(*,*) ' Term H: ', + sidft * vflowint

  if (byparts == 2) then
    WRITE(*,*) 'FRNX'
    WRITE(*,*) 'Term E2: ', - grav * areaint * hhint / 2.0
  ELSEIF (byparts == 3) then
    WRITE(*,*) 'FRNX'
    WRITE(*,*) 'Term E2: ', - grav * areaint * zsint
  end if

  WRITE(*,*) 'Einzelterme: ', FRN, FRNX
  do l = 1, 3
    WRITE(*,*) 'weighting: ', l,  xn(l), dnx(l)
    WRITE(*,*) 'assembled value without weighting: ', (xn(l) * FRN + dnx(l) * FRNX)
    WRITE(*,*) 'assembled value    with weighting: ', ams * (xn(l) * FRN + dnx(l) * FRNX)
  end do

  pause
end subroutine

subroutine ElementResult (nn, ncon, nop1, nop3, h1, h3, sbot, xl, area, qh1, qh3, vel_res, ah1, ah3, sfnod, sfwicht, &
           &  dahdh1, dahdh3, d2ahdh, dqhdh, d2qhdh, hhint, dhhintdx, areaint, dareaintdh, d2areaintdh, daintdx, d2aintdx, &
           &  d2aidhdx, qschint, dqsintdh, d2qsidh, dqsintdx, s0schint, sfint, dsfintdh1, vflowint, dvintdx)
  implicit none

  INTEGER, INTENT (IN) :: ncon(8), nop1, nop3, nn
  REAL (KIND = 8), INTENT (IN) :: sbot, xl (3), area
  real (kind = 8), intent (IN) :: qh1, qh3, ah1, ah3, dahdh1, dahdh3, vel_res (3), h1, h3
  REAL (KIND = 8), INTENT (IN), dimension (1:2) :: sfnod, sfwicht, d2ahdh, dqhdh, d2qhdh
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: hhint, dhhintdx, areaint, dareaintdh, d2areaintdh, daintdx, d2aintdx, d2aidhdx
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: qschint, dqsintdh, d2qsidh, dqsintdx
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: s0schint, sfint, dsfintdh1
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: vflowint, dvintdx
  INTEGER :: i, n1, n3

  n3 = ncon(3)
  n1 = ncon(1)
  WRITE(*,*) '*************************'
  WRITE(*,*) 'Knotendaten, Element: ', nn
  WRITE(*,*) '*************************'
  WRITE(*,'(a18,2(6x,i4))')    '          Knoten: ', nop1, nop3
  WRITE(*,'(a18, 1x,f13.8)')   '    Sohlgefaelle: ', sbot
  WRITE(*,'(a18,2(1x,f13.8))') '          Laenge: ', xl(1), xl(3)
  WRITE(*,*) area
  WRITE(*,'(a18,2(1x,f13.8))') '    Wassertiefen: ', h1, h3
  WRITE(*,'(a18,2(1x,f13.8))') '      Schl-kurve: ', qh1, qh3
  WRITE(*,'(a18,2(1x,f13.8))') '    akt. Abfluss: ', vel_res(1)*ah3, vel_res(3)*ah3
  WRITE(*,'(a18,2(1x,f13.8))') '    Fliessgeschw: ', vel_res(1), vel_res(3)
  WRITE(*,'(a18,2(1x,f13.8))') '   Fliessflaeche: ', ah1, ah3
  WRITE(*,'(a18,2(1x,f13.8))') '    Reibgefaelle: ', sfnod(1), sfnod(2)
  WRITE(*,'(a18,2(1x,f13.8))') '    Wichtung    : ', sfwicht(1), sfwicht(2)

  WRITE(*,'(a18,2(1x,f13.8))') '           dahdh: ', dahdh1, dahdh3
  WRITE(*,'(a18,2(1x,f13.8))') '         d2ahdh2: ', d2ahdh(1), d2ahdh(2)
  WRITE(*,'(a18,2(1x,f13.8))') '         dQSchdh: ', dqhdh(1), dqhdh(2)
  WRITE(*,'(a18,2(1x,f13.8))') '       d2QSchdh2: ', d2qhdh(1), d2qhdh(2)
  WRITE(*,*)                   '******************'
  WRITE(*,'(a18,4(1x,i4))'  )  '     Gausspunkte: ', (i,i = 1, 4)
  WRITE(*,*)                   '******************'
  WRITE(*,'(a18,4(1x,f13.8))') '           hhint: ', (hhint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dhhintdx: ', (dhhintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         areaint: ', (areaint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '      dareaintdh: ', (dareaintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '     d2areaintdh: ', (d2areaintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         daintdx: ', (daintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        d2aintdx: ', (d2aintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '      d2aintdhdx: ', (d2aidhdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         qschint: ', (qschint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdh: ', (dqsintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         d2qsidh: ', (d2qsidh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdx: ', (dqsintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        s0schint: ', (s0schint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '           sfint: ', (sfint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '       dsfintdh1: ', (dsfintdh1(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        vflowint: ', (vflowint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         dvintdx: ', (dvintdx(i), i = 1, 4)
  pause
end subroutine

