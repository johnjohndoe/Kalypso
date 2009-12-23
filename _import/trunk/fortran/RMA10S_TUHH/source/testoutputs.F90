!     Last change:  NIS  22 May 2008    3:31 am
!subroutines for testoutput from coef1DfE subroutine

subroutine MomOvVel (daintdt, areaint, vflowint, dbeiintdx, beiint, daintdx, dvintdx, grav, sfint, icyc, sidft)
  implicit none
  REAL (KIND = 8), INTENT (IN) :: areaint, daintdt, daintdx
  REAL (KIND = 8), INTENT (IN) :: vflowint, dvintdx
  REAL (KIND = 8), INTENT (IN) :: beiint, dbeiintdx
  REAL (KIND = 8), INTENT (IN) :: grav, sfint, sidft
  INTEGER, INTENT (IN) :: icyc

  WRITE(*,*) 'Momentum over velocity'
  WRITE(*,*) 'Direktterme FEEAN'

  if (icyc > 0) then
    WRITE(*,*) 'Term A1: ', + daintdt
  end if

  WRITE(*,*) ' Term B: ', + 2.0 * areaint* vflowint * dbeiintdx
  WRITE(*,*) ' Term C: ', + 2.0 * vflowint * beiint * daintdx
  WRITE(*,*) ' Term D: ', + 2.0 * beiint * areaint * dvintdx

  WRITE(*,*) ' Term F: ', + 2.0 * grav * sfint * areaint / vflowint
  WRITE(*,*) ' Term H: ', + sidft
  WRITE(*,*) 'Ableitungsterme FEEBN'
  WRITE(*,*) ' Term D: ', + 2.0 * beiint * areaint * vflowint
  pause
end subroutine

subroutine MomOvDep (vflowint, dvintdt, dvintdx, hhint, dhintdt, dhhintdx, beiint, dbeiintdh, &
         & d2beiintdhdx, areaint, daintdx, dareaintdh, d2areaintdh, d2aidhdx, sfint, dsfintdh1, yps, &
         & dypsdx, dypsdh, d2ypsdhdx, zsint, dzsintdh, grav, sbot, icyc, byparts)
  implicit none
  REAL (KIND = 8), INTENT (IN) :: vflowint, dvintdt, dvintdx
  real (kind = 8), INTENT (IN) :: hhint, dhintdt, dhhintdx
  real (kind = 8), INTENT (IN) :: beiint, dbeiintdh, d2beiintdhdx
  real (kind = 8), INTENT (IN) :: areaint, daintdx, dareaintdh, d2areaintdh, d2aidhdx
  real (kind = 8), INTENT (IN) :: sfint, dsfintdh1
  real (kind = 8), INTENT (IN) :: yps, dypsdx, dypsdh, d2ypsdhdx
  real (kind = 8), INTENT (IN) :: zsint, dzsintdh
  real (kind = 8), INTENT (IN) :: grav, sbot
  integer, INTENT (IN) :: icyc, byparts

  WRITE(*,*) 'Momentum over depth'
  WRITE(*,*) 'Direktterme FEEAN'

  if (icyc > 0) then
    WRITE(*,*) 'Term A1: ', + dareaintdh * dvintdt
    WRITE(*,*) 'Term A2: ', + dareaintdh * dhintdt
  end if

  WRITE(*,*) ' Term B: ', + vflowint**2 * (dareaintdh * dbeiintdh + areaint * d2beiintdhdx)
  WRITE(*,*) ' Term C: ', + vflowint**2 * (dbeiintdh * daintdx + beiint * d2aidhdx)
  WRITE(*,*) ' Term D: ', + 2.0 * vflowint * dvintdx * (beiint * dareaintdh + areaint * dbeiintdh)

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

subroutine Mom (nn, i, icyc, byparts, areaint, daintdt, daintdx, dareaintdh, vflowint, dvintdt, &
       & dvintdx, hhint, dhhintdx, beiint, dbeiintdx, zsint, yps, dypsdx, sfint, sbot, xn, &
       & dnx, frn, frnx, ams, grav, sidft)


  implicit none
  INTEGER, INTENT (IN) :: nn, i, icyc, byparts
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
    WRITE(*,*) 'Term A1: ', + areaint * dvintdt
    WRITE(*,*) 'Term A2: ', + vflowint * daintdt
  end if

  WRITE(*,*) ' Term B: ', + vflowint**2 * areaint * dbeiintdx
  WRITE(*,*) ' Term C: ', + beiint * vflowint**2 * daintdx
  WRITE(*,*) ' Term D: ', + 2 * beiint * vflowint * areaint * dvintdx

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

subroutine ElementResult (nn, nop1, nop3, h1, h3, sbot, xl, area, qh1, qh3, vel_res, ah1, ah3, sfnod, &
           &  dahdh1, dahdh3, d2ahdh, dqhdh, d2qhdh, hhint, dhhintdx, areaint, dareaintdh, d2areaintdh, daintdx, d2aintdx, &
           &  d2aidhdx, qschint, dqsintdh, d2qsidh, dqsintdx, s0schint, sfint, dsfintdh1, vflowint, dvintdx)


  INTEGER, INTENT (IN) :: nop1, nop3, nn
  REAL (KIND = 8), INTENT (IN) :: sbot, xl (3), area
  real (kind = 8), intent (IN) :: qh1, qh3, ah1, ah3, dahdh1, dahdh3, vel_res (3), h1, h3
  REAL (KIND = 8), INTENT (IN), dimension (1:2) :: sfnod, d2ahdh, dqhdh, d2qhdh
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: hhint, dhhintdx, areaint, dareaintdh, d2areaintdh, daintdx, d2aintdx, d2aidhdx
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: qschint, dqsintdh, d2qsidh, dqsintdx
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: s0schint, sfint, dsfintdh1
  REAL (KIND = 8), INTENT (IN), dimension (1:4) :: vflowint, dvintdx
  INTEGER :: i, n1, n3

  n3 = nop3
  n1 = nop1
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



subroutine Write2DMatrix(nbc, nop, estifm, f, maxp, maxe, nn, ncn, nfix)

implicit none

!global copies
!*************
!nbc    : global equation number
!nop    : array of nodes belonging to an element
!f      : residual vector of element
!estfim : 'element stiffnes matrix' of an element
!nn     : element number
!ncn    : number of corner nodes
!maxp   : maximum node ID-number in the mesh
!maxe   : maximum element ID-number in the mesh
INTEGER         :: nbc (1:maxp, 1:6), nfix(*)
integer (kind = 4) :: nop (1: maxe, 1: 8)
INTEGER         :: nn, ncn, maxp, maxe
REAL (KIND = 8) :: f (80), estifm (80, 80)

!local variables
!***************
!dca                  : counter of active degrees of freedom in element
!nbct                 : means local nbc-array
! nbct, first column  : local node number (position in nop-array)
! nbct, second column : position of equation in nbc-array
!sort                 : type of equation (momentum, continuity, sediment)
!FMT1, FMT2           : Format-descriptor placeholders for data written to the file
!i, j, k              : counter for loops; no special meaning
INTEGER              :: dca
INTEGER              :: nbct (1:32,1:2)
INTEGER              :: i, j, k
integer :: status
!logical :: unitExists, unitHasName, unitIsOpen
!character (len = 1000) :: unitName
CHARACTER (LEN =  1) :: sort(1:32)
CHARACTER (LEN = 16) :: FMT1
CHARACTER (LEN = 42) :: FMT2

!active degreecount  
  dca = 0
!active positions  
  do i = 1, 32
    nbct(i,1) = 0
    nbct(i,2) = 0
    sort(i) = 'N'
  end do

  do i = 1, ncn
    do j = 1, 4
      if (nbc(nop(nn,i), j) /= 0) then
        dca = dca + 1
        if (j <=2) then
          sort(dca) = 'I'
        ELSEIF (j == 3) then
          sort(dca) = 'C'
        else
          sort(dca) = 'S'
        endif
        nbct (dca,1) = i
        nbct (dca,2) = j
      endif
    end do
  end do

  WRITE(FMT1, '(a5,i2.2,a9)') '(33x,', dca, '(1x,i10))'
  write(FMT2, '(a18,i2.2,a22)') '(a1,i1,a2,2(1x,i8),', dca+1, '(1x,f10.2),2(1x,i10))'
!  inquire (unit = 9919, exist = unitExists)
!  if (unitExists) then
!    inquire (unit = 9919, opened = unitIsOpen)
!    inquire (unit = 9919, named = unitHasName)
!    if (unitHasName) then
!      inquire (unit = 9919, name = unitName)
!    endif
!    write (*,*) 'unitIsOpen: ', unitIsOpen
!    write (*,*) 'named:      ', unitHasName
!    write (*,*) 'name:       ', unitName
!  endif

   WRITE(9919,*) 'Element ', nn, 'coef2 t'
   WRITE(9919, FMT1) ( nbc (nop(nn, nbct(j,1)), nbct(j,2)), j=1, dca)
   DO i = 1, dca
     k = (nbct(i,1) - 1) * 4 + nbct(i,2)
     WRITE(9919, FMT2, iostat = status) sort(i), nbct(i,1), ': ', nbc( nop(nn, nbct(i,1)), nbct(i,2)), nop(nn, nbct(i,1)), f(k), &
     & (estifm(k, (nbct(j,1) - 1) * 4 + nbct(j,2)), j=1, dca), nbc( nop(nn, nbct(i,1)), nbct(i,2)), nfix(nop(nn,nbct(i,1)))
     if (status /= 0) then
       continue
     endif
   ENDDO
   WRITE(9919,*)
   WRITE(9919,*)
 end subroutine

