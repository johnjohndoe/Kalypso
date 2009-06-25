C     Last change:  WP    7 Feb 2008    3:42 pm
      SUBROUTINE WTFORM(Q, NCTR, HOW, HUW)
      USE CSVAR

!      CHARACTER (LEN = 25) :: filename

      !catch negative slope
      if (HOW == HUW) then
        Q = 0
        return
      ELSEIF (HOW < HUW) then
        Q = 0
        !TODO:
        !Throw warning
      end if

      FindRow: DO K=1,NROWCS(NCTR)
        NRHI=K
        IF(HOW < HRW(NCTR,K)) EXIT FindRow
      ENDDO FindRow

      !testing
      WRITE (*,*) nrowcs(nctr), nctr
      open (1331, 'bauwerksdaten.txt')
      WRITE(1331,*) 'HWerte', (hcl(nctr,i),i=1,50)
      do i = 1, nrowcs(nctr)
        WRITE(1331,*) hrw(nctr,i), (flwcs (nctr,i,j),j=1,50)
      end do
      close (1331, status = 'keep')
      !testing-ende

      WTR=(HOW-HRW(NCTR,NRHI-1))/(HRW(NCTR,NRHI)-HRW(NCTR,NRHI-1))

      FindColumn: DO K=1,NCOLCS(NCTR)
        NCHI=K
        IF(HUW < HCL(NCTR,K)) EXIT FindColumn
      ENDDO FindColumn

      WTC=(HUW-HCL(NCTR,NCHI-1))/(HCL(NCTR,NCHI)-HCL(NCTR,NCHI-1))


      !nis,jan08: Calculate the single values and interpolate later
      !      |  HCL-1  HUW  HCL
      !-------------------------------
      !      |
      !HRW-1 |   Q11       Q12
      !      | (1)|   (3)   |(2)
      !HOW   |   QRL - Q - QRH
      !      | (1)|         |(2)
      !HRW   |   Q21       Q22
      !      |

      !get tabular values

      !The following values shouldn't be problematic, otherwise the calculation will not work
      Q11 = FLWCS (NCTR, NRHI - 1, NCHI - 1)
      Q22 = FLWCS (NCTR, NRHI, NCHI)

      Q21 = FLWCS (NCTR, NRHI, NCHI - 1)
      if (Q21 < -999.0) then
        Q = Q11 + WTR * (Q22 - Q11)
        return
      end if

      Q12 = FLWCS (NCTR, NRHI - 1, NCHI)
      if (Q12 < -999.0) Q12 = 0


      !interpolate between the rows
      QRL = (Q21 - Q11) * WTR + Q11
      QRH = (Q22 - Q12) * WTR + Q12

      !catch problems



      !interpolate in general
      Q = (QRH - QRL) * WTC + QRL


      !power for simple IDW-method
      power = 1.0d0

      if (UseEnergyCstrc == 1) then
        lowerBoundUW = MAX (NCHI - 1 - 5, 1)
        upperBoundUW = MIN (NCHI - 1 + 5, UBOUND (FLWCS, 3))
        lowerBoundOW = MAX (NRHI - 5, 1)
        upperBoundOW = MIN (NRHI - 5, UBOUND (flwcs, 2))

        do i = lowerBoundUW, upperBoundUW
          j = lowerBoundOW, upperBoundOW
            HUW_tab = HCL (nctr, i)
            how_tab = HRW (nctr, j)
            if (huw_tab < how_tab) then
              Q = flwcs (nctr, j, i)
            else
              huw_tab = how_tab
              Q = 0
            end if
            Weight = 1/ (SQRT ((HUW - huw_tab)**2 + (HOW - how_tab)**2)** power
          enddo
        enddo

      end if




      Diff = ABS(HOW-HUW)
      IF (Diff.gt.0.02) Then
      dummy = 0
      End If


      RETURN
      END