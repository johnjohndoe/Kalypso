C     Last change:  WP    1 Feb 2008    6:31 pm
      SUBROUTINE WTFORM(Q,NCTR,H1,H2)
      USE CSVAR

!      CHARACTER (LEN = 25) :: filename

      !catch negative slope
      if (h1 == h2) then
        Q = 0
        return
      ELSEIF (h1 < h2) then
        Q = 0
        !TODO:
        !Throw warning
      end if

      FindRow: DO K=1,NROWCS(NCTR)
        NRHI=K
        IF(H1 < HRW(NCTR,K)) EXIT FindRow
      ENDDO FindRow

      !testing
      !WRITE (*,*) nrowcs(nctr), nctr
      !open (1331, 'bauwerksdaten.txt')
      !WRITE(1331,*) 'HWerte', (hcl(nctr,i),i=1,250)
      !do i = 1, nrowcs(nctr)
      !  WRITE(1331,*) hrw(nctr,i), (flwcs (nctr,i,j),j=1,250)
      !end do
      !close (1331, status = 'keep')
      !testing-ende

      WTR=(H1-HRW(NCTR,NRHI-1))/(HRW(NCTR,NRHI)-HRW(NCTR,NRHI-1))  

      FindColumn: DO K=1,NCOLCS(NCTR)
        NCHI=K
        IF(H2 < HCL(NCTR,K)) EXIT FindColumn
      ENDDO FindColumn

      WTC=(H2-HCL(NCTR,NCHI-1))/(HCL(NCTR,NCHI)-HCL(NCTR,NCHI-1))


      !nis,jan08: Calculate the single values and interpolate later
      !      |  HCL-1  H2   HCL
      !-------------------------------
      !      |
      !HRW-1 |   Q11       Q12
      !      | (1)|   (3)   |(2)
      !H1    |   QRL - Q - QRH
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




      Diff = ABS(H1-H2)
      IF (Diff.gt.0.02) Then
      dummy = 0
      End If


      RETURN
      END