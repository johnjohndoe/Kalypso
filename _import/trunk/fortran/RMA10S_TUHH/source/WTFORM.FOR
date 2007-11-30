      SUBROUTINE WTFORM(Q,NCTR,H1,H2)
      USE CSVAR

!      CHARACTER (LEN = 25) :: filename

      FindRow: DO K=1,NROWCS(NCTR)
        NRHI=K
        IF(H1 < HRW(NCTR,K)) EXIT FindRow
      ENDDO FindRow

!      WRITE(*,*) nctr
!      WRITE(*,*) k, hrw(nctr, k), hrw(nctr, k+1)

      WTR=(H1-HRW(NCTR,NRHI-1))/(HRW(NCTR,NRHI)-HRW(NCTR,NRHI-1))  

      FindColumn: DO K=1,NCOLCS(NCTR)
        NCHI=K
        IF(H2 < HCL(NCTR,K)) EXIT FindColumn
      ENDDO FindColumn

!      !WRITE(*,*) k, hcl(nctr, k), hcl(nctr, k+1)
!
!      write (filename, '(a11,i2.2,a4)')'testbauwerk', NCTR, '.asc'
!      open (12345, filename)
!      WRITE(12345,*) 'NCOLS', ncolcs(nctr)
!      WRITE(12345,*) 'NROWS', nrowcs(nctr)
!      WRITE(12345,*) 'XLLCORNER 0.0'
!      WRITE(12345,*) 'YLLCORNER 0.0'
!      WRITE(12345,*) 'CELLSIZE 1'
!      WRITE(12345,*) 'NODATA_VALUE -999.999'
!
!      do j = 1, nrowcs(nctr)
!        WRITE(12345, *)
!     +       (flwcs(nctr, j, k), k=1, ncolcs(nctr))
!      enddo
!      close (12345, status = 'keep')
!      !pause

      WTC=(H2-HCL(NCTR,NCHI-1))/(HCL(NCTR,NCHI)-HCL(NCTR,NCHI-1))
    
      QRL=(FLWCS(NCTR,NRHI,NCHI-1)-FLWCS(NCTR,NRHI-1,NCHI-1))*WTR
     +    +FLWCS(NCTR,NRHI-1,NCHI-1)
      QRH=(FLWCS(NCTR,NRHI,NCHI)-FLWCS(NCTR,NRHI-1,NCHI))*WTR
     +    +FLWCS(NCTR,NRHI-1,NCHI)
	Q=(QRH-QRL)*WTC+QRL
	
	Diff = ABS(H1-H2)
	IF (Diff.gt.0.02) Then
	dummy = 0
	End If


	RETURN
	END