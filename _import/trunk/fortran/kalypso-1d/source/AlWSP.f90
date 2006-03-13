!     Last change:  WP   11 Mar 2006    8:16 pm
!--------------------------------------------------------------------------
! This code, AlWSP.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - alg1
! - algeb
! - mengf
! - inpuf
! - ovrla
! - flaec
! - konve
! - zuord
! - winha
! - kreuf
! - zwpol
! - extre
! - normi
! - imron
! - ergen
! - prinf
! - dainf
! - inpul
! - kreul
! - mengl
! - punla
! - verle
! - mengen
! - check
! - vergl
! - dista
! - posit
! - herei
! - powin
! - schni
! - punkt
! - verfl
! - invep
! - inver
! - strec
! - initi
! - doppe
! - prinl
! - unkon
! - einge
! - erstp
! - epunk
! - einfu
! - vorko
!
! Functions:
! - nsgn
! - phi
!
! Copyright (C) 2004  ULF TESCHKE & WOLF PLOEGER.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! Ulf Teschke:      phone: +49 40 42878 3895 mail: teschke@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 August 2004
! Research Associate
!***********************************************************************


SUBROUTINE alg1 (igraf, ifehl)

USE DIM_VARIABLEN
USE IO_UNITS

! mpts = 200 -> entspricht max!
! mic = 10 -> entspricht in einigen Subroutinen max
! min wrid durch max ersetzt, max durch mpts

COMMON / angabe / xko (mpts, 2), yko (mpts, 2), na1, na2
COMMON / xr0yr0 / xr0 (mpts, max2), yr0 (mpts, max2), mr0 (max2), kr0
COMMON / xr1yr1 / xr1 (mpts, max2), yr1 (mpts, max2), mr1 (max2), kr1
COMMON / xr2yr2 / xr2 (mpts, max2), yr2 (mpts, max2), mr2 (max2), kr2

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)


! Uebergabe :
n (0, 1) = na1
n (0, 2) = na2
DO k = 1, 2
  DO i = 1, n (0, k)
    x (i, 0, k) = xko (i, k)
    y (i, 0, k) = yko (i, k)
  END DO
END DO


ifehl = 0
medu = 0
info = 0
ipru = 1
korr = 1
iboo = 0
CALL mengen (ifehl, medu, info, ipru, korr, iboo)

IF (ifehl.gt.0) then

  WRITE (UNIT_OUT_LOG, '(a)') 'Es gibt Fehler in MENGEN'

  RETURN
ENDIF

!     Zurueckgabe :
!               -----> Schnittmengen
kr0 = n3 (1)
IF (kr0.gt.max2) goto 6

DO j = 1, kr0
  mr0 (j) = ns (j, 1)
  DO i = 1, mr0 (j)
    xr0 (i, j) = xs (i, j, 1)
    yr0 (i, j) = ys (i, j, 1)
  END DO
END DO

!               -----> 1.Ergaenzungsmenge
kr1 = n3 (2)
IF (kr1.gt.max2) goto 6

DO j = 1, kr1
  mr1 (j) = ns (j, 2)
  DO i = 1, mr1 (j)
    xr1 (i, j) = xs (i, j, 2)
    yr1 (i, j) = ys (i, j, 2)
  END DO
END DO

!               -----> 2.Ergaenzungsmenge
kr2 = n3 (3)
IF (kr2.gt.max2) goto 6

DO j = 1, kr2
  mr2 (j) = ns (j, 3)
  DO i = 1, mr2 (j)
    xr2 (i, j) = xs (i, j, 3)
    yr2 (i, j) = ys (i, j, 3)
  END DO
END DO

RETURN

6 CONTINUE

WRITE (0, '(a)') 'Parameter max2 ist zu klein'
ifehl = 1
RETURN

END SUBROUTINE alg1                                                                          
                                                                        
                                                                        



! -----------------------------------------------------------------------------------------
SUBROUTINE algeb (ib, ifehl, medu, info)
!                          ib=0 : Schnitmengen                          
!                          ib=1 : 1.Ergenzungsmenge                     
!                          ib=2 : 2.Ergenzungsmenge                     
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

CALL konve (1, ib, ifehl, medu, info)

IF (info.eq.1) then
  k = 1
  PRINT * , 'Gebiet :', k
  ff = 0
  DO j = 1, m (k)
    PRINT * , ' '
    PRINT * , j, '. konvexes Polygon'
    WRITE ( * , '(i5,2f10.3)') (i, x (i, j, k) , y (i, j, k) , i =0, n (j, k) )
    CALL flaec (1, n (j, k), f, j, k)
    ff = ff + f

  END DO

ENDIF

!      write(0,'(a3,f10.6)') 'ff=',ff
IF (ifehl.eq.1) return

CALL konve (2, ib, ifehl, medu, info)
IF (info.eq.1) then
  k = 2
  ff = 0
  PRINT * , 'Gebiet :', k
  DO j = 1, m (k)
    PRINT * , ' '
    PRINT * , j, '. konvexes Polygon'
    WRITE ( * , '(i5,2f10.3)') (i, x (i, j, k) , y (i, j, k) , i =0, n (j, k) )
    CALL flaec (1, n (j, k), f, j, k)
    ff = ff + f
  END DO
ENDIF
!      write(0,'(a3,f10.6)') 'ff=',ff

IF (ifehl.eq.1) return

CALL mengf (ib + 1, medu, info, ifehl)

END SUBROUTINE algeb                                                            
                                                                        





!----------------------------------------------------------------------------------------
SUBROUTINE mengf (jj, medu, info, ifehl)
!                           j=1        : Schnittmengen                  
!                           j=2 oder 3 : Ergenzungsmengen               
USE DIM_VARIABLEN
COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

CHARACTER(LEN=3) :: typ

!WP 25.05.2005
!write (*,1000) jj, medu, info
!1000 format (1X, 'In MENGF!', /, &
!           & 1X, 'jj   = ', I5, /, &
!           & 1X, 'medu = ', I5, /, &
!           & 1X, 'info = ', I5)
                                                                        
!info = 1
!WP 25.05.2005

ifehl = 0

IF (jj.eq.1) j = 1
IF (jj.eq.2) j = 3
IF (jj.eq.3) j = 2

IF (jj.eq.1) then
  IF (info.eq.1) print * , ' '
IF (info.eq.1) print  * , 'Schnittmenge      :'
  typ = 's81'
  lay = 301
ENDIF
IF (jj.eq.2) then
  IF (info.eq.1) print * , ' '
  IF (info.eq.1) print * , 'Ergenzungsmenge 1 :'
  typ = 's51'
  lay = 302
ENDIF
IF (jj.eq.3) then
  IF (info.eq.1) print * , ' '
  IF (info.eq.1) print * , 'Ergenzungsmenge 2 :'
  typ = 's77'
  lay = 303
ENDIF
IF (info.eq.1) print  * , 'Layer :', lay, ' Linientyp :  ', typ

n3 (j) = 0
ii = 0
fff = 0.0
DO 1 i1 = 1, m (1)
  DO 2 i2 = 1, m (2)
    n3 (j) = n3 (j) + 1
    IF (n3 (j) .gt.min2) then
      PRINT * , 'Im MENGF ist der Parameter min2 zu klein'
      ifehl = 1
      RETURN
    ENDIF
    CALL ovrla ( n(i1,1)-1, x(0,i1,1), y(0,i1,1), n(i2,2)-1, x(0,i2,2), y(0,i2,2), isec, j, 1  , fpc)
    !SUBROUTINE ovrla ( na, xpa,       ypa,       nb,        xpb,       ypb,       isec, k, ior, fpc)

    IF (isec.eq.0) then
      ii = ii + 1
      n3 (j) = n3 (j) - 1
      GOTO 2
    ENDIF
    fff = fff + fpc
  2 END DO
1 END DO

CALL epunk (j, ifehl)
IF (ifehl.eq.1) return
CALL unkon (j, ifehl)
IF (ifehl.eq.1) return


DO k = 1, n3 (j)

  CALL imron (k, j)
  IF (info.eq.1) then
    WRITE (*  , * ) '----------- Flache: ', fs (k, j)
    WRITE ( * , '(i5,2f20.6)') (i, xs (i, k, j) , ys (i, k, j) , i = 0, ns (k, j) )
  ENDIF

END DO

END SUBROUTINE mengf
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE inpuf (k)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

m (k) = 1

DO i = 1, n (0, k)
  x (i, m (k), k) = x (i, 0, k)
  y (i, m (k), k) = y (i, 0, k)
END DO

n (m (k), k) = n (0, k)
x (0, m (k), k) = x (n (m (k), k) - 1, 0, k)
y (0, m (k), k) = y (n (m (k), k) - 1, 0, k)

RETURN 

END SUBROUTINE inpuf                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE ovrla (na, xpa, ypa, nb, xpb, ypb, isec, k, ior, fpc)
! Das Programm findet Schnittmenge C zwischen zwei konvexen Polygonen   
! A, B und C :  [xpa(1),ypa(1)...xpa(na),ypa(na)]                       
!               [xpb(1),ypb(1)...xpb(nb),ypb(nb)]                       
!               [xpc(1),ypc(1)...xpc(nc),ypc(nc)]                       
! ior : Orientierungsparameter [ ior=+1, gegen den Uhrzeigersinn]       
!                              [ ior=-1, im Uhrzeigersinn]              
! isec = 1, dann existiert eine Schnittmenge                            
! isec = 0, dann existiert keine Schnittmenge                           
! max2 : max2imale Vektorlaenge                                           
USE DIM_VARIABLEN
DIMENSION xpa (mpts), ypa (mpts), xpb (mpts), ypb (mpts)
DIMENSION xf (2, mpts), yf (2, mpts)

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

l1 = 1
nc = na

DO i = 1, nc
  xf (l1, i) = xpa (i)
  yf (l1, i) = ypa (i)
END DO

x1 = xpb (nb)
y1 = ypb (nb)

DO i = 1, nb

  l2 = 3 - l1
  x2 = xpb (i)
  y2 = ypb (i)
  ca = x2 - x1
  cb = y1 - y2
  cc = - x1 * cb - y1 * ca
  xi1 = xf (l1, nc)
  yi1 = yf (l1, nc)
  vi1 = ca * yi1 + cb * xi1 + cc
  a1 = abs (vi1)

  IF (a1.lt.1.0e-06) then
    i1 = 0
  ELSE
    i1 = nsgn (vi1) * ior
  ENDIF

  ncd = 0

  DO j = 1, nc

    xi2 = xf (l1, j)
    yi2 = yf (l1, j)
    vi2 = ca * yi2 + cb * xi2 + cc
    a2 = abs (vi2)
    IF (a2.lt.1.0e-06) then
      i2 = 0
    ELSE
      i2 = nsgn (vi2) * ior
    ENDIF

    IF (i1.ge.0) then
      ncd = ncd+1

      xf (l2, ncd) = xi1
      yf (l2, ncd) = yi1
    ENDIF

    IF (i1.ne.0.and.i1.ne.i2.and.i2.ne.0) then
      delta = a1 + a2
      ncd = ncd+1
      xf (l2, ncd) = (a2 * xi1 + a1 * xi2) / delta
      yf (l2, ncd) = (a2 * yi1 + a1 * yi2) / delta
    ENDIF
    vi1 = vi2
    i1 = i2
    a1 = a2
    xi1 = xi2
    yi1 = yi2

  END DO

  IF (ncd.lt.3) then
    isec = 0
    RETURN
  ENDIF

  nc = ncd
  l1 = l2

  x1 = x2
  y1 = y2

END DO

! ------------------------- FLAECHE
fpc = 0.
xf (l1, nc + 1) = xf (l1, 1)
yf (l1, nc + 1) = yf (l1, 1)
DO i = 1, nc
  fpc = fpc + (xf (l1, i + 1) - xf (l1, i) ) * (yf (l1, i + 1) + yf (l1, i) )
END DO

fpc = fpc / 2.

IF (abs (fpc) .lt.1.0e-06) then
  isec = 0
  RETURN
ENDIF
!--------------------------

isec = 1
fs (n3 (k), k) = fpc

DO i = 1, nc
  xs (i, n3 (k), k) = xf (l1, i)
  ys (i, n3 (k), k) = yf (l1, i)
END DO

nc = nc + 1
ns (n3 (k), k) = nc
xs (nc, n3 (k), k) = xs (1, n3 (k), k)
ys (nc, n3 (k), k) = ys (1, n3 (k), k)
xs (0, n3 (k), k) = xs (nc - 1, n3 (k), k)
ys (0, n3 (k), k) = ys (nc - 1, n3 (k), k)

END SUBROUTINE ovrla                                                            
                                                                        




!----------------------------------------------------------------------------------------
FUNCTION nsgn (x)
!     1 if x>0                                                          
!     0 if x=0                                                          
!    -1 if x<0                                                          

nsgn = 0 

IF (x.gt.1.0e-06) then 
  nsgn = + 1
ELSEIF (x.lt. - 1.0e-06) then
  nsgn = - 1
ENDIF

END FUNCTION nsgn
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE flaec (n1, n2, f, j, k)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

f = 0.

DO i = n1, n2 - 1
  f = f + (x (i + 1, j, k) - x (i, j, k) ) * (y (i + 1, j, k) + y (i, j, k) )
END DO

f = f / 2.

END SUBROUTINE flaec
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE konve (k, im, ifehl, medu, info)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      ifehl = 0 
!      write(0,'(a8,i5)') 'Gebiet k=',k                                 
      CALL inpuf (k) 
                                                                        
      CALL normi (k, 0) 
      CALL flaec (1, n (1, k), f, 1, k) 
      CALL inver (f, 1, k) 
                                                                        
      IF (im.eq.k) then 
!                                Ergenzungsmenge                        
        CALL ergen (k, ifehl, info) 
        IF (ifehl.eq.1) return 
        CALL flaec (1, n (1, k), f, 1, k) 
        CALL inver (f, 1, k) 
!                               KORREGIERT                              
                                                                        
                                                                        
      ENDIF 
!            write(0,'(a2,f30.6)') 'f=',f                               
      IF (abs (f) .lt.1.0e-06) then 
        ifehl = 1 
        RETURN 
      ENDIF 
                                                                        
!              Grenze ist gegen den Uhrzeigersinn orientiert            
                                                                        
      CALL vorko (k) 
                                                                        
                                                                        
      mal1 = 0 
    4 CONTINUE 
      mal1 = mal1 + 1 
      IF (mal1.gt.1000) then 
        ifehl = 1 
        PRINT * , 'mal1 ist zu gross' 
        RETURN 
      ENDIF 
      m0 = m (k) 
      mal2 = 0 
      DO 3 j = 1, m (k) 
        iw = 1 
    7   CONTINUE 
        mal2 = mal2 + 1 
        IF (mal2 .gt. min2) then 
!      write(0,'(a11,4i7)') 'k,im,j,m(k)',k,im,j,m(k)                   
          WRITE (0, '(a)') 'mal2 ist zu gross' 
          WRITE ( * , '(a)') 'mal2 ist zu gross' 
          RETURN 
        ENDIF 
                                                                        
        CALL zuord (ex, ey, j, iaus, k, ifehl, iw, ii) 
        IF (ifehl.eq.1) return 
!      write(0,'(a3,i5)') 'ii=',ii                                      
        IF (iaus.eq.0) goto 3 
        p = 1.0e+10 
        jn = 0 
        DO 1 i = 2, n (j, k) - 2 
          x1 = x (i, j, k) 
          y1 = y (i, j, k) 
          x2 = x (i + 1, j, k) 
          y2 = y (i + 1, j, k) 
          x3 = x (1, j, k) 
          y3 = y (1, j, k) 
          u = - 1.0 
          v = - 1.0 
          CALL kreuf (x1, y1, x2, y2, x3, y3, ex, ey, u, v, in) 
          ins = 0 
                                                                        
          IF (in.eq.0) goto 1 
                                                                        
          CALL doppe (i1, i2, i3, i4, j, k, ins) 
          IF (ins.eq.1) then 
            x4 = x (i1, j, k) 
            y4 = y (i1, j, k) 
            x5 = x (i2, j, k) 
            y5 = y (i2, j, k) 
            CALL kreul (x4, y4, x5, y5, x3, y3, u, v, a, b, is, 1.0e-06,&
            info)                                                       
                                                                        
            IF (is.ne.0) goto 1 
            x4 = x (i3, j, k) 
            y4 = y (i3, j, k) 
            x5 = x (i4, j, k) 
            y5 = y (i4, j, k) 
            CALL kreul (x4, y4, x5, y5, x3, y3, u, v, a, b, is, 1.0e-06,&
            info)                                                       
                                                                        
            IF (is.ne.0) goto 1 
          ENDIF 
                                                                        
          s = sqrt ( (x (1, j, k) - u) **2 + (y (1, j, k) - v) **2) 
          IF (s.lt.p) then 
            p = s 
            w = u 
            z = v 
            l = i 
            jn = in 
            IF (p.le.1.0e-06) goto 2 
          ENDIF 
    1   END DO 
    2   CONTINUE 
        IF (jn.eq.0) then 
          iw = iw + 1 
          GOTO 7 
        ELSE 
          iw = 1 
        ENDIF 
                                                                        
        CALL zwpol (l, w, z, j, k) 
                                                                        
    3 END DO 
                                                                        
      IF (m (k) .gt.min2) then 
        PRINT * , 'Im KONVEX ist der Parameter min2'
        ifehl = 1
        RETURN 
      ENDIF 
      IF (m (k) .gt.m0) goto 4 
                                                                        
      RETURN 

END SUBROUTINE konve                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE zuord (ex, ey, j, iaus, k, ifehl, iw, ii)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)



      ifehl = 0 
      iaus = 1 
      DO 1 i = iw, n (j, k) - 1 
        x1 = x (i - 1, j, k) 
        y1 = y (i - 1, j, k) 
        x2 = x (i - 0, j, k) 
        y2 = y (i - 0, j, k) 
        x3 = x (i + 1, j, k) 
        y3 = y (i + 1, j, k) 
                                                                        
        IF (sqrt ( (x1 - x2) **2 + (y1 - y2) **2) .lt.1.0e-06) then 
!      write(0,'(a16,4i5)') ' 1 iw,j,n(j,k),i',iw,j,n(j,k),i            
          GOTO 1 
        ENDIF 
        IF (sqrt ( (x3 - x2) **2 + (y3 - y2) **2) .lt.1.0e-06) then 
!      write(0,'(a16,4i5)') ' 2 iw,j,n(j,k),i',iw,j,n(j,k),i            
          GOTO 1 
        ENDIF 
                                                                        
        CALL winha (x (i - 1, j, k), y (i - 1, j, k), x (i, j, k),      &
        y (i, j, k), x (i + 1, j, k), y (i + 1, j, k), ex, ey, ii,      &
        ifehl)                                                          
        IF (ifehl.eq.1) return 
        IF (ii.eq. - 1) goto 2 
    1 END DO 
      iaus = 0 
      RETURN 
    2 CONTINUE 
                                                                        
      IF (i.eq.1) return 
      DO 3 iz = 1, n (j, k) - i 
        DO 4 l = n (j, k), 2, - 1 
          x (l, j, k) = x (l - 1, j, k) 
          y (l, j, k) = y (l - 1, j, k) 
    4   END DO 
        x (1, j, k) = x (n (j, k), j, k) 
        y (1, j, k) = y (n (j, k), j, k) 
                                                                        
        x (0, j, k) = x (n (j, k) - 1, j, k) 
        y (0, j, k) = y (n (j, k) - 1, j, k) 
    3 END DO 
                                                                        
      RETURN 

END SUBROUTINE zuord                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE winha (x1, y1, x2, y2, x3, y3, ex, ey, i, ifehl)

ifehl = 0
                                                                        
      i = 0 
      e = x2 - x1 
      f = y2 - y1 
      ef = sqrt (e**2 + f**2) 
                                                                        
      IF (ef.lt.1.0e-06) then 
        PRINT * , 'ef=', ef 
!      write(0,'(a3,f10.7)') 'ef=',ef                                   
        PRINT *, x1, y1, x2, y2 
        ifehl = 1 
        RETURN 
      ENDIF 
                                                                        
      e = e / ef 
      f = f / ef 
      g = x3 - x2 
      h = y3 - y2 
      gh = sqrt (g**2 + h**2) 
      IF (gh.lt.1.0e-06) then 
        PRINT * , 'gh<<', gh 
!      write(0,'(a3,f10.7)') 'gh=',gh                                   
        ifehl = 1 
        RETURN 
      ENDIF 
                                                                        
      g = g / gh 
      h = h / gh 
      ex = g - e 
      ey = h - f 
      ee = sqrt (ex**2 + ey**2) 
      IF (ee.lt.1.0e-06) return 
      r = e * h - g * f 
      IF (r.lt.1.0e-06) i = - 1 
      ex = - ex 
      ey = - ey 
      RETURN 
END SUBROUTINE winha
                                                                        
                                                                        
!----------------------------------------------------------------------------------------
SUBROUTINE kreuf (x1, y1, x2, y2, xa, ya, ex, ey, u, v, in)
                                                                        
!     u=xa+r*ex                                                         
!     v=ya+r*ey                                                         
!     u=x1+(x2-x1)*R                                                    
!     v=y1+(y2-y1)*R                                                    
!     xa+r*ex=x1+(x2-x1)*R                                              
!     ya+r*ey=y1+(y2-y1)*R                                              
!     ex*r+(x1-x2)*R=x1-xa                                              
!     ey*r+(y1-y2)*R=y1-ya                                              
!     (ex*(y1-y2)-ey*(x1-x2))*r=(x1-xa)*(y1-y2)-(y1-ya)*(x1-x2)         
! -->               dl*r=dr                                             
!     Wenn dl=0, dann sind die Linien parallel                          
!     Wenn dr=0, dann sind die Linien kollinear                         
                                                                        
      in = 0 
      dl = ex * (y1 - y2) - ey * (x1 - x2) 
                                                                        
      IF (abs (dl) .lt.1.0e-06) return 
      dr = (x1 - xa) * (y1 - y2) - (y1 - ya) * (x1 - x2) 
      r = dr / dl 
      IF (r.lt.0.0e+00) return 
      u = xa + r * ex 
      v = ya + r * ey 
      sl = sqrt ( (x1 - u) **2 + (y1 - v) **2) 
      IF (sl.lt.1.0e-06) then 
        u = x1 
        v = y1 
        in = 1 
        RETURN 
      ENDIF 
                                                                        
      sr = sqrt ( (x2 - u) **2 + (y2 - v) **2) 
      IF (sr.lt.1.0e-06) then 
        u = x2 
        v = y2 
        in = 2 
        RETURN 
      ENDIF 
      ss = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
      IF (sl.lt.ss.and.sr.lt.ss) then 
        in = 3 
        RETURN 
      ENDIF 
                                                                        
      RETURN 
END SUBROUTINE kreuf
                                                                        



!----------------------------------------------------------------------------------------
SUBROUTINE zwpol (it, u, v, j, k)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      l = 0 
      m (k) = m (k) + 1 
      l = l + 1 
      x (l, m (k), k) = x (1, j, k) 
      y (l, m (k), k) = y (1, j, k) 
                                                                        
      p = sqrt ( (x (l, m (k), k) - u) **2 + (y (l, m (k), k) - v) **2) 
      IF (p.gt.1.0e-06) then 
        l = l + 1 
        x (l, m (k), k) = u 
        y (l, m (k), k) = v 
      ENDIF 
                                                                        
      DO 1 i = it + 1, n (j, k) 
        p = sqrt ( (x (i, j, k) - x (l, m (k), k) ) **2 + (y (i, j, k)  &
        - y (l, m (k), k) ) **2)                                        
        IF (p.gt.1.0e-06) then 
          l = l + 1 
          x (l, m (k), k) = x (i, j, k) 
          y (l, m (k), k) = y (i, j, k) 
        ENDIF 
    1 END DO 
      n (m (k), k) = l 
      x (0, m (k), k) = x (n (m (k), k) - 1, m (k), k) 
      y (0, m (k), k) = y (n (m (k), k) - 1, m (k), k) 
                                                                        
      l = it 
                                                                        
      p = sqrt ( (x (l, j, k) - u) **2 + (y (l, j, k) - v) **2) 
      IF (p.gt.1.0e-06) then 
        l = l + 1 
        x (l, j, k) = u 
        y (l, j, k) = v 
      ENDIF 
                                                                        
      p = sqrt ( (x (l, j, k) - x (n (j, k), j, k) ) **2 + (y (l, j, k) &
      - y (n (j, k), j, k) ) **2)                                       
      IF (p.gt.1.0e-06) then 
        l = l + 1 
        x (l, j, k) = x (n (j, k), j, k) 
        y (l, j, k) = y (n (j, k), j, k) 
      ENDIF 
                                                                        
      n (j, k) = l 
      x (0, j, k) = x (n (j, k) - 1, j, k) 
      y (0, j, k) = y (n (j, k) - 1, j, k) 
                                                                        
      RETURN 
      END SUBROUTINE zwpol                          


!----------------------------------------------------------------------------------------
SUBROUTINE extre (k, info)
! Diese Routine findet Extremwerte
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / ex / nm (2), xmn, ymn, xmx, ymx, rw, rv, rd

DIMENSION xmi (2), ymi (2), xma (2), yma (2) 
                                                                        
      DO 1 j = 1, k 
                                                                        
        xmi (j) = x (1, 0, j) 
        ymi (j) = y (1, 0, j) 
        xma (j) = x (1, 0, j) 
        yma (j) = y (1, 0, j) 
        nm (j) = 1 
        DO 1 i = 2, n (0, j) 
          IF (xmi (j) .gt.x (i, 0, j) ) then 
            xmi (j) = x (i, 0, j) 
            nm (j) = i 
          ENDIF 
          IF (ymi (j) .gt.y (i, 0, j) ) ymi (j) = y (i, 0, j) 
          IF (xma (j) .lt.x (i, 0, j) ) xma (j) = x (i, 0, j) 
          IF (yma (j) .lt.y (i, 0, j) ) yma (j) = y (i, 0, j) 
    1 CONTINUE 
                                                                        
      xmn = xmi (1) 
      ymn = ymi (1) 
      xmx = xma (1) 
      ymx = yma (1) 
                                                                        
      DO 2 j = 2, k 
        IF (xmn.gt.xmi (j) ) xmn = xmi (j) 
        IF (ymn.gt.ymi (j) ) ymn = ymi (j) 
        IF (xmx.lt.xma (j) ) xmx = xma (j) 
        IF (ymx.lt.yma (j) ) ymx = yma (j) 
    2 END DO 
!        KORREGIERT -1.0 +1.0 <muss !? sub=0>                           
      sub = 0.0 
      xmn = xmn - sub 
      ymn = ymn - sub 
      xmx = xmx + sub 
      ymx = ymx + sub 
      rd = sqrt ( (xmx - xmn) **2 + (ymx - ymn) **2) 
      rw = (xmx - xmn) / rd 
      rv = (ymx - ymn) / rd 
                                                                        
      IF (info.eq.1) then 
        PRINT * , ' ' 
        WRITE ( * , '(4(6x,a4))') 'xmin', 'ymin', 'xmax', 'ymax' 
                                                                        
        WRITE ( * , '(4f10.3)') xmn, ymn, xmx, ymx 
      ENDIF 
                                                                        
      RETURN 
END SUBROUTINE extre
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE normi (j, ip)
!     Diese Routine konstruiert eine Abbildung auf das Rechteck         
!                        [RW x RV]                                      
!     und numeriert die Dateien um (nach nm(2))                         
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / ex / nm (2), xmn, ymn, xmx, ymx, rw, rv, rd
                                                                        
      IF (ip.eq.0) then 
                                                                        
        DO 3 k = 1, nm (j) - 1 
          DO 2 i = 1, n (1, j) 
            x (i - 1, 1, j) = x (i, 1, j) 
            y (i - 1, 1, j) = y (i, 1, j) 
    2     END DO 
          x (n (1, j), 1, j) = x (1, 1, j) 
          y (n (1, j), 1, j) = y (1, 1, j) 
    3   END DO 
        x (0, 1, j) = x (n (1, j) - 1, 1, j) 
        y (0, 1, j) = y (n (1, j) - 1, 1, j) 
                                                                        
      ENDIF 
!                         ++ 01                                         
      DO 1 i = 0, n (1, j) 
        x (i, 1, j) = (x (i, 1, j) - xmn) / rd+0.01 
        y (i, 1, j) = (y (i, 1, j) - ymn) / rd+0.01 
    1 END DO 
      RETURN 
      END SUBROUTINE normi                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE imron (k, im)
! Diese Routine konstruiert eine Abbildung vom Quadrat (0;1)
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)
COMMON / ex / nm (2), xmn, ymn, xmx, ymx, rw, rv, rd

DO i = 0, ns (k, im)
  xs (i, k, im) = (xs (i, k, im) - 0.01) * rd+xmn
  ys (i, k, im) = (ys (i, k, im) - 0.01) * rd+ymn
END DO

fs (k, im) = fs (k, im) * rd * rd

END SUBROUTINE imron
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE ergen (j, ifehl, info)
! Diese Routine konstruiert Ergaenzungsmengen
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / ex / nm (2), xmn, ymn, xmx, ymx, rw, rv, rd


      ifehl = 0 
      n1 = n (1, j) 
                                                                        
      IF (abs (x (n1, 1, j) ) .gt.1.0e-06) then 
        n1 = n1 + 1 
        x (n1, 1, j) = 0.0 
        y (n1, 1, j) = y (n (1, j), 1, j) 
      ENDIF 
                                                                        
      n1 = n1 + 1 
      x (n1, 1, j) = 0.0 
      y (n1, 1, j) = rv + 0.02 
                                                                        
      n1 = n1 + 1 
      x (n1, 1, j) = rw + 0.02 
      y (n1, 1, j) = rv + 0.02 
                                                                        
      n1 = n1 + 1 
      x (n1, 1, j) = rw + 0.02 
      y (n1, 1, j) = 0.0 
                                                                        
      n1 = n1 + 1 
      x (n1, 1, j) = 0.0 
      y (n1, 1, j) = 0.0 
                                                                        
      n1 = n1 + 1 
      x (n1, 1, j) = 0.0 
      y (n1, 1, j) = y (n (1, j), 1, j) 
                                                                        
      IF (abs (x (n (1, j), 1, j) ) .gt.1.0e-06) then 
        n1 = n1 + 1 
        x (n1, 1, j) = x (n (1, j), 1, j) 
        y (n1, 1, j) = y (n (1, j), 1, j) 
      ENDIF 
                                                                        
      IF (n1.gt.mpts) then 
        PRINT * , 'Im ERGEN ist der Parameter mpts zu klein'
        ifehl = 1
        RETURN 
      ENDIF 
                                                                        
      n (1, j) = n1 
      x (0, 1, j) = x (n1 - 1, 1, j) 
      y (0, 1, j) = y (n1 - 1, 1, j) 
                                                                        
      IF (info.eq.1) then 
        PRINT * , '....' 
        WRITE ( * , '(i5,2f10.3)') (i, x (i, 1, j) , y (i, 1, j) , i =  &
        0, n (1, j) )                                                   
        PRINT * , '....' 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE ergen                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE prinf (ifehl, info)
!               Das Programm prueft die Ergebnisse                      
!                           j=1        : Schnittmengen                  
!                           j=2 oder 3 : Ergenzungsmengen               
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

DIMENSION e (3)

      ifehl = 0 
      CALL flaec (1, n (0, 1), f1, 0, 1) 
      CALL flaec (1, n (0, 2), f2, 0, 2) 
      f1 = abs (f1) 
      f2 = abs (f2) 
      IF (f1.lt.1.0e-06.or.f2.lt.1.0e-06) then 
!      print *, 'Ein Gebiet ist Pleite'                                 
        ifehl = 1 
        RETURN 
      ENDIF 
      DO 1 j = 1, 3 
        e (j) = 0. 
        DO 2 i = 1, n3 (j) 
          e (j) = e (j) + fs (i, j) 
    2   END DO 
        e (j) = abs (e (j) ) 
    1 END DO 
      p1 = abs (f1 - e (1) - e (2) ) * 100. 
      p2 = abs (f2 - e (1) - e (3) ) * 100. 
      p = (p1 + p2) / (f1 + f2) 
      IF (info.eq.1) then 
        PRINT * , ' ----------------------------------' 
      PRINT * , '|          Flaecheninhalt          |' 
      PRINT * , '|   Gebiet 1              Gebiet 2 |' 
      WRITE ( * , '(a1,f10.1,12x,f10.1,a3)') '|', f1, f2, '  |' 
      PRINT * , '|   S-Menge   E1-Menge   E2-Menge  |' 
      WRITE ( * , '(a1,f10.1,f10.1,f10.1,a5)') '|', e, '    |' 
        PRINT * , ' ----------------------------------' 
                                                                        
!      else if(p.gt.0.1.or.info.eq.1) then                              
      ENDIF 
      IF (p.gt.0.1) write ( * , '(a14,f10.2,a1)') 'Gesamtfehler :', p, '%'
      RETURN 
      END SUBROUTINE prinf                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE dainf (ifehl, lin, info)
! Das Programm prueft die Eingabedaten
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      ifehl = 0
      lin = 0 
                                                                        
      DO 1 j = 1, 2 
        IF (n (0, j) .gt.mpts) then 
          PRINT * , 'Fuer die', j, '.Datei :'
          PRINT * , 'Im DAINF ist Parameter mpts zu klein' 
          ifehl = 1
          RETURN 
        ELSEIF (n (0, j) .eq.0) then 
          PRINT * , 'Fuer die', j, '.Datei :' 
          PRINT * , 'Anzahl der Punkte ist 0' 
          ifehl = 1 
          RETURN 
        ELSEIF (n (0, j) .lt.0) then 
          PRINT * , 'Fuer die', j, '.Datei :' 
          PRINT * , 'Unzulaessiger Bloedsinn' 
          ifehl = 1 
          RETURN 
        ENDIF 
        x1 = x (1, 0, j) 
        xn = x (n (0, j), 0, j) 
        y1 = y (1, 0, j) 
        yn = y (n (0, j), 0, j) 
        a = sqrt ( (xn - x1) **2 + (yn - y1) **2) 
        IF (a.lt.1.0e-03) then 
          IF (info.eq.1) print * , j, '.Datei beschreibt eine Flaeche' 
        ELSE 
          IF (info.eq.1) print * , j, '.Datei beschreibt eine Linie' 
          lin = lin + j 
        ENDIF 
    1 END DO 
      IF (lin.gt.2) then 
      PRINT * , '    Zwei Linien ? Das kann nur ein Fehler sein !' 
        ifehl = 1 
        RETURN 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE dainf                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE inpul (lin)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      nil = lin 
      DO 1 k = 1, 2 
        IF (k.eq.2) lin = mod (lin, 2) + 1 
        m (k) = 1 
        DO 2 i = 1, n (0, lin) 
          x (i, m (k), k) = x (i, 0, lin) 
          y (i, m (k), k) = y (i, 0, lin) 
    2   END DO 
        n (m (k), k) = n (0, lin) 
        x (0, m (k), k) = x (n (m (k), k) - 1, m (k), k) 
        y (0, m (k), k) = y (n (m (k), k) - 1, m (k), k) 
    1 END DO 
      lin = nil 
      RETURN 
      END SUBROUTINE inpul                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE kreul (x1, y1, x2, y2, x3, y3, x4, y4, u, v, in, gen, info)
                                                                        
!     u=x3+r*(x4-x3)                                                    
!     v=y3+r*(y4-y3)                                                    
!     u=x1+R*(x2-x1)                                                    
!     v=y1+R*(y2-y1)                                                    
!     x3+r*(x4-x3)=x1+R*(x2-x1)                                         
!     y3+r*(y4-y3)=y1+R*(y2-y1)                                         
!     (x4-x3)*r+(x1-x2)*R=x1-x3                                         
!     (y4-y3)*r+(y1-y2)*R=y1-ya                                         
!     ((x4-x3)*(y1-y2)-(y4-y3)*(x1-x2))*r=(x1-x3)*(y1-y2)-(y1-ya)*(x1-x2
! -->               un*r=ob                                             
!     Wenn un=0, dann sind die Linien parallel                          
!     Wenn ob=0, dann sind die Linien kollinear                         
                                                                        
      in = 0 
                                                                        
      s21 = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
      IF (s21.lt.gen) then 
        IF (info.eq.1) then 
          PRINT * , '1.Strecke ist ein Punkt' 
          PRINT * , 's21=', s21 
        ENDIF 
        in = - 1 
        RETURN 
      ENDIF 
                                                                        
      s43 = sqrt ( (x4 - x3) **2 + (y4 - y3) **2) 
      IF (s43.lt.gen) then 
        IF (info.eq.1) then 
          PRINT * , 's43=', s43 
          PRINT * , '2.Strecke ist ein Punkt' 
        ENDIF 
        in = - 2 
        RETURN 
      ENDIF 
                                                                        
      un = (x4 - x3) * (y1 - y2) - (y4 - y3) * (x1 - x2) 
      IF (abs (un) .lt.gen) return 
                                                                        
      ob = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2) 
                                                                        
      r = ob / un 
      u = x3 + r * (x4 - x3) 
      v = y3 + r * (y4 - y3) 
                                                                        
                                                                        
      s10 = sqrt ( (x1 - u) **2 + (y1 - v) **2) 
      IF (s10.lt.gen) then 
        u = x1 
        v = y1 
        in = 1 
        RETURN 
      ENDIF 
                                                                        
      s20 = sqrt ( (x2 - u) **2 + (y2 - v) **2) 
      IF (s20.lt.gen) then 
        u = x2 
        v = y2 
        in = 2 
        RETURN 
      ENDIF 
                                                                        
                                                                        
      s30 = sqrt ( (x3 - u) **2 + (y3 - v) **2) 
      IF (s30.lt.gen) then 
        u = x3 
        v = y3 
        in = 3 
        RETURN 
      ENDIF 
                                                                        
      s40 = sqrt ( (x4 - u) **2 + (y4 - v) **2) 
      IF (s40.lt.gen) then 
        u = x4 
        v = y4 
        in = 4 
        RETURN 
      ENDIF 
                                                                        
      IF (s10.le.s21.and.s20.le.s21.and.s30.le.s43.and.s40.le.s43) then 
        in = 5 
        RETURN 
      ENDIF 
      RETURN 
      END SUBROUTINE kreul                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE mengl (ifehl, medu, info)
                                                                        
!   Das Programm konstruiert Linienstuecke :                            
!    n3 : Anzahl der Linienstuecke                                      
!    ns : Anzahl der Punkte in jedem Linienstueck                       
                                                                        
!   1) n3(1),ns(min,1),xs(0:mpts,min,1),ys(0:mpts,min,1) - innerhalb des  
!                                                        Gebietes
!   2) n3(2),ns(min,2),xs(0:mpts,min,2),ys(0:mpts,min,2) - ausserhalb des 
!                                                        Gebietes
!   3) n3(3),ns(min,3),xs(0:mpts,min,3),ys(0:mpts,min,3) - auf der Grenze 
!                                                        Gebietes
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

DIMENSION ni (mpts)
CHARACTER(3) typ

      ifehl = 0 
      l = 0 
    4 CONTINUE 
      l = l + 1 
      IF (l.ge.1000) then 
        PRINT * , 'Mehr als 999 Schnittpunkte <-- Fehler' 
        ifehl = 1 
        RETURN 
      ENDIF 
      IF (n (1, 1) .gt.mpts) then 
        PRINT * , 'Im MANGL ist Parameter mpts zu klein'
        ifehl = 1
        RETURN 
      ENDIF 
      DO 1 i = 1, n (1, 1) - 1 
                                                                        
        x1 = x (i, 1, 1) 
        y1 = y (i, 1, 1) 
        x2 = x (i + 1, 1, 1) 
        y2 = y (i + 1, 1, 1) 
                                                                        
                                                                        
        DO 2 j = 1, n (1, 2) - 1 
          x3 = x (j, 1, 2) 
          y3 = y (j, 1, 2) 
          x4 = x (j + 1, 1, 2) 
          y4 = y (j + 1, 1, 2) 
          CALL kreul (x1, y1, x2, y2, x3, y3, x4, y4, u, v, in, 1.0e-06, info)
          IF (in.eq.5) then 
            DO 3 k = n (1, 1), i + 1, - 1 
              x (k + 1, 1, 1) = x (k, 1, 1) 
              y (k + 1, 1, 1) = y (k, 1, 1) 
    3       END DO 
            x (i + 1, 1, 1) = u 
            y (i + 1, 1, 1) = v 
                                                                        
                                                                        
            n (1, 1) = n (1, 1) + 1 
            GOTO 4 
          ENDIF 
    2   END DO 
    1 END DO 
                                                                        
                                                                        
      DO 5 i = 1, n (1, 1) - 1 
                                                                        
        x0 = (x (i, 1, 1) + x (i + 1, 1, 1) ) / 2. 
        y0 = (y (i, 1, 1) + y (i + 1, 1, 1) ) / 2. 
        CALL punla (1, n (1, 2), x (1, 1, 2), y (1, 1, 2), x0, y0, in,  &
        1.0e-06, ifehl)                                                 
        IF (in.eq.0) ni (i) = 2 
        IF (in.eq.1) ni (i) = 1 
        IF (in.eq.2) ni (i) = 3 
                                                                        
    5 END DO 
                                                                        
!      write(*,'(14i5)') (ni(i),i=1,n(1,1)-1)                           
                                                                        
                                                                        
      DO 7 j = 1, 3 
        k = 0 
        l = 0 
        DO 6 i = 1, n (1, 1) - 1 
          IF (ni (i) .eq.j) then 
            k = k + 1 
            IF (k.gt.mpts) then 
              ifehl = 1
              PRINT * , 'Im MENGL ist der Parameter mpts zu klein' 
              RETURN
            ENDIF 
            IF (k.eq.1) l = l + 1 
            xs (k, l, j) = x (i, 1, 1) 
            ys (k, l, j) = y (i, 1, 1) 
                                                                        
            xs (k + 1, l, j) = x (i + 1, 1, 1) 
            ys (k + 1, l, j) = y (i + 1, 1, 1) 
            ns (l, j) = k + 1 
            GOTO 6 
          ENDIF 
          k = 0 
    6   END DO 
        n3 (j) = l 
                                                                        
        IF (j.eq.1) then 
          IF (info.eq.1) print * , ' ' 
          IF (info.eq.1) print  * , 'Linienstuecke innerhalb des Gebietes'
          typ = 's81' 
          lay = 301 
        ENDIF 
        IF (j.eq.2) then 
          IF (info.eq.1) print * , ' ' 
          IF (info.eq.1) print  * , 'Linienstuecke ausserhalb des Gebietes'
          typ = 's51' 
          lay = 302 
        ENDIF 
        IF (j.eq.3) then 
          IF (info.eq.1) print * , ' ' 
          IF (info.eq.1) print  * , 'Linienstuecke auf der Grenze des Gebietes'
          typ = 's77' 
          lay = 303 
        ENDIF 
                                                                        
        IF (info.eq.1) print * , 'Layer :', lay, ' Typ : ', typ 
        DO 8 j1 = 1, n3 (j) 
          CALL imron (j1, j) 
          IF (info.eq.1) print * , ' ' 
          IF (info.eq.1) write ( * , '(i5,2f10.3)') (i, xs (i, j1, j) , &
          ys (i, j1, j) , i = 0, ns (j1, j) )                           
                                                                        
          fs (j1, j) = 0. 
          DO 9 i1 = 1, ns (j1, j) - 1 
            x1 = xs (i1, j1, j) 
            y1 = ys (i1, j1, j) 
            x2 = xs (i1 + 1, j1, j) 
            y2 = ys (i1 + 1, j1, j) 
            a = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
            fs (j1, j) = fs (j1, j) + a 
    9     END DO 
                                                                        
    8   END DO 
    7 END DO 
                                                                        
      RETURN
      END SUBROUTINE mengl                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE punla (n1, n2, x, y, x0, y0, in, gen, ifehl)
!***************************************************************        
!  die subroutine ueberprueft, ob der punkt (x0,y0) auf der    *        
!  Grenze (in=2) des gebietes (x,y). wenn nicht, dann wird     *        
!  ueberprueft, ob der punkt innerhalb (in=1) oder ausser-     *        
!  halb des gebietes (x,y) liegt                               *        
!***************************************************************        
USE DIM_VARIABLEN

DIMENSION x (mpts), y (mpts)

      IF (n1.ge.n2) stop 'n1 > n2' 
      IF (n2.gt.mpts) stop 'parameter mpts in ing ist zu klein' 
      ifehl = 0
      in = 0 
      DO 1 k = n1, n2 - 1 
        x1 = x (k) 
        y1 = y (k) 
        x2 = x (k + 1) 
        y2 = y (k + 1) 
        cc = (x2 - x1) **2 + (y2 - y1) **2 
        c = sqrt (cc) 
        aa = (x0 - x1) **2 + (y0 - y1) **2 
        bb = (x0 - x2) **2 + (y0 - y2) **2 
        p = (aa + cc - bb) / c / 2. 
        hh = abs (aa - p * p) 
        IF (hh.ge.gen) goto 1 
        u = x1 + p * (x2 - x1) / c 
        v = y1 + p * (y2 - y1) / c 
        ee = (u - x1) **2 + (v - y1) **2 
        ff = (u - x2) **2 + (v - y2) **2 
                                                                        
        IF (ee.lt.gen.or.ff.lt.gen) then 
          in = 2 
          RETURN 
        ENDIF 
                                                                        
        IF (ee.lt.cc.and.ff.lt.cc) then 
          in = 2 
          RETURN 
        ENDIF 
    1 END DO 
      DO 2 k = n1, n2 - 1 
        xs = (x (k) + x (k + 1) ) / 2. 
        ys = (y (k) + y (k + 1) ) / 2. 
        CALL verle (x0, y0, xs, ys, 0., gen / 3., gen, ifehl) 
        IF (ifehl.eq.1) then 
          ifehl = 1 
          RETURN 
        ENDIF 
        DO 3 l = n1, n2 - 1 
          IF (k.eq.l) goto 3 
          x1 = x (l) 
          y1 = y (l) 
          x2 = x (l + 1) 
          y2 = y (l + 1) 
                                                                        
                                                                        
          in = 0 
          aoben = (x0 - xs) * (y1 - ys) - (y0 - ys) * (x1 - xs) 
          boben = (y1 - ys) * (x2 - x1) - (x1 - xs) * (y2 - y1) 
          unten = (x2 - x1) * (y0 - ys) - (y2 - y1) * (x0 - xs) 
          IF (abs (unten) .le.gen) goto 3 
          ak = aoben / unten 
          bk = boben / unten 
          IF (ak.ge.0..and.bk.ge.0..and.ak.le.1..and.bk.le.1.) then 
            in = 1 
          ENDIF 
                                                                        
          IF (in.eq.1) goto 2 
    3   END DO 
        x1 = x (k) 
        y1 = y (k) 
        x2 = x (k + 1) 
        y2 = y (k + 1) 
                                                                        
        flaech = ( (x1 - x2) * (y1 + y2) + (x2 - x0) * (y2 + y0)        &
        + (x0 - x1) * (y0 + y1) ) / 2.                                  
        IF (flaech.gt.0.) then 
          in = 1 
        ELSE 
          in = 0 
        ENDIF 
        RETURN 
    2 END DO 
      IF (in.eq.0) stop 'parameter in ist nicht definiert' 
      RETURN 
      END SUBROUTINE punla                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE verle (x1, y1, x2, y2, g1, g2, gen, ifehl)
!***********************************************************************
! verkuerzt (g1,g2 < 0) oder verlaengert (g1,g2 > 0) die strecke [1,2] *
!***********************************************************************
      ifehl = 0 
      cx = x2 - x1 
      cy = y2 - y1 
      cc = cx * cx + cy * cy 
      IF (cc.le.gen) then 
        ifehl = 1 
        RETURN 
      ENDIF 
      c = sqrt (cc) 
      cx = cx / c 
      cy = cy / c 
      x1 = x1 - cx * g1 
      y1 = y1 - cy * g1 
      x2 = x2 + cx * g2 
      y2 = y2 + cy * g2 
      RETURN 
END SUBROUTINE verle
                                                                        
                                                                        
                                                                        
!----------------------------------------------------------------------------------------
SUBROUTINE mengen (ifehl, medu, info, ipru, korr, iboo)
!
!      Angabe :  (x(i,0,1),y(i,0,1),i=0,n(0,1))
!                (x(i,0,2),y(i,0,2),i=0,n(0,2))                         
!                                                                       
!      als auch durch die Parameterliste :                              
!                                     (....,medu,info,ipru,korr)        
!      Wenn medu = 0, keine Medusa                                      
!                = 1, mit der Medusa                                    
!      Wenn info = 0, keine Information                                 
!                = 1, mit der Information                               
!      Wenn ipru = 0, keine Pruefung                                    
!                = 1, mit der Pruefung                                  
!                = 2, nur die Pruefung                                  
!      Wenn korr = 0, keine Korrektur                                   
!                = 1, mit der Korrektur                                 
!      Wenn iboo = 0, alle Mengen werden gefunden                       
!           iboo = 1, nur Schnittmenge                                  
!           iboo = 2, erste Ergaenzungsmenge                            
!           iboo = 3, zweite Ergaenzungsmenge                           
                                                                        
!            OUTPUT :                                                   
!      wird sowohl durch den Common-Block gemacht :                     
!      common/s3/ n3(3),ns(min,3),xs(0:mpts,min,3),ys(0:mpts,min,3),fs(min,3)
!      Fuer Flaechen :                                                  
!      n3(1) : Anzahl der Schnittmengen                                 
!      n3(2),n3(3) : Anzahl der beiden Ergenzugsmengen                  
!      ns(...,.) : Anzahl der Punkten in Linien                         
!      xs(...,...,.) und ys(...,...,.) : Koordinaten                    
!      fs(...,.) : Flaecheninhalte                                      
!      Fuer Linie :                                                     
!      n3(1) : Anzahl der Linienstucke innerhalb des Gebietes           
!      n3(2) : Anzahl der Linienstucke ausserhalb des Gebietes          
!      n3(3) : Anzahl der Linienstucke auf der Grenze                   
!              des Gebietes                                             
!      ns(...,.) : Anzahl der Punkten in Linien                         
!      xs(...,...,.) und ys(...,...,.) : Koordinaten                    
!      fs(...,.) : Linienlaengen                                        
!      als auch durch die Parameterliste :                              
!                                     (ifehl,....,....,....,....)       
!      Wenn ifehl = 1, dann gibt's nichtkorregierbare Fehler            
                                                                        
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

CALL initi (ifehl)

IF (ipru.eq.1) then

  IF (info.eq.1) print * , 'Linien werden geprueft'

  CALL check (2, korr, info, ifehl)

  IF (ifehl.eq.1) return

ELSEIF (ipru.eq.2) then

  IF (info.eq.1) print * , 'Linien werden geprueft'

  CALL check (2, korr, info, ifehl)

  IF (info.eq.1) print * , 'Keine Berechnung'

  RETURN

ENDIF

CALL dainf (ifehl, lin, info)

IF (ifehl.eq.1) return

IF (lin.eq.0) then
  ! ------------------------- FLAECHEN
  CALL extre (2, info)

  IF (iboo.eq.0.or.iboo.eq.1) call algeb (0, ifehl, medu, info)
  IF (ifehl.eq.1) return
  IF (iboo.eq.0.or.iboo.eq.2) call algeb (1, ifehl, medu, info)
  IF (ifehl.eq.1) return
  IF (iboo.eq.0.or.iboo.eq.3) call algeb (2, ifehl, medu, info)
  IF (ifehl.eq.1) return

  IF (iboo.eq.0) call prinf (ifehl, info)
  IF (ifehl.eq.1) return
  ! -------------------------
ELSE
  ! ------------------------- LINIEN
  CALL extre (2, info)
  CALL inpul (lin)
  CALL flaec (1, n (1, 2), f, 1, 2)
  CALL normi (1, 1)
  CALL normi (2, 0)
  ! KORREGIERT
  CALL inver (f, 1, 2)

  CALL mengl (ifehl, medu, info)
  IF (ifehl.eq.1) return

  CALL prinl (ifehl, info, lin)
  IF (ifehl.eq.1) return

ENDIF

END SUBROUTINE mengen                                                 


                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE check (k, kor, info, ifehl)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

ifehl = 0
ife = 0

DO j = 1, k

  IF (n (0, j) .eq.0) then
    ifehl = 1
    PRINT * , 'Ich wuerde vorschlagen, zwei Gebiete anzugeben'
    RETURN
  ENDIF

  ! write(*,'(i5,2f10.3)') (i,x(i,0,j),y(i,0,j),i=1,n(0,j))

  ver = 1.0e-06
  CALL vergl (ver, j, info)
  ! VER : kleinste Distanz zwischen anliegenden Punkten
  !       j   : Dateinummer
  !       ive : Anzahl der Fehler
  ! Die Routine macht die Punkte, die sich ein bisschen
  ! (weniger als <ver>) unterscheiden, gleich


  dis = 1.0e-06
  CALL dista (dis, j, idi, kor, info)
  ! DIS : kleinste Distanz zwischen anliegenden Punkten
  !       j   : Dateinummer
  !       idi : Anzahl der Fehler
  ! Die Routine prueft, ob es in der Datei Doppelpunkte gaebe :
  ! 1. Wenn kor=0, werden Fehler gezeigt
  ! 2. Wenn kor=1, werden Fehler korregiert
  ife = ife+idi

  IF (kor.eq. - 1) then
    ifehl = 1
    RETURN
  ENDIF

  pos = 1.0e-06

  !WP 11.05.2005
  !write (*,*) 'POSIT wird von CHECK mit Parametern aufgerufen:'
  !write (*,*) 'POS = ', pos, '  J = ', j, '  IPO = ', ipo, '  KOR = ', kor, '  ifehl = ', ifehl, '  info = ', info

  CALL posit (pos, j, ipo, kor, ifehl, info)
  IF (ifehl.eq.1) return
  ! Die Routine prueft, ob ein Punkt eine Polygonstrecke
  ! beruehrt
  ! POS   : kuerzeste Distanz zwischen einem Punkt und einer Strecke
  ! j     : Dateinummer
  ! ipo   : Anzahl der Fehler
  ! kor=1 : Datei wird korregiert (ein Extrapunkt eigefuehrt)
  ! kor=0 : keine Korrektur
  ife = ife+ipo

  pow = 1.0e-06
  CALL powin (pow, j, iwi, kor, ifehl, info)
  IF (ifehl.eq.1) return
  ! Die Routine findet Polynomwinkel
  ! POW   : kuerzeste Distanz zwischen verschiedenen Punkten
  ! j     : Dateinummer
  ! iwi   : Anzahl der Fehler
  ! kor=1 : Datei wird korregiert (andere Drehrichtung)
  ! kor=0 : keine Korrektur
  ife = ife+iwi

  sch = 1.0e-06
  CALL schni (sch, j, isc, kor, ifehl, info)
  IF (ifehl.eq.1) return
  ! Die Routine findet Kreuzungspunkt zwischen Strecken
  ! SCH   : kuerzeste Distanz zwischen verschiedenen Punkten
  ! j     : Dateinummer
  ! isc   : Anzahl der Fehler
  ! kor=1 : Datei wird korregiert (andere Drehrichtung)
  ! kor=0 : keine Korrektur
  ife = ife+isc

  pun = 1.0e-06
  CALL punkt (pun, j, ipu, kor, ifehl, info)
  IF (ifehl.eq.1) return
  ! Die Routine findet Kreuzungspunkte ( ein Verbindungs-
  ! punkt zwischen Untergebieten ist vorhanden)
  !    PUN : kuerzeste Distanz zwischen verschiedenen Punkten
  !      j : Dateinummer
  !    ipu : Anzahl der Fehler
  !  kor=1 : Datei wird korregiert (andere Drehrichtung)
  !  kor=0 : keine Korrektur
  ife = ife+ipu

  CALL strec (pun, j, ili, kor, ifehl, info)
  IF (ifehl.eq.1) return
  ! Die Routine findet Kreuzungspunkte (eine Verbindungs-
  ! linie zwischen Untergebieten ist vorhanden)
  ! PUN   : kuerzeste Distanz zwischen verschiedenen Punkten
  ! j     : Dateinummer
  ! ili   : Anzahl der Fehler
  ! kor=1 : Datei wird korregiert (andere Drehrichtung)
  ! kor=0 : keine Korrektur

  ife = ife+ili

END DO

IF (info.eq.1) print  * , 'Es gab insgesamt', ife, ' Fehler im Sheet'


END SUBROUTINE check                                                     
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE vergl (ver, j, info)
!                VER : kleinste Distanz zwischen anliegenden Punkten    
!                  j : Dateinummer                                      
!                ive : Anzahl der Fehler                                
!                Die Routine macht die Punkte, die sich ein bisschen    
!                (weniger als <ver>) unterscheiden, gleich              
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      DO 1 i1 = 0, n (0, j) - 1 
        DO 1 i2 = i1 + 1, n (0, j) 
          s = sqrt ( (x (i1, 0, j) - x (i2, 0, j) ) **2 + (y (i1, 0, j) &
          - y (i2, 0, j) ) **2)                                         
          IF (s.le.ver) then 
            IF (info.eq.1) then 
      PRINT * , 'Punkte :', i1, i2, ' in der', j, '.Datei sind gleich' 
            ENDIF 
            x (i2, 0, j) = x (i1, 0, j) 
            y (i2, 0, j) = y (i1, 0, j) 
          ENDIF 
    1 CONTINUE 
      RETURN 
      END SUBROUTINE vergl                          
                                                                        



!----------------------------------------------------------------------------------------
SUBROUTINE dista (dis, j, idi, kor, info)
!                DIS : kleinste Distanz zwischen anliegenden Punkten
!                  j : Dateinummer                                      
!                idi : Anzahl der Fehler                                
!                Die Routine prueft, ob es in der Datei Doppelpunkte    
!                gaebe :                                                
!                         1. Wenn kor=0, werden Fehler gezeigt          
!                         2. Wenn kor=1, werden Fehler korregiert       

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      idi = 0 
      k = 0 
      DO 1 i = 1, n (0, j) 
        k = k + 1 
        x (k, 0, j) = x (i, 0, j) 
        y (k, 0, j) = y (i, 0, j) 
        IF (n (0, j) .eq.2.or.k.eq.1) goto 1 
        s = sqrt ( (x (k, 0, j) - x (k - 1, 0, j) ) **2 + (y (k, 0, j)  &
        - y (k - 1, 0, j) ) **2)                                        
        IF (s.gt.dis) goto 1 
        u = x (i + 0, 0, j) 
        v = y (i + 0, 0, j) 
        IF (kor.eq.1) then 
          k = k - 1 
        ENDIF 
                                                                        
        idi = idi + 1 
        IF (info.eq.1) then 
          PRINT * , 'Punkt :' 
          PRINT * , 'x=', u, ' y=', v 
          PRINT * , 'im', j, '. Gebiet ist doppelt' 
        ENDIF 
    1 END DO 
      n (0, j) = k 
                                                                        
      IF (idi.gt.0.and.info.eq.1) then 
        PRINT * , ' ' 
        PRINT * , 'In der Datei', j, ' gibt es Doppelpunkte :', idi 
        IF (kor.eq.1) print * , 'Ich habe es korrigiert !!!' 
      ENDIF 
                                                                        
      s = sqrt ( (x (2, 0, j) - x (1, 0, j) ) **2 + (y (2, 0, j)        &
      - y (1, 0, j) ) **2)                                              
      IF (n (0, j) .le.2.and.s.lt.dis) then 
        PRINT * , j, '.Datei hat nur einen Punkt ?' 
        idi = idi + 1 
        kor = - 1 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE dista                          
                                                                        



!----------------------------------------------------------------------------------------
SUBROUTINE posit (pos, j, ipo, kor, ifehl, info)
!
! Die Routine prueft, ob ein Punkt eine Polygonstrecke beruehrt.
!
!----------------------------------------------------------------------------------------

USE DIM_VARIABLEN

REAL, INTENT(IN) :: pos      	! Kuerzeste Distanz zwischen einem Punkt und
                                ! einer Strecke
INTEGER, INTENT(IN) :: j        ! Dateinummer ??
INTEGER, INTENT(OUT) :: ipo     ! Iterationsparameter oder Anzahl ??
INTEGER, INTENT(IN) :: kor      ! = 1 : Datei wird korregiert (ein Extrapunkt eigefuehrt)
                                ! = 0 : Keine Korrektur
INTEGER, INTENT(OUT) :: ifehl   ! Fehlerparameter, wird =1 gesetzt, wenn Fehler auftritt,
                                ! sonst = 0.
INTEGER, INTENT(IN) :: info     ! Parameter zur Steuerung einer Info-Ausgabe, wird hier nicht benutzt

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

! Local variables
INTEGER :: mal, i1, i2
REAL :: x0, y0, x1, y1, x2, y2

!WP 11.05.2005
!WP Aufgrund eines Fehlers bei sehr kleinen Werten (Rechengenauigkeit) kann es zu Problemen mit
!WP der Korrektur des Polygonzuges kommen (Aufruf von HEREI und dann wiederholtes Durchlaufen).
!WP Deshalb wird bis auf weiteres die Korrektur in dieser Subroutine ausgeschaltet.


! Initialisierung
ifehl = 0
ipo = 0

!write (*,*) 'In POSIT. j = ', j, '  n(0,j) = ', n(0,j), '  KOR = ', kor

IF (n (0, j) .le. 2) return

mal = 0

! Berechnung
4 CONTINUE

mal = mal + 1

IF (mal .ge. 100) then

  write (*,1000)
  1000 format (1X, 'In Subroutine POSIT gibt es mehr als 100 Beruehrungen!')

  ifehl = 1
  RETURN

ENDIF

outer: DO i1 = 1, n (0, j)

  x0 = x (i1, 0, j)
  y0 = y (i1, 0, j)

  !WP 11.05.2005
  !write (*,*) ' In POSIT. X0 = ', x0, '  Y0 = ', y0, '  MAL = ', mal

  inner: DO i2 = 1, n (0, j) - 1

    x1 = x (i2, 0, j)
    y1 = y (i2, 0, j)
    x2 = x (i2 + 1, 0, j)
    y2 = y (i2 + 1, 0, j)
    cc = (x2 - x1) **2 + (y2 - y1) **2
    c = SQRT(cc)                        ! Abstand zwischen zwei Punkten des Polygons
    !write (*,*) ' In POSIT. I2 = ', i2, '  c = ', c

    IF (c .lt. pos) CYCLE inner         ! Zwei aufeinander folgende Punkte haben haben (fast) keinen Abstand!

    aa = (x1 - x0) **2 + (y1 - y0) **2  ! Quadrat des Abstandes zum Ursprung (ohne Wurzel)
    bb = (x2 - x0) **2 + (y2 - y0) **2  ! Quadrat des Abstandes zum Ursprung (ohne Wurzel)
    w = (aa + cc - bb) / 2.0 / c
    !write (*,*) ' In POSIT. I2 = ', i2, '  w = ', w, '  aa = ', aa, '  bb = ', bb
    IF ( (w .le. 0.0) .or. (w .ge. c) ) CYCLE inner
    ww = w ** 2

    h = sqrt (abs (aa - ww) )

    !write (*,*) ' In POSIT. I2 = ', i2, '  h = ', h

    IF (h .ge. pos) CYCLE inner
    !      print *,' '
    !      print *,'In der',j,'.Datei hat der Punkt :'
    !      print *,'x=',x0,' y=',y0
    !      print *,'falsche Position'
    ipo = ipo + 1

    IF (kor .eq. 0) then
      CYCLE outer
    ELSE
      !WP 11.05.2005
      !WP Polygonzug wird niemals korrigiert (siehe oben)!
      CYCLE outer 
      !CALL herei (i2 + 1, x0, y0, j)
      !GOTO 4
    ENDIF

  END DO inner

END DO outer

!write (*,*) 'In POSIT. Schleife durchlaufen. MAL = ', mal

END SUBROUTINE posit                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE herei (k, u, v, j)
!                     Die Routine ermoeglicht den Einschluss eines      
!                     Punktes <k,u,v> in die Datei <j>                  
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

!write (*,*) ' Hinweis: In HEREI wird ein zusaetzlicher Punkt in Polygonzug eingefuegt!'

DO i = n (0, j), k, - 1
  x (i + 1, 0, j) = x (i, 0, j)
  y (i + 1, 0, j) = y (i, 0, j)
END DO

x (k, 0, j) = u 
y (k, 0, j) = v
n (0, j) = n (0, j) + 1

END SUBROUTINE herei
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE powin (pow, j, iwi, kor, ifehl, info)
!               Die Routine findet Polynomwinkel                        
!               POW: kuerzeste Distanz zwischen verschiedenen Punkten   
!                 j : Dateinummer                                       
!               iwi : Anzahl der Fehler                                 
!             kor=1 : Datei wird korregiert (andere Drehrichtung)       
!             kor=0 : keine Korrektur                                   
!     Sz                                                                
!      include '/usr2/medusa/include/sectsc'                            
!      include '/usr2/medusa/include/darsc'                             
                                                                        
!*** pasche 23.11.99       include '.\include\sectsc'                   
!*** pasche 23.11.99       include '.\include\darsc'                    
!     Sz                                                                
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      ifehl = 0 
      iwi = 0 
      IF (n (0, j) .le.2) return 
      mal = 0 
    3 CONTINUE 
      mal = mal + 1 
      IF (mal.ge.1000) then 
        PRINT * , 'Im POWIN gibt es Fehler' 
        ifehl = 1 
        RETURN 
      ENDIF 
      x1 = x (1, 0, j) 
      y1 = y (1, 0, j) 
      x2 = x (n (0, j), 0, j) 
      y2 = y (n (0, j), 0, j) 
      s = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
      IF (s.lt.pow) then 
        x1 = x (2, 0, j) 
        y1 = y (2, 0, j) 
        x2 = x (n (0, j) - 1, 0, j) 
        y2 = y (n (0, j) - 1, 0, j) 
        s = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
                                                                        
        IF (s.lt.pow) then 
          iwi = iwi + 1 
          IF (info.eq.1) print * , 'Am Punkt 1 gibt es Nullwinkel' 
          IF (kor.ne.0) then 
            IF (info.eq.1) print * , 'Datei wird korregiert ' 
            DO 4 i = 1, n (0, j) - 2 
              x (i, 0, j) = x (i + 1, 0, j) 
              y (i, 0, j) = y (i + 1, 0, j) 
    4       END DO 
            n (0, j) = n (0, j) - 2 
            GOTO 3 
                                                                        
          ENDIF 
        ENDIF 
      ENDIF 
                                                                        
      DO 1 i1 = 2, n (0, j) - 1 
        x1 = x (i1 - 1, 0, j) 
        y1 = y (i1 - 1, 0, j) 
        x2 = x (i1 + 1, 0, j) 
        y2 = y (i1 + 1, 0, j) 
        s = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
        IF (s.lt.pow) then 
          iwi = iwi + 1 
      IF (info.eq.1) print  * , 'Am Punkt', i1, ' gibt es Nullwinkel' 
          IF (kor.eq.0) then 
!                                                                       
            GOTO 1 
          ELSE 
            IF (info.eq.1) print * , 'Datei wird korregiert' 
            DO 2 i = i1 - 1, n (0, j) - 2 
              x (i, 0, j) = x (i + 2, 0, j) 
              y (i, 0, j) = y (i + 2, 0, j) 
    2       END DO 
            n (0, j) = n (0, j) - 2 
            GOTO 3 
          ENDIF 
        ENDIF 
    1 END DO 
      RETURN 
      END SUBROUTINE powin                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE schni (sch, j, isc, kor, ifehl, info)
!               Die Routine findet Kreuzungspunkt zwischen Strecken     
!               SCH : kuerzeste Distanz zwischen verschiedenen Punkten  
!                 j : Dateinummer                                       
!               isc : Anzahl der Fehler                                 
!             kor=1 : Datei wird korregiert (andere Drehrichtung)       
!             kor=0 : keine Korrektur                                   
!      Sz                                                               
!      include '/usr2/medusa/include/sectsc'                            
!      include '/usr2/medusa/include/darsc'                             
                                                                        
!*** pasche 23.11.99       include '.\include\sectsc'                   
!*** pasche 23.11.99       include '.\include\darsc'                    
!     Sz                                                                
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      ifehl = 0 
      isc = 0 
      IF (n (0, j) .le.2) return 
      mal = 0 
    3 CONTINUE 
      mal = mal + 1 
      IF (mal.ge.1000) then 
        PRINT * , 'Im SCHNI gibt es Fehler' 
        ifehl = 1 
        RETURN 
      ENDIF 
                                                                        
      DO 1 i1 = 1, n (0, j) - 3 
        x3 = x (i1, 0, j) 
        y3 = y (i1, 0, j) 
        x4 = x (i1 + 1, 0, j) 
        y4 = y (i1 + 1, 0, j) 
                                                                        
        ss = sqrt ( (x4 - x3) **2 + (y4 - y3) **2) 
        IF (ss.lt.sch) goto 1 
        ex = (x4 - x3) / ss 
        ey = (y4 - y3) / ss 
                                                                        
        DO 2 i2 = i1 + 2, n (0, j) - 1 
                                                                        
          x1 = x (i2, 0, j) 
          y1 = y (i2, 0, j) 
          x2 = x (i2 + 1, 0, j) 
          y2 = y (i2 + 1, 0, j) 
          CALL kreuf (x1, y1, x2, y2, x3, y3, ex, ey, u, v, in) 
          IF (in.ne.3) goto 2 
          sl = sqrt ( (x3 - u) **2 + (y3 - v) **2) 
          IF (sl.lt.sch) goto 2 
          IF (sl.ge.ss) goto 2 
          sr = sqrt ( (x4 - u) **2 + (y4 - v) **2) 
          IF (sr.lt.sch) goto 2 
                                                                        
          IF (sr.ge.ss) goto 2 
          isc = isc + 1 
          IF (info.eq.1) then 
            PRINT * , 'Im Gebiet', j 
            PRINT * , 'Strecken :', i1, i1 + 1, i2, i2 + 1, ' ueberschneiden sich'
          ENDIF 
          IF (kor.eq.0) then 
                                                                        
            GOTO 1 
          ELSE 
                                                                        
            CALL herei (i1 + 1, u, v, j) 
            CALL herei (i2 + 2, u, v, j) 
      IF (info.eq.1) print  * , 'Schnittpunkt wird in die Datei eingeschlossen'
            GOTO 3 
          ENDIF 
                                                                        
                                                                        
    2   END DO 
    1 END DO 
                                                                        
      RETURN 
      END SUBROUTINE schni                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE punkt (pun, j, ipu, kor, ifehl, info)
!               Die Routine findet Kreuzungspunkte (ein Verbindungs-    
!               punkt zwischen Untergebieten ist vorhanden)             
!               PUN : kuerzeste Distanz zwischen verschiedenen Punkten  
!                 j : Dateinummer                                       
!               ipu : Anzahl der Fehler                                 
!             kor=1 : Datei wird korregiert (andere Drehrichtung)       
!             kor=0 : keine Korrektur                                   
!      Sz                                                               
!      include '/usr2/medusa/include/sectsc'                            
!      include '/usr2/medusa/include/darsc'                             
                                                                        
!*** pasche 23.11.99       include '.\include\sectsc'                   
!*** pasche 23.11.99       include '.\include\darsc'                    
!     Sz                                                                
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

DIMENSION ph (4)

      ifehl = 0 
      ipu = 0 
      IF (n (0, j) .le.2) return 
      mal = 0 
    3 CONTINUE 
      mal = mal + 1 
      IF (mal.ge.1000) then 
        PRINT * , 'Im PUNKT gibt es Fehler' 
        ifehl = 1 
        RETURN 
      ENDIF 
      DO 1 i1 = 1, n (0, j) - 2 
                                                                        
        DO 2 i2 = i1 + 1, n (0, j) - 1 
                                                                        
          s = sqrt ( (x (i1, 0, j) - x (i2, 0, j) ) **2 + (y (i1, 0, j) &
          - y (i2, 0, j) ) **2)                                         
          IF (s.gt.pun) goto 2 
          s = sqrt ( (x (i1 - 1, 0, j) - x (i2 + 1, 0, j) ) **2 +       &
          (y (i1 - 1, 0, j) - y (i2 + 1, 0, j) ) **2)                   
          IF (s.le.pun) goto 2 
          s = sqrt ( (x (i1 + 1, 0, j) - x (i2 - 1, 0, j) ) **2 +       &
          (y (i1 + 1, 0, j) - y (i2 - 1, 0, j) ) **2)                   
          IF (s.le.pun) goto 2 
          xl = x (i1 - 1, 0, j) - x (i1 + 0, 0, j) 
          yl = y (i1 - 1, 0, j) - y (i1 + 0, 0, j) 
          IF (sqrt (xl**2 + yl**2) .le.pun) goto 2 
          ph (1) = phi (yl, xl) 
                                                                        
          xr = x (i1 + 1, 0, j) - x (i1 + 0, 0, j) 
          yr = y (i1 + 1, 0, j) - y (i1 + 0, 0, j) 
          IF (sqrt (xr**2 + yr**2) .le.pun) goto 2 
          ph (3) = phi (yr, xr) 
                                                                        
          xl = x (i2 - 1, 0, j) - x (i2 + 0, 0, j) 
          yl = y (i2 - 1, 0, j) - y (i2 + 0, 0, j) 
          IF (sqrt (xl**2 + yl**2) .le.pun) goto 2 
                                                                        
          ph (2) = phi (yl, xl) 
                                                                        
          xr = x (i2 + 1, 0, j) - x (i2 + 0, 0, j) 
          yr = y (i2 + 1, 0, j) - y (i2 + 0, 0, j) 
          IF (sqrt (xr**2 + yr**2) .le.pun) goto 2 
          ph (4) = phi (yr, xr) 
                                                                        
          CALL verfl (ph, ipu) 
          IF (ipu.eq.0) goto 2 
          IF (info.eq.1) then 
            PRINT * , ' ' 
            PRINT * , 'Im Punkt :' 
            PRINT * , 'x=', x (i1, 0, j) , ' y=', y (i1, 0, j) 
            PRINT * , 'kreuzt die Linie', j, ' sich selbst' 
          ENDIF 
          IF (kor.eq.0) then 
                                                                        
            GOTO 1 
          ELSE 
            IF (info.eq.1) print  * , 'Fehler werden automatisch korrigiert'
            CALL invep (i1, i2, j) 
            GOTO 3 
          ENDIF 
    2   END DO 
    1 END DO 
                                                                        
      RETURN 
      END SUBROUTINE punkt                          
                                                                        



!----------------------------------------------------------------------------------------
SUBROUTINE verfl (ph, ipu)

DIMENSION ph (4), nh (4)
LOGICAL lo

      ipu = 0 
      phmin = MIN (ph (1), ph (2), ph (3), ph (4) ) 
                                                                        
      DO 1 j1 = 1, 4 
        ph (j1) = ph (j1) - phmin 
        nh (j1) = j1 
    1 END DO 
                                                                        
      DO 2 j1 = 1, 3 
        DO 2 j2 = 1, 3 
          IF (ph (j2) .gt.ph (j2 + 1) ) then 
            aa = ph (j2) 
            ph (j2) = ph (j2 + 1) 
            ph (j2 + 1) = aa 
                                                                        
            ii = nh (j2) 
            nh (j2) = nh (j2 + 1) 
            nh (j2 + 1) = ii 
                                                                        
          ENDIF 
    2 CONTINUE 
                                                                        
!      write(*,'(4f10.3)') (ph(k),k=1,4)                                
!      write(*,'(4i10  )') (nh(k),k=1,4)                                
                                                                        
      nn = nh (4) + nh (3) * 10 + nh (2) * 100 + nh (1) * 1000 
                                                                        
      lo = nn.eq.1234.or.nn.eq.2341.or.nn.eq.3412.or.nn.eq.4123.or.nn.eq&
     &.1432.or.nn.eq.2143.or.nn.eq.3214.or.nn.eq.4321                   
                                                                        
      IF (lo) then 
        ipu = ipu + 1 
      ENDIF 
      RETURN 
      END SUBROUTINE verfl                          
                                                                        

!----------------------------------------------------------------------------------------
FUNCTION phi (y, x)

IF (sqrt (x**2 + y**2) .lt.1.0e-06) stop 'Fehler in PHI'

phi = atan2 (y, x) 
phi = phi * 180.0 / 3.141593
IF (phi.lt.0.0) phi = phi + 360.0

END FUNCTION phi
                                                                        
!----------------------------------------------------------------------------------------
SUBROUTINE invep (n1, n2, j)
!                   Diese Routine stellt ein Teil der Datei             
!                   von <n1> bis <n2> um                                
!                   j : Dateinummer                                     
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      DO 1 k = n1, n1 + (n2 - n1) / 2 
        h = x (k, 0, j) 
        x (k, 0, j) = x (n2 - k + n1, 0, j) 
        x (n2 - k + n1, 0, j) = h 
        h = y (k, 0, j) 
        y (k, 0, j) = y (n2 - k + n1, 0, j) 
        y (n2 - k + n1, 0, j) = h 
    1 END DO 
      RETURN 
      END SUBROUTINE invep                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE inver (f, j, k)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      IF (f.gt.0) then 
        DO 1 i = 1, n (j, k) / 2 
          xi = x (i, j, k) 
          x (i, j, k) = x (n (j, k) - i + 1, j, k) 
          x (n (j, k) - i + 1, j, k) = xi 
          yi = y (i, j, k) 
          y (i, j, k) = y (n (j, k) - i + 1, j, k) 
          y (n (j, k) - i + 1, j, k) = yi 
    1   END DO 
        f = - f 
      ENDIF 
                                                                        
      x (0, j, k) = x (n (j, k) - 1, j, k) 
      y (0, j, k) = y (n (j, k) - 1, j, k) 
!      print *,' '                                                      
!      print *,'Flaecheninhalt=',f                                      
                                                                        
      RETURN 
      END SUBROUTINE inver                          
                                                                        
                                                                        
!----------------------------------------------------------------------------------------
SUBROUTINE strec (pun, j, ili, kor, ifehl, info)
!               Die Routine findet Kreuzungspunkte (eine Verbindungs-   
!               linie zwischen Untergebieten ist vorhanden)             
!               PUN : kuerzeste Distanz zwischen verschiedenen Punkten  
!                 j : Dateinummer                                       
!               ili : Anzahl der Fehler                                 
!             kor=1 : Datei wird korregiert (andere Drehrichtung)       
!             kor=0 : keine Korrektur                                   
!     Sz                                                                
!      include '/usr2/medusa/include/sectsc'                            
!      include '/usr2/medusa/include/darsc'                             
                                                                        
!*** pasche 23.11.99       include '.\include\sectsc'                   
!*** pasche 23.11.99       include '.\include\darsc'                    
!     Sz                                                                
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      ifehl = 0 
      ili = 0 
      IF (n (0, j) .le.2) return 
      mal = 0 
    3 CONTINUE 
      mal = mal + 1 
      IF (mal.ge.1000) then 
        PRINT * , 'Im LINIE gibt es Fehler' 
        ifehl = 1 
        RETURN 
      ENDIF 
      DO 1 i1 = 1, n (0, j) - 2 
                                                                        
        DO 2 i2 = i1 + 1, n (0, j) - 1 
                                                                        
          a = sqrt ( (x (i1 + 0, 0, j) - x (i2 + 1, 0, j) ) **2 +       &
          (y (i1 + 0, 0, j) - y (i2 + 1, 0, j) ) **2)                   
          b = sqrt ( (x (i1 + 1, 0, j) - x (i2 + 0, 0, j) ) **2 +       &
          (y (i1 + 1, 0, j) - y (i2 + 0, 0, j) ) **2)                   
          IF (a.lt.pun.and.b.lt.pun) then 
                                                                        
            xl = x (i1 - 1, 0, j) - x (i1 + 0, 0, j) 
            yl = y (i1 - 1, 0, j) - y (i1 + 0, 0, j) 
            xr = x (i2 + 2, 0, j) - x (i2 + 1, 0, j) 
            yr = y (i2 + 2, 0, j) - y (i2 + 1, 0, j) 
            xc = x (i1 + 1, 0, j) - x (i1 + 0, 0, j) 
            yc = y (i1 + 1, 0, j) - y (i1 + 0, 0, j) 
                                                                        
            plc = phi (yl, xl) - phi (yc, xc) 
            IF (plc.lt.0.0) plc = plc + 360.0 
            prc = phi (yr, xr) - phi (yc, xc) 
            IF (prc.lt.0.0) prc = prc + 360.0 
            IF (plc.gt.prc) then 
              irot = 1 
            ELSE 
              irot = 0 
            ENDIF 
                                                                        
            xl = x (i2 - 1, 0, j) - x (i2 + 0, 0, j) 
            yl = y (i2 - 1, 0, j) - y (i2 + 0, 0, j) 
            xr = x (i1 + 2, 0, j) - x (i1 + 1, 0, j) 
            yr = y (i1 + 2, 0, j) - y (i1 + 1, 0, j) 
            xc = x (i2 + 1, 0, j) - x (i2 + 0, 0, j) 
            yc = y (i2 + 1, 0, j) - y (i2 + 0, 0, j) 
            qlc = phi (yl, xl) - phi (yc, xc) 
            IF (qlc.lt.0.0) qlc = qlc + 360.0 
            qrc = phi (yr, xr) - phi (yc, xc) 
            IF (qrc.lt.0.0) qrc = qrc + 360.0 
            IF (qlc.gt.qrc) then 
              jrot = 1 
            ELSE 
              jrot = 0 
            ENDIF 
            IF (irot.ne.jrot) then 
              ili = ili + 1 
              IF (info.eq.1) print  * , 'Fehler in der Drehrichtung des Untergebietes'
              u = (x (i1 + 1, 0, j) + x (i1 + 0, 0, j) ) / 2. 
              v = (y (i1 + 1, 0, j) + y (i1 + 0, 0, j) ) / 2. 
              IF (kor.eq.0) then 
                GOTO 1
              ELSE 
                CALL invep (i1 + 1, i2, j) 
                IF (info.eq.1) print * , 'wird korregiert !!!' 
                GOTO 3 
              ENDIF 
            ENDIF 
                                                                        
          ENDIF 
                                                                        
    2   END DO 
    1 END DO 
                                                                        
      RETURN 
      END SUBROUTINE strec                          
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE initi (ifehl)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

      ifehl = 0
      m (1) = 0 
      m (2) = 0 
      DO 1 j = 1, 3 
        DO 1 i = 1, min2 
          n3 (j) = 0
          ns (i, j) = 0 
    1 CONTINUE 
      RETURN 
      END SUBROUTINE initi                          
                                                                        

                                                                        
                                                                        
!----------------------------------------------------------------------------------------
SUBROUTINE doppe (i1, i2, i3, i4, j, k, ins)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

      i1 = 0 
      i2 = 0 
      i3 = 0 
      i4 = 0 
                                                                        
      DO 1 i = 1, n (j, k) - 1 
        i1 = i 
        i2 = i + 1 
        x1 = x (i1, j, k) 
        y1 = y (i1, j, k) 
        x2 = x (i2, j, k) 
        y2 = y (i2, j, k) 
        DO 1 l = 1, n (j, k) - 1 
          i3 = l 
          i4 = l + 1 
          u1 = x (i3, j, k) 
          v1 = y (i3, j, k) 
          u2 = x (i4, j, k) 
          v2 = y (i4, j, k) 
          a = sqrt ( (x1 - u2) **2 + (y1 - v2) **2) 
          b = sqrt ( (x2 - u1) **2 + (y2 - v1) **2) 
                                                                        
          IF (a.lt.1.0e-06.and.b.lt.1.0e-06) then 
            ins = 1 
            RETURN 
          ELSE 
            ins = 0 
          ENDIF 
    1 CONTINUE 
      RETURN 
      END SUBROUTINE doppe                          
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE prinl (ifehl, info, lin)
                                                                        
!   1) n3(1),ns(min,1),xs(0:mpts,min,1),ys(0:mpts,min,1) - innerhalb des  
!                                                        Gebietes
!   2) n3(2),ns(min,2),xs(0:mpts,min,2),ys(0:mpts,min,2) - ausserhalb des 
!                                                        Gebietes
!   3) n3(3),ns(min,3),xs(0:mpts,min,3),ys(0:mpts,min,3) - auf der Grenze 
!                                                        Gebietes
USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)
COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

DIMENSION e (3)

      ifehl = 0 
      g = 0. 
      DO 1 j = 1, 3 
        IF (j.eq.1.and.n3 (j) .gt.0.and.info.eq.1) then 
          PRINT * , ' --------------------------------------------' 
          PRINT * , '| Linie innerhalb des Gebietes besteht aus : |' 
          WRITE ( * , '(a1,i5,10x,a10,19x,a1)') '|', n3 (j) , ' Strecken', '|'
        ENDIF 
        IF (j.eq.2.and.n3 (j) .gt.0.and.info.eq.1) then 
          PRINT * , ' --------------------------------------------' 
          PRINT * , '| Linie ausserhalb des Gebietes besteht aus :|' 
          WRITE ( * , '(a1,i5,10x,a10,19x,a1)') '|', n3 (j) , ' Strecken', '|'
        ENDIF 
        IF (j.eq.3.and.n3 (j) .gt.0.and.info.eq.1) then 
          PRINT * , ' --------------------------------------------' 
          PRINT * , '| Linie auf der Grenze besteht aus :         |'
          WRITE ( * , '(a1,i5,10x,a10,19x,a1)') '|', n3 (j) , ' Strecken', '|'
        ENDIF 
        e (j) = 0. 
        DO 2 i = 1, n3 (j) 
          e (j) = e (j) + fs (i, j) 
    2   END DO 
        IF (n3 (j) .gt.0.and.info.eq.1) then 
      WRITE ( * , '(a28,f10.3,6x,a2)') '| und hat die Gesamtlaenge :', e(j) , ' |'
          PRINT * , ' --------------------------------------------' 
        ENDIF 
        g = g + e (j) 
    1 END DO 
                                                                        
      IF (g.le.1.0e-06) then 
        ifehl = 1 
        PRINT * , 'Linie ist total Pleite' 
        RETURN 
      ENDIF 
                                                                        
      h = 0. 
      DO 3 i = 1, n (0, lin) - 1 
        x1 = x (i, 0, lin) 
        y1 = y (i, 0, lin) 
        x2 = x (i + 1, 0, lin) 
        y2 = y (i + 1, 0, lin) 
        a = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
        h = h + a 
    3 END DO 
                                                                        
      IF (h.le.1.0e-06) then 
        ifehl = 1 
        PRINT * , 'Linie ist total Pleite' 
        RETURN 
      ENDIF 
                                                                        
      p = abs (h - g) * 100.0 / h 
      IF (p.gt.0.1.or.info.eq.1) then 
        WRITE ( * , '(a14,f10.2,a1)') 'Gesamtfehler :', p, '%' 
      ENDIF 
      RETURN 
      END SUBROUTINE prinl                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE unkon (ie, ifehl)
! Programm konstruiert aus vielen angrenzenden Gebieten ein Gebiet
USE DIM_VARIABLEN

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

      ifehl = 0 
      IF (n3 (ie) .le.1) return 
                                                                        
      mal = 0 
    3 CONTINUE 
      mal = mal + 1 
      IF (mal.gt.1000) then 
        ifehl = 1 
        PRINT * , 'Fehler in UNCON' 
        RETURN 
      ENDIF 
      k = 0 
      DO 1 k1 = 1, n3 (ie) - 1 
        IF (ns (k1, ie) .eq.0) goto 1 
        DO 2 k2 = k1 + 1, n3 (ie) 
          IF (ns (k2, ie) .eq.0) goto 2 
          CALL einge (ifehl, k1, k2, ie) 
          IF (ns (k2, ie) .eq.0) k = k + 1 
    2   END DO 
    1 END DO 
      IF (k.gt.0) goto 3 
                                                                        
      k = 0 
      DO 4 j = 1, n3 (ie) 
        IF (ns (j, ie) .eq.0) goto 4 
        k = k + 1 
        ns (k, ie) = ns (j, ie) 
        fs (k, ie) = fs (j, ie) 
        DO 5 i = 0, ns (k, ie) 
          xs (i, k, ie) = xs (i, j, ie) 
          ys (i, k, ie) = ys (i, j, ie) 
    5   END DO 
    4 END DO 
      n3 (ie) = k 
!---------------------------------                                      
                                                                        
      DO 8 k = 1, n3 (ie) 
        mal3 = 0 
    6   CONTINUE 
                                                                        
        mal3 = mal3 + 1 
        IF (mal3.gt.1000) then 
          ifehl = 1 
          PRINT * , 'mal3 ist zu gross' 
          GOTO 8 
        ENDIF 
                                                                        
        DO 9 j = 1, ns (k, ie) - 1 
          x0 = xs (j - 1, k, ie) 
          y0 = ys (j - 1, k, ie) 
          x1 = xs (j - 0, k, ie) 
          y1 = ys (j - 0, k, ie) 
          x2 = xs (j + 1, k, ie) 
          y2 = ys (j + 1, k, ie) 
          a = sqrt ( (x2 - x0) **2 + (y2 - y0) **2) 
          IF (a.lt.1.0e-03) then 
            i = 0 
            DO 7 l = 1, ns (k, ie) - 1 
              IF (l.eq.j.or.l.eq.j - 1) goto 7 
              i = i + 1 
              xs (i, k, ie) = xs (l, k, ie) 
              ys (i, k, ie) = ys (l, k, ie) 
    7       END DO 
                                                                        
            xs (0, k, ie) = xs (i, k, ie) 
            ys (0, k, ie) = ys (i, k, ie) 
            i = i + 1 
            xs (i, k, ie) = xs (1, k, ie) 
            ys (i, k, ie) = ys (1, k, ie) 
            ns (k, ie) = i 
            GOTO 6 
          ENDIF 
    9   END DO 
    8 END DO 
                                                                        
      RETURN 
      END SUBROUTINE unkon                          
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE einge (ifehl, k1, k2, ie)

USE DIM_VARIABLEN

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

      gen = 1.0e-03 
      ifehl = 0 
      iw = 0 
      DO 1 j = 1, ns (k1, ie) - 1 
        x0 = xs (j - 1, k1, ie) 
        y0 = ys (j - 1, k1, ie) 
        x1 = xs (j, k1, ie) 
        y1 = ys (j, k1, ie) 
        x2 = xs (j + 1, k1, ie) 
        y2 = ys (j + 1, k1, ie) 
        DO 2 i = 1, ns (k2, ie) - 1 
          u2 = xs (i - 1, k2, ie) 
          v2 = ys (i - 1, k2, ie) 
          u1 = xs (i, k2, ie) 
          v1 = ys (i, k2, ie) 
          u0 = xs (i + 1, k2, ie) 
          v0 = ys (i + 1, k2, ie) 
                                                                        
          a = sqrt ( (x0 - u0) **2 + (y0 - v0) **2) 
          b = sqrt ( (x1 - u1) **2 + (y1 - v1) **2) 
          c = sqrt ( (x2 - u2) **2 + (y2 - v2) **2) 
                                                                        
          IF (a.le.gen.and.b.le.gen.and.c.gt.gen.and.iw.eq.0) then 
            CALL erstp (ifehl, j, k1, ie) 
            CALL erstp (ifehl, i, k2, ie) 
            iw = 1 
            GOTO 3 
          ENDIF 
    2   END DO 
    1 END DO 
    3 CONTINUE 
                                                                        
      IF (iw.eq.0) return 
                                                                        
      DO 5 j = ns (k1, ie) - 1, 1, - 1 
        x0 = xs (j + 1, k1, ie) 
        y0 = ys (j + 1, k1, ie) 
        x1 = xs (j, k1, ie) 
        y1 = ys (j, k1, ie) 
        x2 = xs (j - 1, k1, ie) 
        y2 = ys (j - 1, k1, ie) 
        DO 6 i = 1, ns (k2, ie) - 1 
          u0 = xs (i - 1, k2, ie) 
          v0 = ys (i - 1, k2, ie) 
          u1 = xs (i, k2, ie) 
          v1 = ys (i, k2, ie) 
          u2 = xs (i + 1, k2, ie) 
          v2 = ys (i + 1, k2, ie) 
                                                                        
          a = sqrt ( (x0 - u0) **2 + (y0 - v0) **2) 
          b = sqrt ( (x1 - u1) **2 + (y1 - v1) **2) 
          c = sqrt ( (x2 - u2) **2 + (y2 - v2) **2) 
          IF (a.le.gen.and.b.le.gen.and.c.gt.gen.and.iw.eq.1) then 
            k = j 
            DO 4 l = i + 1, ns (k2, ie) 
              k = k + 1 
              xs (k, k1, ie) = xs (l, k2, ie) 
              ys (k, k1, ie) = ys (l, k2, ie) 
    4       END DO 
            xs (0, k1, ie) = xs (k - 1, k1, ie) 
            ys (0, k1, ie) = ys (k - 1, k1, ie) 
            ns (k1, ie) = k 
            fs (k1, ie) = fs (k1, ie) + fs (k2, ie) 
            ns (k2, ie) = 0 
            RETURN 
          ENDIF 
    6   END DO 
    5 END DO 

      RETURN 
      END SUBROUTINE einge                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE erstp (ifehl, j, k, ie)

USE DIM_VARIABLEN

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

      ifehl = 0 
      IF (j.gt.1) then 
        DO 1 jz = 1, ns (k, ie) - j 
          DO 2 l = ns (k, ie), 2, - 1 
            xs (l, k, ie) = xs (l - 1, k, ie) 
            ys (l, k, ie) = ys (l - 1, k, ie) 
    2     END DO 
          xs (1, k, ie) = xs (ns (k, ie), k, ie) 
          ys (1, k, ie) = ys (ns (k, ie), k, ie) 
    1   END DO 
        xs (0, k, ie) = xs (ns (k, ie) - 1, k, ie) 
        ys (0, k, ie) = ys (ns (k, ie) - 1, k, ie) 
                                                                        
      ENDIF 
      RETURN 
      END SUBROUTINE erstp                          
                                                                        
                                                                        
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE epunk (ie, ifehl)
                                                                        
! Programm wird Extrapunkte einfuehren                                  
                                                                        
USE DIM_VARIABLEN

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)


      ifehl = 0 
      IF (n3 (ie) .le.1) return 
                                                                        
      DO 1 k1 = 1, n3 (ie) 
        DO 2 k2 = 1, n3 (ie) 
          IF (k1.eq.k2) goto 2 
          CALL einfu (ifehl, k1, k2, ie) 
          IF (ifehl.eq.1) return 
    2   END DO 
    1 END DO 
                                                                        
      RETURN 
      END SUBROUTINE epunk                          
                                                                        

!----------------------------------------------------------------------------------------
SUBROUTINE einfu (ifehl, k1, k2, ie)

USE DIM_VARIABLEN

COMMON / s3 / n3 (3), ns (min2, 3), xs (0:mpts, min2, 3), ys (0:mpts, min2, 3), fs (min2, 3)

      pos = 1.0e-03 
      ifehl = 0 
                                                                        
      DO 1 i1 = 1, ns (k1, ie) 
        x0 = xs (i1, k1, ie) 
        y0 = ys (i1, k1, ie) 
                                                                        
        IF (ns (k2, ie) .gt.mpts) then 
          ifehl = 1
          PRINT * , 'Fehler in EINFU' 
          RETURN 
        ENDIF 
                                                                        
        DO 2 i2 = 1, ns (k2, ie) - 1 
          x1 = xs (i2, k2, ie) 
          y1 = ys (i2, k2, ie) 
          x2 = xs (i2 + 1, k2, ie) 
          y2 = ys (i2 + 1, k2, ie) 
                                                                        
                                                                        
          cc = (x2 - x1) **2 + (y2 - y1) **2 
          aa = (x1 - x0) **2 + (y1 - y0) **2 
          bb = (x2 - x0) **2 + (y2 - y0) **2 
          IF (aa.gt.cc + pos * pos) goto 2 
          IF (bb.gt.cc + pos * pos) goto 2 
          IF (aa.lt.pos * pos) goto 2 
          IF (bb.lt.pos * pos) goto 2 
          IF (cc.lt.pos * pos) goto 2 
                                                                        
          c = sqrt (cc) 
          a = sqrt (aa) 
          b = sqrt (bb) 
                                                                        
          w = (aa + cc - bb) / 2.0 / c 
          IF (w.lt.pos) goto 2 
          IF (abs (w - c) .lt.pos) goto 2 
                                                                        
          IF (w.lt.0.0.or.w.gt.c) goto 2 
                                                                        
          ww = w**2 
          dd = abs (aa - ww) 
          IF (dd.gt.pos * pos) then 
            h = sqrt (dd) 
            IF (h.ge.pos) goto 2 
          ENDIF 
          x0 = x1 + (x2 - x1) / c * w 
          y0 = y1 + (y2 - y1) / c * w 
                                                                        
          DO 3 i = ns (k2, ie), i2 + 1, - 1 
            xs (i + 1, k2, ie) = xs (i, k2, ie) 
            ys (i + 1, k2, ie) = ys (i, k2, ie) 
    3     END DO 
          xs (i2 + 1, k2, ie) = x0 
          ys (i2 + 1, k2, ie) = y0 
          xs (0, k2, ie) = xs (ns (k2, ie), k2, ie) 
          ys (0, k2, ie) = ys (ns (k2, ie), k2, ie) 
          ns (k2, ie) = ns (k2, ie) + 1 
                                                                        
                                                                        
          GOTO 1 
    2   END DO 
    1 END DO 
      RETURN 
      END SUBROUTINE einfu                          
                                                                        


!----------------------------------------------------------------------------------------
SUBROUTINE vorko (k)

USE DIM_VARIABLEN

COMMON / io / m (2), n (0:min2, 2), x (0:mpts, 0:min2, 2), y (0:mpts, 0:min2, 2)

!      write(0,'(a)') 'Input  0'                                        
!      write(0,'(i5,2f10.3)') (i,x(i,1,k),y(i,1,k),i=0,n(1,k))          
      CALL flaec (1, n (1, k), f, j, k) 
!       write(0,'(a3,f10.3)') 'f= ',f                                   
                                                                        
!     Hier ist m(k)=1                                                   
      malv = 0 
    7 CONTINUE 
      malv = malv + 1 
      IF (malv.gt.1000) then 
        WRITE (0, '(a)') 'malv ist zu gross' 
      ENDIF 
      m0 = m (k) 
!      write(0,'(a5,i5)') 'm(k)=',m(k)                                  
      DO 1 j = 1, m (k) 
        DO 2 i1 = 1, n (j, k) - 2 
          x1 = x (i1, j, k) 
          y1 = y (i1, j, k) 
          DO 2 i2 = i1 + 1, n (j, k) - 1 
            x2 = x (i2, j, k) 
            y2 = y (i2, j, k) 
                                                                        
            a = sqrt ( (x2 - x1) **2 + (y2 - y1) **2) 
            IF (a.lt.1.0e-06) then 
              CALL flaec (i1, i2, f1, j, k) 
              CALL flaec (1, i1, f2, j, k) 
              CALL flaec (i2, n (j, k), f3, j, k) 
              f2 = f2 + f3 
!      write(0,'(a7,2f20.7)') 'f1 f2 =',f1,f2                           
                                                                        
              IF (f1.gt.1.0e-06.or.f2.gt.1.0e-06) goto 2 
!      write(0,'(a,2i5)') 'a',i1,i2                                     
              m (k) = m (k) + 1 
              l = 0 
              DO 5 i = i1, i2 
                l = l + 1 
                x (l, m (k), k) = x (i, j, k) 
                y (l, m (k), k) = y (i, j, k) 
    5         END DO 
              n (m (k), k) = l 
              x (0, m (k), k) = x (n (m (k), k) - 1, m (k), k) 
              y (0, m (k), k) = y (n (m (k), k) - 1, m (k), k) 
                                                                        
              l = - 1 
              DO 6 i = 0, n (j, k) 
                IF (i.ge.i1.and.i.lt.i2) goto 6 
                l = l + 1 
                x (l, j, k) = x (i, j, k) 
                y (l, j, k) = y (i, j, k) 
    6         END DO 
              n (j, k) = l 
                                                                        
                                                                        
              GOTO 7 
            ENDIF 
    2   CONTINUE 
                                                                        
    1 END DO 
                                                                        
      IF (m (k) .gt.m0) goto 7 
      mk = 0 
      DO 8 l = 1, m (k) 
        CALL flaec (1, n (l, k), f, l, k) 
        IF (abs (f) .lt.1.0e-06) goto 8 
        mk = mk + 1 
        DO 9 i = 0, n (l, k) 
          x (i, mk, k) = x (i, l, k) 
          y (i, mk, k) = y (i, l, k) 
    9   END DO 
        n (mk, k) = n (l, k) 
    8 END DO 
      m (k) = mk 
                                                                        
                                                                        
      RETURN 
      END SUBROUTINE vorko                          
