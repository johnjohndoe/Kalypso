      SUBROUTINE TRIANINT(XUSR,YUSR,NOPEL,NVERT,NELTS,M,ISWT,ITIME,WGT
     +,NDWT)
      USE BLK10MOD
      USE BLKCDMOD
      SAVE

      REAL*8 XUSR(*),YUSR(*),xminl,xmaxl,yminl,ymaxl
      INTEGER NOPEL(3,*)
  
      DIMENSION WGT(*),NDWT(*)
	CHARACTER*72  IOUTC(10)

C     Search for element that has circumcircle around the node

      IF(ISWT .NE. 0) THEN
        IF(ITIME .EQ. 0) NSTART=1
      ELSE
        NSTART=1
      ENDIF

      DO N=NSTART,NELTS
        IF(NOPEL(1,N) .EQ. 0) GO TO 200
        if(RADS(N) .eq. 0.) then
         CALL CCENTRE(XUSR(NOPEL(1,N)),XUSR(NOPEL(2,N)),XUSR(NOPEL(3,N))      
     +     ,YUSR(NOPEL(1,N)),YUSR(NOPEL(2,N)),YUSR(NOPEL(3,N))                 
     +     ,XCEN(N),YCEN(N),RADS(N)) 
        endif       

        IF(RADS(N)+XCEN(N) .GE. CORD(M,1)) THEN
            NSTART=N
            GO TO 210
          ENDIF
  200   CONTINUE
      ENDDO
  210 CONTINUE

      IRJT=0
      DO N=NSTART,NELTS
        IF(NOPEL(1,N) .EQ. 0) GO TO 250
        if(RADS(N) .eq. 0.) then
         CALL CCENTRE(XUSR(NOPEL(1,N)),XUSR(NOPEL(2,N)),XUSR(NOPEL(3,N))      
     +    ,YUSR(NOPEL(1,N)),YUSR(NOPEL(2,N)),YUSR(NOPEL(3,N))                 
     +    ,XCEN(N),YCEN(N),RADS(N)) 
        endif       
        xminl=dmin1(XUSR(NOPEL(1,N)),XUSR(NOPEL(2,N)),XUSR(NOPEL(3,N)))
        xmaxl=dmax1(XUSR(NOPEL(1,N)),XUSR(NOPEL(2,N)),XUSR(NOPEL(3,N)))
        yminl=dmin1(YUSR(NOPEL(1,N)),YUSR(NOPEL(2,N)),YUSR(NOPEL(3,N)))
        ymaxl=dmax1(YUSR(NOPEL(1,N)),YUSR(NOPEL(2,N)),YUSR(NOPEL(3,N)))
        if(CORD(m,1) .lt. xminl-0.001  .or.  
     +     CORD(m,1) .gt. xmaxl+0.001) then
          go to 250
        elseif(CORD(m,2) .lt. yminl-0.001  .or.
     +         CORD(m,2) .gt. ymaxl+0.001) then
          go to 250
        endif

        DISQ=(CORD(M,1)-XCEN(N))**2+(CORD(M,2)-YCEN(N))**2

C        write(142,*) m,n,disq,rads(n)**2,xusr(m),xcen(n)
        IF(DISQ .LE. RADS(N)**2*1.0001) THEN

C     We have a candidate

          CALL GETWT(XUSR,YUSR,N,NOPEL,CORD(M,1),CORD(M,2),WGT,1)
          DO K=1,3
            IF(WGT(K) .LT. -1E-3  .OR. WGT(K) .GT. 1.001) THEN
	       irjt=IRJT+1
           WRITE(IOUTC(IRJT),'(''RJCT'',2I6,2F10.2,3F12.5)')m,n,diSQ
     +	 ,rads(n)**2,wgt(1),wgt(2),wgt(3)
              GO TO 250
            ENDIF
          ENDDO
          NDWT(1)=NOPEL(1,N)
          NDWT(2)=NOPEL(2,N)
          NDWT(3)=NOPEL(3,N)
          if(irjt .GT. 1) IRJT=0
          GO TO 300
          ENDIF
  250   CONTINUE
      ENDDO
      IF(IRJT .GT. 0) THEN
	  DO I=1,IRJT
	    WRITE(142,*) IOUTC(I)
	  ENDDO
	  IRJT=0
	ENDIF
  300 CONTINUE
      ITIME=1

      RETURN
        END


      SUBROUTINE GETWT(XUSR,YUSR,N,NOPEL,XSW,YSW,WGT,ISWT)

C-                                                                      
C......SUBROUTINE TO EVALUATE FUNCTION AT GRID POINTS                   
C-                                                                      
C-    N = ELEMENT NUMBER        
C_    XSW = X COORDINATE OF DESIRED POINT
C_    YSW = Y COORDINATE OF DESIRED POINT
C     WGT(8) = ARRAY OF WEIGHTING FUNCTIONS
C     ISWT   = SWITCH FOR CHOICE BETWEEN LINEAR AND QUADRATIC WEIGHTING
C            = 1 FOR LINEAR
C            = 2 FOR QUADRATIC
C     FROM COMMON
C     NOP = LIST OF NODAL CONNECTIONS AROUND AN ELEMET
C     CORD = REAL*8 ARRAY OF NODAL COORDINATES
C                                                                       
      USE BLK10MOD
      REAL*8 XN2,DNX,DNY,XSW,YSW,XUSR(*),YUSR(*) 
      DOUBLE PRECISION XG,YG,XK,YK,XP,YP
      
      INTEGER NOPEL(3,*)
C-                                                                      
      DIMENSION X(9),Y(9),WGT(8)
C-                                                                      
      DATA TOL/0.01/ 
C-                                                                      

C-                                                                      
C......DETERMINE ELEMENT TYPE                                           
C-                                                                      
CIPKOCT93 ADD                                                           
      if(n .eq. 1910) then
         aaa=0
      endif
      NCN=6 
      IT=2 
C-                                                                      
C......ESTABLISH LOCAL COORDINATES FOR EACH NODE POINT OF ELEMENT                 
C-                                                                      
      K1=NOPEL(1,N) 
      X(1)=0. 
      Y(1)=0. 
      DO 300 K=3,NCN,2
        K2=NOPEL(K/2+1,N) 
        X(K)=XUSR(K2)-XUSR(K1) 
        Y(K)=YUSR(K2)-YUSR(K1) 
  300 END DO 
      X(2)=X(3)/2.
      Y(2)=Y(3)/2.
      X(4)=(X(3)+X(5))/2.
      Y(4)=(Y(3)+Y(5))/2.
      X(6)=X(5)/2.
      Y(6)=Y(5)/2.
        xminl=min(x(1),x(3),x(5))
        yminl=min(y(1),y(3),y(5))
        xmaxl=max(x(1),x(3),x(5))
        ymaxl=max(y(1),y(3),y(5))


C-                                                                      
C......ESTABLISH LOCAL COORDINATES OF DESIRED POINT                     
C-                                                                      
      XP=XSW-XUSR(K1) 
      YP=YSW-YUSR(K1) 

      if(xp .lt. xminl-0.0001  .or.  xp  .gt. xmaxl+0.0001) then
          wgt(1)=2.0
        return
      elseif(yp .lt. yminl-0.0001  .or.  yp  .gt. ymaxl+0.0001) then
        wgt(1)=2.0
          return
        endif
      XG=0. 
      YG=0. 
C-                                                                      
C......ITERATE TO FIND LOCAL COORDINATE                                 
C-                                                                      
      DO 400 ITER=1,10 
        DXKDX=0. 
        DXKDY=0. 
        DYKDX=0. 
        DYKDY=0. 
        XK=-XP 
        YK=-YP 
        DO 350 K=2,NCN 
          XK=XK+XN2(IT,K,XG,YG)*X(K) 
          YK=YK+XN2(IT,K,XG,YG)*Y(K) 
          DXKDX=DXKDX+DNX(IT,K,XG,YG)*X(K) 
          DYKDX=DYKDX+DNX(IT,K,XG,YG)*Y(K) 
          DXKDY=DXKDY+DNY(IT,K,XG,YG)*X(K) 
          DYKDY=DYKDY+DNY(IT,K,XG,YG)*Y(K) 
  350   END DO 
        DET=DXKDX*DYKDY-DXKDY*DYKDX 
        DX=(-DYKDY*XK+DXKDY*YK)/DET 
        DY=( DYKDX*XK-DXKDX*YK)/DET 
        XG=XG+DX 
        YG=YG+DY 
        IF(ABS(DX).LT.TOL .AND. ABS(DY).LT.TOL) GO TO 420 
  400 END DO 
C-                                                                      
C......NOW GET WEIGHTING FUNCTIONS FOR QUAD FUNCTION
C-                                                                      
  420 CONTINUE 
      DO  K=1,NCN 
        WGT(K)=XN2(IT,K,XG,YG)
      END DO 

      IF(ISWT .EQ. 1) THEN
C-
C-     REDUCE TO LINEAR FUNCTION BY ADDING TERMS
C-
        DO K=2,NCN,2
          WGT(K-1)=WGT(K-1)+WGT(K)/2.
          IF(K .LT. NCN) THEN
            WGT(K+1)=WGT(K+1)+WGT(K)/2.
          ELSE
            WGT(1)=WGT(1)+WGT(K)/2.
          ENDIF
        ENDDO
C-    
C-     THEN COMPACT ARRAY
C-
        DO K=1,NCN/2
          WGT(K)=WGT(2*K-1)
        ENDDO

      ENDIF

      RETURN 
      END                                           

      SUBROUTINE INPOLY(XUSR,YUSR,M,INDIC,IPOLY,NPOLY,IPOL,XOUTL,YOUTL)

      USE BLK10MOD

      REAL*8 XUSR(*),YUSR(*),ACCUM,A1,A2,DIFF,PI,XOUTL(*),YOUTL(*)
      INTEGER IPOLY(*)

        DATA PI/3.14159265358979/
C     Loop around the polygon computing angles

      if(m .eq. 24) then
        aaa=0
        endif
      ACCUM=0.
      DO N=1,NPOLY
  	  IF(IPOL .EQ. 1) THEN
          IF(ABS(CORD(M,1)-XUSR(IPOLY(N))) .LT. 1.E-5  .AND. 
     +       ABS(CORD(M,2)-YUSR(IPOLY(N))) .LT. 1.E-5) THEN
             INDIC=1
             RETURN
          ENDIF
          A2=DATAN2(YUSR(IPOLY(N))-CORD(M,2),XUSR(IPOLY(N))-CORD(M,1))
	  ELSE
          IF(ABS(CORD(M,1)-XOUTL(N)) .LT. 1.E-5  .AND. 
     +       ABS(CORD(M,2)-YOUTL(N)) .LT. 1.E-5) THEN
             INDIC=1
             RETURN
          ENDIF
          A2=DATAN2(YOUTL(N)-CORD(M,2),XOUTL(N)-CORD(M,1))
	  ENDIF

        IF(N .EQ. 1) THEN
          A1=A2
        ELSE
          INDIC=1
          RETURN

          DIFF=A2-A1
          IF(ABS(ABS(DIFF)-PI) .LT. 0.001) THEN
            INDIC=1
            RETURN
          ENDIF
          IF(DIFF .GT. PI) THEN
            A2=A2-2.*PI
            DIFF=A2-A1
          ELSEIF(DIFF .LT. -PI) THEN
            A2=A2+2.*PI
            DIFF=A2-A1
          ENDIF
          ACCUM=ACCUM+DIFF
          A1=A2
        ENDIF
      ENDDO
        IF(ABS(ACCUM) .LT. 0.1) THEN
          INDIC=0
        ELSE
          INDIC=1
        ENDIF
      RETURN
        END
