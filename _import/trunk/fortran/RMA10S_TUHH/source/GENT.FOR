C     Last change:  WP   29 Apr 2008   12:46 pm
CIPK  LAST UPDATE AUG 09 2005 MAJOR REVISION TO ADD XOUTL,YOUTL
CIPK  LAST UPDATE NOV 17 2004 ADD INITIAL VALUE FOR INDIC
      SUBROUTINE GENT
      USE BLK10MOD
      USE BLKABMOD
      
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKAB.COM'
      REAL WGT(8)
      CHARACTER FMT*24,BLANK*24,DLIN80*80
      DATA BLANK/'                        '/

C     Read locations

      READ(ICORDIN,'(8X,I8,A24)') NVERT,FMT
      IF(FMT .EQ. BLANK) THEN
        DO N=1,NVERT
          READ(ICORDIN,'(8X,I8,2F16.0)') J,XUSR(J),YUSR(J)
        ENDDO
      ELSE
        DO N=1,NVERT
          READ(ICORDIN,FMT) J,XUSR(J),YUSR(J)
        ENDDO
      ENDIF

      N1=1
      NPOLY=0
      DO N=1,NVERT
        IPOLY(N)=0
      ENDDO
  250 CONTINUE
      READ(ICORDIN,'(A80)') DLIN80
      IF(DLIN80(1:6) .NE. 'ENDDAT') THEN

C     Read data for outline
cipk aug05 add COUT option
      
      IF (DLIN80(1:3) .EQ. 'OUT') THEN
        IPOL=1
          N2=N1+8
          READ(DLIN80,'(8X,9I8)') (IPOLY(N),N=N1,N2)
          N1=N1+9
      ELSEIF(DLIN80(1:4) .EQ. 'COUT') THEN
        IPOL=0
          READ(DLIN80,'(8X,2F16.0)') XOUTL(N1),YOUTL(N1)
        N1=N1+1
        N2=N1
      ENDIF
        GO TO 250
      ENDIF
      IF(N1 .GT. 1) THEN

C     Determine number of points read
        IF(IPOL .EQ. 1) THEN
          DO N=1,NVERT
            IF(IPOLY(N) .NE. 0) THEN
              NPOLY=N
            ELSE
              GO TO 300
            ENDIF
          ENDDO
      ELSE
        NPOLY=N1-1
        IF(XOUTL(NPOLY) .NE. XOUTL(1)  .OR.
     +       YOUTL(NPOLY) .NE. YOUTL(1)) THEN
          NPOLY=NPOLY+1
          XOUTL(NPOLY) =XOUTL(1)
          YOUTL(NPOLY) =YOUTL(1)
        ENDIF
        GO TO 320
      ENDIF

  300   CONTINUE
        IF(IPOLY(NPOLY) .NE. IPOLY(1)) THEN
          NPOLY=NPOLY+1
          IPOLY(NPOLY)=IPOLY(1)
        ENDIF
      ENDIF

  320 CONTINUE

C     Triangulate locations
      !REMOVE FOR RMA·KALYPSO
      !nis,nov08: Remove writing to obsolete unit imeshout
      !imeshout is obsolete
      !CALL DELN2(XUSR,YUSR,NOPEL,NELTS,NVERT,IMESHOUT) 
      CALL DELN2(XUSR,YUSR,NOPEL,NELTS,NVERT) 
      !-

C     Interpolate to get weighting factors

      ISWT=-1
      ISWT=0
        ITIME=0
      DO M=1,NP
      
       if(m .eq. 8798) then
         aaa=0
       endif      
        DO J=1,3
            NDWT(J)=0
          ENDDO

C       If there is polygon test if point is inside

CIPK NOV04 ADD INITIAL VALUE FOR INDIC
        INDIC=1
C        NPOLY=0
        IF(NPOLY .GT. 0) THEN
            CALL INPOLY(XUSR,YUSR,M,INDIC,IPOLY,NPOLY,IPOL,XOUTL,YOUTL)
          ENDIF
          IF(INDIC .EQ. 0) GO TO 350
        CALL TRIANINT(XUSR,YUSR,NOPEL,NVERT,NELTS,M,ISWT,ITIME,WGT,NDWT)
        DO J=1,3
          WAT(M,J)=WGT(J)
            NODWT(M,J)=NDWT(J)
          ENDDO
  350   CONTINUE
      ENDDO

C     Save weigthing factors

      HEADER(1:10)='WEIGHT    ' 
        IF(IOWGT .GT. 0) THEN
          WRITE(IOWGT,'(A1000)') HEADER
          DO M=1,NP
         WRITE(IOWGT,'(8X,4I8,3F8.5)') 
     +   M,(NODWT(M,J),J=1,3),(WAT(M,J),J=1,3)
          ENDDO
      ENDIF
      !REMOVE FOR RMA·KALYPSO
      !nis,nov08: Remove writing to obsolete unit iobnwgt
      !iobnwgt is obsolete
      RETURN
      END

      !REMOVE FOR RMA·KALYPSO
      !nis,nov08: Remove writing to obsolete unit imeshout
      !imeshout is obsolete
      !SUBROUTINE DELN2(XUSR,YUSR,NOPEL,NELTS,NVERT,IMESHOUT) 
      SUBROUTINE DELN2(XUSR,YUSR,NOPEL,NELTS,NVERT) 
      !-

      USE BLKCDMOD

CIPK AUG05      INCLUDE 'PARAM.COM'
CIPK AUG05      INCLUDE 'BLKCD.COM'
      REAL*8 XUSR(*),YUSR(*)
      INTEGER NOPEL(3,*)
      CHARACTER*7 ENDDATA
      INTEGER NEDGE,NGAP
      DATA NEDGE/0/,NGAP/0/
      VOID = -1.E10


c     Sort points into ascending x order

      CALL SORTDB(XUSR,NKEY,NVERT) 


c     Get location of supertriangle                                     
                                                                        
      iprt=0
                                                                        
      call supert(XUSR,YUSR,NOPEL,XCEN,YCEN,RADS,NVERT) 

      NELTS=1 
                                                                        
      NVERTM=NVERT-3 

c     Loop on the vertices                                              
                                                                        
      DO NN=1,NVERT-3 

c     process next point                                                                        

      IF(NN .EQ. 3003) THEN
        AAA=0
        ENDIF
         N=NKEY(NN) 

c     Skip out if inactive point

         IF(N .EQ. 0) GO TO 500
         IF(NN .LT. NVERTM) THEN 
           DO KK=NN+1,NVERTM 
             K=NKEY(KK) 
             IF(K .NE. 0) THEN 
               IF(XUSR(N) .EQ. XUSR(K)) THEN 
                 IF(YUSR(N) .EQ. YUSR(K)) THEN 
                   NKEY(KK)=0 
                 ENDIF 
               ELSE 
                 GO TO 200 
               ENDIF 
             ENDIF 
  200        CONTINUE 
           ENDDO 
         ENDIF 
                                                                        
c     Set edge buffers to zero                                          
                                                                        
         IF(NEDGE .GT. 0) THEN 
           DO J=1,NEDGE 
             IEDGE(1,J)=0 
             IEDGE(2,J)=0 
           END DO 
         ELSE 
           DO J=1,100 
             IEDGE(1,J)=0 
             IEDGE(2,J)=0 
           END DO 
         ENDIF 
         NEDGE=0 
                                                                        
c     test for point in circumcircle                                    
                                                                        
         DO J=1,NELTS 
           CALL INSIDCIRC(XUSR,YUSR,XCEN,YCEN,RADS,J,N,ISWT) 
                                                                        
c     If inside process edges                                           
                                                                        
           IF(ISWT .EQ. 1) THEN 
             CALL PROCESS(J,IEDGE,NEDGE,NOPEL,IGAP,NGAP)                                 
           ENDIF 
         END DO 
                                                                        
c     Setup to form new triangles                                       

         CALL SETEDG(IEDGE,NEDGE)
                                                              
c     Now form triangles as needed                                      
                                                                        

         DO J=1,NEDGE 
           IF(IEDGE(1,J) .NE. 0) THEN 
             CALL FORMT(XUSR,YUSR,XCEN,YCEN,RADS,J,N,NOPEL,IEDGE
     +             ,IGAP,NGAP,NELTS)                           
           ENDIF 
         END DO 

         NEDGE=0 
  500    continue 
      END DO 
                                                                        
c     Get rid of elements from super point                              
                                                                        
      CALL RIDPOINT(XCEN,YCEN,RADS,NVERT,NOPEL,NELTS) 
c 
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to obsolete unit imeshout
!imeshout is obsolete
!-

      RETURN 
      END SUBROUTINE 

C************************************************************************************************

      SUBROUTINE SUPERT(XPT,YPT,NOPEL,XCEN,YCEN,RADS,NVERT) 
                                                                        
      INTEGER NOPEL(3,*)
      REAL*8 XPT(*),YPT(*),XCEN(*),YCEN(*),RADS(*)

      REAL*8 XMINM,YMINM,X45 
      DATA VDX9/-9.E9/

c     Find minimum x and y                                              

      xminm=1.e20 
      yminm=1.e20 
      x45=-1.e20 
      DO J=1,NVERT 
        IF(XPT(J) .GT. VDX9) THEN
          if(xminm .GT. XPT(j) ) then 
            xminm=XPT(j) 
          end if 
          IF(yminm .GT. YPT(j)) then 
            yminm=YPT(j) 
          endif 
        ENDIF
      ENDDO 

c     Find max at 45 degrees                                            

      DO J=1,NVERT 
        IF(XPT(J) .GT. VDX9) THEN
          X45T=((XPT(J)-XMINM)+(YPT(J)-YMINM))/1.414 
          IF(x45 .LT.  X45T) THEN 
            X45=X45T 
          ENDIF 
        ENDIF
      END DO 
      XPT(NVERT+1)=XMINM-1. 
      YPT(NVERT+1)=YMINM-1. 
      XPT(NVERT+2)=XMINM+1.414*X45+0.707 
      YPT(NVERT+2)=YMINM-1. 
      XPT(NVERT+3)=XMINM-1. 
      YPT(NVERT+3)=YMINM+1.414*X45+0.707 
      NELT=1 
      NOPEL(1,1)=NVERT+1 
      NOPEL(2,1)=NVERT+2 
      NOPEL(3,1)=NVERT+3 
      NVERT=NVERT+3 
      CALL CCENTRE(XPT(NOPEL(1,1)),XPT(NOPEL(2,1)),XPT(NOPEL(3,1))      
     +,YPT(NOPEL(1,1)),YPT(NOPEL(2,1)),YPT(NOPEL(3,1))                  
     +,XCEN(1),YCEN(1),RADS(1))                                         
      RETURN 
      END SUBROUTINE 
                                                                        
C*********************************************************************************************

      SUBROUTINE INSIDCIRC(XPT,YPT,XCEN,YCEN,RADS,J,N,ISWT) 
                                                                        
c     Test for point inside circumcircle                                
                                                                                   
      REAL*8 XPT(*),YPT(*),XCEN(*),YCEN(*),RADS(*)
      REAL*8 DISQ 
                                                                        
c     Get the distance for this element                                 
                                                                        
      DISQ=(XCEN(J)-XPT(N))**2+(YCEN(J)-YPT(N))**2 
                                                                        
c     Test against the radius                                           
                                                                        
      IF(DISQ .GT. RADS(J)*RADS(J)) THEN 
        ISWT=0 
      ELSE 
        ISWT=1 
      ENDIF 
      RETURN 
      END SUBROUTINE 

C*********************************************************************************************
                                                                        
      SUBROUTINE PROCESS(J,IEDGE,NEDGE,NOPEL,IGAP,NGAP)
                                                                        
c     Drop triangle and form edge buffers                               
                                                                        
      INTEGER IEDGE(2,*),NOPEL(3,*),IGAP(*)
                                                                                  
      NEDGE=NEDGE+3 
      IEDGE(1,NEDGE-2)=NOPEL(1,J) 
      IEDGE(1,NEDGE-1)=NOPEL(2,J) 
      IEDGE(1,NEDGE)  =NOPEL(3,J) 
      IEDGE(2,NEDGE-2)=NOPEL(2,J) 
      IEDGE(2,NEDGE-1)=NOPEL(3,J) 
      IEDGE(2,NEDGE)  =NOPEL(1,J) 
      NOPEL(1,J)=0 
      NOPEL(2,J)=0 
      NOPEL(3,J)=0 
      NGAP=NGAP+1 
      IGAP(NGAP)=J 
      RETURN 
      END SUBROUTINE 
                                                                        
C********************************************************************************************

      SUBROUTINE FORMT(XPT,YPT,XCEN,YCEN,RADS,J,N,NOPEL,IEDGE
     +,IGAP,NGAP,NELTS)                                   
                                                                        
c     Form the triangle                                                 
                          
      INTEGER IGAP(*),NOPEL(3,*),IEDGE(2,*)
      REAL*8 XPT(*),YPT(*),XCEN(*),YCEN(*),RADS(*)

      IF(NGAP .GT. 0) THEN 
        K=IGAP(NGAP) 
        NGAP=NGAP-1 
      ELSE 
        NELTS=NELTS+1 
        K=NELTS 
      ENDIF 
      NOPEL(1,K)=IEDGE(1,J) 
      NOPEL(2,K)=IEDGE(2,J) 
      NOPEL(3,K)=N 
                                                                        
c    Now get circumcircle data                                          
      CALL CCENTRE(XPT(NOPEL(1,K)),XPT(NOPEL(2,K)),XPT(NOPEL(3,K))      
     +,YPT(NOPEL(1,K)),YPT(NOPEL(2,K)),YPT(NOPEL(3,K))                  
     +,XCEN(K),YCEN(K),RADS(K))                                         
      RETURN 
      END SUBROUTINE 
                                                                        
C***********************************************************************************************

      SUBROUTINE CCENTRE(X1,X2,X3,Y1,Y2,Y3,XC,YC,RC) 
                                                                        
c     get circumcentre and radius                                       
                                                                        
      REAL*8 X1,Y1,X2,Y2,X3,Y3,A,B,C,D,AF,R1,R2,RC,XC,YC 
      A=X2-X1 
      B=Y2-Y1 
      C=X3-X1 
      D=Y3-Y1 
      AF=2.*(B*C-A*D) 
      R1=(-D*(A**2+B**2) + B*(C**2+D**2))/AF 
      R2=( C*(A**2+B**2) - A*(C**2+D**2))/AF 
      RC=SQRT(R1**2+R2**2) 
      XC=X1+R1 
      YC=Y1+R2 
      RETURN 
      END SUBROUTINE 
                                                                        
C**********************************************************************************************

      SUBROUTINE RIDPOINT(XCEN,YCEN,RADS,NVERT,NOPEL,NELTS) 
                                                                        
      INTEGER NOPEL(3,*)
      REAL*8 XCEN(*),YCEN(*),RADS(*)
                                                                       
      NCOUNT=0
      DO N=1,NELTS 
        DO K=1,3 
          IF(NOPEL(K,N) .GT. NVERT-3) THEN 
            DO L=1,3 
              NOPEL(L,N)=0 
            ENDDO 
            GO TO 500
          ENDIF 
        ENDDO
        NCOUNT=NCOUNT+1
        DO K=1,3
          NOPEL(K,NCOUNT)=NOPEL(K,N) 
        ENDDO
        XCEN(NCOUNT)=XCEN(N)
        YCEN(NCOUNT)=YCEN(N)
        RADS(NCOUNT)=RADS(N)
  500   CONTINUE
      ENDDO 
      NELTS=NCOUNT
      RETURN 
      END                                           
    
C*****************************************************************************************
                                                                    
      SUBROUTINE SORTDB(A,NKEY,N) 
c*********************************** .....SORT.....                     
c-                                                                      
c......SORT IS A SIMPLE SHELL SORT ROUTINE  IN DOUBLE PRECISION         
c-                                                                      
c     SHELL SORT                                                        
      SAVE 
c                                                                       
cIPK JAN94      INTEGER*2 NKEY                                          
      REAL*8 A(*) 
      INTEGER NKEY(*) 
                                                                        
      IF(N.LT.2) RETURN 
      DO 90 J=1,N 
        NKEY(J)=J 
   90 END DO 
      ID = N 
  100 ID = ID / 2 
  110 IB = 1 
  120 GO TO 200 
  130 IB = IB + 1 
      IF( IB .LE. ID ) GO TO 200 
      IF( ID .GT. 1 ) GO TO 100 
      RETURN 
  200 I = IB 
  210 K = I + ID 
  220 IF( A(NKEY(I)) .LE. A(NKEY(K)) ) GO TO 250 
      NKT = NKEY(K) 
      NKEY(K) = NKEY(I) 
      J = I 
  230 K = J - ID 
      IF( K .LT. 1 ) GO TO 240 
      IF( A(NKT) .GT. A(NKEY(K)) ) GO TO 240 
      NKEY(J) = NKEY(K) 
      J = K 
      GO TO 230 
  240 NKEY(J) = NKT 
  250 I = I + ID 
      IF( I + ID .LE. N ) GO TO 210 
      GO TO 130 
      END                                           

C******************************************************************************************

      SUBROUTINE SETEDG(IEDGE,NEDGE)
 
      INTEGER IEDGE(2,*)                                                                             
c     Setup to form new triangles                                       

      DO J=1,NEDGE 
        IF(J .LT. NEDGE) THEN 
          DO K=J+1,NEDGE 
            IF(IEDGE(1,K) .EQ. IEDGE(1,J)) THEN 
              IF(IEDGE(2,K) .EQ. IEDGE(2,J)) THEN 
                IEDGE(1,J)=0 
                IEDGE(2,J)=0 
                IEDGE(1,K)=0 
                IEDGE(2,K)=0 
              ENDIF 
            ELSEIF(IEDGE(1,K) .EQ. IEDGE(2,J)) THEN 
              IF(IEDGE(2,K) .EQ. IEDGE(1,J)) THEN 
                IEDGE(1,J)=0 
                IEDGE(2,J)=0 
                IEDGE(1,K)=0 
                IEDGE(2,K)=0 
              ENDIF 
            ENDIF 
          ENDDO 
        ENDIF 
      ENDDO
       
      RETURN
      END                                                                        
