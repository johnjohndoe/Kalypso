cipk  last update oct 2 2003 compute embankment width
cipk  new routine Mar 1 2000
      SUBROUTINE GTWFLOWS
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSUBMOD
*-
*...... Routine to establish flows for weir elements (type 998)
*-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKSUB.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
!NiS,jul06: Consistent data types for passing paramters
      REAL(KIND=8) :: WH1, WH2
!-

      DATA ITIM/0/

c
c     loop on elements looking for type 998 elements to process
c
      DO N=1,NE
        IF(IMAT(N) .EQ. 998) THEN
          IF(ITIM .EQ. 0) THEN
            IWFLOW(NOP(N,1))=1
            IWFLOW(NOP(N,2))=1
            IWFLOW(NOP(N,3))=1
            IWFLOW(NOP(N,5))=1
            IWFLOW(NOP(N,6))=1
            IWFLOW(NOP(N,7))=1
          ENDIF
c
c     work around elements to get flows
c     convention is flow from the system is positive
c
          wh1=(vold(3,nop(n,1))+vel(3,nop(n,1)))/2.
          wh2=(vold(3,nop(n,7))+vel(3,nop(n,7)))/2.
          ws1=(wsll(nop(n,1))+hol(nop(n,1))+ado(nop(n,1)))/2.
          ws2= (wsll(nop(n,7))+hol(nop(n,7))+ado(nop(n,7)))/2.
          wv1=sqrt(vold(1,nop(n,1))**2+vold(2,nop(n,1))**2)
          wv2=sqrt(vold(1,nop(n,7))**2+vold(2,nop(n,7))**2)
          wec= whgt(nop(n,1))
          wln= wlen(nop(n,1))
CIPK OCT03  ADD WIDEM
	    WIDEM=SQRT((CORD(NOP(N,1),1)-CORD(NOP(N,7),1))**2+
     +          (CORD(NOP(N,1),2)-CORD(NOP(N,7),2))**2)
          CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,WIDEM)
          wflolw(nop(n,1))=wflow(nop(n,1))
          wflolw(nop(n,7))=wflow(nop(n,7))
          wflow(nop(n,1))=(q1T+wflolw(nop(n,1)))/2.
          wflow(nop(n,7))=(q1T+wflolw(nop(n,7)))/2.
          wh1=(vold(3,nop(n,2))+vel(3,nop(n,2)))/2.
          wh2=(vold(3,nop(n,6))+vel(3,nop(n,6)))/2.
          ws1=(wsll(nop(n,2))+hol(nop(n,2))+ado(nop(n,2)))/2.
          ws2= (wsll(nop(n,6))+hol(nop(n,6))+ado(nop(n,6)))/2.
          wv1=sqrt(vold(1,nop(n,2))**2+vold(2,nop(n,2))**2)
          wv2=sqrt(vold(1,nop(n,6))**2+vold(2,nop(n,6))**2)
          wec= whgt(nop(n,2))
          wln= wlen(nop(n,2))
CIPK OCT03  ADD WIDEM
	    WIDEM=SQRT((CORD(NOP(N,1),2)-CORD(NOP(N,6),1))**2+
     +          (CORD(NOP(N,2),2)-CORD(NOP(N,6),2))**2)
          CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,WIDEM)
          wflolw(nop(n,2))=wflow(nop(n,2))
          wflolw(nop(n,6))=wflow(nop(n,6))
          wflow(nop(n,2))=(q1T+wflolw(nop(n,2)))/2.
          wflow(nop(n,6))=(q1T+wflolw(nop(n,6)))/2.
          wh1=(vold(3,nop(n,3))+vel(3,nop(n,3)))/2.
          wh2=(vold(3,nop(n,5))+vel(3,nop(n,5)))/2.
          ws1=(wsll(nop(n,3))+hol(nop(n,3))+ado(nop(n,3)))/2.
          ws2= (wsll(nop(n,5))+hol(nop(n,5))+ado(nop(n,5)))/2.
          wv1=sqrt(vold(1,nop(n,3))**2+vold(2,nop(n,3))**2)
          wv2=sqrt(vold(1,nop(n,5))**2+vold(2,nop(n,5))**2)
          wec= whgt(nop(n,3))
          wln= wlen(nop(n,3))
CIPK OCT03  ADD WIDEM
	    WIDEM=SQRT((CORD(NOP(N,3),1)-CORD(NOP(N,5),1))**2+
     +          (CORD(NOP(N,3),2)-CORD(NOP(N,5),2))**2)
          CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,WIDEM)
          wflolw(nop(n,3))=wflow(nop(n,3))
          wflolw(nop(n,5))=wflow(nop(n,5))
          wflow(nop(n,3))=(q1T+wflolw(nop(n,3)))/2.
          wflow(nop(n,5))=(q1T+wflolw(nop(n,5)))/2.
        ENDIF
      ENDDO
      ITIM=1
      RETURN
      END
