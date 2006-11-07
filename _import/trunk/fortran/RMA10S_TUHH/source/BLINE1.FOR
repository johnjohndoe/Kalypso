C     Last change:  NiS  12 May 2006   10:22 pm
CIPK  LAST UPDATE JUNE 27 2005 ADD CONTROL STRUCTURES
cipk  last update sep20 2001 revise test for NSPL to allow for NSPL=2
CIPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
cipk  last update July 18 199 fix problem with combined SL and SN data
C     Last change:  IPK   3 Oct 98    4:08 pm
cipk  last update Sep 3 1996   Add ocean boundary capability
      SUBROUTINE BLINE(NTR) 
!NiS,may06,com:
!NTR shows calling position
!NTR.eq.0 steady start
!NTR.eq.1 dynamic start
!NTR.gt.1 during iteration
!-

      USE BLK10MOD
      USE BLKSUBMOD
      SAVE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
      INCLUDE 'BLKH.COM'
CIPK JUN05
CIPK AUG05      INCLUDE 'BLKSUB.COM'
C-
      DIMENSION DL(2,2),FMT(2),SRT(3)
     +   
      ALLOCATABLE IFORM(:),IOD(:),ION(:)

C-
      DATA PI2/1.570796/,VOIDP/-1.E19/,ITIMEH/0/
C
      IF(ITIMEH .EQ. 0) THEN
        ITIMEH=1
        ALLOCATE (IFORM(MAXP),IOD(MAXP),ION(MAXP))
        IFORM=0
        IOD=0
        ION=0
      ENDIF
      DO 180 N=1,NP
      IFORM(N)=0
      IOD(N)=0
  180 CONTINUE
      !NiS,may06,com: during iteration NTR.gt.1
      IF(NTR .GT. 1) GO TO 240
cipk sep96 add definitions
      DO N=1,NP
        ION(N)=1
        NFIXSAV(N)=NFIX(N)
	    NFIXSV1(N)=NFIX1(N)
      ENDDO
      DO 230 N=1,NPM
  230 NFIXK(N)=NFIX(N)
  240 CONTINUE
CIPK  SEP96 DEFINE NFIXSAV
      DO N=1,NP
        NFIX(N)=NFIXSAV(N)
	    NFIX1(N)=NFIXSV1(N)
      ENDDO
C-
C.....  COMPUTE BOUNDARY MID-SIDE NODES
C-
      DO 260 N=1,NPM
        IBN(N)=0
        XSLP(N)=0.
        YSLP(N)=0.
  260 CONTINUE
      DO 280 N=1,NEM
        NM=IMAT(N)
        IF(NM .LT. 1) GO TO 280
        IF(MOD(NM,100) .GT. 90) GO TO 280
        IF(NM .GT. 900  .AND. NM .LT. 1000) GO TO 265
        IF(NM .GT. 1000  .AND. NM .LT. 5000) GO TO 280
  265   NCN=NCRN(N)
        IF(NCN .EQ. 5  .AND.  NM .LT. 900) NCN=3
        IF(NCN .EQ. 3  .OR.   NM .GT. 900) THEN
          MCL=1
        ELSE
          MCL=2
        ENDIF
CIPK JUN05
        DO M=MCL,NCN,MCL
          K=NOPS(N,M)
CIPK JUN05
          IF(K .GT. 0) THEN
CIPK DEC00 ALLOW FOR GATE STRUCTURE
CIPK JUN05
            IF(NM .GT. 900 .and. nm .lt. 5000) THEN
              IF( IGTP(N) .EQ. 0  .AND.  NFCTP(N) .EQ. 0) THEN
                IBN(K)=3
              ELSE
CIPK JUN05
                IBN(K)=IBN(K)+10
                NFIXK(K)=01000
              ENDIF
            ELSE
              IF(MCL .EQ. 1) IOD(K)=1
              IBN(K)=IBN(K)+1
            ENDIF
CIPK JUN05
          ENDIF
        ENDDO
  280 CONTINUE
C-
C...... Reset IBN for merger nodes
C-
      DO 290 N=1,NEM
        IF(NETYP(N) .EQ. 18) THEN
          J=NOP(N,19)
          IF (J .GT. 0)  IBN(J)=2
        ENDIF
  290 CONTINUE
      !NiS,may06,com: for every element
      DO 600 N=1,NEM

CIPK OCT98 REVISE FOR F90
        MTYP=IMAT(N)
        IF(MTYP .LT. 1) GO TO 600
        IF(MOD(MTYP,100) .GT. 90) GO TO 600

CIPK DEC00 ALLOW FOR GATE STRUCTURE
CIPK JUN05
        IF(MTYP .GT. 900  .AND.  MTYP .LT. 5000  .AND. IGTP(N) .EQ. 0
     +    .AND.  NFCTP(N) .EQ. 0)
     +     GO TO 600
        NCN=NCRN(N)
        IF(NCN .EQ. 5) NCN=3
        DO 590 M=2,NCN,2
          N2=NOPS(N,M) !NiS,may06: Midsidenode
          N1=NOPS(N,M-1) !NiS,may06: first node
          N3=NOPS(N,1) !NiS,may06: second node; midside of 2D-part, if transition element
          IF(M .LT. NCN) N3=NOPS(N,M+1)
C-
C...... Skip slope calculation for sides that have fixed flow or elevation
C-
          IF(IBN(N2) .EQ. 1) THEN
C-
C...... Get direction of outward normal for 2-d plan elements
C-
        !NiS,may06,com: 2D/3D-part: transition elements with normally ncn.eq.5 are here declared with ncn.eq.3, so only 1D
            IF(NCN .GT. 3) THEN
              CALL OUTNRM(CORD(N1,1),CORD(N1,2),CORD(N2,1),CORD(N2,2),
     +                CORD(N3,1),CORD(N3,2),SRT,1)
        !NiS,may06,com: 1D-part
            ELSE
              CALL OUTNRM(CORD(N1,1),CORD(N1,2),CORD(N2,1),CORD(N2,2),
     +                CORD(N3,1),CORD(N3,2),SRT,2)
            ENDIF
            VOUTN(N1)=SRT(1)
            VOUTN(N2)=SRT(2)
            VOUTN(N3)=SRT(3)
            NFK=NFIXK(N2)/100
            !NiS,may06: following is related to the first three digits for boundary condition of nodal input-line
            IF(NFK .LT. 113  .AND.  NFK .NE. 2) THEN

CIPK JUN05
              N1=NOP(N,M-1)
              N3=NOP(N,1)
              IF(M .LT. NCN) N3=NOP(N,M+1)
              if(ncn .lt. 6) then
                IF(IBN(N1) .GE. 10) IBN(N1)=15
                IF(IBN(N3) .GE. 10) IBN(N3)=15
              else
                if(ibn(n1) .eq. 10) ibn(n1)=15
                if(ibn(n3) .eq. 10) ibn(n3)=15
              endif

          H1=VEL(3,N1)
          H3=VEL(3,N3)
          DL(2,2)=CORD(N3,1)-CORD(N1,1)
          DL(2,1)=CORD(N3,2)-CORD(N1,2)
          IF(CORD(N2,1) .GT. VOIDP  .AND.  NCN .NE. 3) THEN
            DL(1,2)=CORD(N2,1)-CORD(N1,1)
            DL(1,1)=CORD(N2,2)-CORD(N1,2)
          ELSE
            DL(1,2)=DL(2,2)/2.
            DL(1,1)=DL(2,1)/2.
          ENDIF
          IF(IFORM(N1) .EQ. 1) THEN
            FMT(1)=-1.0
          ELSE
            FMT(1)=1.0
          ENDIF
          IF(IFORM(N3) .EQ. 3) THEN
            FMT(2)=-1.0
          ELSE
            FMT(2)=1.0
          ENDIF
          IFORM(N1)=1
          IFORM(N3)=3
          DO 580 NG=1,4
            TEMP1=(DNAL(2,NG)*DL(1,1)+DNAL(3,NG)*DL(2,1))/2.
            TEMP2=(DNAL(2,NG)*DL(1,2)+DNAL(3,NG)*DL(2,2))/2.
            H=(H1+AFACT(NG)*(H3-H1))*HFACT(NG)
            IF(NCN .EQ. 3) H=1.0
            YSLP(N1)=YSLP(N1)+TEMP1*H*XNAL(1,NG)*FMT(1)
            YSLP(N2)=YSLP(N2)+TEMP1*H*XNAL(2,NG)
            YSLP(N3)=YSLP(N3)+TEMP1*H*XNAL(3,NG)*FMT(2)
            XSLP(N1)=XSLP(N1)+TEMP2*H*XNAL(1,NG)*FMT(1)
            XSLP(N2)=XSLP(N2)+TEMP2*H*XNAL(2,NG)
            XSLP(N3)=XSLP(N3)+TEMP2*H*XNAL(3,NG)*FMT(2)
  580     CONTINUE
        ENDIF
      ENDIF
  590 CONTINUE
  600 CONTINUE

CIPK JUN05  SETUP FOR SUBMERGENGE
      !NiS,may06,com: NTR shows position of calling this subroutine
      !Nis,may06,com: It shows the task of the subroutine
      IF(NTR .GT. 0) THEN
        CALL SUBSET
      ENDIF

C-
      DO 701 N=1,NPM
      NFIX(N)=NFIXK(N)
      IF (ITEQV(MAXN) .EQ. 2)  GOTO 701

CIPK OCT98  CONVERT TO F90
      NFTYP=NFIXK(N)
      NQB=MOD(NFTYP,100)
      NLFT=NFTYP/100
      IF(NLFT .EQ. 0) ALFA(N)=0.
      IF(MOD(NLFT,10) .EQ. 2) THEN
C-
C......   THESE ARE ONE-D ELEMENTS TYPE 102 012 OR 002
C......   GET     ALFA   AND SET FORM FOR NFIX
C-
        IF(NLFT .EQ. 102) THEN
C-
C......   SPECIAL CASE OF VERTICAL LINE RESET 102 TO 012 AND CHANGE ALFA
C-
          ALFA(N)=PI2
          NFIX(N)=01200+NQB
        ELSE
C-
          IF(XSLP(N) .NE. 0.) THEN
            ALFA(N)=ATAN(YSLP(N)/XSLP(N))
            NFIX(N)=01200+NQB
          ELSE
            IF(YSLP(N) .NE. 0.) THEN
              ALFA(N)=PI2
              NFIX(N)=01200+NQB
            ENDIF
          ENDIF
        ENDIF
cycw aug96 refine test to permit 012 case to be testes
c        IF(IOD(N) .EQ. 1  .AND.  NTR .GT. 0) THEN
        IF((IOD(N).EQ.1.OR.NFIX(N)/100.EQ.12).AND.NTR.GT.0) THEN
C-
C.......   ADJUST VELOCITY COMPONENTS FOR NEW ANGLE
C-
          IF(VEL(1,N) .EQ. 0.) ALOLD=PI2
          IF(VEL(1,N) .NE. 0.) ALOLD=ATAN(VEL(2,N)/VEL(1,N))
          IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND.
     +       ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
          CSX=COS(ALOLD)
          SSX=SIN(ALOLD)
          CSN=COS(ALFA(N))
          SSN=SIN(ALFA(N))
          VT=VEL(1,N)*CSX+VEL(2,N)*SSX
          VEL(1,N)=VT*CSN
          VEL(2,N)=VT*SSN
          VT=VOLD(1,N)*CSX+VOLD(2,N)*SSX
          VOLD(1,N)=VT*CSN
          VOLD(2,N)=VT*SSN
          VT=VDOT(1,N)*CSX+VDOT(2,N)*SSX
          VDOT(1,N)=VT*CSN
          VDOT(2,N)=VT*SSN
          VT=VDOTO(1,N)*CSX+VDOTO(2,N)*SSX
          VDOTO(1,N)=VT*CSN
          VDOTO(2,N)=VT*SSN
cycw aug96 add logic to 
C Correct velocities along boundary for subsurface nodes

          K = NREF(N) + 1
          IF (K .GT. 1  .AND.  NDEP(N) .GT. 0) THEN

            L = K + NDEP(N)-2
            DO M=K,L
              IF(NFIX(M)/1000 .EQ. 0  .OR. NFIX(M)/1000 .EQ. 11) THEN
                ALFA(M)=0.
                GO TO 609
              ENDIF
              ALFA(M) = ALFA(N)
              IF (VEL(1,m) .EQ. 0.) ALOLD = PI2
              IF (VEL(1,m) .NE. 0.) 
     &                        ALOLD = ATAN(VEL(2,m)/VEL(1,m))
              IF (ABS(ALFA(m)-ALOLD) .GT. PI2  .AND.
     +                 ABS(ALFA(m)-ALOLD) .LT. 3.*PI2)
     &                        ALOLD = ALOLD + 2.*PI2
              CSX = COS(ALOLD)
              SSX = SIN(ALOLD)
              CSN = COS(ALFA(m))
              SSN = SIN(ALFA(m))
              VT = VEL(1,m)*CSX + VEL(2,m)*SSX
              VEL(1,m) = VT*CSN
              VEL(2,m) = VT*SSN
              VT = VOLD(1,m)*CSX + VOLD(2,m)*SSX
              VOLD(1,m) = VT*CSN
              VOLD(2,m) = VT*SSN
              VT = VDOT(1,m)*CSX + VDOT(2,m)*SSX
              VDOT(1,m) = VT*CSN
              VDOT(2,m) = VT*SSN
              VT = VDOTO(1,m)*CSX + VDOTO(2,m)*SSX
              VDOTO(1,m) = VT*CSN
              VDOTO(2,m) = VT*SSN
  609         CONTINUE
            ENDDO
          ENDIF
cycw aug96 end additions
        ENDIF
***
      ELSE
        IF(NFIX(N) .LE. 10010) GO TO 610
        IF(IOD(N) .EQ. 0) GO TO 700
C-
C..... ONLY ONE-D LEFT  NFIX = 11 OR 31
C-
        IF(NFIX(N) .GT. 11000) THEN
          GO TO 630
        ELSE
          ALFA(N)=0.
          GO TO 700
        ENDIF
  610   CONTINUE
CIPK JUN05
        IF(ALFAK(N) .NE. 0.  .AND. IBN(N) .NE. 15) THEN
          ALFA(N)=ALFAK(N)
        ELSEIF(XSLP(N) .NE. 0.) THEN
          ALFA(N)=ATAN(YSLP(N)/XSLP(N))
        ELSE
          IF(YSLP(N) .EQ. 0.) GO TO 700
          ALFA(N)=PI2
        ENDIF
        NFIX(N)=01000+NQB
cipk juN05
        IF(IBN(N) .EQ. 15  .OR.  IBN(N) .EQ. 10) THEN
          DO 620 M=1,NE
            IF(IMAT(M) .EQ. 999) GO TO 620
            IF(IMAT(M) .GT. 903) THEN
              NCN=NCORN(M)
              IF(NCN .EQ. 8) THEN
                DO 615 K=1,8
                  IF(NOP(M,K) .EQ. N) THEN
                    IMT=IMAT(M)-900
	              IF(NJT(IMT).NE. 10) THEN
                      IF(QD(IMT) .GE. 0.) THEN
                        IF(QD(IMT)-ALFA(N) .LT. 1.5708) GO TO 630
                        ALFA(N)=ALFA(N)+3.14159
                      ELSE
                        IF(ALFA(N)-QD(IMT) .LT. 1.5708) GO TO 630
                        ALFA(N)=ALFA(N)-3.14159
                      ENDIF
	              ELSE
	                
                      AG1=ATAN2(CORD(NOP(M,6),2)-CORD(NOP(M,2),2),
     +                 CORD(NOP(M,6),1)-CORD(NOP(M,2),1))
                      AZ=AG1-ALFA(N)      
                      IF(AG1 .GE. 0.) THEN
                        IF(AG1-ALFA(N) .LT. 1.570795  .OR.
     +                     AG1-ALFA(N) .GT. 4.712385) GO TO 630
	
	                  if(icyc .lt. 2 .and .maxn .eq. 1) then
	                   write(75,*)'redirect weir',n,alfa(n),ag1,ibn(n)
                         write(75,*) AZ,nfix(n),wsll(n),whgt(n),ISUBM(N)
     +                   ,VEL(1,N),VEL(2,N)
	                  endif
                        ALFA(N)=ALFA(N)+3.14159
                      ELSE
                        IF(ALFA(N)-AG1 .LT. 1.570795 .OR.
     +                     ALFA(N)-AG1 .GT. 4.712385) GO TO 630
	                  if(icyc .lt. 2  .and. maxn .eq. 1) then
	                   write(75,*)'redirect weir',n,alfa(n),ag1,ibn(n)
                         write(75,*) AZ,nfix(n),wsll(n),whgt(n),ISUBM(N)
     +                   ,VEL(1,N),VEL(2,N)
     	                  endif
                        ALFA(N)=ALFA(N)-3.14159
                      ENDIF

	              ENDIF
                    GO TO 630
                  ENDIF
  615           CONTINUE
              ENDIF
            ENDIF
  620     CONTINUE


cipk mar00
        ELSEIF(IBN(N) .GT. 10) then
CIPK JUL00 REVISE TEST
          if(ntr .ne. 0) then
            if(isubm(n) .eq. 1) then
              nfix(n)=0
            endif
          endif
        ENDIF
 
 
  630   CONTINUE
        IF(NTR .GT. 0) THEN
          IF(VEL(1,N) .EQ. 0.) ALOLD=PI2
          IF(VEL(1,N) .NE. 0.) ALOLD=ATAN(VEL(2,N)/VEL(1,N))
cipk JUN05 test for submerged case

          IF(ISUBM(N) .EQ. 1  .AND. 
     +      (IBN(N) .EQ. 11  .OR.  IBN(N) .EQ. 20)) THEN
            GO TO 700
          ENDIF
          IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND.
     +       ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
          CSX=COS(ALOLD)
          SSX=SIN(ALOLD)
          CSN=COS(ALFA(N))
          SSN=SIN(ALFA(N))
          VT=VEL(1,N)*CSX+VEL(2,N)*SSX
          VEL(1,N)=VT*CSN
          VEL(2,N)=VT*SSN
          VT=VOLD(1,N)*CSX+VOLD(2,N)*SSX
          VOLD(1,N)=VT*CSN
          VOLD(2,N)=VT*SSN
          VT=VDOT(1,N)*CSX+VDOT(2,N)*SSX
          VDOT(1,N)=VT*CSN
          VDOT(2,N)=VT*SSN
          VT=VDOTO(1,N)*CSX+VDOTO(2,N)*SSX
          VDOTO(1,N)=VT*CSN
          VDOTO(2,N)=VT*SSN
cycw Aug96 
C Correct velocities along boundary for subsurface nodes

          K = NREF(N) + 1
          IF (K .GT. 1  .AND.  NDEP(N) .GT. 0) THEN
            L = K + NDEP(N)-2
            DO 702 M=K,L
              IF(NFIX(M)/1000 .EQ. 0  .OR. NFIX(M)/1000 .EQ. 11) THEN
                ALFA(M)=0.
                GO TO 702
              ENDIF
              ALFA(M) = ALFA(N)
              IF (VEL(1,m) .EQ. 0.) ALOLD = PI2
              IF (VEL(1,m) .NE. 0.) 
     &                        ALOLD = ATAN(VEL(2,m)/VEL(1,m))
              IF (ABS(ALFA(m)-ALOLD) .GT. PI2  .AND.
     +                 ABS(ALFA(m)-ALOLD) .LT. 3.*PI2)
     &                        ALOLD = ALOLD + 2.*PI2
              CSX = COS(ALOLD)
              SSX = SIN(ALOLD)
              CSN = COS(ALFA(m))
              SSN = SIN(ALFA(m))
              VT = VEL(1,m)*CSX + VEL(2,m)*SSX
              VEL(1,m) = VT*CSN
              VEL(2,m) = VT*SSN
              VT = VOLD(1,m)*CSX + VOLD(2,m)*SSX
              VOLD(1,m) = VT*CSN
              VOLD(2,m) = VT*SSN
              VT = VDOT(1,m)*CSX + VDOT(2,m)*SSX
              VDOT(1,m) = VT*CSN
              VDOT(2,m) = VT*SSN
              VT = VDOTO(1,m)*CSX + VDOTO(2,m)*SSX
              VDOTO(1,m) = VT*CSN
              VDOTO(2,m) = VT*SSN
  702       CONTINUE
          ENDIF
cycw aug96 end additions
        ENDIF
        IF(NFIX(N)/10000 .EQ. 1) ALFA(N)=0.
  700 CONTINUE
      ENDIF
  701 CONTINUE
  720 CONTINUE
      do 725 n=1,nem
      if(netyp(n) .eq. 18) then
       nn=nop(n,19)
       n1=nop(n,7)
       if (nn .gt. 0  .and.  n1 .gt. 0)  then
       alfa(nn)=alfa(n1)
       nfix(nn)=nfix(n1)
       endif
       nn=nop(n,20)
       n1=nop(n,5)
       if (nn .gt. 0  .and.  n1 .gt. 0) then
       alfa(nn)=alfa(n1)
       nfix(nn)=nfix(n1)
       endif
      endif
  725 continue 
Csep93 ipk  added code
      DO 728 N=1,NP
        IF(NFIX(N)/1000 .EQ. 31  .or.  NFIX(N)/1000 .EQ. 13) THEN
C-
C.......   ADJUST VELOCITY COMPONENTS FOR NEW ANGLE OF SPEC FLOW
C-
            IF(VEL(1,N) .EQ. 0.  .AND.  VEL(2,N) .EQ. 0.) THEN
              ALOLD=0.0
            ELSE
              ALOLD=ATAN2(VEL(2,N),VEL(1,N))
            ENDIF
            IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND.
     +         ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
            CSX=COS(ALOLD)
            SSX=SIN(ALOLD)
            CSN=COS(ALFA(N))
            SSN=SIN(ALFA(N))
            VT=VEL(1,N)*CSX+VEL(2,N)*SSX
            VEL(1,N)=VT*CSN
            VEL(2,N)=VT*SSN
            VT=VOLD(1,N)*CSX+VOLD(2,N)*SSX
            VOLD(1,N)=VT*CSN
            VOLD(2,N)=VT*SSN
            VT=VDOT(1,N)*CSX+VDOT(2,N)*SSX
            VDOT(1,N)=VT*CSN
            VDOT(2,N)=VT*SSN
            VT=VDOTO(1,N)*CSX+VDOTO(2,N)*SSX
            VDOTO(1,N)=VT*CSN
            VDOTO(2,N)=VT*SSN
          ENDIF
  728 CONTINUE
csep93 ipk end of added code
C
C..... Set forced value of ALFA from ALFAK
C-
      DO 730 N=1,NP
        ADIF(N)=0.
CIPK JUN05
        IF(ALFAK(N) .NE. 0.  .AND.  
     +     (IBN(N) .NE. 15  .AND.  IBN(N) .NE. 10)) ALFA(N)=ALFAK(N)
  730 CONTINUE
C-
C...... EXAMINE FOR CASE WHERE 1-D CHANNEL FORMS DEAD END
C-
      DO 750 M=1,NEM
        IF(IMAT(M) .GT. 0) THEN
          IF(IMAT(M) .LT. 900  .AND. NCRN(M) .EQ. 3) THEN
            DO 740 L=1,NCRN(M),2
              N=NOP(M,L)
              IF(IBN(N) .EQ. 1) THEN
                IF(NFIX(N)/100 .EQ. 010) THEN
                  NFIX(N)=11000+MOD(NFIX(N),100)
                  WRITE(*,6010) N
                  WRITE(LOUT,6010) N
 6010       FORMAT('  NODE',I5,' FORMS DEAD END WITHOUT NFIX = 11000')
                ENDIF
              ENDIF
  740       CONTINUE
          ENDIF
        ENDIF
  750 CONTINUE
C
C   Restore IBN for 2d vertical junctions
C
      DO 755 M=1,NEM
        IF(IMAT(M) .GT. 900  .AND.  IMAT(M) .LT. 1000) THEN
          DO 753 L=1,NCRN(M)
            N=NOP(M,L)
            IF(NDEP(N) .GT. 1) IBN(N)=1
  753     CONTINUE
        ENDIF
  755 CONTINUE 
C-
C...... Adjust directions when specified flow would reverse
C-
      IF(NTR .GT. 0) THEN
        DO 760 N=1,NP 
          IF(NFIX(N)/1000 .EQ. 31) THEN
           IF (ABS(ALFA(N)-SPEC(N,2)) .GT. 1.570796  .AND.
     +          ABS(ALFA(N)-SPEC(N,2)) .LT. 4.713388) THEN
              SPEC(N,1)=-SPEC(N,1)
              IF(ALFA(N) .GT. SPEC(N,2)) THEN
                SPEC(N,2)=SPEC(N,2)+3.141592
              ELSE
                SPEC(N,2)=SPEC(N,2)-3.141592
              ENDIF
           ENDIF
          ENDIF
  760   CONTINUE
      ENDIF
C-
C...... Compute angular difference at 1D - 2D junctions
C-
      DO 800 N=1,NEM
        IF(NCRN(N) .EQ. 5) THEN
        IF(IMAT(N) .LT. 901  .OR.  IMAT(N) .GT. 5000) THEN
          N1=NOPS(N,3)
          N2=NOPS(N,4)
          N3=NOPS(N,5)
          ADIF(N2)=ALFA(N2)-ALFA(N1)
          IF(ADIF(N2) .GT. PI2) THEN
            ALFA(N2)=ALFA(N2)-2.*PI2
            ADIF(N2)=ALFA(N2)-ALFA(N1)
          ELSEIF(ADIF(N2) .LT. -PI2) THEN
            ALFA(N2)=ALFA(N2)+2.*PI2
            ADIF(N2)=ALFA(N2)-ALFA(N1)
          ENDIF
          ADIF(N3)=ALFA(N3)-ALFA(N1)
          IF(ADIF(N3) .GT. PI2) THEN
            ALFA(N3)=ALFA(N3)-2.*PI2
            ADIF(N3)=ALFA(N3)-ALFA(N1)
          ELSEIF(ADIF(N3) .LT. -PI2) THEN
            ALFA(N3)=ALFA(N3)+2.*PI2
            ADIF(N3)=ALFA(N3)-ALFA(N1)
          ENDIF
          IF(NTR .GT. 0) THEN
            K=NREF(N2)+1
            IF(K .GT. 1  .AND.  NDEP(N2) .GT. 0) THEN
              L=K+NDEP(N2)-2
              DO 765 M=K,L
                ALFA(M)=ALFA(N2)
                ADIF(M)=ADIF(N2)
  765         CONTINUE
            ENDIF
            K=NREF(N3)+1
            IF(K .GT. 1  .AND.  NDEP(N3) .GT. 0) THEN
              L=K+NDEP(N3)-2
              DO 770 M=K,L
                ALFA(M)=ALFA(N3)
                ADIF(M)=ADIF(N3)
  770         CONTINUE
            ENDIF
          ENDIF
          ENDIF
c         WRITE(90,*) 'SETTING ADIF',N1,N2,N3,ADIF(N2),ADIF(N3)
        ENDIF
  800 CONTINUE
      IF(NTR .GT. 0) THEN
CIPK SEP01        DO 840 N=1,NP
        DO 840 N=1,NPM
cipk jul99
cipk sep01 alter test
          IF(NSPL(N) .eq. 0) THEN
            K=NREF(N)+1
            IF(K .GT. 1  .AND.  NDEP(N) .GT. 0) THEN
              L=K+NDEP(N)-2
              DO 820 M=K,L
                ALFA(M)=ALFA(N)
                ADIF(M)=ADIF(N)
                NFIX(M)=NFIX(N)
                NFIX1(M)=NFIX1(N)
  820         CONTINUE
            ENDIF
          ENDIF
  840   CONTINUE

        DO N=1,NPM
          IF(NFIX(N)/1000 .EQ. 0  .AND.  NFIX(N)/1000 .EQ. 11) THEN
            ALFA(N)=0.
          ENDIF 
          K=NREF(N)+1
          IF(K .GT. 1  .AND.  NDEP(N) .GT. 0) THEN
            L=K+NDEP(N)-2
            DO M=K,L
              IF(NFIX(M)/1000 .EQ. 0  .AND.  NFIX(M)/1000 .EQ. 11) THEN
                ALFA(M)=0.
              ENDIF
            ENDDO
          ENDIF
        ENDDO    

      ENDIF
cipk sep96 add logic to determine in/out bc's for salinity
c
c     Copy bcs's to save file
c
      if(ntr .eq. 0) return

      IF(ITEQV(ntr) .EQ. 2  .AND.  NRBD .GT. 0) THEN
        DO I=1,NRBD
          N=ABS(IRBD(I))
          IBC=IACTVBC(N)
          IF(IBC .EQ. 0) THEN
            NFD=NFIXSAV(N)/100
            NFIX(N)=NFD*100+MOD(NFIXSAV(N),10)
            IRBD(I)=-abs(IRBD(I))
            ION(N)=0
          ELSEIF(IRBD(I) .LT. 0) THEN
            SPEC(N,4)=VOLD(4,N)*PRCNT/100.+(1.-PRCNT/100.)*SPEC(N,4)
            VEL(4,N)=SPEC(N,4)
            VDOT(4,N)=ALTM*(VEL(4,N)-VOLD(4,N))
     +                      -(ALPHA-1.)*VDOTO(4,N)
            IRBD(I)=-IRBD(I)
            ION(N)=0
          ELSE
            IF(ION(N) .EQ. 1) THEN
              SPEC(N,4)=VOLD(4,N)
              VEL(4,N)=SPEC(N,4)
              VDOT(4,N)=-(ALPHA-1.)*VDOTO(4,N)
              ION(N)=0
            ENDIF
          ENDIF
        ENDDO
      ENDIF
CC	do j=1,np
CC	  write(150,*) j,ibn(j),nfix(j),isubm(j),isubmel(j)
CC	enddo
      RETURN
      END
