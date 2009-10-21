SUBROUTINE BLINE(NTR) 
!NiS,may06,com:
!NTR shows calling position
!NTR = 0: steady start; after reading the boundary conditions in getbc.sub
!NTR = 1: dynamic start; after reading the boundary conditions in inputd.sub
!NTR > 1: during iteration
!-

USE BLK10MOD, only: maxp, np, nfixsav, nfixsv1, nfix1, npm, nfixk, &
&  nfix, ibn, xslp, yslp, nem, imat, ncn, ncrn, &
&  nops, igtp, maxlt, translines, lmt, line, lineelement, &
&  netyp, nop, cord, voutn, vel, maxn, iteqv, &
&  alfa, vold, vdot, vdoto, nref, ndep, alfak, &
&  ne, ncorn, njt, qd, icyc, wsll, adif, lout, &
&  spec, nspl, nrbd, irbd, prcnt, altm, alpha 
!meaning of the variables
!------------------------
!voutn      vector of outward normal direction [rad] (measured from global x-axis) of the three nodes along an arc of an element
!

USE BLKSUBMOD, only: nfctp, whgt, isubm
use parakalyps
implicit none
SAVE
      
COMMON/BLKC/ ATEMP(7,3),WAIT(7),AFACT(4),HFACT(4),SLOAD(2), AX(3,3),DNAL(3,4),XNAL(3,4)
!is the same as 'include blkh.com'; make a module out of it

      real (kind = 8) :: pi2, voidp
      real (kind = 8) :: h1, h3, h, vt
real (kind = 8), dimension (1:3) :: srt
real (kind = 8), dimension (1:2, 1:2) :: dl
      real (kind = 8) :: fmt
      real (kind = 8) :: temp1, temp2, dnal
      real (kind = 8) :: afact, hfact, xnal, wait, sload, ax
      real (kind = 8) :: csx, ssx, alold, csn, ssn, ag1, az, atemp
      integer (kind = 4) :: transnumber, nodeno, lino, lile
      integer (kind = 4) :: itimeh
      integer (kind = 4) :: iform, iod, ion
      integer (kind = 4) :: i, j, k, l, m, n
      integer (kind = 4) :: ng
      integer (kind = 4) :: imt
      integer (kind = 4) :: nn, ntr, mcl, mtyp
      integer (kind = 4) :: n1, n2, n3
      integer (kind = 4) :: nfk, nftyp, nqb, nlft
      integer (kind = 4) :: ibc, iactvbc, nfd
      DIMENSION FMT(2)
      ALLOCATABLE IFORM(:),IOD(:),ION(:)
!meaning of the variables
!------------------------
!srt        copy of voutn; vector of outward normal direction (measured from global x-axis) of the three nodes along an arc of an element
!dl         dx and dy lengths of an arc:
!             d(2,1) dx between corner nodes
!             d(2,2) dy between corner nodes
!           case 2D-element:
!             d(1,1) dx between midside node and first node
!             d(1,2) dy between midside node and second node
!           case 1D-element:
!             d(1,1) dx as half of the element dx-length from d(2,1)
!             d(1,2) dy as half of the element dy-length from d(2,2)
!

      DATA PI2/1.570796/,VOIDP/-1.E19/,ITIMEH/0/


!if first time called
!--------------------
if (itimeh == 0) then
  itimeh = 1
  !allocation and initialization
  allocate (iform (maxp), iod (maxp), ion (maxp))
  iform = 0
  iod = 0
  ion = 1
endif

!some copying of values
!----------------------
if (ntr <= 1) then
  do n = 1, np
    nfixsav (n) = nfix (n)
    nfixsv1 (n) = nfix1 (n)
  enddo
  do n = 1, npm
    nfixk (n) = nfix (n)
  enddo
endif 

!define nfixsav
!--------------
do n = 1, np
  nfix (n) = nfixsav (n)
  nfix1 (n) = nfixsv1 (n)
enddo



!compute boundary mid-side nodes
!-------------------------------
do n = 1, npm
  ibn (n) = 0
  xslp (n) = 0.
  yslp (n) = 0.
enddo

!Start examining ibn and partly nfixk
!------------------------------------
!objectives: ibn, nfixk, iod
elements: do n = 1, nem
  !material type
  mtyp = imat (n)
  !cycle on deactive elements
  if(mtyp < 1) cycle elements
  !cycle what ever type of elements
  !TODO: What is imat type 90+?
  if (mod (mtyp, 100) > 90) cycle elements
  !cycle 3D elements
  if (mtyp > 1000 .and. mtyp < 5000) cycle elements
  ncn = ncrn (n)
  !prevent special treatment of 1D/2D element-2-element transition
  if (ncn == 5 .and. mtyp < 900) ncn = 3
  !for 1D elements and control structure elements
  if (ncn == 3 .or. mtyp > 900) then
    mcl = 1
  !other elements
  else
    mcl = 2
  endif
  !set up ibn and define partly nfixk
  do m = mcl, ncn, mcl
    k = nops (n, m)
    !If the node exists
    if (k > 0) then
      !This happens for
      !  junction elements: [901,...,903]
      !  control structures: [904,...,989]
      !  3D elements: [1001,...]
      if (mtyp > 900 .and. mtyp < 5000) then
        !TODO: Test, what happens with junction elements and what with control structure elements!
        if (igtp (n) == 0 .and. nfctp (n) == 0) then
          ibn (k) = 3
        else
          ibn (k) = ibn (k) + 10
          nfixk (k) = 01000
        endif

      !Following happens for
      !  1D elements, 2D elements and 1D-2D transition elements
      !    Last are numerically treated almost as a 1D element. Thus there is no
      !    reason to distinguish
      !mcl became:
      !    1 - for 1D elements [1,...,89]; [101,...,188]
      !    2 - for 2D elements [1,...,89]; [101,...,188]
      !    1 - for control structure elements (no meaning at all)
      !    1 - for junction elements (no meaning at all)
      !    Reason: Running through midside nodes for 2D elements; running through
      !      corner nodes for 1D elements; those nodes are the connecting nodes to
      !      neighbours and are meaningful counters thus!
      else
        !set iod-flag for 1D elements
        if (mcl == 1) iod (k) = 1
        !increase midside nodes' connections
        ibn (k) = ibn (k) + 1
        !ibn: Counting the occurance of a node in all active elements, i.e. 
        !     in all 1D elements, 1D-2D transition elements, junction element
      endif
    endif
  enddo
enddo elements

!Increase for all 1D transitioning nodes the counter ibn to settle, they're not dead ends
!----------------------------------------------------------------------------------------
!objectives: ibn
TransNode1DUpdate: do j = 1, maxlt
  k = translines (j, 3)
  if (k > 0) ibn (k) = ibn (k) + 1
end do TransNode1DUpdate

!increase the IBN entries at the 2D midside nodes on the arcs connected to a 1D/2D line-2-element transition, so the 2D elements are not considered to be a boundary
!-------------------------------------------------------------------------------------------------------------------------------------------------------------------
!objectives: ibn
TransNodes2DUpdate: do i = 1, MaxLT
  if (TransLines (i, 1) /= 0) then
    LiNo = TransLines (i, 2)
    LiLe = lmt (LiNo)
    do j = 2, LiLe - 1, 2
      ibn (line (LiNo, j)) = ibn (line (LiNo, j)) + 1
    enddo
  end if
enddo TransNodes2DUpdate

!Reset IBN for merger nodes in 2D-3D transition elements
!-------------------------------------------------------
!objectives: ibn
do n = 1, nem
  if (netyp (n) == 18) then
    j = nop (n, 19)
    if (j > 0) ibn (j) = 2
  endif
enddo

!For every element
elements2: do n = 1, nem
  !get material type
  mtyp = imat (n)
  !cycle deactivated elements
  if (mtyp < 1) cycle elements2
  !cycle what ever elements ???
  !TODO: What is imat type 90+?
  if (mod (mtyp, 100) > 90) cycle elements2
  !cycle gates and NFCTP-s ???
  if (mtyp > 900 .and. mtyp < 5000 .and. igtp (n) == 0 .and. nfctp (n) == 0) cycle elements2
  !get number of corner nodes
  ncn = ncrn (n)
  !treat 1D/2D element-2-element transitions as 1D elements
  if (ncn == 5) ncn = 3

  midsides: do m = 2, ncn, 2
    !midside
    n2 = nops (n, m)
    !fore midside node
    n1 = nops (n, m - 1)
    !after midside node
    n3 = nops (n, 1)
    if (m < ncn) n3 = nops (n, m + 1)

    !Checking ibn of midsides to find outer 2D-elements; midside nodes of 1D elements have ibn==0
    if (ibn (n2) == 1) then

      !Get direction of outward normal for 2-d plan elements
      if (ncn > 3) then
        call outnrm (cord (n1, 1), cord (n1, 2), cord (n2, 1), cord (n2, 2), cord (n3, 1), cord (n3, 2), srt, 1)
      !Get direction for 1D elements and 1D/2D elt-2-elt transition elements
      else
        call outnrm (cord (n1, 1), cord (n1, 2), cord (n2, 1), cord (n2, 2), cord (n3, 1), cord (n3, 2), srt, 2)
      endif

      !store outward normals into global arrays
      voutn (n1) = srt (1)
      voutn (n2) = srt (2)
      voutn (n3) = srt (3)
      
      !get the boundary condition type of the midside node's arc
      !nfk = 310 -> q-boundary condition
      !nfk = 130 -> qx-boundary condition
      !nfk =   2 -> water level condition
      !nfk = 110 -> v-condition
      nfk = nfixk (n2)/ 100

      !if not any q- or h-condition, then the boundary slope has to be calculated
      if(nfk < 113  .and.  nfk /= 2) then

        !TODO: When does this happen? It seems as it is connected to junctions or 1D elements, that are
        !      control structures
        !reset ibn-values for passive boundaries (implicit boundary conditions)
        if (ncn < 6) then
          if (ibn (n1) >= 10) ibn (n1) = 15
          if (ibn (n3) >= 10) ibn (n3) = 15
        else
          if (ibn (n1) == 10) ibn (n1) = 15
          if (ibn (n3) == 10) ibn (n3) = 15
        endif

        !get the water depths at the corners of the arc
        h1 = vel (3, n1)
        h3 = vel (3, n3)
        !calculate the length of the arc between corner nodes
        dl (2, 2) = cord (n3, 1) - cord (n1, 1)
        dl (2, 1) = cord (n3, 2) - cord (n1, 2)

        !for 2D elements calculate the distance between the midside node and the first node of the arc
        if (cord (n2, 1) > voidp .and. ncn /= 3) then
          dl (1, 2) = cord (n2, 1) - cord (n1, 1)
          dl (1, 1) = cord (n2, 2) - cord (n1, 2)
        !for 1D elements, just half the element length
        else
          dl (1, 2) = dl (2, 2) / 2.
          dl (1, 1) = dl (2, 1) / 2.
        endif

        if(iform(n1) == 1) then
          fmt(1)=-1.0
        else
          fmt(1)=1.0
        endif
        if(iform(n3) == 3) then
          fmt(2)=-1.0
        else
          fmt(2)=1.0
        endif

        iform(n1)=1
        iform(n3)=3
        forgaussnodes: do ng = 1, 4
          temp1 = (dnal (2, ng) * dl (1, 1) + dnal (3, ng) * dl (2, 1)) / 2.
          temp2 = (dnal (2, ng) * dl (1, 2) + dnal (3, ng) * dl (2, 2)) / 2.
          h = (h1 + afact (ng) * (h3 - h1)) * hfact (ng)
          if (ncn == 3) h = 1.0
          yslp (n1) = yslp (n1) + temp1 * h * xnal (1, ng) * fmt (1)
          yslp (n2) = yslp (n2) + temp1 * h * xnal (2, ng)
          yslp (n3) = yslp (n3) + temp1 * h * xnal (3, ng) * fmt (2)
          xslp (n1) = xslp (n1) + temp2 * h * xnal (1, ng) * fmt (1)
          xslp (n2) = xslp (n2) + temp2 * h * xnal (2, ng)
          xslp (n3) = xslp (n3) + temp2 * h * xnal (3, ng) * fmt (2)
        enddo forgaussnodes
      endif
    endif
  enddo midsides
enddo elements2

!CIPK JUN05  SETUP FOR SUBMERGENGE
      !NiS,may06: NTR is again the calling position of BLINE
      IF(NTR .GT. 0) THEN
        CALL SUBSET
      ENDIF

!C-
      DO 701 N=1,NPM
        !FIXME: This is just for showing, that inner boundary condition nodes shall not be touched;
        !  If such nodes are not ignored here, the values of the inner boundaries will totally be changed
        !  by this subroutine. Thus after each successful calculation step, the boundary condition status
        !  of all nodes is nullified to be resetted during the preperation phase of the Schwarz-Iteration
        if (nfix(n) == -1) go to 701
        !-

        NFIX(N)=NFIXK(N)
        !nis,jul07 (work around): iteqv(0) is not defined. This happens, when BLINE(NTR==1) ist called, the beginning of a dynamic time step
        if (maxn > 0) then
          IF (ITEQV(MAXN) .EQ. 2)  GOTO 701
        end if
        !-

!CIPK OCT98  CONVERT TO F90
        NFTYP=NFIXK(N)
        NQB=MOD(NFTYP,100)
        NLFT=NFTYP/100

        IF(NLFT .EQ. 0) ALFA(N)=0.

        IF(MOD(NLFT,10) .EQ. 2) THEN
!C-
!C......   THESE ARE ONE-D ELEMENTS TYPE 102 012 OR 002
!C......   GET     ALFA   AND SET FORM FOR NFIX
!C-
          IF(NLFT .EQ. 102) THEN
!C-
!C......   SPECIAL CASE OF VERTICAL LINE RESET 102 TO 012 AND CHANGE ALFA
!C-
            ALFA(N)=PI2
            NFIX(N)=01200+NQB
          ELSE
!C-
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
!cycw aug96 refine test to permit 012 case to be testes
!c        IF(IOD(N) .EQ. 1  .AND.  NTR .GT. 0) THEN
          IF((IOD(N).EQ.1.OR.NFIX(N)/100.EQ.12).AND.NTR.GT.0) THEN
!C-
!C.......   ADJUST VELOCITY COMPONENTS FOR NEW ANGLE
!C-
            IF(VEL(1,N) .EQ. 0.) ALOLD=PI2
            IF(VEL(1,N) .NE. 0.) ALOLD=ATAN(VEL(2,N)/VEL(1,N))
            IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND.ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
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
!cycw aug96 add logic to 
!C Correct velocities along boundary for subsurface nodes

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
                IF (VEL(1,m) .NE. 0.) ALOLD = ATAN(VEL(2,m)/VEL(1,m))
                IF (ABS(ALFA(m)-ALOLD) .GT. PI2  .AND. ABS(ALFA(m)-ALOLD) .LT. 3.*PI2) ALOLD = ALOLD + 2.*PI2
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
  609           CONTINUE
              ENDDO
            ENDIF
!cycw aug96 end additions
          ENDIF
        ELSE

          IF(NFIX(N) .LE. 10010) GO TO 610
          IF(IOD(N) .EQ. 0) GO TO 700
!C-
!C..... ONLY ONE-D LEFT  NFIX = 11 OR 31
!C-
          IF(NFIX(N) .GT. 11000) THEN
            GO TO 630
          ELSE
            ALFA(N)=0.
            GO TO 700
          ENDIF
  610     CONTINUE

!CIPK JUN05
          !nis,nov06: This must be the point for 1D-2D-transition-elements to assign angle ALFA=ALFAK. ALFAK is set in GETGEO!
          !nis,mar07: Because of machine accuracy, the node might have a none-zero alfak-value, changing to a check for range
          !IF(ALFAK(N) .NE. 0.  .AND. IBN(N) .NE. 15) THEN
          IF(ABS(ALFAK(N)) > 0.00001  .AND. IBN(N) .NE. 15) THEN
          !-
            ALFA(N)=ALFAK(N)
          ELSEIF(XSLP(N) .NE. 0.) THEN
            ALFA(N)=ATAN(YSLP(N)/XSLP(N))
          ELSE
            IF(YSLP(N) .EQ. 0.) GO TO 700
            ALFA(N)=PI2
          ENDIF
          NFIX(N)=01000+NQB
!cipk juN05
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

                        AG1=ATAN2(CORD(NOP(M,6),2)-CORD(NOP(M,2),2), CORD(NOP(M,6),1)-CORD(NOP(M,2),1))
                        AZ=AG1-ALFA(N)
                        IF(AG1 .GE. 0.) THEN
                          IF(AG1-ALFA(N) .LT. 1.570795  .OR. AG1-ALFA(N) .GT. 4.712385) GO TO 630

                          if(icyc .lt. 2 .and. maxn .eq. 1) then
                         write(75,*)'redirect weir',n,alfa(n),ag1,ibn(n)
                         write(75,*) AZ,nfix(n),wsll(n),whgt(n),ISUBM(N) ,VEL(1,N),VEL(2,N)
                          endif
                          ALFA(N)=ALFA(N)+3.14159
                        ELSE
                          IF(ALFA(N)-AG1 .LT. 1.570795 .OR. ALFA(N)-AG1 .GT. 4.712385) GO TO 630
	                  if(icyc .lt. 2  .and. maxn .eq. 1) then
                         write(75,*)'redirect weir',n,alfa(n),ag1,ibn(n)
                         write(75,*) AZ,nfix(n),wsll(n),whgt(n),ISUBM(N) ,VEL(1,N),VEL(2,N)
     	                  endif
                          ALFA(N)=ALFA(N)-3.14159
                        ENDIF

                      ENDIF
                      GO TO 630
                    ENDIF
  615             CONTINUE
                ENDIF
              ENDIF
  620       CONTINUE


!cipk mar00
        ELSEIF(IBN(N) .GT. 10) then
!CIPK JUL00 REVISE TEST
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
!cipk JUN05 test for submerged case

          IF(ISUBM(N) .EQ. 1  .AND. (IBN(N) .EQ. 11  .OR.  IBN(N) .EQ. 20)) THEN
            GO TO 700
          ENDIF
          IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND. ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
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
!cycw Aug96 
!C Correct velocities along boundary for subsurface nodes

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
              IF (VEL(1,m) .NE. 0.) ALOLD = ATAN(VEL(2,m)/VEL(1,m))
              IF (ABS(ALFA(m)-ALOLD) .GT. PI2  .AND. ABS(ALFA(m)-ALOLD) .LT. 3.*PI2) ALOLD = ALOLD + 2.*PI2
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
!cycw aug96 end additions
        ENDIF
        IF(NFIX(N)/10000 .EQ. 1) ALFA(N)=0.
  700   CONTINUE
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
!Csep93 ipk  added code
      DO 728 N=1,NP
        IF(NFIX(N)/1000 .EQ. 31  .or.  NFIX(N)/1000 .EQ. 13) THEN
!C-
!C.......   ADJUST VELOCITY COMPONENTS FOR NEW ANGLE OF SPEC FLOW
!C-
          IF(VEL(1,N) .EQ. 0.  .AND.  VEL(2,N) .EQ. 0.) THEN
            ALOLD=0.0
          ELSE
            ALOLD=ATAN2(VEL(2,N),VEL(1,N))
          ENDIF
          IF(ABS(ALFA(N)-ALOLD) .GT. PI2  .AND. ABS(ALFA(N)-ALOLD) .LT. 3.*PI2) ALOLD=ALOLD+2.*PI2
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
!csep93 ipk end of added code
!C
!C..... Set forced value of ALFA from ALFAK
!C-
      DO 730 N=1,NP
        ADIF(N)=0.
!CIPK JUN05
        !TOASK
        !nis,mar07: Because of machine accuracy, the node might have a none-zero alfak-value, changing to a check for range
        !IF(ALFAK(N) .NE. 0.  .AND.
        IF(ABS(ALFAK(N)) > 0.00001  .AND.(IBN(N) .NE. 15  .AND.  IBN(N) .NE. 10)) ALFA(N)=ALFAK(N)
  730 CONTINUE
!C-
!C...... EXAMINE FOR CASE WHERE 1-D CHANNEL FORMS DEAD END
!C-
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
!C
!C   Restore IBN for 2d vertical junctions
!C
      DO 755 M=1,NEM
        IF(IMAT(M) .GT. 900  .AND.  IMAT(M) .LT. 1000) THEN
          DO 753 L=1,NCRN(M)
            N=NOP(M,L)
            IF(NDEP(N) .GT. 1) IBN(N)=1
  753     CONTINUE
        ENDIF
  755 CONTINUE 
!C-
!C...... Adjust directions when specified flow would reverse
!C-
      IF(NTR .GT. 0) THEN
        DO 760 N=1,NP 
          IF(NFIX(N)/1000 .EQ. 31) THEN
           IF (ABS(ALFA(N)-SPEC(N,2)) .GT. 1.570796  .AND. ABS(ALFA(N)-SPEC(N,2)) .LT. 4.713388) THEN
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
!C-
!C...... Compute angular difference at 1D - 2D junctions
!C-
!nis,nov06: Enlarge computation of angular differences for 1D-2D-line-transitions

      !nis,nov06: Change way of do loop
      !DO 800 N=1,NEM
      angulardiffs: DO N=1,NEM
      !-
      !nis,jan07: Check, whether element is part of line transition
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
  765           CONTINUE
              ENDIF
              K=NREF(N3)+1
              IF(K .GT. 1  .AND.  NDEP(N3) .GT. 0) THEN
                L=K+NDEP(N3)-2
                DO 770 M=K,L
                  ALFA(M)=ALFA(N3)
                  ADIF(M)=ADIF(N3)
  770           CONTINUE
              ENDIF
            ENDIF
          ENDIF
        !nis,nov06: Test for 1D-2D-line-transitions
        ELSEIF (ncrn(n) == 3) then

          !if there is no line transition cycle
          if (MaxLT == 0) CYCLE angulardiffs

          !find the correct line transition number
          findtranselt: do j = 1, MaxLT
            !if the element n is part of any transition go on with the dirAdjustment-loop
            if (TransLines(j,1) == n) then
              TransNumber = J
              EXIT findtranselt
            ENDIF
            !if the last line still shows no consistency with the active element n, this is no transition element then and the main loop has to cycle
            if (j == MaxLT) CYCLE angulardiffs
          end do findtranselt

          !get local copy of line no.
          LiNo = TransLines (TransNumber, 2)

          !Reset angular difference vector, because elements might have become dry/wet
          do nodeno = 1, lmt (lino)
            n1 = line (lino, nodeno)
            adif (n1) = 0.0
          enddo

          do i = 1, 2
            !first wet node
            if (i == 1) then 
              !Find first wet node in 1D-2D-line-transition
              SetFirstAdif: do nodeno = 1, lmt (lino)-2, 2
                n1 = line (lino, nodeno)
                !find dry/wet status of node:
                if (imat (lineelement (lino,nodeno+1)) > 0) then
                  !set direction reference node
                  n2 = line (lino, nodeno + 2)
                  exit SetFirstAdif
                endif
              enddo SetFirstAdif
            !last wet node
            else
              !Find last wet node in 1D-2D-line-transition
              SetLastAdif: do nodeno = lmt (lino), 3, -2
                n1 = line (lino, nodeno)
                !find dry/wet status of node:
                if (imat (lineelement (lino,nodeno-1)) > 0)  exit SetLastAdif
              enddo SetLastAdif
            endif
            !TODO: Support dry gaps in the 1D-2D-line-transition
            !TODO: Check for the distance between the two wet border nodes: There must be at least
            !      one node in between so minimum 2 elements within the line transition.
            
            !Fix adif for wet nodes
            !Calculate angular difference of first and third node that has for shure the line's directio fix
            ADIF(n1) = ALFA(n1) - ALFA(N2)
            !Correct direction, so that the vectors points into quadrant 1 or 4
            IF(ADIF(n1) .GT. PI2) THEN
              ALFA(n1)=ALFA(n1)-2.*PI2
              ADIF(n1)=ALFA(n1)-ALFA(N2)
            ELSEIF(ADIF(n1) .LT. -PI2) THEN
              ALFA(n1)=ALFA(n1)+2.*PI2
              ADIF(n1)=ALFA(n1)-ALFA(N2)
            ENDIF
          enddo

        ENDIF
!nis,nov06: new way of do loop
!  800 CONTINUE
      ENDDO angulardiffs
!-



      IF(NTR .GT. 0) THEN
!CIPK SEP01        DO 840 N=1,NP
        DO 840 N=1,NPM
!cipk jul99
!cipk sep01 alter test
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
!cipk sep96 add logic to determine in/out bc's for salinity
!c
!c     Copy bcs's to save file
!c
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
            VDOT(4,N)=ALTM*(VEL(4,N)-VOLD(4,N)) -(ALPHA-1.)*VDOTO(4,N)
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
!CC	do j=1,np
!CC	  write(150,*) j,ibn(j),nfix(j),isubm(j),isubmel(j)
!CC	enddo
      RETURN
      END
