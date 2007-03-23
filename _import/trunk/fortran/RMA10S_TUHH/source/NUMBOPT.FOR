cipk  last update apr 12 2006 replace BLK4 WITH BLKFRNT and cleanup NBS
CIPK  LAST UPDATE SEP 6 2004 RENAME character variable FNAM to FNAMT add error file
c-----------------------------------------------------------------optim
       subroutine optim
c---------------------------------------------------------------------c
c purpose:                                                            c
c         To optimize element ordering.                               c
c---------------------------------------------------------------------c
c Array 'perm' is created to store old number in order of new numbers.c
c---------------------------------------------------------------------c
ccc    implicit real*8 (a-h,o-z)
       USE BLK10MOD
       USE BLK10
CIPK APR06      INCLUDE 'BLK10.COM'
CIPK APR06       INCLUDE 'BLKDR.COM'
CIPK APR06       INCLUDE 'BLK4.COM'
       integer newpro,LCMAX,NIOP
       real*8 NSUM

       common /block/nblock,nspac,nt7
c
       nbss=lbmax
       LIOP=-1
       call SECOND(SEC0)
       kmax=0
       nt7=77
       nspac=NBSS-50
       mh=nspac+3
       nkk=1
       nxadj=nkk+4*2
       nadjc=1
       nperm=nxadj+2*(NSZF+5)
       nmask=nperm+2*(NSZF+1)
       nxls=nmask+2*(NSZF+1)
       nfrede=nxls+2*(NSZF+1)
       nnh=nfrede+2*(NSZF+5)
       nemin=nnh+2*(NSZF+5)
       write(*,*) 'nbss',nbss
       if((nemin+8*(ne+5)).gt. lbmax) goto 900
c
c------RECL in Alpha is mh, while 4*mh in SunStation
c
CCC       OPEN(nt7,FILE='OPTM_TMP',
CCC     +         STATUS='unknown',FORM='unformatted')
c
c------read the element ordering
c
CCC       if(LIOP.lt.0) then
CCC          rewind nt7
CCC          read(nt7,err=20,end=20) nme,NIOP
CCC          if(nme.ne.NE) goto 20
CCC          if(NIOP.ne.ABS(LIOP)) goto 20
CCC          rewind nt7
CCC          read(nt7,err=20,end=20) nme,NIOP,(NFIXH(i),I=1,nme)
CCC          goto 100
CCC   20     LIOP=ABS(LIOP)
CCC       endif
       
       call egraph(QS(nadjc),LHS(nxadj),LHS(nkk),LHS(nfrede),LHS(nnh),
     +             NSPAC,NSZF+1,kmax)
       call label(NSZF,QS(nadjc),LHS(nxadj),LHS(nperm),LHS(nmask),
     +             NSPAC,LIOP)
c
c------check the existing element ordering
c
       call eopts(NFIXH,LHS(nxadj),NE,NSZF,NSUM,LCMAX)
CIPK JAN07
       INSUM=NSUM
       write(*,6001) INSUM,LCMAX,NFIXH(1)
 6001  FORMAT(' NSUM,LCMAX,ELSTART =',3I16)     
       write(75,6001)INSUM,LCMAX,NFIXH(1)
c
c------set new ordering
c
       call reord(LHS(nperm),LHS(nxls),LHS(nfrede),LHS(nmask),LHS(nnh),
     +            LHS(nemin),NSZF,NE)
       call eopts(NFIXH,LHS(nperm),NE,NSZF,NSUM,LCMAX)
CIPK JAN07
       INSUM=NSUM
       write(*,6001) INSUM,LCMAX,NFIXH(1)
       write(75,6001)INSUM,LCMAX,NFIXH(1)
CCC       if(ABS(LIOP).gt.0) THEN
CCC          REWIND NT7
CCC          write(nt7) NE,ABS(LIOP),(NFIXH(I),I=1,NE)
CCC       ENDIF
CCC  100  close(nt7)
       call SECOND(SEC1)
       DSEC=SEC1-SEC0
       write(*,'(/a,f15.1/)') ' TIME SPEND IN NUMBER ORDERING = ',DSEC
       return
  900  continue
cipk sep04
       CLOSE(75)
       OPEN(75,FILE='ERROR.OUT')
       write(*,*) ' ** Buff size not large enough for reordering **'
       write(*,'(a,I12,a)') '    Increase NBS to',nemin+8*(ne+5),
     +                      'in PARAMETER statement'
       write(75,*) ' ** Buff size not large enough for reordering **'
       write(75,'(a,I12,a)') '    Increase NBS to',nemin+8*(ne+5),
     +                      'in PARAMETER statement'
       stop
       end
c
c-----------------------------------------------------------------reord
       subroutine reord(perm,invp,frede,nmax,nmin,emin,nsiz,nsiz1)
c---------------------------------------------------------------------c
       USE BLK10MOD
      
CIPK APR06      INCLUDE 'BLK10.COM'
CIPK APR06       INCLUDE 'BLKDR.COM'
CIPK APR06       INCLUDE 'BLK4.COM'
       integer perm(nsiz),invp(nsiz),nmax(nsiz),nmin(nsiz),frede(nsiz),
     +         emin(4,nsiz1)
 
c      do i=1,NSZF
c         invp(perm(i))=i
c      enddo
c      do i=1,NSZF
c         perm(i)=invp(NSZF-i+1)
c      enddo
       do i=1,NSZF
c         invp(perm(i))=i
          invp(i)=perm(i)
       enddo
c
c------calculate maximum bandwidth
c
       do i=1,NSZF
          nmax(i)=0
          nmin(i)=0
       enddo
       do i=1,ne
          nn=0
          ncn=ncorn(i)
          do m=1,ncn
             if(nop(i,m).ne.0) then
                do n=1,NDF
                   if(NBC(nop(i,m),n).gt.0) then
                      nn=nn+1
                      frede(nn)=invp(NBC(nop(i,m),n))
                   endif
                enddo
             endif
          enddo
          do m=1,nn
             do n=1,nn
                nmax(frede(m))=max(nmax(frede(m)),frede(m)-frede(n))
                nmin(frede(m))=min(nmin(frede(m)),frede(m)-frede(n))
             enddo
          enddo
       enddo
       ncap=0
       nband=0
       do i=1,NSZF
          ncap=ncap+nmax(i)-nmin(i)
          nband=max(nband,(nmax(i)-nmin(i)))
cc          ncap=ncap+nmax(i)
cc          nband=max(nband,nmax(i))
       enddo
cc     write(*,'(/a,i10)') 'MAXIMUM BANDWIDTH =',nband
cc     write(*,'(a,i10/)') '  MATRIX CAPACITY =',ncap
c
c------find minimum equation number in each element
c
       do i=1,ne
          nn=0
          ncn=ncorn(i)
          do m=1,ncn
             if(nop(i,m).ne.0) then
                do n=1,NDF
                   if(NBC(nop(i,m),n).gt.0) then
                      nn=nn+1
                      frede(nn)=invp(NBC(nop(i,m),n))
                   endif
                enddo
             endif
          enddo
          do k=1,4
             if(k.le.nn) then
                emin(k,i)=NSZF
                do m=1,nn
                   if(frede(m).lt.emin(k,i)) then
                      emin(k,i)=frede(m)
                      ik=m
                   endif
                enddo
                frede(ik)=NSZF+1
c               if(imat(i).lt.1) emin(k,i)=NSZF
             else
                emin(k,i)=0
             endif
          enddo
       enddo
c
c------sort to ascending order
c
       do i=1,ne
          imin=NSZF+NSZF
          do m=1,ne
             if(emin(1,m).lt.imin) then
                imin=emin(1,m)
                nfixh(i)=m
             elseif(emin(1,m).eq.imin) then
                do k=2,4
                   if(emin(k,m).lt.emin(k,nfixh(i))) then
                      imin=emin(1,m)
                      nfixh(i)=m
                      goto 50
                   endif
                enddo
   50           continue
             endif
          enddo
          emin(1,nfixh(i))=NSZF+1
       enddo
             
       return
       end
c
c----------------------------------------------------------------egraph
       subroutine egraph(adjc,xadj,kk,frede,nh,NSIZ1,NSIZ2,kmax)
c---------------------------------------------------------------------c
c purpose:                                                            c
c         This routine sets up (xadj,adjc), i.e. the adjacency vector c
c         pair for RCM-algorithm.                                     c
c---------------------------------------------------------------------c
c output paramaters:                                                  c
c                  (xadj,adjancy)--adjancy vectors......              c
c---------------------------------------------------------------------c
ccc    implicit real*8 (a-h,o-z)
       USE BLK10MOD
CIPK APR06      INCLUDE 'BLK10.COM'
CIPK APR06       INCLUDE 'BLKDR.COM'
CIPK APR06       INCLUDE 'BLK4.COM'
       integer xadj(NSIZ2),adjc(NSIZ1),kk(NSIZ2),frede(NSIZ2),nh(NSIZ2)
       common /block/nblock,nspac,nt7
c
       print *,'NDF,NSZF,NE=',NDF,NSZF,NE
c
c------estimate xadj
c
       do i=1,NSZF
          xadj(i)=0
          nh(i)=0
          adjc(i)=0
       enddo
       do i=1,ne
          nn=0
          ncn=ncorn(i)
          do m=1,ncn
             if(nop(i,m).ne.0) then
                do n=1,NDF
                   if(NBC(nop(i,m),n).gt.0) then
                      nn=nn+1
                      frede(nn)=NBC(nop(i,m),n)
                   endif
                enddo
             endif
          enddo
          do m=1,nn
             do n=1,nn
                if(frede(n).ne.frede(m)) then
                   xadj(frede(m))=xadj(frede(m))+1
                endif
                nh(frede(m))=max(nh(frede(m)),frede(m)-frede(n))
                adjc(frede(m))=min(adjc(frede(m)),frede(m)-frede(n))
             enddo
          enddo
       enddo
       frede(1)=1
       do i=1,NSZF
          frede(i+1)=frede(i)+xadj(i)
          nband=max(nband,(nh(i)-adjc(i)))
       enddo
       do i=1,NSZF+1
          xadj(i)=frede(i)
          nh(i)=frede(i)
       enddo
c      write(*,'(/a,i10/)') 'MAXIMUM BANDWIDTH =',nband
c
c------caculate no. of blocks
c
       nblock=int(float(xadj(NSZF+1)-1)/float(nspac)+1.0)
       kk(1)=0
       kk(3)=0
       nfds=1
       do 5 k=1,nblock+1
            if(kk(3).ge.xadj(NSZF+1)-1) goto 5
            kk(2)=kk(3)+1
  300       do i=nfds,NSZF
               if(xadj(i+1)-kk(2).gt.nspac) then
                  kk(3)=xadj(i)-1
                  nfde=i-1
                  goto 400
               endif
            enddo
            kk(3)=xadj(NSZF+1)-1
            nfde=NSZF
  400       if(nfds.gt.nfde) goto 900
            do 20 i=1,ne
                  nn=0
                  ncn=ncorn(i)
                  do m=1,ncn
                     if(nop(i,m).ne.0) then
                        do n=1,NDF
                           if(NBC(nop(i,m),n).gt.0) then
                              nn=nn+1
                              frede(nn)=NBC(nop(i,m),n)
                           endif
                        enddo
                     endif
                  enddo
                  do m=1,nn
                     if(frede(m).ge.nfds.and.frede(m).le.nfde) then
                        do n=1,nn
                           if(frede(n).ne.frede(m)) then
                             do L=xadj(frede(m)),nh(frede(m))-1
                               if(frede(n).eq.adjc(L-kk(2)+1)) goto 15
                             enddo
                             if(nh(frede(m))-kk(2)+1.gt.nspac) then
                             endif
                             adjc(nh(frede(m))-kk(2)+1)=frede(n)
                             nh(frede(m))=nh(frede(m))+1
                           endif
   15                   enddo
                     endif
                  enddo
   20       continue
c
c-----------strip zeros in ADJC
c
            izeros=0
            do i=nfds,nfde
               izeros=izeros+xadj(i+1)-nh(i)
               do m=xadj(i+1),nh(i+1)
                  adjc(m-izeros)=adjc(m)
               enddo
               xadj(i+1)=xadj(i+1)-izeros
            enddo
            nh(nfde+1)=nh(nfde+1)-izeros
            do i=nfde+1,NSZF
               xadj(i+1)=xadj(i+1)-izeros
               nh(i+1)=nh(i+1)-izeros
            enddo
            kk(3)=kk(3)-izeros
            nfds=nfde+1
            if(xadj(nfde+1)-kk(2).lt.nspac.and.nfde.lt.NSZF) goto 300
            kk(1)=kk(1)+1
c           call cstorg(adjc,kk,kmax)
    5  continue
       nblock=kk(1)
       return
  900  continue
cipk sep04
       CLOSE(75)
       OPEN(75,FILE='ERROR.OUT')
       write(*,1000)
       write(75,1000)
 1000  format(1x,' ** INCORE STORAGE NOT ENOUGH IN NUMBERING',
     *        ' OPTIMISING PROGRAM **'/)
       stop
       end
c
c----------------------------------------------------------------cstorg
       subroutine cstorg(adjc,kk,kmax)
c---------------------------------------------------------------------c
c purpose:                                                            c
c         This routine is used to write array ADJC to out core .      c
c---------------------------------------------------------------------c
ccc    implicit real*8 (a-h,o-z)
       integer adjc(*),kk(3)
       common /block/nblock,nspac,nt7
       character*4 qnam
CIPK SEP04
       character*6 fnamt
c
       if(kk(1) .gt. kmax) kmax=kk(1)
       write(qnam,6000) kk(1)
 6000  format(i4.4)
cipk sep04
       fnamt='RS'//qnam(1:4)
       OPEN(nt7,FILE=fnamt,
     1         STATUS='unknown',FORM='unformatted')

       write(nt7) (kk(i),i=1,3),(adjc(i),i=1,kk(3)-kk(2)+1)
       return
       end
c
c----------------------------------------------------------------rstorg
       subroutine rstorg(adjc,kk,mm,nbr)
       SAVE
c---------------------------------------------------------------------c
c purpose:                                                            c
c        To read array 'ADJC' from out core of storage.               c
c---------------------------------------------------------------------c
ccc    implicit real*8 (a-h,o-z)
       integer adjc(*),kk(*)
       character*4 qnam
cipksep04
       character*6 fnamt,fnamo
c
       common /block/nblock,nspac,nt7
c
       if(mm.ge.kk(2).and.mm.le.kk(3)) goto 530
       gh=float(mm)/float(nspac)
       mh=mm/nspac
       if(gh.eq.float(mh)) goto 500
       mh=int(gh+1.0)
       goto 510
  500  if(mh.eq.0) mh=1
  510  if(kk(1).eq.mh) goto 520

  540  continue

       write(qnam,6000) mh
 6000  format(i4.4)
cipk sep04
       fnamt='RS'//qnam(1:4)
       if(fnamt .ne. fnamo) then
         close (nt7)
         OPEN(nt7,FILE=fnamt,
     1         STATUS='unknown',FORM='unformatted')
       else
         rewind nt7
       endif
       fnamo=fnamt
       read(nt7) (kk(i),i=1,3),(adjc(i),i=1,kk(3)-kk(2)+1)
  520  if(mm.ge.kk(2).and.mm.le.kk(3)) goto 530
       mh=mh+1
       goto 540
  530  nbr=adjc(mm-kk(2)+1)
       return
       end
c------------------------------------------------------------------EOPTS
      SUBROUTINE EOPTS(NELP,LHDI,NSIZ1,NSIZ2,NSUM,LCMAX)
c----------------------------------------------------------------------c
c purpose:                                                             c
c         To count the operations and maximum front width in FRONT.    c
c----------------------------------------------------------------------c
      USE BLK10MOD
      USE BLK11MOD
      USE BLK10
CIPK APR06      INCLUDE 'BLK10.COM'
CIPK APR06      INCLUDE 'BLKDR.COM'
CIPK APR06      INCLUDE 'BLK4.COM'
C-
      INTEGER*2 NELP(NSIZ1)
      INTEGER   LHDI(NSIZ2),NSIZ,LCMAX
      REAL*8    NSUM
      COMMON/FNT/ LDEST1(120),NK(120)
C
C-----Find last appearance of each node
C

      IF (.NOT. ALLOCATED(NLSTLL)) ALLOCATE (NLSTLL(NSZF))

      NLSTLL=0

      K=NE+1
      DO NN=1,NE
        K=K-1
        N=NELP(K)
        if(n .eq. 0) go to 10
        IF(IMAT(N) .GT. 0) THEN
          NM=IMAT(N)
          IF(NM .LT. 900) THEN
             IF(ORT(NM,1) .EQ. 0.) GO TO 10
          ENDIF
          NCN=NCORN(N)
          IF(NOP(N,3) .EQ. 0) NCN=2
          DO M=1,NCN
            L=NOP(N,M)
            IF(L .GT. 0) THEN
              DO I=1,NDF
                J=NBC(L,I)
                IF(J .NE. 0) THEN
                  IF(NLSTLL(J) .EQ. 0) THEN
                    NLSTLL(J)=N
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
   10   CONTINUE
      ENDDO
C
C-----Initialize
C
      NSUM=0
      LCMAX=0
      NMAX=MFWW
      NELL=0
      LCOL=0
C
   18 NELL=NELL+1
      IF(NELL.GT.NE) GO TO 380
      N=NELP(NELL)

      if(n .eq. 0) go to 18
C
      IF(IMAT(N).LE.0) GO TO 18
      NM=IMAT(N)
      IF(NM .LT. 901) THEN
        IF(ORT(NM,1) .EQ. 0.) GO TO 18
      ENDIF
      NCN=NCORN(N)
      NBN = NCN*NDF
      DO LK=1,NBN
        NK(LK)=0
      ENDDO
      KC=0
      DO 23 J=1,NCN
        I=NOP(N,J)
        IF(I .EQ. 0) THEN
          KC=KC+NDF
          GO TO 23
        ENDIF
        DO 22 L=1,NDF
          LL=NBC(I,L)
          KC=KC+1
          NK(KC)=LL
          IF(LL .EQ. 0) GO TO 22
          IF(NLSTLL(LL) .EQ. N) NK(KC)=-LL
   22   CONTINUE
   23 CONTINUE
C
C-----SET UP HEADING VECTORS
C
      DO 52 LK=1,NBN
        NODE=NK(LK)
        IF(NODE.EQ.0) GO TO 52
        IF(LCOL.EQ.0)GO TO 28
        DO 24 L=1,LCOL
          LL=L
          IF(ABS(NODE) .EQ. ABS(LHDI(L)))GO TO 32
   24   CONTINUE
   28   LCOL=LCOL+1
        LHDI(LCOL)=NODE
        GO TO 36
   32   LHDI(LL)=NODE
   36   CONTINUE
   52 CONTINUE
      IF(LCOL .GT. LCMAX) LCMAX=LCOL
C
C-----FIND OUT WHICH MATRIX ELEMENTS ARE FULLY SUMMED
C
   60 DO 64 L=1,LCOL
        IF(LHDI(L) .GT. -1) GO TO 64
        LPIVCO=L
        GOTO 68
   64 CONTINUE
      GOTO 18
C
C-----SUM UP THE NUMBER OF OPERATIONS
C
   68 NSUM=NSUM+float(LCOL)**2
C
C-----REARRANGE HEADING VECTORS
C
      LCOL=LCOL-1
      IF(LPIVCO.EQ.LCOL+1)GO TO 136
      DO 132 L=LPIVCO,LCOL
        LHDI(L)=LHDI(L+1)
  132 CONTINUE
  136 CONTINUE
C
C-----DETERMINE WHETHER TO ASSEMBLE,ELIMINATE,OR BACKSUBSTITUTE
C
      IF(LCOL.GT.0) GO TO 60
      GO TO 18
  380 CONTINUE
      IF(LCOL .GT. 0) THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,479) LCOL
        WRITE(*,479) LCOL
  479   FORMAT( '  UNSATISFIED ELIMINATION ERROR STOP'/
     1  '   LCOL =',I5)
        print *,'ne,lcol,lcmax=',ne,lcol,lcmax,nsum,nell
        STOP 'Unsatisfied elimination error in EOPTS'
      ENDIF
      RETURN
      END
C------------------------------------------------------------------LABEL
       SUBROUTINE LABEL(N,ADJ,XADJ,NNN,IW,NSIZ,LIOP)
c----------------------------------------------------------------------c
       INTEGER N,I1,I2,I3,I,SNODE,LSTNUM,NC,OLDPRO,NEWPRO,MNODE,LCMAX
       INTEGER ADJ(NSIZ),NNN(N),IW(6*N+1),XADJ(N+1),WW(10)
       INTEGER W1,W2,DEPTH,WIDTH,NKEY(4),NN,LIOP,JSTRT,JSTOP
       REAL*8  SSUM,SSUM0
       DATA WW/0,1,2,3,4,5,6,7,8,9/
C
       I1=1
       I2=I1+N
       I3=I2+N+1
       I4=I3+N
       I5=I4+N
       I6=I5+N
       SSUM0=1.0e30
C
       DO 10 I=1,N
             NNN(I)=0
   10  CONTINUE
C
       LSTNUM=0
   20  IF(LSTNUM.LT.N) THEN
C
          CALL DIAMTR(N,ADJ,XADJ,NNN,IW(I1),IW(I2),IW(I3),IW(I6),
     +                NC,NSIZ,NKEY,NN,LIOP,LSTNUM)
C
C---------LOOP for different weights
C
          NUMSAV=LSTNUM
          NN=1
          DO M=1,NN
            MNODE=NKEY(M)
            DO I=1,NC
               NNN(IW(I1+I-1))=0
            ENDDO
            MNODE=NKEY(1)
            CALL ROOTLS(N,MNODE,N+1,ADJ,XADJ,NNN,IW(I1),IW(I2),DEPTH,
     +                  WIDTH,NSIZ)
            write(*,*) 'MNODE,DEPTH,WIDTH=',MNODE,DEPTH,WIDTH
C
            DO I=1,DEPTH
               JSTRT=IW(I2+I-1)
               JSTOP=IW(I2+I)-1
               DO J=JSTRT,JSTOP
                  NNN(IW(I1+J-1))=I-1
               ENDDO
            ENDDO
            DO I=1,7
              W1=WW(I)
              DO J=2,5
                if(I.eq.1.and.J.gt.2) goto 30
                if(I.ne.2.and.J.eq.I) goto 30
                if(I.eq.3.and.J.eq.5) goto 30
                if(I.eq.5.and.J.eq.3) goto 30
                W2=WW(J)
                DO K=1,NC
                   IW(I3+K-1)=NNN(K)
                   IW(I4+K-1)=IW(I1+K-1)
                ENDDO
                LSTNUM=NUMSAV
                MNODE=NKEY(2)
                CALL NUMBER(N,NC,MNODE,LSTNUM,ADJ,XADJ,IW(I3),IW(I4),
     +                      IW(I2),W1,W2,NSIZ)
                CALL PROFIL1(NC,IW(I3),ADJ,XADJ,IW(I6),NSIZ,LCMAX,SSUM)
                write(*,*) 'NSUM,LCMAX=',W1,W2,SSUM,LCMAX
                IF(SSUM.LT.SSUM0) THEN
                   SSUM0=SSUM
                   DO K=1,NC
                      IW(I5+K-1)=IW(I3+K-1)
                   ENDDO
                ENDIF
   30         ENDDO
            ENDDO
          ENDDO
          DO K=1,NC
             NNN(K)=IW(I5+K-1)
          ENDDO
C
          GOTO 20
       ENDIF
C
       END
c
c-----------------------------------------------------------------DIAMTR
       SUBROUTINE DIAMTR(N,ADJ,XADJ,MASK,LS,XLS,HLEVEL,IW,
     +                   NC,NSIZ,NKEY,NN,LIOP,LSTNUM)
C----------------------------------------------------------------------C
       INTEGER NC,J,SNODE,DEGREE,MINDEG,ISTRT,ISTOP,HSIZE,NODE,JSTRT,
     +        JSTOP,EWIDTH,I,WIDTH,DEPTH,ENODE,N,SDEPTH,MINWD,MINDP,
     +        MNODE,SDEPTH0,SWIDTH,WIDTH0,LSTNUM,NUMSAV,
     +        XADJ(N+1),ADJ(NSIZ),XLS(N+1),LS(N),MASK(N),
     +        HLEVEL(N),IW(N),NKEY(4),NN,LIOP
       REAL*8 SSUM,SSUM0
C
       SSUM0=1.0e30
       MINDEG=N
       DO 10 I=1,N
          IF(MASK(I).EQ.0) THEN
             DEGREE=XADJ(I+1)-XADJ(I)
             IF(DEGREE.LT.MINDEG) THEN
                SNODE=I
                MINDEG=DEGREE
             ENDIF
          ENDIF
   10  CONTINUE
C
       CALL ROOTLS(N,SNODE,N+1,ADJ,XADJ,MASK,LS,XLS,SDEPTH,WIDTH,NSIZ)
C
       NC=XLS(SDEPTH+1)-1
C
       MINDP=0
       MINWD=N
       print *,'SNODE,SDEPTH,WIDTH=',SNODE,SDEPTH,WIDTH
       IF(ABS(LIOP).GT.1) goto 40
       NN=1
C
       NC=XLS(SDEPTH+1)-1
C
   15  CONTINUE
C
       HSIZE=0
       ISTRT=XLS(SDEPTH)
       ISTOP=XLS(SDEPTH+1)-1
       DO 20 I=ISTRT,ISTOP
          NODE=LS(I)
          HSIZE=HSIZE+1
          HLEVEL(HSIZE)=NODE
          XLS(NODE)=XADJ(NODE+1)-XADJ(NODE)
   20  CONTINUE
C
       print *,'hsize=',hsize
       IF(HSIZE.GT.1) CALL ISORTI(HSIZE,HLEVEL,N,XLS)
C
       ISTOP=HSIZE
       HSIZE=1
       DEGREE=XLS(HLEVEL(1))
       DO 25 I=2,ISTOP
          NODE=HLEVEL(I)
          IF(XLS(NODE).NE.DEGREE) THEN
             DEGREE=XLS(NODE)
             HSIZE=HSIZE+1
             HLEVEL(HSIZE)=NODE
          ENDIF
   25  CONTINUE
C
       SDEPTH0=SDEPTH
       WIDTH0=MINWD
       EWIDTH=NC+1
       DO 30 I=1,HSIZE
          ENODE=HLEVEL(I)
C
          CALL ROOTLS(N,ENODE,NC+1,ADJ,XADJ,MASK,LS,XLS,DEPTH,WIDTH,
     +                NSIZ)
          print *,'NODE,DEPTH,WIDTH=',ENODE,DEPTH,WIDTH
          MINDEG=N
          JSTRT=XLS(DEPTH)
          JSTOP=XLS(DEPTH+1)-1
          DO K=JSTRT,JSTOP
             NODE=LS(K)
             DEGREE=XADJ(NODE+1)-XADJ(NODE)
             IF(DEGREE.LT.MINDEG) THEN
                MINDEG=DEGREE
                SNODE=NODE
             ENDIF
          ENDDO
          IF(DEPTH.GT.MINDP) THEN
             SDEPTH=DEPTH
             MINDP=DEPTH
             MINWD=WIDTH
             MNODE=ENODE
             NKEY(1)=MNODE
             NKEY(2)=SNODE
          ELSEIF(WIDTH.LT.MINWD.AND.DEPTH.GE.MINDP) THEN
             MINDP=DEPTH
             MINWD=WIDTH
             MNODE=ENODE
             NKEY(1)=MNODE
             NKEY(2)=SNODE
          ENDIF
   30  CONTINUE
       IF(SDEPTH.NE.SDEPTH0.OR.WIDTH0.NE.MINWD) THEN
          CALL ROOTLS(N,MNODE,NC+1,ADJ,XADJ,MASK,LS,XLS,SDEPTH,
     +                WIDTH,NSIZ)
          goto 15
       ENDIF
C
       NKEY(1)=MNODE
C
   40  IF(ABS(LIOP).LT.2) RETURN
C
C------IDENTIFY THE NODE WITH THE SMALLEST WIDTH
C
       SWIDTH=N
       MINDP=0
       HSIZE=0
       DO I=SDEPTH,1,-1
          ISTRT=XLS(I)
          ISTOP=XLS(I+1)-1
          DO J=ISTRT,ISTOP
             NODE=LS(J)
             DEGREE=XADJ(NODE+1)-XADJ(NODE)
             if(DEGREE.LE.36) THEN
                HSIZE=HSIZE+1
                HLEVEL(HSIZE)=NODE
                XLS(NODE)=DEGREE
             ENDIF
          ENDDO
       ENDDO
       IF(HSIZE.GT.1) CALL ISORTI(HSIZE,HLEVEL,N,XLS)
C
C------REMOVE THE NODES CONNECTED
C
       II=0
       DO I=1,HSIZE
          SNODE=HLEVEL(I)
          IF(SNODE.NE.0) THEN
             JSTRT=XADJ(SNODE)
             JSTOP=XADJ(SNODE+1)-1
             DO J=JSTRT,JSTOP
                DO K=I+1,HSIZE
                   IF(ADJ(J).EQ.HLEVEL(K)) THEN
                      HLEVEL(K)=0
                      II=II+1
                   ENDIF
                ENDDO
             ENDDO
          ENDIF
       ENDDO
       PRINT *,'HSIZE=',HSIZE,II
       DO I=1,HSIZE
          ENODE=HLEVEL(I)
          IF(ENODE.EQ.0) GOTO 50
          IF(ABS(LIOP).EQ.3) THEN
             CALL ROOTLS(N,ENODE,NC+1,ADJ,XADJ,MASK,LS,XLS,DEPTH,
     +                   WIDTH,NSIZ)
             MINDEG=N
             DO J=1,DEPTH
                JSTRT=XLS(J)
                JSTOP=XLS(J+1)-1
                DO K=JSTRT,JSTOP
                   NODE=LS(K)
                   IW(NODE)=J-1
                ENDDO
                IF(J.EQ.DEPTH) THEN
                   DO K=JSTRT,JSTOP
                      NODE=LS(K)
                      DEGREE=XADJ(NODE+1)-XADJ(NODE)
                      IF(DEGREE.LT.MINDEG) THEN
                         MINDEG=DEGREE
                         SNODE=NODE
                      ENDIF
                   ENDDO
                ENDIF
             ENDDO
             NUMSAV=LSTNUM
             CALL NUMBER(N,NC,SNODE,NUMSAV,ADJ,XADJ,IW,LS,
     +                   XLS,1,1,NSIZ)
             CALL PROFIL1(NC,IW,ADJ,XADJ,XLS,NSIZ,LCMAX,SSUM)
             IF(SSUM.lt.SSUM0) then
                NKEY(1)=ENODE
                NKEY(2)=SNODE
                SSUM0=SSUM
             ENDIF
             print *,'SSUM,LCMAX=',I,SNODE,DEPTH,WIDTH,SSUM0
             print *,'NKEY=',NKEY(1),NKEY(2),SSUM,LCMAX
          ELSE
             CALL ROOTLS(N,ENODE,SWIDTH,ADJ,XADJ,MASK,LS,XLS,DEPTH,
     +                   WIDTH,NSIZ)
             MINDEG=N
             JSTRT=XLS(DEPTH)
             JSTOP=XLS(DEPTH+1)-1
             DO K=JSTRT,JSTOP
                NODE=LS(K)
                DEGREE=XADJ(NODE+1)-XADJ(NODE)
                IF(DEGREE.LT.MINDEG) THEN
                   MINDEG=DEGREE
                   SNODE=NODE
                ENDIF
             ENDDO
             IF(WIDTH.LT.SWIDTH) THEN
                NKEY(1)=ENODE
                NKEY(2)=SNODE
                SWIDTH=WIDTH
                MINDP=DEPTH
             ELSEIF(WIDTH.EQ.SWIDTH) THEN
               IF(DEPTH.GT.MINDP) THEN
                   NKEY(1)=ENODE
                   NKEY(2)=SNODE
                   SWIDTH=WIDTH
                   MINDP=DEPTH
               ENDIF
             ENDIF
          ENDIF
   50  ENDDO
       NN=1
C
       END
C-----------------------------------------------------------------ROOTLS
       SUBROUTINE ROOTLS(N,ROOT,MAXWID,ADJ,XADJ,MASK,LS,XLS,DEPTH,WIDTH,
     +                   NSIZ)
C----------------------------------------------------------------------C
       INTEGER ROOT,DEPTH,NBR,MAXWID,LSTRT,LSTOP,LWDTH,NODE,NC,WIDTH,N,
     +         JSTRT,JSTOP,I,J,XADJ(N+1),ADJ(NSIZ),MASK(N),
     +         XLS(N+1),LS(N)
C
       MASK(ROOT)=1
       LS(1)=ROOT
       NC=1
       WIDTH=1
       DEPTH=0
       LSTOP=0
       LWDTH=1
   10  IF(LWDTH.GT.0) THEN
C
          LSTRT=LSTOP+1
          LSTOP=NC
          DEPTH=DEPTH+1
          XLS(DEPTH)=LSTRT
C
          DO 30 I=LSTRT,LSTOP
             NODE=LS(I)
             JSTRT=XADJ(NODE)
             JSTOP=XADJ(NODE+1)-1
             DO 20 J=JSTRT,JSTOP
                NBR=ADJ(J)
                IF(MASK(NBR).EQ.0) THEN
                   NC=NC+1
                   LS(NC)=NBR
                   MASK(NBR)=1
                ENDIF
   20        CONTINUE
   30     CONTINUE
C
          LWDTH=NC-LSTOP
          WIDTH=MAX(LWDTH,WIDTH)
C
          IF(WIDTH.GT.MAXWID) GOTO 35
          GOTO 10
       ENDIF
       XLS(DEPTH+1)=LSTOP+1
C
   35  CONTINUE
       DO 40 I=1,NC
          MASK(LS(I))=0
   40  CONTINUE
C
       END
C-----------------------------------------------------------------ISORTI
       SUBROUTINE ISORTI(NL,LIST,NK,KEY)
C----------------------------------------------------------------------C
       INTEGER NL,NK,I,J,T,VALUE,LIST(NL),KEY(NK)
C
       DO 20 I=2,NL
          T=LIST(I)
          VALUE=KEY(T)
          DO 10 J=I-1,1,-1
             IF(VALUE.GE.KEY(LIST(J))) THEN
                LIST(J+1)=T
                GOTO 20
             ENDIF
             LIST(J+1)=LIST(J)
   10     CONTINUE
          LIST(1)=T
   20  CONTINUE
C
       END
C-----------------------------------------------------------------NUMBER
       SUBROUTINE NUMBER(N,NC,SNODE,LSTNUM,ADJ,XADJ,S,Q,P,W1,W2,NSIZ)
C----------------------------------------------------------------------C
       INTEGER NC,LSTNUM,JSTRT,JSTOP,ISTOP,NBR,NABOR,I,J,NEXT,ADDRES,NN,
     +         NODE,SNODE,ISTRT,MAXPRT,PRTY,N,W1,W2,Q(NC),XADJ(N+1),
     +         ADJ(NSIZ),P(N),S(N)
C
       DO 10 I=1,NC
          NODE=Q(I)
          P(NODE)=W1*S(NODE)-W2*(XADJ(NODE+1)-XADJ(NODE)+1)
          S(NODE)=-2
   10  CONTINUE
C
       NN=1
       Q(NN)=SNODE
       S(SNODE)=-1
C
   30  IF(NN.GT.0) THEN
C
          ADDRES=1
          MAXPRT=P(Q(1))
          DO 35 I=2,NN
             PRTY=P(Q(I))
             IF(PRTY.GT.MAXPRT) THEN
                ADDRES=I
                MAXPRT=PRTY
             ENDIF
   35     CONTINUE
C
          NEXT=Q(ADDRES)
C
          Q(ADDRES)=Q(NN)
          NN=NN-1
          ISTRT=XADJ(NEXT)
          ISTOP=XADJ(NEXT+1)-1
          IF(S(NEXT).EQ.-1) THEN
C
             DO 50 I=ISTRT,ISTOP
C
                NBR=ADJ(I)
                P(NBR)=P(NBR)+W2
C
                IF(S(NBR).EQ.-2) THEN
                   NN=NN+1
                   Q(NN)=NBR
                   S(NBR)=-1
                ENDIF
   50        CONTINUE
          ENDIF
C
          LSTNUM=LSTNUM+1
          S(NEXT)=LSTNUM
C
          DO 80 I=ISTRT,ISTOP
             NBR=ADJ(I)
             IF(S(NBR).EQ.-1) THEN
C
                P(NBR)=P(NBR)+W2
                S(NBR)=0
C
                JSTRT=XADJ(NBR)
                JSTOP=XADJ(NBR+1)-1
                DO 60 J=JSTRT,JSTOP
                   NABOR=ADJ(J)
C
                   P(NABOR)=P(NABOR)+W2
                   IF(S(NABOR).EQ.-2) THEN
C
                      NN=NN+1
                      Q(NN)=NABOR
                      S(NABOR)=-1
                   ENDIF
   60           CONTINUE
             ENDIF
   80     CONTINUE
          GOTO 30
       ENDIF
C
       END
C----------------------------------------------------------------PROFIL
       SUBROUTINE PROFIL(N,NNN,ADJ,XADJ,NSIZ,NEWPRO)
C---------------------------------------------------------------------C
       INTEGER NEWPRO,I,J,N,JSTRT,JSTOP,OLDPRO,NEWMIN,OLDMIN,OLDMAX,
     +         NEWMAX,NNN(N),XADJ(N+1),ADJ(NSIZ)
C
       NEWPRO=0
       DO 20 I=1,N
          JSTRT=XADJ(I)
          JSTOP=XADJ(I+1)-1
          NEWMIN=NNN(ADJ(JSTRT))
          NEWMAX=NNN(ADJ(JSTRT))
C
          DO 10 J=JSTRT+1,JSTOP
             NEWMIN=MIN(NEWMIN,NNN(ADJ(J)))
             NEWMAX=MAX(NEWMAX,NNN(ADJ(J)))
   10     CONTINUE
C
C         NEWPRO=NEWPRO+DIM(NEWMAX,NEWMIN)
          NEWPRO=NEWPRO+DIM(NNN(I),NEWMIN)
   20  CONTINUE
C
       NEWPRO=NEWPRO+N
C
       END
C----------------------------------------------------------------PROFIL
       SUBROUTINE PROFIL1(N,NNN,ADJ,XADJ,MASK,NSIZ,LCMAX,SSUM)
C---------------------------------------------------------------------C
       INTEGER NEWPRO,I,J,N,JSTRT,JSTOP,NSIZ,LCMAX,LCOL,
     +         NNN(N),XADJ(N+1),ADJ(NSIZ),MASK(N*2)
       REAL*8  SSUM
C
       LCOL=0
       LCMAX=0
       SSUM=0.0
       DO I=1,N
          MASK(NNN(I))=I
          NNN(I)=0
       ENDDO
C
       DO I=1,N
          IF(NNN(MASK(I)).EQ.0) THEN
             LCOL=LCOL+1
             NNN(MASK(I))=1
          ENDIF
          JSTRT=XADJ(MASK(I))
          JSTOP=XADJ(MASK(I)+1)-1
          DO J=JSTRT,JSTOP
             IF(NNN(ADJ(J)).EQ.0) THEN
                LCOL=LCOL+1
                NNN(ADJ(J))=1
             ENDIF
          ENDDO
          SSUM=SSUM+LCOL**2
          LCMAX=MAX(LCMAX,LCOL)
          LCOL=LCOL-1
       ENDDO
       print *,'LCOL=',LCOL
       DO I=1,N
          NNN(MASK(I))=I
       ENDDO
C
       END
