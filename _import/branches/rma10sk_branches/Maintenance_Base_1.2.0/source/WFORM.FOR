C     Last change:  NIS  16 Aug 2007    7:23 pm
cipk last update Oct 2 2003 revise frIction equivalence
CIPK LAST UPDATE JUL30 2000 REVISE VERSION
cipk  new routine Mar 1 2000

       SUBROUTINE WFORM(Q,H1,W1,V1,H2,W2,V2,EC,L,ITP,WIDEM)
       REAL L
c
c      Q   is flow per unit width
c      UH  is upstream level above bed
c      W   is upstream water surface elevation
c      UV  is upstream velocity
c      DH  is downstream level above bed
c      DW  is downstream water surface  level
c      DV  is downstram velocity
c      EC  is elevation of weir crest
c      L   is width of crest
c      ITP is switch controlling surface type 0 = paved, 1 = gravel
C      WIDEM is total embankment width
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: H1, H2
!-
c
c      test direction of flow
c
       IPRT=0
       todepth=2.0
       toldoh=1.000
cipk oct03
       cscf=1.0
       
       IF(W1 .GT. W2) THEN
         UH=H1
         WS=W1
         UV=V1
         DD=H2
         DW=W2
         DV=V2
c
c      flow is downstream (convention)
c
         ICONV=1
       ELSE
         UH=H2
         WS=W2
         UV=V2
         DD=H1
         DW=W1
         DV=V1
c
c      flow is upstream (convention)
c
         ICONV=2
       ENDIF

C      H  = upstream depth over the weir
c      DH = downstream depth over the weir

       H=WS-EC
       DH=DW-EC
       IF(H .GT. 0.) THEN  
c
c      get total head upstream and downstream relative to weir crest
c
         HEAD=H+UV*UV/(2.*9.80)
CIPK JUN03
         HEAD2=DV**2/(2*9.80)+DH
 
         IF(HEAD2 .GT. 0.) THEN

c      SE = ratio of heads upstream to downstream

           SE=HEAD2/HEAD
         ELSE
           SE=0.
         ENDIF

c      HOL = ratio of head to length of crest
c      DOH = ratio of downstream depth over weir to upstream head over weir

         HOL=HEAD/L
         DOH=DH/HEAD

         IF(HOL .LT. 0.16) THEN
c
c      look in table A for conveyance factor CF
c
           IF(ITP .EQ. 1) THEN
c
c      paved
c
             IF(HEAD .LT. 0.1) THEN
               CF=1.59+HEAD*0.8
             ELSEIF(HEAD .LT. 0.9) THEN
               CF=1.67
             ELSEIF(HEAD .LT. 1.2) THEN
               CF=1.67 + (HEAD-0.9)*0.2/3.
             ELSE
               CF=1.69
             ENDIF

             cscf=1.9*hol**0.22-0.28
cLF nov06 the following command is cancelled, otherwise cf is always 1.69
c	       cf=1.69
           ELSE
c
c     gravel
c
             IF(HEAD .LT. 0.9) THEN
               CF=1.38+0.3*SQRT(HEAD)
             ELSEIF(HEAD .LT. 1.2) THEN
               CF=1.67 + (HEAD-0.9)*0.2/3.
             ELSE
               CF=1.69
             ENDIF
           ENDIF
         ELSE
c
c      look in table B
c
           IF(ITP .EQ. 1) THEN
c
c      paved
c
             CF=1.686+0.05*SQRT(HOL-0.16)
           ELSE
c
c     gravel
c
             IF(HOL .LT. 0.288) THEN
               CF=1.634+0.07*HOL/.128   
             ELSE
               CF=1.686+0.05*SQRT(HOL-0.16)
             ENDIF
           ENDIF
         ENDIF         
         IF(DOH .LT. 0.76 ) THEN
cipk sep03 remove 0.5 test         IF(DOH .LT. 0.76  .and. DH .lt. 0.5) THEN
c
c     not submerged apply formula
c
cipk oct03
           Q=cscf*CF*HEAD*SQRT(HEAD)
C
C     TEST FOR SIMPLE FRICTION EQUATION 
C
           SLOP= ABS(W1-W2)/L
cipk oct03 change to reduce to .10 and use crest width
c            QTEMP=UH**0.6667*SQRT(SLOP)/0.60*UH
           QTEMP=UH**0.6667*SQRT(SLOP)/0.10*UH
           Q1=Q
           IF(QTEMP .LT. Q) THEN
             Q=QTEMP
           ENDIF
           IF(ICONV .EQ. 2) Q=-Q
         ELSE
c
c      test if weir totally submerged
c
           IF(HEAD .GT. TODEPTH  .AND.  DOH .GT. TOLDOH) THEN
             Q=0.
             RETURN
           ENDIF
              
c
c     get submergence correction
c
           IF(ITP .EQ. 1) THEN
c
c      paved
c
cipk dec03
             CSCF1=1.0-0.65*((DOH-0.76)/.24)**4
           ELSE
c
c     gravel
c
cipk dec03
             CSCF1=1.0-0.80*((DOH-0.76)/.24)**3
           ENDIF
cipk dec03
ccc           if(cscf .gt. cscf1) cscf=cscf
           if(cscf .gt. cscf1) cscf=cscf1
           Q= CF*HEAD*SQRT(HEAD)*CSCF

C
C     TEST FOR SIMPLE FRICTION EQUATION 
C
           SLOP= ABS(W1-W2)/widem
cipk oct03 change to reduce to .10 and use full width
           QTEMP=UH**0.6667*SQRT(SLOP)/0.10*UH
           Q1=Q
           IF(QTEMP .LT. Q) THEN
             Q=QTEMP
           ENDIF

           IF(ICONV .EQ. 2) Q=-Q
         ENDIF
       ELSE
         Q=0.
       ENDIF
       IF(Q .NE. 0.) THEN
ccc         WRITE(69,*) 'Q,Q1,qtemp,H1,H2,H,EC,HEAD,DOH,DH,HOL,CF,CSCF'
ccc     +   ,Q,Q1,qtemp,H1,H2,H,EC,HEAD,DOH,DH,HOL,CF,CSCF
c         WRITE(69,'(6F11.5,2I5)') H1,H2,H,W1,W2,EC,ICONV
CIPK FEB04  REDUCE VELS FOR VERY SHALLOW       IF(IPRT .EQ. 1) THEN
CC         WRITE(166,'(5F15.6)') Q,CSCF,CF,QTEMP,Q1
	   IF(H .LT. 0.1) THEN
  	     Q=Q*H*10.
         ENDIF
CC	   ENDIF
       ENDIF
cc	   if(3 .eq. 1770 ) then
cc         write(69,*) 'W1,W2,W1-W2,ICONV',w1,w2,w1-w2,iconv
cc         WRITE(69,*) 'Q,Q1,qtemp',Q,Q1,qtemp
cc	   write(69,*) 'H1,H2,H,EC,HEAD',H1,H2,H,EC,HEAD
cc	   write(69,*) 'DOH,DH,HOL,CF,CSCF',DOH,DH,HOL,CF,CSCF
cc	   endif
       RETURN
       END




               
