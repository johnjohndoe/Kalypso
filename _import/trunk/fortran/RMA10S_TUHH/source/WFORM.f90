!     Last change:  NIS  16 Aug 2007    7:23 pm
!ipk last update Oct 2 2003 revise frIction equivalence
!IPK LAST UPDATE JUL30 2000 REVISE VERSION
!ipk  new routine Mar 1 2000
!
       SUBROUTINE WFORM(Q,H1,W1,V1,H2,W2,V2,EC,L,ITP,WIDEM)
       REAL L
!
!      Q   is flow per unit width
!      UH  is upstream level above bed
!      W   is upstream water surface elevation
!      UV  is upstream velocity
!      DH  is downstream level above bed
!      DW  is downstream water surface  level
!      DV  is downstram velocity
!      EC  is elevation of weir crest
!      L   is width of crest
!      ITP is switch controlling surface type 0 = paved, 1 = gravel
!      WIDEM is total embankment width
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: H1, H2
!-
!
!      test direction of flow
!
       IPRT=0
       todepth=2.0
       toldoh=1.000
!ipk oct03
       cscf=1.0
!
       IF(W1 > W2) THEN
         UH=H1
         WS=W1
         UV=V1
         DD=H2
         DW=W2
         DV=V2
!
!      flow is downstream (convention)
!
         ICONV=1
       ELSE
         UH=H2
         WS=W2
         UV=V2
         DD=H1
         DW=W1
         DV=V1
!
!      flow is upstream (convention)
!
         ICONV=2
       ENDIF
!
!      H  = upstream depth over the weir
!      DH = downstream depth over the weir
!
       H=WS-EC
       DH=DW-EC
       IF(H > 0.) THEN  
!
!      get total head upstream and downstream relative to weir crest
!
         HEAD=H+UV*UV/(2.*9.80)
!IPK JUN03
         HEAD2=DV**2/(2*9.80)+DH
!
         IF(HEAD2 > 0.) THEN
!
!      SE = ratio of heads upstream to downstream
!
           SE=HEAD2/HEAD
         ELSE
           SE=0.
         ENDIF
!
!      HOL = ratio of head to length of crest
!      DOH = ratio of downstream depth over weir to upstream head over weir
!
         HOL=HEAD/L
         DOH=DH/HEAD
!
         IF(HOL < 0.16) THEN
!
!      look in table A for conveyance factor CF
!
           IF(ITP == 1) THEN
!
!      paved
!
             IF(HEAD < 0.1) THEN
               CF=1.59+HEAD*0.8
             ELSEIF(HEAD < 0.9) THEN
               CF=1.67
             ELSEIF(HEAD < 1.2) THEN
               CF=1.67 + (HEAD-0.9)*0.2/3.
             ELSE
               CF=1.69
             ENDIF
!
             cscf=1.9*hol**0.22-0.28
!LF nov06 the following command is cancelled, otherwise cf is always 1.69
!             cf=1.69
           ELSE
!
!     gravel
!
             IF(HEAD < 0.9) THEN
               CF=1.38+0.3*SQRT(HEAD)
             ELSEIF(HEAD < 1.2) THEN
               CF=1.67 + (HEAD-0.9)*0.2/3.
             ELSE
               CF=1.69
             ENDIF
           ENDIF
         ELSE
!
!      look in table B
!
           IF(ITP == 1) THEN
!
!      paved
!
             CF=1.686+0.05*SQRT(HOL-0.16)
           ELSE
!
!     gravel
!
             IF(HOL < 0.288) THEN
               CF=1.634+0.07*HOL/.128   
             ELSE
               CF=1.686+0.05*SQRT(HOL-0.16)
             ENDIF
           ENDIF
         ENDIF         
         IF(DOH < 0.76 ) THEN
!ipk sep03 remove 0.5 test         IF(DOH < 0.76 .AND. DH < 0.5) THEN
!
!     not submerged apply formula
!
!ipk oct03
           Q=cscf*CF*HEAD*SQRT(HEAD)
!
!     TEST FOR SIMPLE FRICTION EQUATION 
!
           SLOP= ABS(W1-W2)/L
!ipk oct03 change to reduce to .10 and use crest width
!            QTEMP=UH**0.6667*SQRT(SLOP)/0.60*UH
           QTEMP=UH**0.6667*SQRT(SLOP)/0.10*UH
           Q1=Q
           IF(QTEMP < Q) THEN
             Q=QTEMP
           ENDIF
           IF(ICONV == 2) Q=-Q
         ELSE
!
!      test if weir totally submerged
!
           IF(HEAD > TODEPTH .AND. DOH > TOLDOH) THEN
             Q=0.
             RETURN
           ENDIF
!
!
!     get submergence correction
!
           IF(ITP == 1) THEN
!
!      paved
!
!ipk dec03
             CSCF1=1.0-0.65*((DOH-0.76)/.24)**4
           ELSE
!
!     gravel
!
!ipk dec03
             CSCF1=1.0-0.80*((DOH-0.76)/.24)**3
           ENDIF
!ipk dec03
!cc           if(cscf > cscf1) cscf=cscf
           if(cscf > cscf1) cscf=cscf1
           Q= CF*HEAD*SQRT(HEAD)*CSCF
!
!
!     TEST FOR SIMPLE FRICTION EQUATION 
!
           SLOP= ABS(W1-W2)/widem
!ipk oct03 change to reduce to .10 and use full width
           QTEMP=UH**0.6667*SQRT(SLOP)/0.10*UH
           Q1=Q
           IF(QTEMP < Q) THEN
             Q=QTEMP
           ENDIF
!
           IF(ICONV == 2) Q=-Q
         ENDIF
       ELSE
         Q=0.
       ENDIF
       IF(Q /= 0.) THEN
!cc         WRITE(69,*) 'Q,Q1,qtemp,H1,H2,H,EC,HEAD,DOH,DH,HOL,CF,CSCF'
!cc     +   ,Q,Q1,qtemp,H1,H2,H,EC,HEAD,DOH,DH,HOL,CF,CSCF
!         WRITE(69,'(6F11.5,2I5)') H1,H2,H,W1,W2,EC,ICONV
!IPK FEB04  REDUCE VELS FOR VERY SHALLOW       IF(IPRT == 1) THEN
!C         WRITE(166,'(5F15.6)') Q,CSCF,CF,QTEMP,Q1
         IF(H < 0.1) THEN
             Q=Q*H*10.
         ENDIF
!C         ENDIF
       ENDIF
!c         if(3 == 1770 ) then
!c         write(69,*) 'W1,W2,W1-W2,ICONV',w1,w2,w1-w2,iconv
!c         WRITE(69,*) 'Q,Q1,qtemp',Q,Q1,qtemp
!c         write(69,*) 'H1,H2,H,EC,HEAD',H1,H2,H,EC,HEAD
!c         write(69,*) 'DOH,DH,HOL,CF,CSCF',DOH,DH,HOL,CF,CSCF
!c         endif
       RETURN
       END
!
!
!
!
!
