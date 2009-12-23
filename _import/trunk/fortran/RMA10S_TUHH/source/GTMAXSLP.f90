      SUBROUTINE GETMAXSLP(NN,bslcrit)
      USE BLKSMOD
      USE BLK10MOD
      USE BLKSANMOD
!
!     Routine to get max slop in element NN
!
      USE COEF2MOD, only: XM, DMX, DMY, XL, YL
!
      real*8 elmin,elmax
!
      REAL J11,J12,J21,J22
      DIST(N1,N2)=                                                      &
     &  SQRT((CORD(N1,1)-CORD(N2,1))**2+(CORD(N1,2)-CORD(N2,2))**2)
!
      IF(NN == 10 .OR. NN == 130 .OR. NN == 3 .OR. NN == 123)           &
     &   THEN
         AAA=0
      ENDIF
      NCN=NCORN(NN)
      IF(NCN == 6) THEN
!
!     Work  for triangle
!
        ELMIN=MIN(AO(NOP(NN,1)),AO(NOP(NN,3)),AO(NOP(NN,5)))
        ELMAX=MAX(AO(NOP(NN,1)),AO(NOP(NN,3)),AO(NOP(NN,5)))
!
!     First get them in order
!
        ILMIN=0
        ILMAX=0
        DO M=1,5,2
          IF(AO(NOP(NN,M)) == ELMIN .AND. ILMIN == 0) THEN
              ILMIN=NOP(NN,M)
          ELSEIF(AO(NOP(NN,M)) == ELMAX .AND. ILMAX == 0) THEN
              ILMAX=NOP(NN,M)
          ELSE
            ILMID=NOP(NN,M)
            ELMID=AO(ILMID)
          ENDIF
        ENDDO
          IF(ELMIN == ELMAX) THEN
          SLPMAX=0.0
          RETURN
        ENDIF
      ELSE
!
!     Work  for quad
!
        ELMIN=                                                          &
     &  MIN(AO(NOP(NN,1)),AO(NOP(NN,3)),AO(NOP(NN,5)),AO(NOP(NN,7)))
        ELMAX=                                                          &
     &  MAX(AO(NOP(NN,1)),AO(NOP(NN,3)),AO(NOP(NN,5)),AO(NOP(NN,7)))
!
!     First get them in order
!
        ILMIDL=0
        ILMIDU=0
        ILMIN=0
        ILMAX=0
        DO M=1,7,2
          IF(AO(NOP(NN,M)) == ELMIN .AND. ILMIN == 0) THEN
              ILMIN=NOP(NN,M)
          ELSEIF(AO(NOP(NN,M)) == ELMAX .AND. ILMAX == 0) THEN
              ILMAX=NOP(NN,M)
          ELSE
            IF(ILMIDL == 0) THEN
                ILMIDL=NOP(NN,M)
              ELMIDL=AO(ILMIDL)
            ELSE
              ILMIDU=NOP(NN,M)
              ELMIDU=AO(ILMIDU)
              IF(AO(ILMIDL) > AO(ILMIDU)) THEN
                ITEMP=ILMIDL
                 ILMIDL=ILMIDU
                ILMIDU=ITEMP
                ELMIDL=AO(ILMIDL)
                ELMIDU=AO(ILMIDU)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
          IF(ELMIN == ELMAX) THEN
          SLPMAX=0.0
          RETURN
        ENDIF
      ENDIF
!-
!-.....COMPUTE LOCAL CORDS.....
!
      MR=NOP(NN,1)
      DO K = 1, NCN
        N=NOP(NN,K)
        DX=CORD(N,1)-CORD(MR,1)
        DY=CORD(N,2)-CORD(MR,2)
        XL(K)=DX
        YL(K)=DY
      ENDDO
      NCNX=NCN/2
      IF( NCN < 8 ) THEN
        NGP = 7
      ELSE
        NGP = 9
      ENDIF
!-
!-.....COPY SHAPE FUNCTIONS
!-
      CALL SB2(NCN,NGP)
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
      IF(NCN == 6) THEN
        I=1
      ELSE
        I=5
      ENDIF
!-
!-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
!-
      J11 = 0.0
      J12 = 0.0
      J21 = 0.0
      J22 = 0.0
!
      DO  K = 2, NCN
        J11 = J11 + DA(K,I) * XL(K)
        J12 = J12 + DA(K,I) * YL(K)
        J21 = J21 + DB(K,I) * XL(K)
        J22 = J22 + DB(K,I) * YL(K)
      ENDDO
!
      DETJ = J11 * J22 - J12 * J21
!-
!-      FOR LINEAR FUNCTION
!-
      JJ=0
      DO  J=1,NCN,2
        JJ=JJ+1
        XM(JJ)=XMX(JJ,I)
        DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/DETJ
        DMY(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
      ENDDO
!
      BSX=0.
      BSY=0.
      DO J=1,NCNX
        BSX=BSX+DMX(J)*AO(NOP(NN,2*J-1))
        BSY=BSY+DMY(J)*AO(NOP(NN,2*J-1))
      ENDDO
!
!     BSMAX is the max bed slope at the centrod of the element
!
      BSMAX=SQRT(BSX**2+BSY**2)
!
!     Test if is exceeds the critical value
!
      if(bsmax > bslcrit) then
        write(144,'(2i8,2g15.5)') icyc,nn,bsmax,bslcrit
        ratioc=bslcrit/bsmax
!
!     RATIOC is the amount over critical 
!
        IF(NCN == 6) THEN
!
!     For triangles get the slopes along the edges to determine where the load will go
!
          sl1=(elmax-elmin)/dist(ilmax,ilmin)
          sl2=(elmax-elmid)/dist(ilmax,ilmid)
          deltatp=(1.-ratioc)*(elmax-elmin)
!
!     DELTATP is the amount to be removed at this node
!
            if(sl1 > sl2) then
!
!     This is the case where it goes to the lowest elevation node
!
            if(trslop(ilmax) == 0.) then
              if(deltatp > deltab(ilmax)) then
                deltab(ilmax)=deltatp
                nsrc(ilmax)=ilmin
                trslp(ilmax)=sl1
                   endif
            else
              if(sl1 > trslp(ilmax)) then
                deltab(ilmax)=deltatp
                nsrc(ilmax)=ilmin
                  trslp(ilmax)=sl1
              endif
            endif
!     Now move the middle node downwards
!
            deltatp=(1.-ratioc)*(elmid-elmin)
            if(deltatp > deltab(ilmid)) then
              deltab(ilmid)=deltatp
              nsrc(ilmid)=ilmin
              trslp(ilmid)=sl2
            endif
          else
!
!     This is the case where it goes to the middle node
!
            if(deltatp > deltab(ilmax)) then
              deltab(ilmax)=deltatp
              nsrc(ilmax)=ilmid
            endif
          endif
        ELSE
!
!     For quads get the slopes along the edges to determine where the load will go
!
          sl1=(elmax-elmin)/dist(ilmax,ilmin)
          sl2=(elmax-elmidl)/dist(ilmax,ilmidl)
          deltatp=(1.-ratioc)*(elmax-elmin)
            if(sl1 > sl2) then
!
!     This is the case where it goes to the lowest elevation node
!
            if(deltatp > deltab(ilmax)) then
              deltab(ilmax)=deltatp
              nsrc(ilmax)=ilmin
            endif
          else
!
!     This is the case where it goes to the bode above the lowest elevation node
!
            if(deltatp > deltab(ilmax)) then
              deltab(ilmax)=deltatp
              nsrc(ilmax)=ilmidl
            endif
          endif
!
!     For quads get the slopes along the edges from the upper mid node 
!     to determine where the load will go
!
          sl1=(elmidu-elmin)/dist(ilmidu,ilmin)
          sl2=(elmidu-elmidl)/dist(ilmidu,ilmidl)
          deltatp=(1.-ratioc)*(elmidu-elmin)
            if(sl1 > sl2) then
!
!     This is the case where it goes to the lowest elevation node
!
            if(deltatp > deltab(ilmidu)) then
              deltab(ilmidu)=deltatp
              nsrc(ilmidu)=ilmin
            endif
          else
!
!     This is the case where it goes to the bode above the lowest elevation node
!
            if(deltatp > deltab(ilmidu)) then
              deltab(ilmidu)=deltatp
              nsrc(ilmidu)=ilmidl
            endif
          endif
!
        ENDIF
      endif
!
      RETURN
      END