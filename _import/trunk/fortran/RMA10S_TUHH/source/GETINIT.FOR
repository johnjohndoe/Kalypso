CIPK  LAST UPDATE APRIL 05 2006 ADD MULTUPILE ENTRIES SO THAT SS INITIAL CONDITIONS CAN BE READ
cipk  last update FEB 11 2004  add IOV option
cipk  last update sEP 30 2002 ADD READ OF ICETHICKNESS FROM RESTART FILE
cipk  last update Dec 6 1999 allow for INIT data
cipk  last updated Aug 7 1998 revise restart format
CIPK  LAST UPDATED MAR 23 1998
CIPK  LAST UPDATED DEC 5 1997
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATE MAY 1 1996
      SUBROUTINE GETINIT(IBIN,ITIMESINIT)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKFTMOD
	!********************** DJW 20/07/04
      USE WBMMODS
      !********************** 
      SAVE
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK MAR98
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
C-
CIPK APR06
      REAL*8 VDUM
      DIMENSION TV(3),TVD(3)
C-
C-.....INPUT INITIAL CONDITIONS, IF SPECIFIED.....
C-
      IF( NB .EQ. 0 ) GO TO 170
      NXX = NB
      NB = IABS(NB)

CIPK APR06   READ FOR SUS SED DATA ONLY
      IF(ITIMESINIT .EQ. 1) GO TO 120

      REWIND NB
      READ(NB) TETT,NPX,NDX,IYRR
      REWIND NB
      WRITE(*,*) TETT,NPX,NDX,IYRR,NDF
      IF(LSAND .EQ. 0   .AND.  LSS .EQ. 0) THEN
        READ(NB) TETT,NPX,NDX,IYRR,((VDUM,VDUM1,K=1,7)
     +           ,VVELDUM,J=1,NPX)
     +     ,(helDUM,hdetDUM,j=1,npx)

      ELSEIF(LSAND .GT. 0) THEN
          READ(NB) TETT,NPX,NDX,IYRR,((VDUM,VDUM1,K=1,7)
     +           ,VVELDUM,J=1,NPX)
     +     ,(helDUM,hdetDUM,j=1,npx)
     +   ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NPX)
	ELSE
	  IF(INEWBED .EQ. 0) THEN
          READ(NB) TETT,NPX,NDX,IYRR,((VDUM,VDUM1,K=1,7)
     +           ,VVELDUM,J=1,NPX)
     +     ,(helDUM,hdetDUM,j=1,npX)
     +     ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NPX)
     +     ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
     &     ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NPX)
	  ELSE
	    READ(NB) TETT,NPX,NDX,IYRR
	    REWIND (NB)
          READ(NB) TETT,NPX,NDX,IYRR,((VDUM,VDUM1,K=1,7)
     +           ,VVELDUM,J=1,NPX)
     +     ,(helDUM,hdetDUM,j=1,npX)
        ENDIF
      ENDIF
      RETURN
      
CIPK APR06 READ FOR MAIN DATA BUT DO NOT READ SS VALUES
  120 CONTINUE            
      IF(NB .GE. 100 ) GO TO 165
      REWIND NB
      READ(NB) TETT,NPX,NDX,IYRR
      REWIND NB
      WRITE(*,*) TETT,NPX,NDX,IYRR,NDF
      IF(LSAND .EQ. 0   .AND.  LSS .EQ. 0) THEN
        READ(NB) TETT,NPX,NDX,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NPX)
     +     ,(hel(j),hdet(j),j=1,npx)

      ELSEIF(LSAND .GT. 0) THEN
          READ(NB) TETT,NPX,NDX,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NPX)
     +     ,(hel(j),hdet(j),j=1,npx)
     +   ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NPX)
	ELSE
	  IF(INEWBED .EQ. 0) THEN
          READ(NB) TETT,NPX,NDX,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NPX)
     +     ,(hel(j),hdet(j),j=1,npX)
CIPK APR06     +     ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NPX)
CIPK APR06     +     ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
CIPK APR06     &     ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NPX)
	  ELSE
	    READ(NB) TETT,NPX,NDX,IYRR
	    REWIND (NB)
          READ(NB) TETT,NPX,NDX,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NPX)
     +     ,(hel(j),hdet(j),j=1,npX)
     	  ENDIF
      ENDIF	    



CIPK SEP02 ADD RESTART DATA FOR BED
CIPK SEP02 ADD ICE THICKNESS READ IF NEEDED
cipk aug98 add line above for consistent restart

      IF(ICESW .GT. 0) THEN
        READ(NB) (ICETHK(J) ,J=1,NPM)
	ENDIF

cipk FEB04  add IOV option
      IF(IOV .EQ. 1) THEN
        IYRR=IYKK
        DAYOFY=IDTM
        TET=TTEM
      ELSE
cipk MAY96 decode TET
        IDAY=TETT/24.
        DAYOFY=IDAY
        TET=TETT-DAYOFY*24.
        DAYOFY=DAYOFY+1
      ENDIF
      write(LOUT,6240)  iyrr,dayofy,tet
6240  FORMAT(//' RESTART FOR YEAR',I6,'  DAY',I5,'  HOUR',F8.2)

cipk nov02 set bed to input level
      do N=1,npx

      IF(N .EQ. 2150  .OR.  N .EQ. 2359  .OR.  N .EQ. 1836) THEN
	AAA=0
	ENDIF
CIPK APR03
        VAAA=VEL(3,N)
        CALL AMF(HEL(N),VAAA,AKP(N),ADT(N),ADB(N),D1,D2,0)
CIPK JUN05
	  IF(LSAND .GT. 0) THEN
	    DIFF=ELEVB(N)-AO(N)
	    AO(N)=ELEVB(N)
          ADO(N)=ADO(N)+DIFF
        ENDIF
        WSLL(N)=HEL(N)+ADO(N)
C        HEL(N)=WSLL(N)-ADO(N)
        CALL AMF(HEL(N),VAAA,AKP(N),ADT(N),ADB(N),D1,D2,1)
        VEL(3,N)=VAAA
	enddo
CIPK DEC97 DEVELOP VALUE FOR HEL
cipk aug98 read in instead
cipk aug98      DO J=1,NPX
cipk aug98        HTP=VEL(3,J)
cipk aug98        CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,0)
cipk aug98      ENDDO

C  *** CALL PRESR TO HAVE DENSITIES FOR MELLII FIRST HOT START ITER
      CALL PRESR
      DO 159 N=1,NP
      DO 159 K=1,NDF
      VDOTO(K,N)=VDOT(K,N)
  159 VOLD(K,N)=VEL(K,N)
      IF(NPX .GE. NP) GO TO 164
      SCFC=6./(UMIN+1.+4.*(UMIN+(1.-UMIN)*0.5**PWERIN))
      DO 163 M=1,NPM
      IF(NDEP(M) .EQ. 1) GO TO 163
      NL=NREF(M)+1
      NT=NL+NDEP(M)-2
      DO 161 N=NL,NT
      FCTV(N)=SCFC*(UMIN+(1.-UMIN)*CORD(N,3)**PWERIN)
      DO 160 K=1,NDF
      FA=FCTV(N)
      IF( K .EQ. 3) FA=1.0
      VEL(K,N)=VEL(K,M)*FA
      VOLD(K,N)=VEL(K,M)*FA
      VDOT(K,N)=VDOT(K,M)*FA
      VDOTO(K,N)=VDOT(K,M)*FA
  160 CONTINUE
      VVEL(N)=VVEL(M)
  161 CONTINUE
      FCTV(M)=SCFC*(UMIN+(1.-UMIN)*CORD(M,3)**PWERIN)
      DO 162 K=1,NDF
      FA=FCTV(M)
      IF( K .EQ. 3) FA=1.0
      VEL(K,M)=VEL(K,M)*FA
      VOLD(K,M)=VEL(K,M)*FA
      VDOT(K,M)=VDOT(K,M)*FA
      VDOTO(K,M)=VDOT(K,M)*FA
  162 CONTINUE
  163 CONTINUE
  164 IF( NXX  .GT. 0 ) GO TO 175
  165 READ(IBIN,5032) M,TV,TVD
      IF( M .GE. 9999 ) GO TO 175
      DO 167 K = 1, NDF
      VEL(K,M) = TV(K)
      VDOT(K,M) = TVD(K)
  167 CONTINUE
      GO TO 165
C-
C-.....INITIALIZE PRIMARY STATE VARIABLE ARRAY.....
C-
  170 CONTINUE
      DO 171 J = 1, NP
	VEL(1,J) = 0.00
	VEL(2,J) =0.00
CIPK NOV9T	VEL(3,J) = ELEV - AO(J)
CIPK NOV97	IF(VEL(3,J) .LT. HMIN) VEL(3,J)=HMIN
CIPK DEC99 CHANGE TP ELEV1
        HEL(J)=ELEV1-ADO(J)
        CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,1)
	  VEL(3,J)=HTP
        IF(VEL(3,J) .LT. HMIN) VEL(3,J)=HMIN
        HOL(J)=HEL(J)
	  IF(HMNN .LT. 0.) VEL(3,J)=-HMNN
	  VOLD(3,J)=VEL(3,J)
cipk mar05
        WSLL(J)=ELEV1
C     VEL(4,J)=0.
C     IF(ICK .EQ. 1) VEL(4,J)=TEMP
      VEL(4,J)=SALI
      VEL(5,J)=TEMPI
      VEL(6,J)=SEDI
      VOLD(4,J)=SALI
      VOLD(5,J)=TEMPI
      VOLD(6,J)=SEDI

      VOLD(7,J)=SEDI
      VEL(7,J)=SEDI
	GPB(J,1)=SEDI
  171 CONTINUE
!********************************************************************djw 20/07/04
!
!     WBM Initital Salinity Sediment and Temperature Hook
!
      If (wbm_InitCons) Then
           
!
!     Read in Mid File of network and then extract Values from the Salinity Column Assigning Values to Nodes as 
!     We Go.
!
        Write (*,*) "Assigning Initial Conditions from Mid File"
	
        Allocate (wbm_NewConditions(Size(VEL,1),Size(VEL,2)))
        wbm_NewConditions = VEL
	  Call wbm_AssignInits
	  VEL = wbm_NewConditions
	  Do J = 1,NP
          VOLD(4,J) = VEL(4,J)
	    VOLD(5,J) = VEL(5,J)
	    VOLD(6,J) = VEL(6,J)
	    VOLD(7,J) = VEL(7,J)
	  End Do
	  Deallocate (wbm_NewConditions)
	End If
!********************************************************************end djw 20/07/04


C-
C...... Set up 1-D velocities
C-
      DO 173 N=1,NE
	IF(IMAT(N) .EQ. 0) GO TO 173
	IF(IMAT(N) .GT. 900) GO TO 173
	IF(NOP(N,6) .GT. 0) GO TO 173
	IF(NCRN(N) .EQ. 5) THEN
	  NLM=2
	ELSE
	  NLM=3
	ENDIF
	DO 172 M=1,NLM
	  NA=NOP(N,M)
	  VEL(1,NA)=UNOM
  172   CONTINUE
  173 CONTINUE
  175 CONTINUE
c      DO 1780 N=1,NP
c        VEL(2,N)=0.1
c 1780 CONTINUE
C-
C-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
C-
      CALL BFORM(0)
c      IF(NB .EQ. 0) THEN
c        DO 185 N=1,NP
c          IF(VEL(1,N) .NE. 0.) GO TO 185
c          IF(NFIX(N)/1000 .EQ. 0) THEN
c              VEL(1,N)=UNOM*COS(UDIR)
c              VEL(2,N)=UNOM*SIN(UDIR)
c          ELSE
c              VEL(1,N)=UNOM*COS(ALFA(N))
c              VEL(2,N)=UNOM*SIN(ALFA(N))
c          ENDIF
c  185   CONTINUE
c      ENDIF
C-
C
cipk dec00 set water surface elevation

        DO  J = 1, NPM
          IF (IDNOPT.NE.0) THEN
            HS = VEL(3,J)
            ISWT = 0
cipk jan01  change AME to AME1
            CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
            WSLL(J) = H + ADO(J)
          ELSE
            WSLL(J) = VEL(3,J) + AO(J)
          ENDIF
          K=NREF(J)+1
          IF(K .NE. 1) THEN
            L=NREF(J)+NDEP(J)-1
            IF(L .GE. K) THEN
              DO M=K,L
                WSLL(M)=WSLL(J)
              ENDDO
            ENDIF
          ENDIF
        ENDDO

CIPK APR01  NOW RESET FOR MID-SIDES
        DO K=1,NEM
          IF(NCRN(K) .GT. 0) THEN

            DO L=2,NCRN(K),2
              IF(IMAT(K) .LT. 901  .OR.  IMAT(K) .GT. 903) THEN
                J=NOPS(K,L)
                N1=NOPS(K,L-1)
                IF(MOD(L,NCRN(K)) .EQ. 0) THEN
                  N2=1
                ELSE
                  N2=NOPS(K,L+1)
                ENDIF
                WSLL(J) = (WSLL(N1)+WSLL(N2))/2.
                KK=NREF(J)+1
                IF(KK .NE. 1) THEN
                  LL=NREF(J)+NDEP(J)-1
                  IF(LL .GE. KK) THEN
                    DO M=KK,LL
                      WSLL(M)=WSLL(J)
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
       RETURN
 5032  FORMAT( I5,6E10.0 )
       END
