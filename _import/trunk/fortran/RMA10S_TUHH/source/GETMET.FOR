cipk  last update nov 5 2002 fix divide test
CIPK  LAST UPDATE SEP27 2002 ADD SNOW DEPTH
cipk  last update Dec 17 1997 
CIPK  LAST UPDATE APR 30 1996
CIPK  last update jan 16 1996 
cipk  last update nov 26 1995  Major rewrite to permit input of met file
      SUBROUTINE GETMET(TIME,IYRK)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKMETMOD
      USE PARAMMOD

      SAVE
      CHARACTER*72 METITL
c     Time = time in (hrs)
C-
C...... This routine interpolates met conditions
C-
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKMET.COM'
      ALLOCATABLE SOLI(:),DIVIDE(:)

      DATA ITIMTR/1/
      IF(ITIMTR .EQ. 1) THEN
        ALLOCATE (SOLI(MMAT1),DIVIDE(MAXP))
      ENDIF
C-
C...... Initialize
C-
      WRITE(75,*) 'IN GETMET IT,,NMETF',IT,NMETF
        TETM=0.
cipk nov95        NSCR2=8
        NSCRM=40
C-
        DO KK=1,NMETF
          IF(ITIMTR .EQ. 1) THEN
cipk dec97 cleanup initialisation
            DA(KK)=0.
            TTM(KK)=0.
            SOLIN0(KK)=0.
            SOLIN1(KK)=0.
            DSOL(KK)=0.
          NSCRMP=NSCRM+1
          READ(NSCRMP) DAINIT,HRINIT
          REWIND NSCRMP
          ENDIF
c
c...... Make an initial read to get a starting time
c
CC            IT=2
          NSCRM=NSCRM+1
          SOLIN0(KK)=SOLIN1(KK)
  450     CONTINUE
          WRITE(75,*) 'DA,TTM',DA(KK),TTM(KK),IYRR,IYS(KK)
          IF(IYRR .GT. IYS(KK)  .OR.  (IYRR .EQ. IYS(KK)  .AND. 
     +      (DAYOFY .GT. DA(KK)  .OR. (DAYOFY .EQ. DA(KK)  .AND. 
     +    TIME .GT. TTM(KK)))))THEN
C-
C...... TETM not large enough.  Move V1 to V0 and read another
C-
            DAT0=DA(KK)
            T0=DAT0*24.+TTM(KK)
cipk dec97 extend limit to 9 for solar values
cipk sep02 extend limit to 10 for snow depth values
CIPK AUG03 EXTEND LIMIT TO 11 FOR WIND DIRECTION
            DO  L=3,11
              WDT0(L,KK)=WDT1(L,KK)
            ENDDO
cipk dec97 extend limit to 9 for solar values
cipk sep02 extend limit to 10 for snow depth values
CIPK AUG03 EXTEND LIMIT TO 11 FOR WIND DIRECTION
            READ(NSCRM)
     +                (WDT1(J,KK),J=1,11)
            DA(KK)=WDT1(1,KK)
            IF(DA(KK) .LT. DAT0) THEN
              IYS(KK)=IYS(KK)+1
            ENDIF
            TTM(KK)=WDT1(2,KK)

            T1=DA(KK)*24. + TTM(KK)
            IF(T1 .LT. T0) THEN
              IF(MOD(IYS(KK),4) .EQ. 0) THEN
                ILP=1
              ELSE
                ILP=0
              ENDIF
              TNY=(365+ILP)*24.
            ELSE
              TNY=0.
            ENDIF
            T1=T1+TNY
            TSTEP=(T1-T0)
            SOLIN1(KK)=SOLIN0(KK)+TSTEP*WDT1(9,KK)
            GO TO 450
          ELSE
C-
C...... We have now spanned across the time for interpolation
C-
            WRITE(LOUT,6226) DAYOFY,TOFDAY
 6226       FORMAT (/' DAY OF YEAR =',I8,' TIME OF DAY =',f8.2/)
            T1=DA(KK)*24. + TTM(KK)
            IF(T1 .LT. T0) THEN
              IF(MOD(IYS(KK),4) .EQ. 0) THEN
                ILP=1
              ELSE
                ILP=0
              ENDIF
              TNY=(365+ILP)*24.
            ELSE
              TNY=0.
            ENDIF
            TS=DAYOFY*24. + TIME
            FCTI=(TS-T0)/(T1+TNY-T0)
            DO L=1,9
              K=METID(KK,L)
              IF(K .GT. 0) THEN
                DAT(K)=WDT0(3,KK)+FCTI*(WDT1(3,KK)-WDT0(3,KK))
                CLOUD(K)=WDT0(4,KK)+FCTI*(WDT1(4,KK)-WDT0(4,KK))
                DRYBLB(K)=WDT0(5,KK)+FCTI*(WDT1(5,KK)-WDT0(5,KK))
                WETBLB(K)=WDT0(6,KK)+FCTI*(WDT1(6,KK)-WDT0(6,KK))
                ATMPR(K)=WDT0(7,KK)+FCTI*(WDT1(7,KK)-WDT0(7,KK))
                WIND(K)=WDT0(8,KK)+FCTI*(WDT1(8,KK)-WDT0(8,KK))
                TEMP=SOLIN0(KK)+FCTI*(SOLIN1(KK)-SOLIN0(KK))
                DSOL(K)=TEMP-SOLI(K)
                SOLI(K)=TEMP
                AE(K)=AETMP(KK)
                BE(K)=BETMP(KK)
cipk sep02 extend limit to 10 for snow depth values
                SNOWMT(K)=WDT0(10,KK)+FCTI*(WDT1(10,KK)-WDT0(10,KK))
CIPK AUG03 EXTEND TO WIND DIRECTION
	          WDIRT(K)= WDT0(11,KK)+FCTI*(WDT1(11,KK)-WDT0(11,KK))

                WRITE(LOUT,1019) K,DAT(K),CLOUD(K),DRYBLB(K),WETBLB(K)
     +         ,ATMPR(K),WIND(K),AE(K),BE(K),SNOWMT(K),WDIRT(K)
cipk sep02 add snow depth values
 1019         FORMAT(10x,'INTERPOLATED ATMOSPHERIC DATA'//
     +       'MAT NO  ABSORP   CLOUD DRYBULB  WETBLB    ATMPR   WIND'
     +      ,'       AE       BE    SNOWD WIND-DIR'/
     +        i6,4f8.2,f9.2,f7.2,2f9.6,F9.3,F9.0)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        ITIMTR=2

CIPK SEP02 ASSOCIATE A NODAL VALUE WITH AIR TEMP AND SNOW DEPTH 

      DO J=1,NPM
        SNOWD(J)=0.
        AIRTMP(J)=0.
        DIVIDE(J)=0.
CIPK AUG03 ADD WIND SPEED AND DIRECTION FOR NODAL VALUES
        WNDSP(J)=0.
        WNDDR(J)=0.
      ENDDO

      DO K=1,NEM
        DO L=1,8
          immt=imat(k)
          LL=MOD(IMMT,100)
          IF(LL .GT. 0) THEN
            J=NOPS(K,L)
            IF(J .GT. 0) THEN
              AIRTMP(J)=AIRTMP(J)+DRYBLB(LL)
              SNOWD(J)=SNOWD(J)+SNOWMT(LL)
              DIVIDE(J)=DIVIDE(J)+1.
CIPK AUG03 ADD WIND SPEED AND DIRECTION FOR NODAL VALUES
              WNDSP(J)=WNDSP(J)+WIND(LL)
              WNDDR(J)=WNDDR(J)+WDIRT(LL)
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO J=1,NPM
cipk nov02   Use correct division test

        IF(DIVIDE(J) .GT. 0.) THEN
          SNOWD(J)=SNOWD(J)/DIVIDE(J)
          AIRTMP(J)=AIRTMP(J)/DIVIDE(J)
CIPK AUG03 ADD WIND SPEED AND DIRECTION FOR NODAL VALUES correct to RMA direction
          WNDSP(J)=WNDSP(J)/DIVIDE(J)
          WNDDR(J)=270.-WNDDR(J)/DIVIDE(J)
        ENDIF
      ENDDO



C-
cipk jan96 add definition of METSWT
  600 CONTINUE
C      DO K=1,NMAT
C        METSWT(K)=0
C      ENDDO
      RETURN
cipk jan96 end addition
CIPK SEP02 1010 FORMAT(A80)
CIPK SEP02 1016 FORMAT(8X,I8)
CIPK SEP02 1020 FORMAT(8X,9F8.4)
CIPK SEP02 1021 FORMAT(A8,9F8.4)
      END

