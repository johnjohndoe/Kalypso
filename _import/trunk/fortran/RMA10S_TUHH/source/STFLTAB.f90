!     Last change:  NIS   2 Jun 2008    9:57 pm
!IPK  LAST UPDATE SEP 06 2004 ADD ERROR FILE
!IPK  LAST UPDATE AUG 8 2004   ADD ERROR TEST FOR STAGE FLOW TABLE
!IPK  NEW ROUTINE AUG 26 2002
!
      SUBROUTINE STFLTAB(M,SRFEL,DFDH,FF,ISWTB)
!
!IPK FEB05
      USE BLK10mod
!
      CHARACTER*8 IDT
      CHARACTER*72 DLINT
!
      REAL ITBL(5),IDTBL(5),ELEVTBL(5,40),FLOWTBL(5,40)
!
!     FOR ISWTB = 0 SET NODAL SWITCH FOR APPRORIATE CONTINUITY LINE
!
      IF(ISWTB == 0) THEN
        DO N=1,NP
                ISTAB(N)=0
        ENDDO
        IF(INSFL == 0) RETURN
!
!     FIRST ENTRY FOR TABLE READ DATA
!
        NTBL=1
        CALL GINPT(INSFL,IDT,DLINT)
        IF(IDT(1:3) == 'TIT') THEN
          READ(DLINT,'(A72)') TITLETB
        ELSE
!IPK SEP04
          CLOSE(75)
          OPEN(75,file='ERROR.OUT')
          WRITE(75,*) 'UNABLE TO FIND TITLE LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'UNABLE TO FIND TITLE LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'EXECUTION TERMINATED'
          STOP
        ENDIF
        CALL GINPT(INSFL,IDT,DLINT)
!
!only if the file is emptyly present        
        if (IDT(1:6) == 'ENDDAT') RETURN
!
  250   IF(IDT(1:3) == 'CTL') THEN
          READ(DLINT,'(I8)') IDTBL(NTBL)
        ELSE
!IPK SEP04
          CLOSE(75)
          OPEN(75,file='ERROR.OUT')
          WRITE(75,*) 'UNABLE TO FIND CTL LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'UNABLE TO FIND CTL LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'EXECUTION TERMINATED'
          STOP
        ENDIF
        I=1
  300   CALL GINPT(INSFL,IDT,DLINT)
        IF(IDT(1:3) == 'STD') THEN
          READ(DLINT,'(2F8.0)') ELEVTBL(NTBL,I),FLOWTBL(NTBL,I)
          I=I+1
          GO TO 300
        ELSEIF(IDT(1:6) == 'ENDDAT') THEN
          ITBL(NTBL)=I-1
          GO TO 500
        ELSEIF(IDT(1:3) == 'CTL') THEN
          ITBL(NTBL)=I-1
          NTBL=NTBL+1
          GO TO 250
        ELSE
!IPK SEP04
          CLOSE(75)
          OPEN(75,file='ERROR.OUT')
          WRITE(75,*) 'UNABLE TO FIND STD LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'UNABLE TO FIND STD LINE FOR STAGE FLOW TABLE'
          WRITE(*,*) 'EXECUTION TERMINATED'
          STOP
        ENDIF
!
  500   CONTINUE
!
        DO J=1,NTBL
          IF(ITBL(J) > 0) THEN
            KK=LMT(IDTBL(J))
            DO K=1,KK
              ISTAB(LINE(IDTBL(J),K))=J
            ENDDO
          ENDIF
        ENDDO
        RETURN
      ENDIF
!
!     PROCESS CASE
!
      ILINTB=ISTAB(M)
!
      IF(ITBL(ILINTB) == 0) THEN
        RETURN
      ENDIF
!JREDERR=0      
      DO I=1,ITBL(ILINTB)
        IF(SRFEL < ELEVTBL(ILINTB,I)) THEN
          IF(I == 1) THEN
!IF(JRED == 0) THEN            
!IPK SEP04
!CLOSE(75)             
!OPEN(75,file='ERROR.OUT')             
!ENDIF             
!           WRITE(75,*)'WATER SURFACE ELEVATION BELOW LOWEST TABLE ENTRY'
!           WRITE(75,*)'ILINTB,SRFEL,ELEVTBL(ILINTB,1)',ILINTB,SRFEL
!     +                 ,ELEVTBL(ILINTB,1)
!           WRITE(*,*)'WATER SURFACE ELEVATION BELOW LOWEST TABLE ENTRY'
!IPK AUG04
!IF(JRED > 0) THEN           
!  JREDERR=1           
!  RETURN           
!ENDIF           
!           WRITE(*,*)'EXECUTION TERMINATED'
!           STOP
            call ErrorMessageAndStop (4002, 0, 0.0d0, 0.0d0)
          ELSE
           FACT=(SRFEL-ELEVTBL(ILINTB,I-1))/                            &
     &          (ELEVTBL(ILINTB,I)-ELEVTBL(ILINTB,I-1))
           FF=FLOWTBL(ILINTB,I-1)                                       &
     &         +FACT*(FLOWTBL(ILINTB,I)-FLOWTBL(ILINTB,I-1))  
           DFDH=(FLOWTBL(ILINTB,I)-FLOWTBL(ILINTB,I-1))/                &
     &          (ELEVTBL(ILINTB,I)-ELEVTBL(ILINTB,I-1))
!
           RETURN
          ENDIF
        ENDIF
      ENDDO
!IF(JRED == 0) THEN      
!IPK SEP04
!  CLOSE(75)      
!  OPEN(75,file='ERROR.OUT')      
!ENDIF      
!      WRITE(75,*)'WATER SURFACE ELEVATION ABOVE HIGHEST TABLE ENTRY'
!      WRITE(75,*)'ILINTB,SRFEL,ELEVTBL(ILINTB,1)',ILINTB,SRFEL
!     +                 ,ELEVTBL(ILINTB,1)
!      WRITE(*,*)'WATER SURFACE ELEVATION ABOVE HIGHEST TABLE ENTRY'
!IPK AUG04
!IF(JRED > 0) THEN      
!  JREDERR=1      
!  RETURN      
!ENDIF      
!      WRITE(*,*)'EXECUTION TERMINATED'
!      STOP
      call ErrorMessageAndStop (4003, 0, 0.0d0, 0.0d0)
!
      END
