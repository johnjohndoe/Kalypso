      SUBROUTINE GETWIND(iyrr,daynow,time,teth, tw, ta)
      USE PARAMMOD
      USE BLK11MOD
      use blk_ifu
      SAVE
!
      ALLOCATABLE NCLIN(:),nhy(:),tatime(:,:),                          &
     &          twin(:,:),tain(:,:),                                    &
     &          DYQ(:,:),IYDAT(:)
!
      CHARACTER*32 FNAMT
      CHARACTER*80 HTITLE
      CHARACTER*72 DLIN
      CHARACTER*8 ID
      integer (kind = 4) :: daynow
      DATA NHYD/0/,ITIMEH/0/
      integer :: ioError
!
      IF(iwindin == 0) THEN
        WRITE(*,*) 'Filename for wind graph not defined'
        WRITE(*,*) 'Enter filename for wind graph'
        READ(*,4800) FNAMT
        iwindin=71
        OPEN(UNIT=71,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(ITIMEH == 0) THEN
        ALLOCATE (NCLIN(NHDS),nhy(nhds),tatime(NCHOBS,NHDS),            &
     &          twin(NCHOBS,NHDS),tain(nchobs,nhds),                    &
     &          DYQ(NCHOBS,NHDS),IYDAT(NHDS))
        ITIMEH=1
        NCLIN=0
        nhy = 0
        tatime=0.
        twin=0.
        tain=0.
        DYQ=0.
        IYDAT=0
      ENDIF
!
      if (nhyd == 0) then
        TSTARTS=(DAYNOW-1)*24.+TIME-TETH      
  100   READ(iwindin,'(A8,A72)',iostat = ioerror) ID,HTITLE
        call ginpt(iwindin,id,dlin)
        if (id(1:4) == 'YEAR') then
          nhyd = nhyd + 1
          nhy(nhyd) = 0       
          read(dlin,'(i8)')iyd                 
          DO 120 I=1,NCHOBS
            call ginpt(iwindin,id,dlin)           
            IF(ID(1:2) == 'WI') THEN
              READ(ID(5:8),'(F4.0)') DYQ(I,NHYD)
              READ(DLIN,'(3F8.0)')                                      &
     &        tatime(i,nhyd),twin(I,NHYD),tain(i,nhyd)     
              nhy(nhyd) = nhy(nhyd)+1
              IF(I == 1) THEN
!
!      reduce input time to time since that set to start simulation
!
  110           CONTINUE
                IF(MOD(IYD,4) == 0) THEN
                  ILP=1
                ELSE
                  ILP=0
                ENDIF
                IF(IYD == IYRR) THEN
!
!      If now for for the same year
!
                  TCUR1=(DYQ(I,NHYD)-1.)*24.+tatime(I,NHYD)
!
!      set time as the difference
!
                  tatime(I,NHYD)=TCUR1-TPRVH-TSTARTS  
                ELSEIF(IYD < IYRR) THEN
                  IF(MOD(IYD,4) == 0) THEN
                    TPRVH=TPRVH+366.*24.
                  ELSE
                    TPRVH=TPRVH+365.*24.
                  ENDIF
                  IYD=IYD+1
                  GO TO 110
                ELSE
                  CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  WRITE(*,*) ' Wind graph for wrong year'
                  WRITE(*,*)  ' Excution stopped'
                  WRITE(75,*) ' Wind graph for wrong year'
                  WRITE(75,*)  ' Excution stopped'
                  STOP
                ENDIF
              ELSE
                IF(DYQ(I,NHYD) < DYQ(I-1,NHYD)) THEN
                  TCUR1=TCUR1-365.*24.
                  IF(ILP == 1) TCUR1=TCUR1-24.
                  IYD=IYD+1
                  IF(MOD(IYD,4) == 0) THEN
                    ILP=1
                  ELSE
                    ILP=0
                  ENDIF
                ENDIF
                TCUR=(DYQ(I,NHYD)-1.)*24.+tatime(I,NHYD)
                tatime(I,NHYD)=tatime(I-1,NHYD)+TCUR-TCUR1
                TCUR1=TCUR
              ENDIF
            else
              nhy(nhyd) = nhy(nhyd) + 1
              DYQ(NHY(NHYD),NHYD)=1.E+6                          
              tatime(nhy(nhyd),nhyd) = 1.E+8           
              twin(nhy(nhyd),nhyd) = twin(nhy(nhyd)-1,nhyd)       
              tain(nhy(nhyd),nhyd) = tain(nhy(nhyd)-1,nhyd)
              goto 200
            endif
  120     CONTINUE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'More than dimensioned lines in wind graph'
          WRITE(*,*) 'Execution terminated'
          WRITE(75,*) 'More than dimensioned lines in wind graph'
          WRITE(75,*) 'Execution terminated'
          stop
        endif
      endif
  200 continue
!
      CLOSE (iwindin)
!
!     INTERPOLATE TO OBTAIN HYDROGRAPH
!
      CTIM=TETH      
      DO 300 J=1,NHYD
          DO 260 I=2,NHY(J)
            TIM=tatime(I-1,J)
            TIN=tatime(I,J)       
            IF(CTIM < TIN) THEN
             tw=twin(I-1,J)+(twin(I,J)-twin(I-1,J))*                    &
     &        (CTIM-TIM)/(TIN-TIM)  
!EFa oct09, testing for winddirections               
!ta=tain(I-1,J)+(tain(I,J)-tain(I-1,J))*              
!+        (CTIM-TIM)/(TIN-TIM)                             
             if (abs(tain(i-1,j))+abs(tain(i,j)) >= 180.0)then
               ta = -(abs(tain(i,j))+abs(tain(i-1,j)))+360.
               if (tain(i-1,j) < 0.0) then
                 ta = tain(i-1,j)-ta*(ctim-tim)/(tin-tim)
               else
                 ta = tain(i-1,j)+ta*(ctim-tim)/(tin-tim)
               endif
               if (ta < -180.) then
                 ta = 360.-ta
               elseif (ta > 180.) then
                 ta = ta - 360
               endif               
             else                          
               ta=tain(I-1,J)+(tain(I,J)-tain(I-1,J))*                  &
     &         (CTIM-TIM)/(TIN-TIM)
             endif    
!-                      
              RETURN
!-              
            ENDIF
  260     CONTINUE
  300 CONTINUE
 4800 FORMAT(A32)
      write(*,*) 'Unable to interpolate data value from wind graph'
      write(75,*) 'Unable to interpolate data value from wind graph'
      STOP
      END
!
