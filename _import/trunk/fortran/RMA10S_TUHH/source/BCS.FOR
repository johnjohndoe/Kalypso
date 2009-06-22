C     Last change:  WP   14 Nov 2007    6:41 pm
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
cipk  LAST UPDATE JAN 19 2004 save data to a scratch file
cipk  last update dec 9 2002 do not change direction
CIPK  LAST UPDATE NOV 26 1997
CIPK  LAST UPDATE SEPT 26 1996
CIPKFEB94
CHANGE MADE TO READ FROM MIBIN
      SUBROUTINE BCS(IBIN,CMIN,CPR)
      USE BLK10MOD
      SAVE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
C-
C......INSERT EXTERNAL BOUNDARY SPECIFICATIONS
C-
      NNEW=0
      DO 150 N=1,NPM
cipk nov01      IF(NDEP(N) .EQ. 1) GO TO 150
        IF(NDEP(N) .EQ. 1) GO TO 149
        N1=NREF(N)+1
        NV=NDEP(N)+N1-2
  146   SPEC(N,1)=SPEC(N,1)*(POWER+1.)/(VMIN*POWER+1.)
cipk dec02 do not change direction
cipk dec02        SPEC(N,2)=SPEC(N,2)*(POWER+1.)/(VMIN*POWER+1.)
        DO 148 M=N1,NV
          CF=(CORD(M,3)-AO(M))/(ELEV-AO(M))
          IF(CF .LT. 0. ) CF=0.
          IF(NFIX(N) .GT. 1000) THEN
              if(cf .gt. 0.) then
                SPEC(M,1)=SPEC(N,1)*(VMIN+(1.-VMIN)*CF**POWER)
cipk dec02 do not change direction
cipk dec02                 SPEC(M,2)=SPEC(N,2)*(VMIN+(1.-VMIN)*CF**POWER)
              else
                spec(m,1)=spec(n,1)*vmin
cipk dec02 do not change direction
cipk dec02                spec(m,2)=spec(n,2)*vmin
              endif
          ENDIF
          NCPON=ICPON(N)
          CMIN=CINT(NCPON)
          CPR=CPOW(NCPON)
          IF(MOD(NFIX(N),100)/10 .GE. 1  .AND.  CPR .NE. 0.) THEN
            IF(SPEC(N,4) .GE. 0.) THEN
              SPEC(M,4)=SPEC(N,4)*(CMIN+(1.-CMIN)*CF**CPR)
            ELSE
              SPEC(M,4)=-SPEC(N,4)
            ENDIF
CIPK SEP96
            IF(MOD(NFIX(N),100)/10 .EQ. 1) THEN
              VEL(4,M)=SPEC(M,4)
              VDOT(4,M)=ALTM*(VEL(4,M)-VOLD(4,M))-(ALPHA-1.)*VDOTO(4,M)
              IESPC(4,M)=1
            ENDIF
          ENDIF
          IF(MOD(NFIX(N),10) .GT.  0  .AND.  CPR .NE. 0.) THEN
            IF(SPEC(N,5) .GE. 0.) THEN
              SPEC(M,5)=SPEC(N,5)*(CMIN+(1.-CMIN)*CF**CPR)
            ELSE
              SPEC(M,5)=-SPEC(N,5)
            ENDIF
CIPK SEP96
            IF(MOD(NFIX(N),10) .EQ. 1) THEN
              VEL(5,M)=SPEC(M,5)
              VDOT(5,M)=ALTM*(VEL(5,M)-VOLD(5,M))-(ALPHA-1.)*VDOTO(5,M)
              IESPC(5,M)=1
            ENDIF
          ENDIF
          IF(NFIX1(N) .GT.  0  .AND.  CPR .NE. 0.) THEN
            IF(SPEC(N,6) .GE. 0.) THEN
              SPEC(M,6)=SPEC(N,6)*(CMIN+(1.-CMIN)*CF**CPR)
            ELSE
              SPEC(M,6)=-SPEC(N,6)
            ENDIF
CIPK SEP96
            IF(NFIX1(N) .EQ. 1) THEN
              VEL(6,M)=SPEC(M,6)
              VDOT(6,M)=ALTM*(VEL(6,M)-VOLD(6,M))-(ALPHA-1.)*VDOTO(6,M)
              IESPC(6,M)=1
            ENDIF
          ENDIF
          IF(MOD(NFIX(N)/100,10) .EQ. 2) THEN
            SPEC(M,3)=SPEC(N,3)
          ENDIF
  148     CONTINUE
cipk nov01
  149     continue
        IF(SPEC(N,4) .LT. 0.) SPEC(N,4)=-SPEC(N,4)
        IF(SPEC(N,5) .LT. 0.) SPEC(N,5)=-SPEC(N,5)
        IF(SPEC(N,6) .LT. 0.) SPEC(N,6)=-SPEC(N,6)
  150   CONTINUE
C-
C......READ SPECIAL CASE BOUNDARY CONDITIONS
C
  200   CONTINUE
        write(*,'(a8)') id
        IF(ID(1:2) .EQ. 'SN') THEN
          NNEW=NNEW+1
          IF(NNEW .EQ. 1) WRITE(LOUT,6030)
          READ(DLIN,5050) N,NFIX(N),NFIX1(N),(SPEC(N,M),M=1,6)
CIPK NOV97          READ(IBIN,'(A8,A72)') ID,DLIN
          call ginpt(ibin,id,dlin)

cipk JAN04 save data to a scratch file
          if(isvs .eq. 1) then
            write(nscrin,7000) id,dlin
          endif

          NSPL(N)=1
        NL=NREF(N)
        NT=NL+NDEP(N)-1
        M=N
        DO MM=NL,NT
          IF(MM .GT. NL) THEN
            M=MM
              IF(ID(1:2) .EQ. 'SD') THEN
                READ(DLIN,5051)  NFIX(M),NFIX1(M),(SPEC(M,L),L=1,6)
cipk nov97                READ(IBIN,'(A8,A72)') ID,DLIN
                call ginpt(ibin,id,dlin)

cipk JAN04 save data to a scratch file
                if(isvs .eq. 1) then
                  write(nscrin,7000) id,dlin
                endif
              ENDIF
            ENDIF
          ENDDO
          GO TO 200
        ENDIF

        !TODO, ErrorMessage
        IF(ID(1:7) .NE. 'ENDSTEP') THEN
          WRITE(*,*) 'NOT AT LINE TYPE -ENDSTEP-'
	    WRITE(*,*) 'ID =',ID(1:8)
CIPK SEP04 CREATE ERROR FILE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'NOT AT LINE TYPE -ENDSTEP-'
	    WRITE(75,*) 'ID =',ID(1:8)
          STOP
        ENDIF
        RETURN
 5050 FORMAT(I8,I7,I1,6E8.0)
 5051 FORMAT(8X,I7,I1,6E8.0)
 6030 FORMAT(//' SPECIAL DISTRIBUTION BOUNDARY CONDITIONS')
 7000 FORMAT(A8,A72)
      END
