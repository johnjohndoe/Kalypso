C     Last change:  WP   23 Feb 2006   11:01 am
      SUBROUTINE EXSWAN
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKSANMOD

      INCLUDE 'SWANF.COM'
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSST.COM'
      INTEGER INODESIN,ITEMP,ITEMP1

      CHARACTER*32 WVDATA
	CHARACTER*96 SWANFLX
	CHARACTER*6 TSDAT
	LOGICAL STARPROC

      SWANFLX='SWANTEMP.DAT'
C     Form SWAN data file

      WRITE(WVDATA,5000) HSSW,TPSW,DIRSW,SPRSW
 5000 FORMAT(4F8.3)

      DO N=1,NLINSW1
        IF(SWANDAT(N,1)(1:9) .EQ. 'BOUNDPAR2') THEN

C     Parse this line backwards to find PARS
          DO M=120,1,-1
            IF(SWANDAT(N,1)(M:M) .EQ. 'R') THEN
              NCHAR=M
              GO TO 300
            ENDIF
          ENDDO
  300     CONTINUE
          SWANDAT(N,1)=SWANDAT(N,1)(1:NCHAR)//WVDATA
	  ENDIF
      ENDDO

C     Look for tide level line

      DO N=1,NLINSW2
        IF(SWANDAT(N,1)(1:9) .EQ. 'SET LEVEL') THEN
          WRITE(TSDAT,5001) TIDSW
 5001     FORMAT(F6.2)
          SWANDAT(N,1)=SWANDAT(N,1)(1:9)//TSDAT
          GO TO 400
        ENDIF
      ENDDO

  400 CONTINUE
      OPEN(106,FILE=SWANFLX,STATUS='UNKNOWN',FORM='FORMATTED')

      DO N=1,NLINSW1
        WRITE(106,'(A120)') SWANDAT(N,1)
      ENDDO
      CLOSE(106)

      WRITE(75,*) 'ABOUT TO GO TO SWAN EXECUTION PARAMETERS ARE'
	WRITE(75,'(I6,F8.2,4F10.3)') DAYOFY,TET,HSSW,TPSW,DIRSW,SPRSW
      WRITE(*,*) 'ABOUT TO GO TO SWAN EXECUTION PARAMETERS ARE'
	WRITE(*,'(I6,F8.2,4F10.3)') DAYOFY,TET,HSSW,TPSW,DIRSW,SPRSW

cWP JAN 2006, starproc is visual fortran subroutine, not available with Lahey FORTRAN
c      if(starproc(SWANEX,SWANFLX)) then
c         write(*,*) 'came out OK'
c	else
c	   write(*,*) 'came out bad'
c	   STOP
c	endif


C-
      DO N=1,NLINSW2
        IF(SWANDAT(N,2)(1:9) .EQ. 'BOUNDPAR2') THEN

C     Parse this line backwards to find PARS
          DO M=120,1,-1
            IF(SWANDAT(N,2)(M:M) .EQ. 'R') THEN
              NCHAR=M
              GO TO 500
            ENDIF
          ENDDO
  500     CONTINUE
          SWANDAT(N,2)=SWANDAT(N,2)(1:NCHAR)//WVDATA
	  ENDIF
      ENDDO

C     Look for tide level line

      DO N=1,NLINSW2
        IF(SWANDAT(N,2)(1:9) .EQ. 'SET LEVEL') THEN
          WRITE(TSDAT,5001) TIDSW
          SWANDAT(N,2)=SWANDAT(N,2)(1:9)//TSDAT
          GO TO 600
        ENDIF
      ENDDO

  600 CONTINUE
      OPEN(106,FILE=SWANFLX,STATUS='OLD',FORM='FORMATTED')

      DO N=1,NLINSW1
        WRITE(106,'(A120)') SWANDAT(N,2)
      ENDDO
      CLOSE(106)

cWP JAN 2006, starproc is visual fortran subroutine, not available with Lahey FORTRAN
c      if(starproc(SWANEX,SWANFLX)) then
c         write(*,*) 'came out OK'
c	else
c	   write(*,*) 'came out bad'
c	   STOP
c	endif
C-
      OPEN(106,FILE=SWANOUT,STATUS='OLD',FORM='BINARY')
	READ(106) HEADER
cipk feb03	READ(106) INODESIN,ITEMP
	READ(106) INODESIN
	WRITE(75,*) 'INODESIN,ITEMP',INODESIN
	DO N=1,NP
	  STRESS(N,1)=0.
	  STRESS(N,2)=0.
	  WAVEHT(N)=0.
        PEAKPRD(N)=0.
	  WAVEDR(N)=0.
	ENDDO

	DO N=1,INODESIN
        READ(106) I, STR11(I),STR21(I),
     +               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)
C        write(75,*) I, STR11(I),STR21(I),
C     +               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)
CIPK JAN03	  READ(106) I,STRESS(I,1),STRESS(I,2),
CIPK JAN03     +            WAVEHT(I),PEAKPRD(I),WAVEDR(I)
	ENDDO
	CLOSE(106)
C	WRITE(75,*) I,INODESIN,STRESS(I,1),STRESS(I,2),
C     +            WAVEHT(I),PEAKPRD(I),WAVEDR(I)

      RETURN
      END     

