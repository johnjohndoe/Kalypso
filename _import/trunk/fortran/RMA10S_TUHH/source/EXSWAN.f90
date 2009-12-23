!WP   Last change:  K     FEB 2006
      SUBROUTINE EXSWAN
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKSANMOD
      USE BLKSWANF
!
      INTEGER INODESIN,ITEMP,ITEMP1
!
      CHARACTER*32 WVDATA
      CHARACTER*96 SWANFLX
      CHARACTER*6 TSDAT
!LOGICAL STARPROC      
!
      SWANFLX='SWANTEMP.DAT'
!     Form SWAN data file
!
      WRITE(WVDATA,5000) HSSW,TPSW,DIRSW,SPRSW
 5000 FORMAT(4F8.3)
!
      DO N=1,NLINSW1
        IF(SWANDAT(N,1)(1:9) == 'BOUNDPAR2') THEN
!
!     Parse this line backwards to find PARS
          DO M=120,1,-1
            IF(SWANDAT(N,1)(M:M) == 'R') THEN
              NCHAR=M
              GO TO 300
            ENDIF
          ENDDO
  300     CONTINUE
          SWANDAT(N,1)=SWANDAT(N,1)(1:NCHAR)//WVDATA
        ENDIF
      ENDDO
!
!     Look for tide level line
!
      DO N=1,NLINSW1
!
!     DJW Changes 10/06/04 : Altered so that it correctly picks up a "SET" line, not a 
!     SET LEVEL line.
!
         IF(SWANDAT(N,1)(1:3) == 'SET') THEN
! djw 10/06/04
!        IF(SWANDAT(N,1)(1:9) == 'SET LEVEL') THEN 
          WRITE(TSDAT,5001) TIDSW
 5001     FORMAT(F6.2)
!          SWANDAT(N,1)=SWANDAT(N,1)(1:9)//TSDAT
          SWANDAT(N,1)=SWANDAT(N,1)(1:3)//TSDAT
          GO TO 400
        ENDIF
      ENDDO
!
  400 CONTINUE
      OPEN(106,FILE=SWANFLX,STATUS='UNKNOWN',FORM='FORMATTED')
!
      DO N=1,NLINSW1
        WRITE(106,'(A120)') SWANDAT(N,1)
      ENDDO
      CLOSE(106)
!
      WRITE(75,*) 'ABOUT TO GO TO SWAN EXECUTION PARAMETERS ARE'
      WRITE(75,'(I6,F8.2,4F10.3)') DAYOFY,TET,HSSW,TPSW,DIRSW,SPRSW
      WRITE(*,*) 'ABOUT TO GO TO SWAN EXECUTION PARAMETERS ARE'
      WRITE(*,'(I6,F8.2,4F10.3)') DAYOFY,TET,HSSW,TPSW,DIRSW,SPRSW
!
!WP JAN 2006, starproc is visual fortran subroutine, not available with Lahey FORTRAN
!      if(starproc(SWANEX,SWANFLX)) then
!         write(*,*) 'came out OK'
!      else
!         write(*,*) 'came out bad'
!         STOP
!      endif
!
!
!     Handles Execution of Wave Model for Second Nested Mesh
!     DJW 10/06/04 Included Surrounding Conditional If based on number of Meshes to be used.
!     At this point in time, only two meshes are possible, in future this subroutine could be
!     rationalised to include up to 10 meshes.
!-
      IF (ISWMESH > 1) THEN
         DO N=1,NLINSW2
           IF(SWANDAT(N,2)(1:9) == 'BOUNDPAR2') THEN
!
!     Parse this line backwards to find PARS
             DO M=120,1,-1
               IF(SWANDAT(N,2)(M:M) == 'R') THEN
                 NCHAR=M
                 GO TO 500
               ENDIF
             ENDDO
  500        CONTINUE
             SWANDAT(N,2)=SWANDAT(N,2)(1:NCHAR)//WVDATA
           ENDIF
         ENDDO
!
!     Look for tide level line
!
         DO N=1,NLINSW2
!
! DJW 10/06/04
!        IF(SWANDAT(N,2)(1:9) == 'SET LEVEL') THEN 
           IF(SWANDAT(N,2)(1:3) == 'SET') THEN
             WRITE(TSDAT,5001) TIDSW
! DJW 10/06/04
!          SWANDAT(N,2)=SWANDAT(N,2)(1:9)//TSDAT 
             SWANDAT(N,2)=SWANDAT(N,2)(1:3)//TSDAT
             GO TO 600
           ENDIF
         ENDDO
!
  600    CONTINUE
         OPEN(106,FILE=SWANFLX,STATUS='OLD',FORM='FORMATTED')
!
         DO N=1,NLINSW2
           WRITE(106,'(A120)') SWANDAT(N,2)
         ENDDO
         CLOSE(106)
!
!WP JAN 2006, starproc is visual fortran subroutine, not available with Lahey FORTRAN
!      if(starproc(SWANEX,SWANFLX)) then
!         write(*,*) 'came out OK'
!      else
!         write(*,*) 'came out bad'
!         STOP
!      endif
      END IF
!
!     Finish of Execution of Second SWAN Mesh.
!     END DJW CHANGES 10/06/04
!
!-
!
!
!
!     Handles Execution of Wave Model for Third Nested Mesh
!     DJW 24/01/04 Included Surrounding Conditional If based on number of Meshes to be used.
!     At this point in time, only three meshes are possible, in future this subroutine could be
!     rationalised to include up to 10 meshes.
!-
      IF (ISWMESH > 2) THEN
         DO N=1,NLINSW3
           IF(SWANDAT(N,3)(1:9) == 'BOUNDPAR2') THEN
!
!     Parse this line backwards to find PARS
             DO M=120,1,-1
               IF(SWANDAT(N,3)(M:M) == 'R') THEN
                 NCHAR=M
                 GO TO 700
               ENDIF
             ENDDO
  700        CONTINUE
             SWANDAT(N,3)=SWANDAT(N,3)(1:NCHAR)//WVDATA
           ENDIF
         ENDDO
!
!     Look for tide level line
!
         DO N=1,NLINSW3
!
! DJW 10/06/04
!        IF(SWANDAT(N,2)(1:9) == 'SET LEVEL') THEN 
           IF(SWANDAT(N,3)(1:3) == 'SET') THEN
             WRITE(TSDAT,5001) TIDSW
! DJW 10/06/04
!          SWANDAT(N,2)=SWANDAT(N,2)(1:9)//TSDAT 
             SWANDAT(N,3)=SWANDAT(N,3)(1:3)//TSDAT
             GO TO 800
           ENDIF
         ENDDO
!
  800    CONTINUE
         OPEN(106,FILE=SWANFLX,STATUS='OLD',FORM='FORMATTED')
!
         DO N=1,NLINSW3
           WRITE(106,'(A120)') SWANDAT(N,3)
         ENDDO
         CLOSE(106)
!
!WP JAN 2006, starproc is visual fortran subroutine, not available with Lahey FORTRAN
!         if(starproc(SWANEX,SWANFLX)) then
!            write(*,*) 'came out OK'
!         else
!            write(*,*) 'came out bad'
!            STOP
!         endif
      END IF
!
!     Finish of Execution of Third SWAN Mesh.
!     END DJW CHANGES 24/01/04
!
!-
!
!
!
      OPEN(106,FILE=SWANOUT,STATUS='OLD',FORM='BINARY')
      READ(106) HEADER
!ipk feb03      READ(106) INODESIN,ITEMP
      READ(106) INODESIN
      WRITE(75,*) 'INODESIN,ITEMP',INODESIN
      DO N=1,NP
        STRESS(N,1)=0.
        STRESS(N,2)=0.
        WAVEHT(N)=0.
        PEAKPRD(N)=0.
        WAVEDR(N)=0.
      ENDDO
!
      DO N=1,INODESIN
        READ(106) I, STR11(I),STR21(I),                                 &
     &               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)
!
! djw Override to maintain Tp as put into boundary of SWAN.  
                     PEAKPRD1(I) = TPSW  
!
!  DJW 28/04/05 Limit set on wave height to provide stability for larger boundary waves, Also, Forces Throttled to try and achieve stability
!
!        IF (WAVEHT1(I) > 4.0) THEN
!          FORCEFACTOR = 16.0/((WAVEHT1(I))*(WAVEHT1(I)))
!          STR11(I) = STR11(I) *  FORCEFACTOR
!          STR21(I) = STR21(I) *  FORCEFACTOR
!          WAVEHT1(I) = 4.0
!        END IF
!    
!     END CHANGES DJW 28/04/05
!
!        write(75,*) I, STR11(I),STR21(I),
!     +               WAVEHT1(I),PEAKPRD1(I),WAVEDR1(I)
!IPK JAN03        READ(106) I,STRESS(I,1),STRESS(I,2),
!IPK JAN03     +            WAVEHT(I),PEAKPRD(I),WAVEDR(I)
      ENDDO
      CLOSE(106)
!      WRITE(75,*) I,INODESIN,STRESS(I,1),STRESS(I,2),
!     +            WAVEHT(I),PEAKPRD(I),WAVEDR(I)
!
      RETURN
      END     
!
