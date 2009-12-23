      SUBROUTINE GETWAVE
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSANMOD
      SAVE
!-
!...... This routine interpolates wave data
!-
!
      CHARACTER*1000 HEDR
!
      data itime/0/,hrinc/0./
!-
!...... Initialize
!-
!EFa aug08, changed if-clause      
!IF(IT == 1.) THEN      
      IF(itime == 0)then
!-      
!EFa aug08, add weighting factors for external coordinates        
          READ(INWGT,'(A1000)') HEADWT
          DO N=1,NP
            READ(INWGT,5000) M,(NODWT(M,J),J=1,3),(WAT(M,J),J=1,3)
 5000     FORMAT(8X,I8,3I8,3F8.5)
          ENDDO
          CLOSE(INWGT)
!-        
!ipk jan98
        iydd=iyrr
        iyrd=iyrr
        IUT=0
!
        IFRST=0
        IFSV=0
        TTT=0.
!IPK MAY96 DEFINE TETADD
!IPK MAY02        TETADD=TTT-TCORR
        TETADD=TTT
!EFa aug08, added for ASCII wave data input        
          rewind iwvin
          READ(iwvin,'(A100)') HEDR (1:100)
          if (hedr(1:8) == 'WAVE-S') then
            MBND = 1
          else
            MBND = 0
          end if
!-        
      ENDIF
!-
  161 CONTINUE
!EFa aug08, added itime because of changed if-clause      
      itime = 1
!-      
!-
!..... MBND = 1 is the case of only 1 wave data file
!-
!
      WRITE(75,*) 'GETWAVE-54',MBND,HEDR(101:105)
      IF(MBND == 1) THEN
!EFa aug08, changed if-clause        
!IF(IT == 1.) THEN        
        IF(itime == 1)then
        itime = 2
!-        
!
!      WAVE DATA READ
!
!
          DO K=1,NP
            WAVEHT(K)=0.
            PEAKPRD(K)=0.
            WAVEDR(K)=0.
          ENDDO
!
!EFa aug08, add case for ASCII wave datat input          
            READ(iwvin,5011,ERR=165 ,END=166)ttt,nv,iydd
            WRITE(*,*)ttt,nv,iydd
 5011       FORMAT(8x,f8.0,2i8)
!EFa aug08, changed if-clause of input parameters because of added external coordinates            
            if (icordin == 0) then
              do l=1,nv
                READ(iwvin,'(8x,i8,3f16.3)')                            &
     &              k,waveht(k),peakprd(k),wavedr(k)
              end do
            else
              do l=1,nv
                READ(iwvin,'(8x,i8,3f16.3)')k,wvh1(k),wvt1(k),wva1(k)
!WRITE(*,*)k,wvh1(k),wvt1(k),wva1(k)               
              end do
            endif
!-            
            GOTO 166
!-          
  165     CONTINUE
          STOP 'ERROR IN WAVE DATA'
  166     CONTINUE 
        ENDIF
!ipk dec97 add check for zero nodes on input file
        IF(NV == 0) THEN
          WRITE(75,*) 'Error!!  total nodes = 0 on solution file'
          WRITE(75,*) 'Header contains time',ttt,' Year',iydd
          WRITE(*,*) 'Error!!  total nodes = 0 on solution file'
          WRITE(*,*) 'Header contains time',ttt,' Year',iydd
          STOP
        ENDIF
        WRITE(75,*) 'Time ONLY file =',ttt,'current time =',TET
!EFa aug08, added weighting fctors for external coordinates        
        if (icordin /= 0) then
          do n=1,np
            do m=1,3
              k=nodwt(n,m)
              if (k > 0) then
                waveht(n) = waveht(n) + WAT(n,m)*wvh1(k)
                peakprd(n) = peakprd(n) + WAT(n,m)*wvt1(k)
                wavedr(n) = wavedr(n) + WAT(n,m)*wva1(k)
              end if
            end do
          end do
        endif
!-        
        GO TO 600
!-
!..... MBND = 0 is the case of multiple files to be interpolated
!-
      ELSE
  250   CONTINUE
!
        IYDD=IYRD
!
!     logic to pass end of year
!
        IF(IYDD > IYRR) THEN
          IF(MOD(IYRR,4) == 0) THEN 
            HRINC=HRINC+366.*24.
          ELSE
            HRINC=HRINC+365.*24.
          ENDIF
        ENDIF
!
!    Test for data year less than current
!
        IF(IYDD < IYRR) THEN
          IF(MOD(IYDD,4) == 0) THEN 
            HRINC=HRINC-366.*24.
          ELSE
            HRINC=HRINC-365.*24.
          ENDIF
          IYDD=IYDD+1   
        ENDIF
!
        WRITE(75,*) 'Simulation time,file time,year correction'
        WRITE(75,*) TET+(DAYOFY-1.)*24.,TTT,HRINC,IYDD,IYRR
        WRITE(75,*) 'tet,dayofy',tet,dayofy
!
        IF(TET+(DAYOFY-1.)*24. > TTT+HRINC) THEN
!ipk  reset hrinc
          HRINC=0.
!
!-
!...... TTT not yet large enough  move 1 to 0 and read another
!-
          DO K=1,NP
            WVH0(K) = WVH1(K)
            WVT0(K) = WVT1(K)
            WVA0(K) = WVA1(k)
            WVH1(K) = 0.
            WVT1(K) = 0.
            WVA1(K) = 0.
          ENDDO
          T0=TTT
!
!      WAVE DATA  read
!
!EFa aug08, add case for ASCII wave datat input          
            READ(iwvin,5011,ERR=286)ttt,nv,iydd
            do l=1,nv
              READ(iwvin,'(8x,i8,3f16.3)')k,wvh1(k),wvt1(k),wva1(k)
            end do
!-          
          WRITE(75,*) 'TIME FROM WAVE DATA FILE',TTTV,NV,IYDD
          GO TO 287
  286     CONTINUE
          STOP 'ERROR IN WAVE DATA'
!
  287     CONTINUE 
!            write(75,*) 'tttv,nv,iydd,irma2,tetadd'
!            write(75,*)  tttv,nv,iydd,irma2,tetadd
!ipk jan98 add to preserve iydd
          IYRD=IYDD
!     check for zero nodes on input file
!
          if(nv == 0) then
            write(75,*) 'Error!!  total nodes = 0 on solution file'
            Write(75,*) 'Header contains time',tttv,' Year',iydd
            write(*,*) 'Error!!  total nodes = 0 on solution file'
            Write(*,*) 'Header contains time',tttv,' Year',iydd
            stop
          endif
          TTT=TTTV+TETADD
!
  288     CONTINUE
          IF(MOD(IYDD,4) == 0) THEN
            HYR=366.*24.
          ELSE
            HYR=365.*24.
          ENDIF
!
          IF(TTT > HYR) THEN
            TTT=TTT-HYR
            IYDD=IYDD+1
          ELSE
            GO TO 289
          ENDIF
          GO TO 288
  289     CONTINUE
          IYRD=IYDD
!
          WRITE(75,6225) TTTV,TTT,iydd,iyrr
 6225     FORMAT( /5X, 'WAVE DATA FILE READ AT TIME =', F10.2,          &
     &    '  CORRECTED TIME = ',F10.2/'YEARS',2I8 )
!ipk            write(75,*) 'Time from file =',ttt,'current time =',TET
          GO TO 250
        ELSE
!-
!...... We have now spanned across the time for interpolation
!-
          write(75,*) 'time,t0,ttt',TET,t0,ttt
          FCTI=(TET+(DAYOFY-1)*24.-T0)/(TTT+HRINC-T0)
          TTTC=TTT+HRINC
          HRINC=0.
          WRITE(75,*) 'GETWAVE-188  FCTI',FCTI
!
!EFa aug08, added if-clause for the weighting factors of extrernal coordinates          
          if (icordin == 0) then
            do k=1,np
              WAVEHT(K)=wvh0(K)+FCTI*(wvh1(K)-wvh0(K))
              WAVEDR(K)=wva0(K)+FCTI*(wva1(K)-wva0(K))
              PEAKPRD(K)=wvt0(K)+FCTI*(wvt1(K)-wvt0(K))
            end do
          else
            DO n=1,NP
              waveht(n) = 0
              peakprd(n) = 0
              wavedr(n) = 0
              do m = 1,3
                k = nodwt(n,m)
                if (k > 0) then
                  WAVEHT(n)=waveht(n)+(wvh0(k)+FCTI*(wvh1(K)-wvh0(K)))  &
     &                      *wat(n,m)
                  WAVEDR(n)=wavedr(n)+(wva0(k)+FCTI*(wva1(K)-wva0(K)))  &
     &                      *wat(n,m)
                  PEAKPRD(n)=peakprd(n)+(wvt0(k)+FCTI*(wvt1(K)-wvt0(K)))&
     &                       *wat(n,m)
                end if
              end do
!-            
            ENDDO
          end if
        ENDIF
      ENDIF
  600 CONTINUE
!-
!
!
      DO K=1,NP
        WRITE(75,*)'WAVEDATA',K,WAVEHT(K),PEAKPRD(K),WAVEDR(K)
      ENDDO
!
      RETURN
!
      END
!
