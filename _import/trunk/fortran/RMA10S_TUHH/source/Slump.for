C     Last change:  AF   17 Jul 2006   12:21 pm
      SUBROUTINE SLUMPIT

C     Routine to get slump from channel sides

      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSANMOD
      
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'

!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: HTP
!-

      DATA ITIME/0/

C     First time, get tributary area

      IF(ITIME .EQ. 0) THEN
        write(145,6000) 
 6000   format('    NODAL EROSION DETAILS'//
     +'    Step    Node   New Bed Elev  Change in Bed')
        write(144,6001) 
 6001   format('    BED SLOPE DETAILS'//
     +'    Step    Elmt      Bed Slope Crit Bed Slope')
        CONP=1.-POSA
        RHOBS=1000.*CONP*SGSA
	  CALL GETCON
	  ITIME=1
	ENDIF
C
C     Sort bed elevations from max to min
C

      CALL SORTAO

C     first initialize

      DO NN=1,NE
	  IEDONE(NN)=0
	ENDDO
	DO J=1,NP
        DELTAB(J)=0.0
        NSRC(J)=0
	ENDDO

C     Now loop through nodes to get elements to then compute changes

      DO J=NP,1,-1
	  N=NKEY1(J)
	  DO K=1,12
	    NN=ELTON(N,K)
	    IF(NN .GT. 0) THEN

	      IF(IEDONE(NN) .EQ. 0) THEN
              NIMAT=MOD(IMAT(NN),100)
	        IF(CRSLOP(NIMAT) .GT. 0.) THEN
                CALL GETMAXSLP(NN,CRSLOP(NIMAT))
	          IEDONE(NN)=1
	        ENDIF
	      ENDIF
	    ELSE
	      GO TO 200
	    ENDIF
	  ENDDO
  200   CONTINUE
	ENDDO


C     Take out excess material and allocate to nodes below as source

      DO N=1,NP
	  EXTLD(N)=0.
	ENDDO
      DO N=1,NP
	  IF(NSRC(N) .GT. 0.) THEN
          EXTLD(NSRC(N))=EXTLD(NSRC(N))+DELTAB(N)*TRIBAREA(N)*RHOBS
     +    /(DELT*3600.*TRIBAREA(NSRC(N)))*1000.
	    ELEVB(N)=ELEVB(N)-DELTAB(N)

          write(145,'(2i8,2g15.5)') icyc,n,elevb(n),deltab(n)
	    
	    TTHICK(N)=TTHICK(N)-DELTAB(N)
	    DELBED(N)=DELBED(N)-DELTAB(N)
	  ENDIF
	ENDDO
C	DO N=1,NP
C	WRITE(75,*) N,EXTLD(N),ELEVB(N),TTHICK(N),DELBED(N)
C	ENDDO

c     set distribution linear

      ncn=0
      do n=1,ne
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              delbed(nop(n,j))=(delbed(nop(n,j1))+delbed(nop(n,j2)))/2.
              elevb(nop(n,j))=(elevb(nop(n,j1))+elevb(nop(n,j2)))/2.
              tthick(nop(n,j))=(tthick(nop(n,j1))+tthick(nop(n,j2)))/2.
              dgn(nop(n,j))=(dgn(nop(n,j1))+dgn(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 

CIPK MAY02 UPDATE BED ELEVATION
	DO N=1,NP

        IF(N .EQ. 141) THEN
	  AAA=0
	  ENDIF
	  DIFF=ELEVB(N)-AO(N)
	  IF(DIFF .NE. 0.) THEN
  	    AO(N)=ELEVB(N)
	    ADO(N)=ADO(N)+DIFF
          HEL(N)=WSLL(N)-ADO(N)
          CALL AMF(HEL(N),HTP,AKP(N),ADT(N),ADB(N),D1,D2,1)
	    VEL(3,N)=HTP
	  ENDIF

	ENDDO

	RETURN
	END
