CIPK  NEW ROUTINE FEB 26 2001 TO SET PARAMETERS WHEN GATES ARE CLOSED
      SUBROUTINE SETGT(N)
      USE BLK10MOD

cipk aug05	INCLUDE 'BLK10.COM'

      NBC(NOP(N,1),1)=0
      NBC(NOP(N,2),1)=0
      NBC(NOP(N,3),1)=0
      NBC(NOP(N,1),2)=0
      NBC(NOP(N,2),2)=0
      NBC(NOP(N,3),2)=0
      NBC(NOP(N,2),4)=0
      VEL(1,NOP(N,1))=0.
      VEL(2,NOP(N,1))=0.
      VEL(1,NOP(N,2))=0.
      VEL(2,NOP(N,2))=0.
      VEL(1,NOP(N,3))=0.
      VEL(2,NOP(N,3))=0.
      VDOT(1,NOP(N,1))=0.
      VDOT(2,NOP(N,1))=0.
      VDOT(1,NOP(N,2))=0.
      VDOT(2,NOP(N,2))=0.
      VDOT(1,NOP(N,3))=0.
      VDOT(2,NOP(N,3))=0.
      NSTRT(NOP(N,2),1)=1     
	RETURN
	END
