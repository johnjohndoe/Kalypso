!nis  Adding some comments to the if-structures of AMF
cipk  last update April 24 1997
      SUBROUTINE AMF(H,HSIG,AKAPM,ATM,ABM,AM,DAMH,ISWT)
c
c     H is the true depth
c     HSIG is the transformed depth in the model (VEL(3,*)
c     AKAPM is effective porosity
c     ATM is the depth at the top of the range
c     ABM is the depth at the bottom of the range
c     AM is the adjustment to the bottom elevation measured from ADO 
c     DAMH is the rate of change of the adjustment to the bottom elevation
c     = 0 to transform HSIG to H
c     = 1 to transform H to HSIG
c
cipk apr97
      common /epor/ efpor

!NiS,jul06: Declaration of HSIG and H
      REAL(KIND=8) :: hsig, h
!-

C
      IF(ISWT .EQ. 0) THEN
C
C...... Convert HSIG to H
C

!       Calculate the virtual water depth out of the real water depth
!       am   : difference between virtual and real bed elevation
!       damh : additional friction losses

!       ???Marsh-option not active, because of high permeability of underground???
        IF(AKAPM > 0.9999) THEN
          H=HSIG
          AM=0.
          DAMH=0.
cipk apr97
          efpor=akapm

!       minimum range (h beneath abm but above virtual bed elevation)
!                      waterlevel beyond real bed elevation??

        ELSEIF(HSIG < AKAPM*ABM) THEN
          H=HSIG/AKAPM
          AM=H-HSIG
          DAMH=1.0/AKAPM-1.
cipk apr97
          efpor=akapm

!       normal range (h over atm)
!       waterlevel above the transition range (waterlevel certainly above real bed elevation)
        ELSEIF( HSIG .GT. ATM-(ABM+ATM)/2.*(1.-AKAPM) ) THEN
          H=HSIG+(1.-AKAPM)/2.*(ABM+ATM)
          AM=H-HSIG
          DAMH=0.0
cipk apr97
          efpor=1.0

!       transition range
!       waterlevel around the real bed elevation
        ELSE
          SQ=SQRT((ATM-ABM)*(AKAPM*(AKAPM*(ATM+ABM)-2.*ABM)
     +                      +2.*HSIG*(1.-AKAPM)))
          H=(ABM-AKAPM*ATM+SQ)/(1.-AKAPM)
          AM=H-HSIG
          DAMH=-1.+(ATM-ABM)/SQ
cipk apr97
          efpor=akapm+(h-abm)/(atm-abm)*(1.-akapm)
        ENDIF
C*******************
C                         TEST MODIFICATION OF DERIVATIVES
C*******************
C       IF (HSIG .LT.  2.+ ATM-(ABM+ATM)/2.*(1.-AKAPM)) THEN
C         DAMH=AM/HSIG
C       ENDIF
C*******************
C                               END CHANGE
C*******************


      ELSE
C
C...... Convert H to HSIG
C

!       Marsh-option not active, because of high permeability of underground???
        IF(AKAPM .GT. 0.9999) THEN
          HSIG=H

!       minimum range (h beneath abm but above virtual bed elevation)
!       water level beneath real bed elevation
        ELSEIF(H .LT. ABM) THEN
          HSIG=H*AKAPM
!       normal range (h over atm)
!       water level certainly above real bed elevation
        ELSEIF(H .GT. ATM) THEN
          HSIG=AKAPM*H+(1.-AKAPM)*((ATM-ABM)/2.+(H-ATM))
!       transition range
!       water level around the real bed elevation
        ELSE
          HSIG=AKAPM*H+(1.-AKAPM)/2.*(H-ABM)**2/(ATM-ABM)
        ENDIF
      ENDIF
      RETURN
      END
c     H is the true depth
c     HSIG is the transformed depth in the model (VEL(3,*)
c     AKAPM is effective porosity
c     ATM is the depth at the top of the range
c     ABM is the depth at the bottom of the range
c     AM is the adjustment to the bottom elevation measured from ADO 
c     DAMH is the rate of change of the adjustment to the bottom elevation
c     = 0 to transform HSIG to H
c     = 1 to transform H to HSIG

