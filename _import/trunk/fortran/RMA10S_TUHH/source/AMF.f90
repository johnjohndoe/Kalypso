!nis  Adding some comments to the if-structures of AMF
!ipk  last update April 24 1997
      SUBROUTINE AMF(H,HSIG,AKAPM,ATM,ABM,AM,DAMH,ISWT)
      USE EPORMOD
!
!     H is the true depth
!     HSIG is the transformed depth in the model (VEL(3,*)
!     AKAPM is effective porosity
!     ATM is the depth at the top of the range
!     ABM is the depth at the bottom of the range
!     AM is the adjustment to the bottom elevation measured from ADO 
!     DAMH is the rate of change of the adjustment to the bottom elevation
!     = 0 to transform HSIG to H
!     = 1 to transform H to HSIG
!
!
!NiS,jul06: Declaration of HSIG and H
      REAL(KIND=8) :: hsig, h
!-
!
!
      IF(ISWT == 0) THEN
!
!...... Convert HSIG to H
!
!
!       Calculate the virtual water depth out of the real water depth
!       am   : difference between virtual and real bed elevation
!       damh : additional friction losses
!
!       ???Marsh-option not active, because of high permeability of underground???
        IF(AKAPM > 0.9999) THEN
          H=HSIG
          AM=0.
          DAMH=0.
!ipk apr97
          efpor=akapm
!
!       minimum range (h beneath abm but above virtual bed elevation)
!                      waterlevel beyond real bed elevation??
!
        ELSEIF(HSIG < AKAPM*ABM) THEN
          H=HSIG/AKAPM
          AM=H-HSIG
          DAMH=1.0/AKAPM-1.
!ipk apr97
          efpor=akapm
!
!       normal range (h over atm)
!       waterlevel above the transition range (waterlevel certainly above real bed elevation)
        ELSEIF( HSIG > ATM-(ABM+ATM)/2.*(1.-AKAPM) ) THEN
          H=HSIG+(1.-AKAPM)/2.*(ABM+ATM)
          AM=H-HSIG
          DAMH=0.0
!ipk apr97
          efpor=1.0
!
!       transition range
!       waterlevel around the real bed elevation
        ELSE
          SQ=SQRT((ATM-ABM)*(AKAPM*(AKAPM*(ATM+ABM)-2.*ABM)             &
     &                      +2.*HSIG*(1.-AKAPM)))
          H=(ABM-AKAPM*ATM+SQ)/(1.-AKAPM)
          AM=H-HSIG
          DAMH=-1.+(ATM-ABM)/SQ
!ipk apr97
          efpor=akapm+(h-abm)/(atm-abm)*(1.-akapm)
        ENDIF
!*******************
!                         TEST MODIFICATION OF DERIVATIVES
!*******************
!       IF (HSIG < 2.+ ATM-(ABM+ATM)/2.*(1.-AKAPM)) THEN
!         DAMH=AM/HSIG
!       ENDIF
!*******************
!                               END CHANGE
!*******************
!
!
      ELSE
!
!...... Convert H to HSIG
!
!
!       Marsh-option not active, because of high permeability of underground???
        IF(AKAPM > 0.9999) THEN
          HSIG=H
!
!       minimum range (h beneath abm but above virtual bed elevation)
!       water level beneath real bed elevation
        ELSEIF(H < ABM) THEN
          HSIG=H*AKAPM
!       normal range (h over atm)
!       water level certainly above real bed elevation
        ELSEIF(H > ATM) THEN
          HSIG=AKAPM*H+(1.-AKAPM)*((ATM-ABM)/2.+(H-ATM))
!       transition range
!       water level around the real bed elevation
        ELSE
          HSIG=AKAPM*H+(1.-AKAPM)/2.*(H-ABM)**2/(ATM-ABM)
        ENDIF
      ENDIF
      RETURN
      END
!     H is the true depth
!     HSIG is the transformed depth in the model (VEL(3,*)
!     AKAPM is effective porosity
!     ATM is the depth at the top of the range
!     ABM is the depth at the bottom of the range
!     AM is the adjustment to the bottom elevation measured from ADO 
!     DAMH is the rate of change of the adjustment to the bottom elevation
!     = 0 to transform HSIG to H
!     = 1 to transform H to HSIG
!
