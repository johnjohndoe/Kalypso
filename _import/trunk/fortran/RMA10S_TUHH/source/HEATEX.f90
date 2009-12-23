!IPK   LAST UPDATE JAN 23 2003 RESOLVE STB INCONSISTENCY
!IPK   LAST UPDATE MAR 8 2002 ALLOW FOR VERY LOW DEWPOINT
!IPK   LAST UPDATE JULY01 2001 ADD DEWPOINT OPTION
!IPK   LAST UPDATED MAR 23 1998
!IPK   LAST UPDATED MAY 2 1996
!IPK   LAST UPDATED SEPT 8 1995
       SUBROUTINE HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
!IPK AUG05       SUBROUTINE HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
!
!                             HEATEX COMPUTES THE NET AMOUNT OF HEAT
!                             RADIATION FLUX BEING TRANSFERRED ACROSS
!                             THE AIR-WATER INTERFACE BASED ON AN
!                             ENERGY BUDGET WHICH CONSIDERS SOLAR
!                             RADIATION, ATMOSPHERIC RADIATION, BACK
!                             RADIATION, CONDUCTION, AND EVAPORATION.
       USE BLKMETMOD
       USE BLK11MOD
!
       SAVE
!
!CC             NCASI Commentary, HEATEX Section A. (QUAL2 Step 1-0)
!CC                     A. Compute and/or define required constants.
!CC
!
!CC             NCASI Commentary, HEATEX Section F.
!CC                     F. Test for beginning of a new day.
!CC
!JFD cheap fix to prevent incrementing time for each material type ...
!JFD jump passed time calcs, this only works because local values are saved
!
      DATA ITIME/0/
!
!-
!IPK  MAY96 CHANGES BELOW TO CATCH CASE WHEN MET COMPUTATION NOT ACTIVATED
!
!     Get Delta Time in hours
!
      DELTH = DELT/3600.0
!
      IF(ITPAS == 0) THEN
        TOFDAY = TOFDAY+DELTH
      ENDIF
      IF(MMET == 0) THEN
!IPK MAR98        IF(TOFDAY > 24.0) THEN
!IPK MAR98
          IF(TOFDAY > 23.999) THEN
            TOFDAY=TOFDAY-24.
!nis,oct08: TET describes the ongoing time in hours and should not be changed in any subroutine            
!           Line deactivated.            
!TET=TOFDAY            
!-            
            DAYOFY=DAYOFY+1.
            IF(MOD(IYRR,4) == 0) THEN
              IYDAYS=366
            ELSE
              IYDAYS=365
            ENDIF
            IYRN=IYRR
            IF(DAYOFY > IYDAYS) THEN
              IYRR=IYRR+1
              DAYOFY=DAYOFY-IYDAYS
            ENDIF
          ENDIF
!
!IPK AUG03
          IF(IWNDMET > 0) THEN
            CALL GETMET(TOFDAY,IYRR)
              DO J=1,NPM11
              SIGMA(J,1)=CHI*COS(WNDDR(J)/57.3)*WNDSP(J)**2
              SIGMA(J,2)=CHI*SIN(WNDDR(J)/57.3)*WNDSP(J)**2
            ENDDO
            WRITE(75,*) 'WIND',SIGMA(1,1),SIGMA(1,2),WNDSP(1),wnddr(1)
!
          ENDIF
!
          RETURN
       ENDIF
!IPK MAY96 END CHANGES
!
!IPK MAY94 CHANGED ORDER
!
      IF(ITIME == 0) THEN
        PI=3.141628  
        CON1=2.0*PI/365.0
        CON2=PI/180.0*LAT
        CON3=180.0/PI
        CON4=23.45*PI/180.0
        CON5=PI/12.0
        CON6=12.0/PI
        DELTSL=(LONG-STANM)/15.0
!IPK MAR94
        IF(METRIC == 0) THEN
          SOLCON=438.0
          ELEXP=EXP(-ELEVAT/2532.0)
        ELSE
          SOLCON=4870.8
!          ELEXP=EXP(-ELEV*3.2808/2532.0)
          ELEXP=EXP(-ELEVAT/771.76)
        ENDIF
!IPK MAR94 END CHANGES
      ENDIF
      TLEFT=0.
      DO 55 NN=1,NMAT
        TSOLHR(NN)=0.
        HA(NN)=0.
   55 CONTINUE
      ITIME=ITIME+1
      IF(ITIME == 1) THEN
        INWDAY=1
      ELSE
        INWDAY=0
      ENDIF
!       INWDAY =1 signals a new day or start of simulation
!
      IF(DELTH == 0.) THEN
        DELTH=1.0
        TOFDAY=0.00
        TLEFT=24.
      ENDIF
!
!       write(*,*) TOFDAY, dayofy
!     WRITE(IOT,*) 'TOFDAY DAYOFY',TOFDAY,DAYOFY
   56 CONTINUE
      IF(DELT == 0.) THEN
        TLEFT=TLEFT-DELTH*1.0001
        TOFDAY=TOFDAY+DELTH
        GO TO 85
      ENDIF
      IF(TLEFT > 0.0) THEN
!
!   TLEFT > 0 OCCURS ONLY AFTER GOING THROUGH HEATEX ONCE AND GOING BACK TO 56
!
        TOFDAY=TLEFT
        DAYOFY=DAYOFY + 1.0
        INWDAY=1
        TET = TLEFT
        IF(MOD(IYRR,4) == 0) THEN
          IYDAYS=366
        ELSE
          IYDAYS=365
        ENDIF
        IYRN=IYRR
        IF(DAYOFY > IYDAYS) THEN
          IYRR=IYRR+1
          DAYOFY=DAYOFY-IYDAYS
        ENDIF
!
        IF (TOFDAY > 24.001) THEN
          TLEFT=TOFDAY-24.00
          TOFDAY=24.00
          DELTH=24.00
        ELSE
          DELTH=TLEFT
          TLEFT=0.0
        ENDIF
        GO TO 85
      ENDIF
      IF (TOFDAY > 24.001) THEN
        TLEFT=TOFDAY-24.00
        TOFDAY=24.00
        DELTH=DELTH-TLEFT
        INWDAY=0
      ENDIF
   85 CONTINUE
       write(LOUT,*)'at 85 tleft,tofday,delth,inwday',tleft,tofday,delth&
     &    ,inwday
      CALL GETMET(TOFDAY,IYRR)
      IF(IWNDMET > 0) THEN
        DO J=1,NPM11
          SIGMA(J,1)=CHI*COS(WNDDR(J)/57.3)*WNDSP(J)**2
          SIGMA(J,2)=CHI*SIN(WNDDR(J)/57.3)*WNDSP(J)**2
        ENDDO
        WRITE(75,*) 'WIND',SIGMA(1,1),SIGMA(1,2),WNDSP(1)
      ENDIF
!
!ipk sep02 add call to get ice thicness
!
      CALL ICETHICK(DELTH)
!
      IF(INWDAY == 1) THEN
!
!CC
!CC             NCASI Commentary, HEATEX Section B. (QUAL2 Step 2-0)
!CC                     B. Begin computations  for calculating the
!CC                         net solar radiation term.
!CC
!CC                     B.1 Test for beginning of a new day.
!CC
!CC
!CC                     B.1a Calculate seasonal and daily position
!CC                           of the sun relative to the location
!CC                           of the basin on the earth's
!CC                           surface. (QUAL2 Step 2-1)
!CC
        REARTH=1.0+0.017*COS(CON1*(186.0-DAYOFY))
        DECLIN=CON4*COS(CON1*(172.0-DAYOFY))
        RR=REARTH**2
        EQTIME=0.000121-0.12319*SIN(CON1*(DAYOFY-1.0)-0.07014)          &
     &       -0.16549*SIN(2.0*CON1*(DAYOFY-1.0)+0.3088)
        DECLON=ABS(DECLIN)
!C
!C              Replace TAN function with SIN/COS.
!C
        TANA = SIN(CON2)/COS(CON2)
        TANB = SIN(DECLON)/COS(DECLON)
        ACS = TANA*TANB
!C
        IF (ACS /= 0.0) THEN   
          XX=SQRT(1.0-ACS*ACS)
          XX=XX/ACS
!ipk oct94          ACS=ATAN(XX)
          if(xx > 0.) then
            ACS=abs(ATAN(XX))
            IF (DECLIN > 0.0) ACS=PI-ACS
          else
            acs=abs(atan(xx))
            if (declin < 0.0) acs=pi-acs
          endif
        ELSE
          ACS=PI/2.0
        ENDIF
!CC
!CC                     B.1a Calculate the standard time of
!CC                           sunrise (STR) and sunset (STS).
!CC                           (QUAL2 Step 2-2)
!CC
!
        STR=12.0-CON6*ACS+DELTSL
        STS=24.0-STR+2.0*DELTSL
        DAYLEN=STS-STR
        STB=TOFDAY-DELTH
        STE=STB+DELTH
        WRITE(LOUT,*) 'AFTER 2-2 STR,STS,STB,STE',STR,STS,STB,STE
!CC
!CC                     B.2 Increment the variables that define the
!CC                          time of the beginning(STB) and the
!CC                          end (STE) of the time interval.
!CC
      ELSE
!ipk jan03  reset STB based on TOFDAY
!IPK JAN03        STB=STB+DELTH
        STB=TOFDAY-DELTH
        STE=STB+DELTH
      ENDIF
      DO 200 NN=1,NMAT
        IF(ORT(NN,1) == 0.) GO TO 200
!ipk feb98
        IF(ISOL(NN) /= 0) THEN
!
!     convert to kJ/m2
          TSOLHR(NN)=DSOL(NN)*3.6
          GO TO 190
        ENDIF
!
!
!CC
!CC                     B.3 Test if time to read in local
!CC                          climatological data. (QUAL2 Step 2-3)
!CC
!JFD      IF (TRLCD /= 1.0) GO TO 82
!CC
!CC                     B.7 Compute vapor pressures (VPWB and
!CC                          VPAIR), dew point (DEWPT), AND
!CC                          dampening effect of clouds (CNS
!CC                          and CNL). (QUAL2 Step 2-4)
!CC
!IPK MAR94
!
      IF(METRIC == 0) THEN
!
!ipk jul01  add dew point option  Dew point stored in WETBLB
        IF(IDPT(NN) == 1) THEN
          VPAIR=0.180*(exp(0.03739*(WETBLB(NN)-32.)))
          DEWPT=WETBLB(NN)
!
        ELSE
          VPWB=0.1001*EXP(0.03*WETBLB(NN))-0.0837
          VPAIR=VPWB-0.000367*ATMPR(NN)*(DRYBLB(NN)-WETBLB(NN))         &
     &     *(1.0+(WETBLB(NN)-32.0)/1571.0)
!IPK MAR02 ADD TEST FOR DEWPT DETERMINATION
          IF(VPAIR+0.0837 < 0.) THEN
              DEWPT=-58.
          ELSE
            DEWPT=ALOG((VPAIR+0.0837)/0.1001)/0.03
          ENDIF
        ENDIF
      ELSE
!        VPWB=(0.1001*EXP(0.03*(WETBLB(NN)*1.8+32.))-0.0837)/29.53*1000.
!        VPAIR=VPWB-0.000367*ATMPR(NN)*(DRYBLB(NN)-WETBLB(NN))*1.8
!     *     *(1.0+(WETBLB(NN)*1.8)/1571.0)
!        DEWPT=((ALOG((VPAIR/1000.*29.53+0.0837)/0.1001)/0.03)-32.0)/1.8
!        IF(NN == 1) WRITE(75,*) 'VPWB',VPWB,VPAIR,DEWPT
!ipk jul01  add dew point option  Dew point stored in WETBLB
        IF(IDPT(NN) == 1) THEN
          VPAIR=0.180*(exp(0.03739*9./5.*WETBLB(NN)))*1000./29.53
          DEWPT=WETBLB(NN)
!
        ELSE
          vpwb=8.8534*exp(0.054*wetblb(nn))-2.8345
          VPAIR=VPWB-0.0006606*ATMPR(NN)*(DRYBLB(NN)-WETBLB(NN))        &
     &     *(1.0+WETBLB(NN)/872.78)
!IPK MAR02 ADD TEST FOR DEWPT DETERMINATION
          IF(VPAIR+2.8345 < 0.) THEN
            DEWPT=-50.
          ELSE
            DEWPT=ALOG((VPAIR+2.8345)/8.8534)/0.054
            ENDIF
        ENDIF
!       vpwb in millibars
!       vpair in millibars
!       dewpt in deg C
      ENDIF
!IPK MAR94 END CHANGES
      CS=1.0-0.65*CLOUD(NN)**2
      IF (CLOUD(NN) > 0.9) CS=0.50
      CNL=CLOUD(NN)*10.0+1.0
      NL=CNL
82    CONTINUE
       IF (STS <= STB .OR. STR >= STE) GO TO 35
!      IF(STR > STB .AND. STR < STE) GO TO 41
!      IF (STS < STE .AND. STS > STB) GO TO 42
       ST1=STB
       ST2=STE
       IF(STB < STR) ST1=STR
       IF(STE > STS) ST2=STS 
!CC
!CC             NCASI Commentary, HEATEX Section C. (QUAL2 Step 2-5)
!CC                     C. Continue with calculations for solar
!CC                         radiation.
!CC
!CC                     C.1 Calculate hour angles (TB and TE).
!CC
!     TB=STB-12.0-DELTSL+EQTIME
!     TE=STE-12.0-DELTSL+EQTIME
!     WRITE(LOUT,*) '40 ,DELTSL,EQTIME,STB,STE,TB,TE',
!    +DELTSL,EQTIME,STB,STE,TB,TE
!     GO TO 43
!  41 TB=STR-12.0-DELTSL+EQTIME
!     TE=STE-12.0-DELTSL+EQTIME
!     WRITE(LOUT,*) '41 ,DELTSL,EQTIME,STR,STE,TB,TE',
!    +DELTSL,EQTIME,STR,STE,TB,TE
!     GO TO 43
!  42 TB=STB-12.0-DELTSL+EQTIME
!     TE=STS-12.0-DELTSL+EQTIME
!     WRITE(LOUT,*) '42 ,DELTSL,EQTIME,STB,STS,TB,TE',
!    +DELTSL,EQTIME,STB,STR,TB,TE
!  43 CONTINUE
      TB=ST1-12.0-DELTSL+EQTIME
      TE=ST2-12.0-DELTSL+EQTIME
!      WRITE(LOUT,*) '43 ,DELTSL,EQTIME,ST1,ST1,TB,TE',
!     +DELTSL,EQTIME,ST1,ST2,TB,TE
!
      TALT=(TB+TE)/2.0
!      WRITE(LOUT,*) 'TB,TE',TB,TE
!CC
!CC                     C.2 Compute amount of clear sky, solar
!CC                          radiation(SOLAR), and altitude of
!CC                          the sun (ALPHT). (QUAL2 Step 2-6)
!CC
!
      SOLAR=SOLCON/RR*(SIN(CON2)*SIN(DECLIN)*(TE-TB)+CON6*COS(CON2)*    &
     &      COS(DECLIN)*(SIN(CON5*TE)-SIN(CON5*TB)))
      ALPHT=SIN(CON2)*SIN(DECLIN)+COS(CON2)*COS(DECLIN)*COS(CON5*TALT)
      IF (ABS(ALPHT) == 1.0) GO TO 4
      Y=SQRT(1.0-ALPHT*ALPHT)
      Y=ALPHT/Y
      ALPHT=ATAN(Y)
      GO TO 5
    4 IF (ALPHT == -1.0) GO TO 6
      ALPHT=PI/2.0
      GO TO 5
    6 ALPHT=-PI/2.0
    5 CONTINUE
!      write(LOUT,*) 'alpht',alpht
      IF (ALPHT < 0.01) GO TO 35
!CC
!CC                     C.3 Compute absorption and scattering due
!CC                          to atmospheric conditions. (QUAL2
!CC                          Step 2-7)
!CC
!IPK MAR94
      IF(METRIC == 0) THEN
        PWC=0.00614*EXP(0.0489*DEWPT)
      ELSE
!        PWC=0.00614*EXP(0.0489*(DEWPT*1.8+32.0))
!        IF(NN == 1) WRITE(75,*) 'PWC',PWC
        PWC=0.02936*EXP(0.08802*DEWPT)
!        IF(NN == 1) WRITE(75,*) 'PWC',PWC
      ENDIF
!IPK MAR94 END CHANGES
      OAM=ELEXP/(SIN(ALPHT)+0.15*(ALPHT*CON3+3.885)**(-1.253))
      A1=EXP(-(0.465+0.0408*PWC)*(0.129+0.171*EXP(-0.880*OAM))*OAM)
      A2=EXP(-(0.465+0.0408*PWC)*(0.179+0.421*EXP(-0.721*OAM))*OAM)
!CC
!CC                     C.4 Compute reflectivity coefficient (RS).
!CC                          (QUAL2 Step 2-8)
!CC
      GO TO (30,31,31,31,31,31,32,32,32,32,33), NL
   30 AR=1.18
      BR=-0.77
      GO TO 34
   31 AR=2.20
      BR=-0.97
      GO TO 34
   32 AR=0.95
      BR=-0.75
      GO TO 34
   33 AR=0.35
      BR=-0.45
   34 CONTINUE
      RS=AR*(CON3*ALPHT)**BR
!      write(LOUT,*) 'rs',rs
!C
!C              Add test for RS greater than 1.0.
!C
        IF(RS >= 1.0) GO TO 35
!C
!CC
!CC                     C.5 Compute atmospheric transmission term (ATC).
!CC
      ATC=(A2+0.5*(1.0-A1-DAT(NN)))/(1.0-0.5*RS*(1.0-A1+DAT(NN)))
!CC
!CC                     C.6 Compute net solar radiaiont for the time
!CC                          interval delta t. (QUAL2 Step 2-9)
!CC
!IPK JAN03      write(LOUT,*) 'solar,atc,cs',solar,atc,cs
      TSOLHR(NN) = TSOLHR(NN)+SOLAR*ATC*CS*(1.0-RS)
      GO TO 36
   35 TSOLHR(NN) = TSOLHR(NN)+0.0
   36 CONTINUE
!IPK FEB98
  190 CONTINUE
      CLC=1.0+0.17*CLOUD(NN)**2
!CC
!CC             NCASI Commentary, HEATEX Section D. (QUAL2 Step 3-0)
!CC                     D. Compute heat fluxes from other terms.
!CC
!CC                     D.1 Long wave atmospheric radiation (HA).
!CC
!IPK MAR94
      IF(METRIC == 0) THEN
        HA(NN) = HA(NN)+                                                &
     &     0.97*1.73E-09*2.89E-06*(DRYBLB(NN)+460.0)**6*CLC*DELTH
      ELSE
!
!
!          HA(NN) = HA(NN) +
!     +     0.97*1.73E-09*2.89E-06*(DRYBLB(NN)*1.8+32.+460.0)**6
!     +       *CLC*DELTH*4870.8/438.
!         IF(NN == 1) WRITE(75,*) 'HA',HA(NN)
        HA(NN) = HA(NN) +                                               &
     &     0.97*9.37e-06*2.0412E-07*(DRYBLB(NN)+273.0)**6               &
     &       *CLC*DELTH
!         IF(NN == 1) WRITE(75,*) 'HA',HA(NN)
      ENDIF
!
!IPK FEB98 END CHANGES
!IPK MAR94 END CHANGES
!IPK JAN03      WRITE(LOUT,'(''STB,STE,ST1,ST2,TB,TE,TALT'',7F7.2)')
!IPK JAN03     +       STB,STE,ST1,ST2,TB,TE,TALT
      WRITE(LOUT,*) 'tofday,tsolhr,ha',                                 &
     &    TOFDAY, TSOLHR(NN), HA(NN)
      IF(IOMET > 0 .AND. NN == 1) THEN
        WRITE(IOMET,'(I8,F8.2,2F15.2)')                                 &
     &             DAYOFY,TOFDAY,TSOLHR(NN)/DELTH,HA(NN)/DELTH
      ENDIF
  200 CONTINUE
      IF(TLEFT > 0.) GO TO 56
!
!JFD cheap fix to prevent incrementing time for each material type ...
!C
      DELTH = DELT/3600.
      RETURN
      END
!
