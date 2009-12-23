!     Last change:  MD    8 Jun 2009    7:50 pm
!IPK  LAST UPDATE APR 4 1997 USE OLD VALUES OF CONCENTRATION FOR SETTLING
      SUBROUTINE SETVEL
!           THIS SUBROUTINE SETS THE SETTLING VELOCITIES FOR EACH
!       NODE, AT EACH TIMESTEP THAT THE CODE ARRAY ISVL HAS A
!       NON-ZERO VALUE.
!       IF ISVL=1 SETTLING VELOCITIES ARE SET TO A CONSTANT VSST
!       WHICH HAS BEEN READ IN. IF ISVL=2 A  SET. VEL MODEL
!       IS USED TO GENERATE THE VALUES.
!----------------------------------------------------------------------
!
      USE BLK10MOD
      USE BLKSEDMOD
!-
      IF(ISVL == 1) THEN
        DO I=1,NP
          VS(I)=VSST
        ENDDO
!
      ELSEIF(ISVL == 2 .OR. ISVL == 3) THEN
!
!....    THIS IS THE PLACE TO PUT IN SOME GRAND SET. VEL. MODEL
        K=LSS
        DO  I=1,NP
          IF(VEL(6,I) < CRCON1(I)) THEN
            VS(I)=VSS1(I)
          ELSEIF(VEL(6,I) > CRCON2(I)) THEN
            VS(I)=VSS2(I)
          ELSE
            VS(I)=VSK(I)* (VEL(6,I)**EXP2(I))
          ENDIF
        ENDDO
!
      ENDIF
!
      RETURN
      END
