!     Last change:  WP   20 May 2008    4:53 pm
!
      SUBROUTINE DEL
      USE BLK10MOD
      USE BLKDRMOD
      SAVE
!
!     DIMENSION IMATO(MEL),NDRYO(MNP),IPT(MNP)
!-
       DATA IENT/0/
!-
!......SUBROUTINE TO DROP OUT ELEMENTS
!-
!parameters      
!**********      
!      
! globally      
! ********      
! IMATO (N) : Material type of element N from the last time step/ calculation step/ iteration      
! NDRYO (K) : Dry node pointer for node K; means 'was dry' or 'was wet'; values are described below!      
! DSETD     : depth at which nodes are considered to be wetted, if dry before      
! DSET      : depth at which nodes are considered to be dry, if wetted before      
! locally      
! *******      
! IENT: Switch, that shows, whether program has been already here or not; this part of the code is only initially been reached      
! IPT (J)   : Below it is counted and stored which nodes/ elements are dried out/ wetted; it is only used here WHY IS IT GLOBALLY DEFINIED?      
!             ELIMINATE FROM GLOBAL DEFINITION!      
!
!meaning of NDRY      
!NDRY (J) = 1 .AND. NDRYO (J) = 1 : node is wet and was wet      
!NDRY (J) = 1 .AND. NDRYO (J) = 2 : node is wet and was dry ; node is added to the system      
!NDRY (J) = 2 .AND. NDRYO (J) = 1 : node is dry and was wet      
!NDRY (J) = 2 .AND. NDRYO (J) = 2 : node is dry and was dry ; node is eliminated from the system      
!NDRY (J) = -1                    : node was just possibly selected to be rewetted in the previous execution of rewet subroutine      
!
!*********************************************      
!Initialize 'old' values in the very first run      
!*********************************************      
      IF(IENT == 0) THEN
        ElementOld: DO N = 1, NE
          IMATO (N) = IMAT (N)
        ENDDO ElementOld
        nodesOld: DO N = 1, NP
          NDRYO (N) = NDRY(N)
        ENDDO nodesOld
      ENDIF
!RESET IENT, i.e. program has initialized those values above      
      IENT = 1
!
!
!-
!......RESET MAT TYPES
!-
!****************************************************************      
!Rewet all elements and dry them out, if at least one node is dry      
!****************************************************************      
      DryOutElements: DO N = 1, NE
!Reset all elements to be wet        
        IMAT (N) = ABS (IMAT (N))
!cycle deactivated element        
        IF (IMAT (N) == 0) CYCLE DryOutElements
!get number of corner nodes        
        NCN = NCORN (N)
!only consider up to 2D elements including control structures        
        IF (NCN < 9 .AND. IMAT (N) < 1000) THEN
          DryOutNodes: DO M = 1, NCN, 2
!get node            
            L = ABS (NOP (N, M))
!greater then activation depth: surely wet            
            IF (VEL (3, L) > DSETD) CYCLE DryOutNodes
!only possible value is NDRY (L) = -1, i.e. node is just rewetted in this run. If it doesn't reach the rewetting depth dsetd, element is still dry            
! .OR.             
!depth is below deactivation depth            
            IF (NDRY (L) < 1 .OR. VEL (3, l) <= dset) THEN
!Set element dry              
              IMAT (N) = - ABS (IMAT (N))
            ENDIF
          ENDDO DryOutNodes
        ENDIF
      ENDDO DryOutElements
!
!****************************      
!Initialize nodes to be 'dry'      
!****************************      
      DO N = 1, NP
        NDRY (N) = 2
      ENDDO
!
!**********************************************      
!Reinitialize nodes to be 'wet' in wet elements      
!**********************************************      
      WetNodes: DO N = 1, NE
!cycle dry or deactivated elements        
        IF (IMAT (N) < 1) CYCLE WetNodes
!get number of nodes        
        NCN = NCORN (N)
!Initialize nodes to be wet for wet elements        
        DO M = 1, NCN
          L = ABS (NOP (N, M))
          NDRY (L) = 1
        ENDDO
      ENDDO wetnodes
!
!******************************      
!Nullify variables of dry nodes      
!******************************      
      NullifyVariables: DO L = 1, NP
!cycle wet nodes        
        IF (NDRY (L) == 1) CYCLE NullifyVariables
!for dry nodes reduce the variables to 0.0        
        DO IA = 1, 3
          VEL (IA, L) = 0.
        ENDDO
      ENDDO NullifyVariables
!
!***************************      
!Process eliminated elements      
!***************************      
!Write header of this output section      
      WRITE(LOUT,6005)
 6005 FORMAT(' THE FOLLOWING ELEMENTS HAVE BEEN ELIMINATED')
!Initialize counter      
      NR = 0
!Examine eliminated elements      
      EliminatedElements: DO N = 1, NE
!cycle wet and deactivated elements        
        IF (IMAT (N) >= 0) CYCLE EliminatedElements
!cycle dry elements that were already dry        
        IF (IMATO (N) < 0) CYCLE EliminatedElements
!count and remember eliminated elements        
        NR = NR + 1
        IPT (NR) = N
      ENDDO EliminatedElements
!Write out eliminated elements      
      IF (NR > 0) WRITE (LOUT, *) (IPT (N), N = 1, NR)
 6030 FORMAT(10I5)
!
!******************************      
!Process (re)activated elements      
!******************************      
!Write header of this output section      
      WRITE(LOUT,6007)
 6007 FORMAT(' THE FOLLOWING ELEMENTS HAVE BEEN ADDED')
!Initialize counter      
      NR = 0
!Examine (re)activated elements      
      ReactivatedElements: DO N = 1, NE
!cycle wet and deactivated elements        
        IF (IMAT (N) <= 0) CYCLE ReactivatedElements
!cycle wet elements that were already wet        
        IF (IMATO (N) > 0) CYCLE ReactivatedElements
!count and remember (re)activated elements        
        NR = NR + 1
        IPT (NR) = N
      ENDDO ReactivatedElements
!Write out (re)activated elements      
      IF (NR > 0) WRITE (LOUT, 6030) (IPT (N), N = 1, NR)
!
!************************      
!Process eliminated nodes      
!************************      
!Write header of this output section      
      WRITE(LOUT,6008)
 6008 FORMAT(' THE FOLLOWING NODES HAVE BEEN ELIMINATED')
!Initialize counter      
      NR = 0
!Examine eliminated nodes      
      EliminatedNodes: DO N = 1, NP
!cycle wet nodes        
        if (NDRY (N) == 1) CYCLE EliminatedNodes
!cycle nodes, that is dry and was dry; what about rewetted nodes (NDRY = -1)        
        if (NDRYO (N) /= 1) CYCLE EliminatedNodes
!count and remember eliminated node        
        NR = NR + 1
        IPT (NR) = N
      ENDDO EliminatedNodes
!Write eliminated nodes      
      IF (NR > 0) WRITE (LOUT, *) (IPT (N), N = 1, NR)
!
!**********************      
!Process rewetted nodes      
!**********************      
!Write header of this output section      
      WRITE (LOUT, 6009)
 6009 FORMAT(' THE FOLLOWING NODES HAVE BEEN ADDED')
!Initialize counter      
      NR = 0
!Examine rewetted nodes      
      RewetNodes: DO N = 1, NP
!cycle dry nodes        
        IF (NDRY (N) /= 1) CYCLE RewetNodes
!cycle nodes, that are wet and were wet        
        IF (NDRYO (N) == 1) CYCLE RewetNodes
!count and remember rewetted nodes        
        NR = NR + 1
        IPT (NR) = N
      ENDDO RewetNodes
!Write out rewetted nodes      
      IF (NR > 0) WRITE (LOUT, 6030) (IPT (N), N = 1, NR)
!
!**************************************      
!devolve current values to 'old' values      
!**************************************      
      DO N = 1, NE
        IMATO (N) = IMAT (N)
      ENDDO
      DO N = 1, NP
        NDRYO (N) = NDRY (N)
      ENDDO
!
!
      RETURN
      END
