C     Last change:  WP    5 Jun 2008    3:48 pm

      SUBROUTINE REWET
      USE BLK10MOD
      USE BLKDRMOD
      SAVE

      DATA VOID/1.E20/,VDRY/0./


      !**********************************************************************************************
      !water depth of all not wet nodes is set to 0.0d0 and NDRY for those is set to 'dry' (ndry = 2)
      !**********************************************************************************************

      InitNodesDry: DO N = 1, NP
        DREC (N) = VOID
        !cycle, if node is wet
        IF (NDRY (N) == 1) CYCLE InitNodesDry
        !IF (NDRY (N) == 1 .AND. vel(3, n) /= 0.0) GO TO 200
        !Set node N dry again
        NDRY (N) = 2
        !Set depth of node to 0.0d0
        VEL (3, N) = VDRY
      ENDDO InitNodesDry

      !**************************************************
      !All elements, that include one dry node become dry
      !**************************************************

      DryOutElements: DO M = 1, NE
        !Reset elements to be wet
        IMAT (M) = ABS (IMAT (M))
        !Get number of nodes
        NCN = NCORN (M)
        !Just consider up to 2D elements including control structures
        IF (NCN < 9 .AND. IMAT (M) < 1000) THEN
          !Set elements dry, if at least one node is dry
          CheckForDryNodes: DO K = 1, NCN
            L = NOP (M, K)
            IF (L == 0) CYCLE CheckForDryNodes
            !If a node is dry, set element dry and process next element
            IF (NDRY (L) /= 1) then
              IMAT (M) = -ABS (IMAT (M))
              CYCLE DryOutElements
            ENDIF
          ENDDO CheckForDryNodes
        ENDIF
      ENDDO DryOutElements

C-
C......SET NUMBER OF PASSES
C-
      !number rewetting runs through the mesh structure. Isn't 19 a bit too small?
      !TODO: Why not let the user decide how many runs through the mesh to potentially rewet elements
      NPAS = 19

      RewetRuns: DO NPA = 1, NPAS
        IL = 0
        ILO = 0

        !process all dry elements
        !************************
        ForElements: DO N = 1, NE
          !cycle wet elements
          IF (IMAT (N) >= 0) CYCLE ForElements
          !get number of nodes
          NCN = NCORN (N)

          ELD = VOID

          !process all dry corner nodes (look for next wet node, if not be found for next rewetted node)
          !*********************************************************************************************
          ForDryNodes: DO M = 1, NCN, 2
            !get node number
            K = ABS (NOP (N, M))
            !if node is not dry cycle
            IF (NDRY (K) /= 2) CYCLE ForDryNodes


            !process all wet corner nodes
            !****************************
            ForWetNodes: DO MM = 1, NCN, 2
              !get node corner node
              KK = ABS (NOP (N, MM))
              !Don't consider dry nodes
              IF (NDRY (KK) == 2) cycle ForWetNodes
              !Check whether depth of 2nd run node is below drying depth
              IF (VEL (3, KK) < DSET) cycle ForWetNodes

              !Get distance between nodes; how does this really work with the line on top of this routine?
              !ELD = SQRT (DIST (K, KK))
              cord1x = cord (K, 1)
              cord1y = cord (K, 2)
              cord2x = cord (KK, 1)
              cord2y = cord (KK, 2)
              !get the distance between the current dry and current wet node
              ELD = DIST (cord1x, cord1y, cord2x, cord2y)

              !if the distance is higher then the already examined shortest distance cycle; only consider shorter distances
              IF (ELD > DREC(K)) cycle ForWetNodes
              !overwrite, because distance must be smaller
              DREC (K) = ELD


C-
C......-HF- IS BEING COMPUTED TO ESTIMATE HEAD LOSS WITH A 0.2 VEL
C-
              IF(ZMANN(N) /= 0.) then
                HF =
     +           3./(VEL(3,KK)-DSET)*(1./DSET**0.333-1./VEL(3,KK)**.333)
                HLOS = HF*(0.2/1.486)**2*ZMANN(N)**2*ELD
              else
c  207         HF=1./(VEL(3,KK)-DSET)*ALOG(VEL(3,KK)/DSET)
                HF = 1./(VEL(3,KK)-DSET)*LOG(VEL(3,KK)/DSET)
                !nis,sep08: catch division by zero, if chez is not set; actually this shouldn't happen,
                !           if applied in normal way, but because RMA10Kalypso is using Darcy-Weisbach
                !           roughnes lambda, HLOS is not covered yet.
                if (CHEZ(N) /= 0.0) then
                  HLOS=HF*0.04*ELD/CHEZ(N)**2
                else
                  HLOS = 0.0
                endif
                
              ENDIF


              !TODO: Marsh-algorithm must be considered here, because this might cause stair structures at the border?!?
              !*********************************************************************************************************
              !nis,may08: consider marsh algorithm, if rewetting
              !           don't consider head loss, if rewetting
              if (idnopt == 0) then
                !DEP = VEL (3, KK) + AO (KK) - AO (K) - HLOS
                DEP = VEL (3, KK) + AO (KK) - AO (K)
                if (dep <= 0.0d0) cycle ForWetNodes
              elseif (idnopt < 0) then
                VirtDepth = vel (3, kk)
                RealDepth = 0.0d0
                CALL AMF(RealDepth,VirtDepth, AKP(KK), ADT(KK),ADB(KK),
     +                   DUM1, DUM2, 0)
                !RealDepth = RealDepth + ado (kk) - ado (k) - HLOS
                RealDepth = RealDepth + ado (kk) - ado (k)
                !if not rewetted
                if (RealDepth <= 0.0d0) cycle ForWetNodes
                !otherwise calculate flow depth of potentially rewetted node
                CALL AMF(RealDepth, Dep, AKP(K), ADT(K),ADB(K),
     +                   DUM1, DUM2, 1)
              endif
                
              VEL (3, K) = DEP
              VDOT (3, K) = ALTM * (VEL (3, K) - VOLD (3, K))
              VDOTO (3, K) = 0.
              IL = IL + 1
              LISTEL (IL) = N
              !nis,jul08: Error, il - 1 == 0, because, that is not defined
              if (il /= 1) then
                IF (IL > 1 .AND. LISTEL (IL) == LISTEL (IL - 1))
     +            IL = IL-1
              endif

            ENDDO ForWetNodes
          ENDDO ForDryNodes
          IF (ELD == VOID) cycle ForElements
          IF (IL == ILO) cycle ForElements
          ILO = IL
        ENDDO ForElements

        IF (IL == 0) EXIT RewetRuns

        !****************************************************************
        !Rewet nodes belonging to elements, that are potentially rewetted
        !****************************************************************

        RewetNodes: DO I = 1, IL
          !get element, that includes rewetted nodes from list
          NN = LISTEL (I)
          !get corner nodes
          NCN = NCORN (NN)
          !only consider up 2D elements including control structures
          IF (NCN < 9 .AND. ABS (IMAT (NN)) < 1000) THEN
            !Process all corner nodes
            DO M = 1, NCN, 2
              !get node
              N = ABS (NOP (NN, M))
              !check whether node is dry but has a depth after rewet run; then set it rewetted (NDRY becomes -1)
              IF (NDRY (N) == 2 .AND. vel (3, n) > 0.0) NDRY (N) = -1
            ENDDO
          ENDIF
        ENDDO RewetNodes

      ENDDO RewetRuns

      RETURN
      END

      !***************************************************************************

      function DIST (cord1x, cord1y, cord2x, cord2y)
        REAL (KIND = 8)              :: dist
        REAL (KIND = 8), INTENT (IN) :: cord1x, cord1y, cord2x, cord2y
      !Calculate a distance between nodes; Which nodes are N2 and N1; something like a function definition
      DIST = sqrt ((cord2x - cord1x)**2 + (cord2y - cord1y)**2)
      RETURN
      end function

