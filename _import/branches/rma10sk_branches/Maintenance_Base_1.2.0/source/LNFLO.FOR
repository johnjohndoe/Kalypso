
      SUBROUTINE LNFLO (surfLayElt, flowLoad, qualLoad, 
     +                  NEST, LN, ELINX, ELINY)
     
     
      !Subroutine does four steps
      !--------------------------
      !1. step: TODO: needs to be examined; is related to nest > 1
      !2. step: Transform total loads to distributed loads
      !3. step: Apply load to according element
      !4. step: Apply load to depending elements in 3D applications, if desired

      USE BLK10MOD, only: 
        !output
     +  sidf, sidq, einx, einy,
        !input
     +  imat, ncorn, tvol, tvolc, nedep, neref
     
      !meaning of the variables
      !------------------------
      !sidf         sink/source flow term without momentum
      !sidq         sink/source quality term
      !einx, einy   sink/source flow term with momentum (means with direction),
      !             i.e. x- and y-component
      !
      !imat         material type of the according element      
      !ncorn        number of corner nodes of the according element
      !tvol         plan view area of the according element
      !tvolc        volume of the according element
      !nedep        count of depending elements in 3D-layers under according surface element
      !neref        IDs of depending elements in 3D-layers under according surface element
      
      implicit none
      SAVE

      !arguments
      !---------
      integer, intent (in) :: surfLayElt
      integer, intent (inout) :: NEST
      integer, intent (in) :: LN
      real (kind = 8), intent (in) :: flowLoad
      real (kind = 8), intent (in) :: qualLoad(1:3)
      real (kind = 8), intent (in) :: elinx, eliny
      !meaning of the variables
      !------------------------
      !(input)
      !-------
      !surfLayElt      element to which flow will be applied; can be surface element on which
      !                layer elements depend
      !nest            switch that shows, whether flow is 
      !                0 - distributed load
      !                1 - total load
      !                TODO: needs to be examined
      !                >1 - special handling
      !ln              number of the layer to apply values to
      !                0 - apply to all layers
      !                1 - apply to surface layer element
      !                >1 - apply to that particular layer 
      !(output)
      !--------
      !flowLoad        sink/source term without momentum that can be total load or distributed load;
      !                it will be transformed to distributed load within this subroutine
      !                depending on nest
      !qualLoad        sink/source quality terms, i.e. constituents as sink/source for element
      !elinx, eliny    sink/source element inflow with momentum, i.e. with direction
      !                (x- and y- component)

      !local variables
      !---------------
      integer (kind = 4) :: layElt, firstLayElt, lastLayElt
      real (kind = 8) :: SF


      !local copy that can be transformed
      !----------------------------------
      SF = flowLoad

      !1st step: TODO: Examine in detail; Apply load to some elements
      !---------
      !normally nest is 0 or 1, while calling a value is added to the input of the user for showing special cases
      if (nest >= 2) then

        nest = nest - 2

        !only if neither for elements marked in a special way nor for a special layer
        if (.not. ((nest + 2) == 3  .or. ln /= 0)) then

          !and only for 3D-applications
          if (.not. (imat (surfLayElt) < 1000 
     +               .and. ncorn (surfLayElt) < 10)) THEN


            if (imat (surfLayElt) / 1000 == 1) then
              SIDF (surfLayElt) = SIDF(surfLayElt) + SF
              SIDQ (surfLayElt, 1) = qualLoad (1)
              SIDQ (surfLayElt, 2) = qualLoad (2)
              SIDQ (surfLayElt, 3) = qualLoad (3)
CIPK JAN03  ADD MOMENTUM
              EINX (surfLayElt) = ELINX
              EINY (surfLayElt) = ELINY
            endif

            RETURN
          endif
        endif
      endif

      !2nd step: transform total loads to distributed
      !---------
      !Distribute TOTAL LOAD to length/ area/ volume of element
      IF (NEST /= 0) THEN
        IF (LN == 0) THEN
          SF = SF / TVOLC (surfLayElt)
        ELSEIF (LN == 1) THEN
          SF = SF / TVOL (surfLayElt)
        ELSE
          layElt = NEREF (surfLayElt) + LN - 1
          SF = SF / TVOL (layElt)
        ENDIF
      ENDIF    
      
      !3rd step: Apply distributed load to according layer
      !---------
      !Apply to surface layer (ln==1) or 
      !      to all layers, including surface layer (ln==0)
      FindLayElt: if (ln == 0 .or. ln == 1) then
        layElt = surfLayElt
      !Apply to any specific layer
      else FindLayElt
        layElt = NEREF (surfLayElt) + LN - 1
      endif FindLayElt

      !source/sink flow term without moment
      SIDF (layElt) = SIDF (layElt) + SF
      !source/sink quality terms
      SIDQ (layElt, 1) = qualLoad (1)
      SIDQ (layElt, 2) = qualLoad (2)
      SIDQ (layElt, 3) = qualLoad (3)
      !source/sink flow term with momentum (means with direction)
      EINX (layElt) = ELINX
      EINY (layElt) = ELINY
      
      !4th step: Apply distributed load to depending layer elements, if desired
      !---------
      !Additionally apply to all layers, except surface layers (case ln==0)
      !ATTENTION: This only works, if the elements that are below a surface element
      !           are numbered sequentially!!!
      if (ln == 0) then
        IF (NEDEP (surfLayElt) > 1) THEN
          firstLayElt = NEREF (surfLayElt) + 1
          lastLayElt = NEREF (surfLayElt) + NEDEP (surfLayElt) - 1
          !Apply values
          DO layElt = firstLayElt, lastLayElt
            !source/sink flow term without momentum
            sidf (layElt) = SIDF (layElt) + SF
            !source/sink quality term
            sidq (layElt, 1) = qualLoad (1)
            sidq (layElt, 2) = qualLoad (2)
            sidq (layElt, 3) = qualLoad (3)
            !source/sink flow term with momentum (means with direction)
            einx (layElt) = ELINX
            einy (layElt) = ELINY
          enddo
        endif
      endif

      RETURN
      END
