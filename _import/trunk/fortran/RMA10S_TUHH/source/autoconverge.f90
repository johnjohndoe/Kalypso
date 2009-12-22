 !     Last change:  WP   22 Nov 2007    7:56 pm
 SUBROUTINE autoconverge(noc)

 USE blk10
 USE BLK10MOD
 USE BLK11MOD
 USE BLKDRMOD
 USE BLKSEDMOD
 USE BLKSANMOD
 USE BLKSSTMOD
 USE PARAKalyps
 USE WBMMODS
 USE Para1DPoly

 REAL,INTENT(IN)  :: noc
 !DIMENSION temp_speccc(3)

       if (noc==-1.) then

         OPEN(789,FILE='autoconverge.txt')
         WRITE(789,*)'time step    iteration cycle        time    iurvl &
     &         NCL    HC/QC HC2/QDIR'

       ELSEIF (noc==1.) then

        call feldgroesse(3,nita)

        deltindex = 0.
        temp_delt = 1000.
        deltsum = temp_delt
        deltn = 1000.

        do i = 1,ncl

          if (speccc(i,1) == 1.5) then

            specccold(i,1) = specccfut(i,1)
            specccold(i,2) = elev

            if (specccfut(i,3) /= 0.) then

              specccold(i,3) = specccold(i,2)

            end if

            if (lmt(i) == 1.0) then

              if (width(line(i,1)) == 0.0) then

                specccold(i,2) = (hhmax(line(i,1))-hhmin(line(i,1)))/2.+ &
    &                            hhmin(line(i,1))

                if (specccfut(i,3) /= 0.0) then

                  specccold(i,3) = specccold(i,2)

                end if

              end if

            end if

            do k = 4,8

              specccold(i,k) = specccfut(i,k)

            end do

          elseif ((speccc(i,1)) == 2.5) then

            specccold(i,1) = specccfut(i,1)
            specccold(i,2) = 0.0

            do k = 3,8

              specccold(i,k) = specccfut(i,k)

            end do

          end if

        end do

        temp_iteqs = iteqs(1)
        temp_iteqv = iteqv(1)
        temp_iurvl = iurvl(1)
        temp_itlvl = itlvl(1)

        temp_iurvl1 = iurvl(1)

        do k = 1,np

          do i = 1,ndf

            temp_vel(i,k) = vel(i,k)
            temp_vdot(i,k) = vdot(i,k)
            temp_vdoto(i,k) = vdoto(i,k)

          end do

        end do

      ELSEIF (noc==2.) then

        iteqs(maxn) = temp_iteqs
        iteqv(maxn) = temp_iteqv
        iurvl(maxn) = temp_iurvl
        itlvl(maxn) = temp_itlvl

        if (exterr == 1.0) then

          do k = 1,np

            do i = 1,ndf

              if (deltindex == 0.) then

                vel(i,k) = temp_vel(i,k)
                vdot(i,k) = temp_vdot(i,k)
                vdoto(i,k) = temp_vdoto(i,k)

              else

                vel(i,k) = vold(i,k)
                vdot(i,k) = vdoto(i,k)
                vdoto(i,k) = v2ol(i,k)

              end if

            end do

          end do

        end if

        exterr = 0.0

      ELSEIF (noc==3.) then

        if (temp_iurvl <= 8.0) then

          temp_iurvl = temp_iurvl + 1.

          WRITE(*,*)'Daempfung erhoeht :',temp_iurvl

        else

          deltn = delt / nnnst
          deltsum = deltsum - delt + deltn
          delt  = deltn

          if (deltindex == 0.) then

            deltsum = delt

          end if

          WRITE(*,*)'Zwischenzeitschritt : ',deltsum

          WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

          do i = 1,ncl

            if ((speccc(i,1)) == 1.5) then

              call autoboundaryh(specccfut(i,2),specccold(i,2),    &
     &             specccfut(i,3),specccold(i,3),temp_delt,deltsum,&
     &             linlog,hhh,hhh2)

              speccc(i,2) = hhh
              speccc(i,3) = hhh2

              temp_speccc(1) = speccc(i,6)
              temp_speccc(2) = speccc(i,7)
              temp_speccc(3) = speccc(i,8)

              call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

              WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                  ' an Kontinuitaetslinie ',i

              WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

            ELSEIF ((speccc(i,1)) == 2.5) then

              call autoboundaryQ(specccfut(i,2),specccold(i,2),    &
     &             specccfut(i,3),specccold(i,3),temp_delt,deltsum,&
     &             linlog,qqq,qqqdir)

              speccc(i,2) = qqq
              speccc(i,3) = qqqdir

              temp_speccc(1) = speccc(i,6)
              temp_speccc(2) = speccc(i,7)
              temp_speccc(3) = speccc(i,8)

              call qgen(i, qqq, qqqdir, temp_speccc)

              WRITE(*,*)'neue Durchflussrandbedingung  ',qqq,&
     &                  ' an Kontinuitaetslinie ',i

              WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

            end if

          end do

          call bform(0)

        end if

        maxn = 0.

        call feldgroesse(6,nita)

        niti = nitizero
        nita = nitazero

        call feldgroesse(5,nita)

        do j = 1,nitazero

          iteqv(j) = temp_iteqv

        end do

        autoindex = 1.

        return

      ELSEIF (noc==4.) then

          if (nconv == 0.)then

            call statistic(maxn,rss,rrr,nitazero,extranita)

            IF(rrr < 0.)then

              WRITE(*,*)'rrr lower than zero'

              call feldgroesse(6,nita)

              nitn=nitn+extranita
              nita=nita+extranita

              call feldgroesse(5,nita)

              do j = 1,nita

                iteqv(j) = temp_iteqv
                
              end do

              autoindex = 1.

              return

            else

              WRITE(*,*)'rrr higher than zero'

              if (temp_iurvl <= 8.) then

                call feldgroesse(6,nita)

                nitn=nitn+extranita
                nita=nita+extranita
                temp_iurvl=temp_iurvl+1

                call feldgroesse(5,nita)

                do j = 1,nita

                  iteqv(j) = temp_iteqv

                end do

                WRITE(*,*)'Daempfung erhoeht :',temp_iurvl

                autoindex = 1.

                return

              else

                deltn = delt / nnnst
                deltsum = deltsum - delt + deltn
                delt = deltn

                if (deltindex == 0.) then

                  deltsum = delt

                end if

                WRITE(*,*)'Zwischenzeitschritt :',deltsum
                WRITE(*,*)'Zeitschrittweite    :',delt

                WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

                do i = 1,ncl

                  if (speccc(i,1)==1.5) then

                    call autoboundaryh(specccfut(i,2),specccold(i,2),&
     &                   specccfut(i,3),specccold(i,3),temp_delt,    &
     &                   deltsum,linlog,hhh,hhh2)

                    speccc(i,2) = hhh
                    speccc(i,3) = hhh2

                    temp_speccc(1) = speccc(i,6)
                    temp_speccc(2) = speccc(i,7)
                    temp_speccc(3) = speccc(i,8)

                    call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

                    WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                        ' an Kontinuitaetslinie ',i

                    WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

                  ELSEIF ((speccc(i,1)) == 2.5) then

                    call autoboundaryQ(specccfut(i,2),specccold(i,2),   &
     &                   specccfut(i,3),specccold(i,3),temp_delt,deltsum&
     &                   ,linlog,qqq,qqqdir)

                    speccc(i,2) = qqq
                    speccc(i,3) = qqqdir

                    temp_speccc(1) = speccc(i,6)
                    temp_speccc(2) = speccc(i,7)
                    temp_speccc(3) = speccc(i,8)

                    call qgen(i, qqq, qqqdir, temp_speccc)

                    WRITE(*,*)'neue Durchflussrandbedingung  ',qqq,&
     &                        ' an Kontinuitaetslinie ',i

                    WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

                  end if

                end do

                call bform(0)

                maxn = 0.

                do j = 1,np

                  do k = 1, ndf

                    vel(k,j) = vold(k,j)
                    vdot(k,j) = vdoto(k,j)
                    vdoto(k,j) = v2ol(k,j)

                  end do

                end do

                call feldgroesse(6,nita)

                niti = nitizero
                nita = nitazero

                call feldgroesse(5,nita)

                do j = 1,nita

                  iteqv(j) = temp_iteqv

                end do

                autoindex = 1.

                return

              end if

            endif

         else

         WRITE(lout,*)'vor zeitschrittberechnung'

           if (deltsum < temp_delt) then

             if (deltindex == 0.) then

               deltsum = delt

             end if

             deltindex = deltindex + 1
             delt = temp_delt - deltsum
             deltsum = temp_delt
             temp_iurvl = temp_iurvl1

             WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

             WRITE(*,*)'Zwischenzeitschritt :',deltsum
             WRITE(*,*)'Zeitschrittweite    :',delt

             do i = 1,ncl

               if (speccc(i,1)==1.5) then

                 call autoboundaryh(specccfut(i,2),specccold(i,2),&
     &                specccfut(i,3),specccold(i,3),temp_delt,    &
     &                deltsum,linlog,hhh,hhh2)

                      speccc(i,2) = hhh
                      speccc(i,3) = hhh2

                      temp_speccc(1) = speccc(i,6)
                      temp_speccc(2) = speccc(i,7)
                      temp_speccc(3) = speccc(i,8)

                      call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

                 WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                     ' an Kontinuitaetslinie ',i

                 WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

               ELSEIF ((speccc(i,1)) == 2.5) then

                 call autoboundaryQ(specccfut(i,2),specccold(i,2),   &
     &                specccfut(i,3),specccold(i,3),temp_delt,deltsum&
     &               ,linlog,qqq,qqqdir)

                 speccc(i,2) = qqq
                 speccc(i,3) = qqqdir

                 temp_speccc(1) = speccc(i,6)
                 temp_speccc(2) = speccc(i,7)
                 temp_speccc(3) = speccc(i,8)

                 call qgen(i, qqq, qqqdir, temp_speccc)

                 WRITE(*,*)'neue Durchflussrandbedingung  ',qqq,&
     &                     ' an Kontinuitaetslinie ',i

                 WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

               end if

             end do

             call bform(0)

             maxn = 0.

             do j = 1,np

               do k = 1, ndf

                 vold(k,j) = vel(k,j)
                 v2ol(k,j) = vdoto(k,j)
                 vdoto(k,j) = vdot(k,j)

               end do

             end do

             call feldgroesse(6,nita)

             niti = nitizero
             nita = nitazero

             call feldgroesse(5,nita)

             do j = 1,nita

               iteqv(j) = temp_iteqv

             end do

             autoindex = 1.

             return

           endif

           do i = 1,ncl

             do k = 1,3

               specccold(i,k) = specccfut(i,k)

             end do

           end do

           call feldgroesse(6,nita)

           nitn=nitnzero
           nita=nitazero

           call feldgroesse(5,65)

          end if

      ELSEIF (noc==5.) then

        nitazero = nitn

        if (niti == 0.) then

          do i = 1,ncl

            if (speccc(i,1) == 1.5) then

              specccold(i,1) = specccfut(i,1)
              specccold(i,2) = elev

              if (specccfut(i,3) /= 0.0) then

                 specccold(i,3) = specccold(i,2)

              end if

              if (lmt(i) == 1.0) then

                if (width(line(i,1)) == 0.0) then

                  specccold(i,2) = (hhmax(line(i,1))-hhmin(line(i,1)))/&
     &                              2.+hhmin(line(i,1))

                  if (specccfut(i,3) /= 0.0) then

                    specccold(i,3) = specccold(i,2)

                  end if

                end if

              end if

              do k = 4,8

                specccold(i,k) = specccfut(i,k)

              end do

            ELSEIF (speccc(i,1) == 2.5)then

              specccold(i,1) = specccfut(i,1)
              specccold(i,2) = 0.0

              do k = 3,8

                specccold(i,k) = specccfut(i,k)
             
              end do

            end if

          end do

          call feldgroesse(3,65)

        end if

      !end if

      ELSEIF (noc==6.) then

          deltindex = 0
          temp_delt = delt
          deltsum = temp_delt
          deltn = delt

          temp_iteqs=iteqs(1)
          temp_iteqv=iteqv(1)
          temp_iurvl=iurvl(1)
          temp_itlvl=itlvl(1)

          temp_iurvl1 = iurvl(1)

        do k = 1,np

          do i = 1,ndf

            temp_vel(i,k) = vel(i,k)
            temp_vdot(i,k) = vdot(i,k)
            temp_vdoto(i,k) = vdoto(i,k)

          end do

        end do

      ELSEIF (noc==7.) then

         iteqs(maxn) = temp_iteqs
         iteqv(maxn) = temp_iteqv
         iurvl(maxn) = temp_iurvl
         itlvl(maxn) = temp_itlvl

         if (exterr == 1.0) then

           do k = 1,np

             do i = 1,ndf

               if (deltindex == 0 .AND. niti == 0.) then

                 vel(i,k) = temp_vel(i,k)
                 vdot(i,k) = temp_vdot(i,k)
                 vdoto(i,k) = temp_vdoto(i,k)

               else

                 vel(i,k) = vold(i,k)
                 vdot(i,k) = vdoto(i,k)
                 vdoto(i,k) = v2ol(i,k)

                end if

             end do

           end do

         end if

         exterr = 0.0

      ELSEIF (noc==8.) then

        if (temp_iurvl <= 8.0) then

          temp_iurvl = iurvl(1) + 1.

          WRITE(*,*)'Daempfung erhoeht :',temp_iurvl

        else

          deltn = delt / nnnunst
          deltsum = deltsum - delt + deltn
          delt  = deltn

          if (deltindex == 0.) then

            deltsum = delt

          end if

          WRITE(*,*)'Zwischenzeitschritt :',deltsum
          WRITE(*,*)'Zeitschrittweite    :',delt

          WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

          do i = 1,ncl

            if ((speccc(i,1)) == 1.5) then

              call autoboundaryh(specccfut(i,2),specccold(i,2),&
     &             specccfut(i,3),specccold(i,3),temp_delt,deltsum,&
     &             linlog,hhh,hhh2)

              speccc(i,2) = hhh
              speccc(i,3) = hhh2

              temp_speccc(1) = speccc(i,6)
              temp_speccc(2) = speccc(i,7)
              temp_speccc(3) = speccc(i,8)

              WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                  ' an Kontinuitaetslinie ',i

              call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

              WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

            elseif ((speccc(i,1)) == 2.5) then

              call autoboundaryQ(specccfut(i,2),specccold(i,2),    &
     &             specccfut(i,3),specccold(i,3),temp_delt,deltsum,&
     &             linlog,qqq,qqqdir)

              speccc(i,2) = qqq
              speccc(i,3) = qqqdir

              temp_speccc(1) = speccc(i,6)
              temp_speccc(2) = speccc(i,7)
              temp_speccc(3) = speccc(i,8)

              call qgen(i, qqq, qqqdir, temp_speccc)

              WRITE(*,*)'neue Durchflussrandbedingung  ',qqq,&
     &                  ' an Kontinuitaetslinie ',i

              WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

            end if

          end do

          call bform(0)

        end if

        maxn = 0.

        call feldgroesse(6,nita)

        nitn = nitnzero
        nita = nitazero

        call feldgroesse(5,nita)

        do j = 1,nita

          iteqv(j) = temp_iteqv
          
        end do

      ELSEIF (noc==9.) then

          if (nconv == 0.)then

            call statistic(maxn,rss,rrr,nitazero,extranita)

            IF(rrr < 0.)then

              WRITE(*,*)'rrr lower than zero'

              call feldgroesse(6,nita)

              nitn=nitn+extranita
              nita=nita+extranita

              call feldgroesse(5,nita)

              do j = 1,nita

                iteqv(j) = temp_iteqv
               
              end do

              autoindex = 2.

              return

            else

              WRITE(*,*)'rrr higher than zero'

              if (temp_iurvl <= 8.) then

                call feldgroesse(6,nita)

                nitn=nitn+extranita
                nita=nita+extranita
                temp_iurvl=temp_iurvl+1

                call feldgroesse(5,nita)

                WRITE(*,*)'Daempfung erhoeht :',temp_iurvl

                do j = 1,nita

                  iteqv(j) = temp_iteqv

                end do

                autoindex = 2.

                return

              else

                deltn = delt / nnnunst
                deltsum = deltsum - delt + deltn
                delt = deltn

                if (deltindex == 0.) then

                  deltsum = delt

                end if

                WRITE(*,*)'Zwischenzeitschritt: ',deltsum

                WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

                do i = 1,ncl

                  if (speccc(i,1)==1.5) then

                    call autoboundaryh(specccfut(i,2),specccold(i,2),&
     &                   specccfut(i,3),specccold(i,3),temp_delt,    &
     &                   deltsum,linlog,hhh,hhh2)

                    speccc(i,2) = hhh
                    speccc(i,3) = hhh2

                    temp_speccc(1) = speccc(i,6)
                    temp_speccc(2) = speccc(i,7)
                    temp_speccc(3) = speccc(i,8)

                    call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

                    WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                        ' an Kontinuitaetslinie ',i

                    WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

                  ELSEIF (speccc(i,1)==2.5) then

                    call autoboundaryQ(specccfut(i,2),specccold(i,2),&
     &                   specccfut(i,3),specccold(i,3),temp_delt,    &
     &                   deltsum,linlog,qqq,qqqdir)

                    speccc(i,2) = qqq
                    speccc(i,3) = qqqdir

                    temp_speccc(1) = speccc(i,6)
                    temp_speccc(2) = speccc(i,7)
                    temp_speccc(3) = speccc(i,8)

                    call qgen(i, qqq, qqqdir, temp_speccc)

                    WRITE(*,*)'neue Durchflussrandbedingung ',qqq,&
     &                        ' an Kontinuitaetslinie ',i

                    WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

                  end if

                end do

                call bform(0)

                maxn = 0.

                do j = 1, np

                  do k = 1, ndf

                    vel(k,j) = vold(k,j)
                    vdoto(k,j) = v2ol(k,j)
                    vdot(k,j) = vdoto(k,j)

                  end do

                end do

                call feldgroesse(6,nita)

                nitn = nitnzero
                nita = nitazero

                call feldgroesse(5,nita)

                do j = 1,nita

                  iteqv(j) = temp_iteqv

                end do

                autoindex = 2.

                return

              endif

            end if

          else

            if (deltsum < temp_delt) then

              if (deltindex == 0.) then

                deltsum = delt

              end if

              deltindex = deltindex + 1
              delt = temp_delt - deltsum
              deltsum = temp_delt
              temp_iurvl = temp_iurvl1

              WRITE(789,'(7x,i3,14x,i5,4x,f8.2,4x,i5)')   icyc,maxn,deltsum,temp_iurvl

              WRITE(*,*)'Zwischenzeitschritt :',deltsum

              WRITE(*,*)'Zeitschrittweite    :',delt

              do i = 1,ncl

                if (speccc(i,1)==1.5) then

                  call autoboundaryh(specccfut(i,2),specccold(i,2),&
     &                 specccfut(i,3),specccold(i,3),temp_delt,    &
     &                 deltsum,linlog,hhh,hhh2)

                  speccc(i,2) = hhh
                  speccc(i,3) = hhh2

                  temp_speccc(1) = speccc(i,6)
                  temp_speccc(2) = speccc(i,7)
                  temp_speccc(3) = speccc(i,8)

                  call hgen (0, i, hhh, hhh2, 0, temp_speccc, hfd)

                  WRITE(*,*)'neue Wasserstandrandbedingung ',hhh,&
     &                      ' an Kontinuitaetslinie ',i

                  WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,hhh,hhh2

                ELSEIF (speccc(i,1)==2.5) then

                  call autoboundaryQ(specccfut(i,2),specccold(i,2),&
     &                 specccfut(i,3),specccold(i,3),temp_delt,    &
     &                 deltsum,linlog,qqq,qqqdir)

                  speccc(i,2) = qqq
                  speccc(i,3) = qqqdir

                  temp_speccc(1) = speccc(i,6)
                  temp_speccc(2) = speccc(i,7)
                  temp_speccc(3) = speccc(i,8)

                  call qgen(i, qqq, qqqdir, temp_speccc)

                  WRITE(*,*)'neue Durchflussrandbedingung  ',qqq,&
     &                     ' an Kontinuitaetslinie ',i

                  WRITE(789,'(60x,i3,2x,2(f7.2,2x))')i,qqq,qqqdir

                end if

              end do

              maxn = 0.

              call bform(0)

              do j = 1, np

                do k = 1, ndf

                  vold(k,j) = vel(k,j)
                  v2ol(k,j) = vdoto(k,j)
                  vdoto(k,j) = vdot(k,j)

                end do

              end do

              call feldgroesse(6,nita)

              nitn = nitnzero
              nita = nitazero

              call feldgroesse(5,nita)

              do j = 1,nita

                iteqv(j) = temp_iteqv

              end do

              autoindex = 2.

              return

            endif

            do i = 1,ncl

              do k = 1,3

                specccold(i,k) = specccfut(i,k)

              end do

            end do

            call feldgroesse(6,nita)

            nitn=nitnzero
            nita=nitazero

            call feldgroesse(5,65)

          end if

      else

        call ErrorMessageAndStop(4101, 0, 0.0D0, 0.0D0)

      endif

END
