!     Last change:  KV   15 Sep 2008    4:10 pm

!**************************************LICENSE**************************************************
!
! This code is part of the library 'Kalypso-NA'.
! KALYPSO-NA is a deterministic, non-linear, detailed Rainfall-Runoff-Model (RRM).
! The model permits a complete simulation of the land bound
! part of the global water balance as a reaction on observed precipitation events.
! Copyright (C) 2004  HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering (in co-operation with Bjoernsen Cunsulting Engineers)
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Dipl.-Ing. Jessica Hübsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Hübsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************
subroutine inp_vgp_parm(typanz,namvgp,typchar,alpha,n,l,ierr)



        USE generic_LOGGER
        USE Units
	implicit none
	include	'include\param.cmn'


        REAL:: alpha(idimnutz2),n(idimnutz2),l(idimnutz2)

	character(LEN=8):: typchar(idimnutz2)
	character(LEN=80):: namvgp
	integer	typanz,vgp,ivgp
        INTEGER :: ju0gfu,ierr,istat


        vgp=ju0gfu()

        open (vgp,iostat=ierr,err=996,status='old',file=namvgp)

        read(vgp,*,IOSTAT=istat) 
        read(vgp,*,IOSTAT=istat) 
        read(vgp,*,IOSTAT=istat) 

        ivgp=0
    	Do ivgp=1,typanz

       if(ivgp.gt.idimnutz2) then
        write(*,9006)namvgp,idimnutz2
9006	format('xxx anzahl bodentyp ueberschreitet felddimensionierung!', &
               /'xxx erhoehe parameter idimnutz in param.cmn!',/          &
               'xxx filename:           ',a,/                             &
               'xxx erlaubte typanzahl: ',i6)

        call writeLogIntInt (7,'Anzahl der max. möglichen Bodenprofile ueberschritten!',&
              & 'Amount of soil type exceeds max. number!',namvgp,'',0,'idimnutz2',idimnutz2,'inp_vgp_parm')
        call writeFooter()
        stop

        END if

        read(vgp,*,IOSTAT=istat)typchar(ivgp)(1:8),		           &
                       alpha(ivgp),n(ivgp),l(ivgp)


      if (istat /= 0) exit

        END do

    close (vgp)

        return

!***********************************************************************************************************************


996	write(*,906)namvgp

906	format( 'xxx datenfile mit bodendaten KALYPSO-NA nicht gefunden!',/   &
               'xxx filename: ',a)

        call writeLogString(7,'Datenfile nicht vorhanden!','File not found!',namvgp,'','','','','inp_vgp_parm')
        call writeFooter()
	stop





end

