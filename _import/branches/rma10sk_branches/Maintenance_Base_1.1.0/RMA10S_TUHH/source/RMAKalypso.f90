!*********************************************           
!   RMA∑Kalypso
!   COPYRIGHT
!
!   Hamburg University of Technology
!   Denickestraﬂe 22
!   21075 Hamburg
!   Germany
!
!   Technische Universit‰t Hamburg-Harburg
!   Denickestraﬂe 22
!   21075 Hamburg
!   Deutschland
!*********************************************           
!......... Main program unit for the 1D/2D coupled hydrodynamics model RMA∑Kalypso
!
!          The splitting of RMA∑Kalypso from its origin RMA-10S 8.0 took place in the
!          end of 2008. The capabilities of the software were adapted for the usage
!          of 1D/2D modelling in the modelling environment Kalypso, available under
!          sourceforge.com
!
!          REVISION 1.1.0
!          October 28th, 2008
!
!          initial revision with the following capabilites
!           - 2D modelling (more details necessary!)
!           - 1D modelling with descriptive polynomials for natural open channels and pipes (pressure flow)
!           - 1D/2D modelling along line transitions
!           - ... to be continued
!
!          --- Nico Schrage
!
!......... Main program unit for the 1D/2D coupled hydrodynamics model RMA∑Kalypso

PROGRAM RMAKalypso


  !program needs the memory knowledge at the beginning
  !TODO: This shouldn't be here
  USE Blk10, only: NBS

  implicit none

  integer :: i
  character (len = 11) :: fnam0
  character (len = 12) :: version
  character (len = 10) :: builddate
      
  version = '1.1.0'
  builddate = '2008/10/28'

!information during the execution:
  do i = 1, 8
    write(*,*)
  enddo
  write(*,*) '     ************************************************'
  write(*,*) '     *                                              *'
  write(*,*) '     *                  **********                  *'
  write(*,*)
  WRITE(*,*) '        You are executing version ', trim(version)
  WRITE(*,*) '                       of RMA∑Kalypso'
  write(*,*) '                    build date: ', builddate
  write(*,*)
  write(*,*) '     *                  **********                  *'
  write(*,*)
  WRITE(*,*) '                        © copyright '
  write(*,*) '              Hamburg University of Technology'
  write(*,*) '         Institute of River and Coastal Engineering'
  WRITE(*,*) '                      Denickestrasse 22'
  WRITE(*,*) '                        21073 Hamburg'
  WRITE(*,*) '                         Germany'
  write(*,*)
  WRITE(*,*) '                    October, 28th 2008'
  write(*,*)
  write(*,*) '     *                  **********                  *'
  write(*,*) '     *                                              *'
  write(*,*) '     ************************************************'



  !control file name is fixed for RMA∑KALYPSO
  fnam0 = 'control.r10'

  do i = 1, 8
    write(*,*)
  enddo
  WRITE (*,'(A)') 'The name of the control file is: ', FNAM0
  

  !nis,jun07: Moving file.sub to a point before calling initl.sub leads to an error because zvrs.sub, called from file.sub, uses some values that will be set there
  !setting those necessary values at first directly here
  NBS = 5000000
  !-

  !open the input files
  CALL FILE (1, fnam0)
  !read size of the model geometry
  CALL GETGEO1
  !start main execution of the simulation model
  CALL RMA10SUB

!end execution of RMA∑Kalypso
END
