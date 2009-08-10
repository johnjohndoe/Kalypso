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
!          REVISION 1.1.0
!          January 09th, 2009
!
!          initial revision with the following capabilites
!           - shore line friction support for Darcy-Weisbach in coef25 and coef25nt; not yet for coef2d*
!           - Reactivation of the efpor scaling; problems occur, if elements are too steep perpendicular to the flow direction
!           - bugfixes and code modernizations
!           - Changes for input lines C5 and C6 regarding output frequencies
!
!          --- Nico Schrage
!......... Main program unit for the 1D/2D coupled hydrodynamics model RMA∑Kalypso

PROGRAM RMAKalypso

  
  implicit none
  
  integer :: i
  character (len = 11) :: fnam0
  character (len = 12) :: version
  character (len = 10) :: builddate
  character (len = 1000) :: defaultModelName = 'defaultModelID'
      
  
  version = '1.1.2 beta'
  builddate = '2009/02/13'

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
  WRITE(*,*) '                    February, 13th 2009'
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

  !open the input files
  call filehandling (1, fnam0)
  !read size of the model geometry
  call getgeo1
  !start main execution of the simulation model
  call rma_kalypso (defaultModelName)

!end execution of RMA∑Kalypso
END
