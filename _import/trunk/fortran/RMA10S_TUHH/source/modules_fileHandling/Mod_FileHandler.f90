module mod_fileHandler
!fileHandlingModules  
  use mod_fileType
!specific modules  
  use mod_storageElt
  
  type FileCtrl
!    type (file), pointer :: icfl => null()
    type (file), pointer :: lin => null()
    type (file), pointer :: volWlFil => null()
!    type (file), pointer :: lout => null()
!    type (file), pointer :: litr => null()
!    type (file), pointer :: imesout => null()
!    type (file), pointer :: ikalypsofm => null()
!    type (file), pointer :: ifile => null()
!    type (file), pointer :: nb => null()
!    type (file), pointer :: ibup => null()
!    type (file), pointer :: incstr => null()
!    type (file), pointer :: insfl => null()
!    type (file), pointer :: iwvfc => null()
!    type (file), pointer :: instr => null()
!    type (file), pointer :: iowgt => null()
!    type (file), pointer :: icordin => null()
!    type (file), pointer :: iocon => null()
!    type (file), pointer :: iwvin => null()
!    type (file), pointer :: intims => null()
!    type (file), pointer :: inwgt => null()
!    type (file), pointer :: imassout => null()
!    type (file), pointer :: itimfl => null()
!    type (file), pointer :: nscr => null()
!    type (file), pointer :: nd1 => null()
!meaning of the variables      
!------------------------      
!           TODO: examine, how ICFL works      
!icfl:      unit number definition of the console      
!LIN        unit of control file (control.r10)      
!LOUT       after 1st call of file.sub unit of output file (.ech)      
!           after 2nd call of file.sub LOUT becomes the general output unit (.out-file)      
!LITR       unit of iteration file (.itr)      
!IMESOUT    unit of Message file (MESS.ech)      
!IKALYPSOFM unit number of the results ouput files      
!IFILE      model geometry (in *.2d-format)      
!NB         restart input file (binary or ASCII)      
!ibup       external boundary condition file      
!INCSTR     control structure data      
!insfl      input control stage flow relationship file      
!IWVFC      input surface stress data file      
!INSTR      surface traction from external grid      
!IOWGT      Mesh weighting output      
!ICORDIN    external grid data      
!IOCON      continuity line hydrograph file      
!IWVIN      input wave data file      
!INTIMS     on/off controlling of constrol structure time series      
!INWGT      external inpolation weighting data file      
!IMASSOUT   ???      
!ITIMFL     processing time data      
!           TODO: Make one unit number from nscr and nd1      
!nscr       scratch file unit for solver purposes      
!nd1        copy of scratch file unit nscr      
!IRPOFIN   INPUT PROFILE DATA FILE FOR SIMULATION OF BANK EVOLUTION       

  end type

  
  CONTAINS

!open file (old style)
!---------
subroutine fileOpen (unitNo, fileName, statusString, formString, localFileName, globalErrorStatus)
  implicit none
!passed input variables  
  integer (kind = 4), intent (in)  :: unitNo
  character (len = *), intent (in) :: fileName
  character (len = *), intent (in) :: statusString, formString
!passed output variables  
  integer (kind = 4), intent (out) :: globalErrorStatus
  character (len = *), intent (out) :: localFileName
!local variables  
  integer (kind = 4) :: ioStatus = 0
!meaning of the variables  
!------------------------
!unitNo        : unit number during runtim for the file that shall be openend
!fileName      : name of the file that shall be opened
!statusString  : describes the status, which is used during opening statement
!formString    : describes the form(at) of the file that shall be opened
!globalErrorStatus : Becomes 0 or 1
!                0 means no problems with any file
!                1 means problem with any arbitrary file during opening process
!localFileName : copy of the input file name
!ioStatus      : gives back the system error ID, if any problems occur during opening process

!open the file  
  OPEN (UNIT = unitNo, FILE = fileName, STATUS = statusString, FORM = formString, IOSTAT = ioStatus)
!check for status and - on demand - write problem message and get global error status  
  IF(ioStatus /= 0) CALL iosmsg (iostatus, trim (fileName), globalErrorStatus)
!copy the file name  
  localFileName = fileName
end subroutine
  
!reading the volume waterlevel relation file  
!-------------------------------------------  
  subroutine readVolumeWaterlevelFile (volWlFile, storageElt)
    implicit none
!input parameters    
    type (file), pointer :: volWlFile
    type (StorageElement), pointer :: storageElt(:)
  
!local variables    
    type (StorageElement), pointer :: storElt => null()
    integer (kind = 4) :: iostatus = 0
    integer (kind = 4) :: storEltID    
    character (len = 1000) :: linestring
    real (kind = 8) :: volume, waterlevel
    
    
    readFile: do 
      iostatus = 0
      read (volWlFile.unit, '(a)', iostat = iostatus) linestring

!ENDDATA line      
      if (linestring (1:7) == 'ENDDATA') then
        exit readFile

!TIT line      
      elseif (linestring (1:3) == 'TIT') then
        cycle readFile

!POL line      
      elseif (linestring (1:3) == 'POL') then
        storEltID = 0
        read (linestring(4:), *) storEltID
        storElt => storageElt (storEltID)
        call addVolWaterlevelRel (storElt)

!DATA line      
      else
        if ( .NOT. (associated (storElt))) then
          write (*,*) 'data without reference storage element'
        elseif ( .NOT. (associated (storElt.volWlRel))) then
          write (*,*) 'data without reference function relation in storage element'
        else
          read (linestring, *, iostat = iostatus) volume, waterlevel
          if (iostatus /= 0) then
            write(*,*) 'problematic line in volwlfil'
            cycle readFile
          endif
          call addValuePair (storElt, volume, waterlevel)
        endif
      endif
    enddo readFile

  end subroutine
end module
