module mod_fileType

  type file
    integer (kind = 4)  :: unit = 0
    character (len = 96) :: name = ''
    logical :: IsOpen = .false.
    character (len = 20) :: status = 'NEW'
  end type

contains

!--------------------------------------------------------------------------
!function: newFile
!--------------------------------------------------------------------------

  function newFile (unitNo, fileName, fileStatus)
    implicit none
  
!function name    
    type (file), pointer :: newFile
!arguments    
    integer (kind = 4), optional :: unitNo
    character (len = *), optional :: fileName
    character (len = *), optional :: fileStatus
!file    
    type (file), pointer :: new
    
!create the file    
    allocate (new) 
    if (present (unitNo)) new.unit = unitNo
    if (present (fileName)) new.name = fileName
    if (present (fileStatus)) new.status = fileStatus
    
    newFile => new
    return
  end function

!--------------------------------------------------------------------------
!subroutine: openFileObject
!--------------------------------------------------------------------------
  subroutine openFileObject (file2Open, searchUnit)
    implicit none
!input arguments    
    type (file), pointer :: File2Open
    logical, intent (in), optional :: searchUnit
!local variables    
    integer (kind = 4) :: ioerror = 0
    
    if (present (searchUnit)) then
      if (searchUnit) File2Open.unit = findFreeUnit()
    endif
    
!open the file    
    OPEN (unit = File2Open.unit, file = File2Open.name, IOSTAT = ioerror, status = File2Open.status)
!check for errors    
    if (ioerror /= 0) then
      call ErrorMessageAndStop (1013, 0, 0.0d0, 0.0d0)
    else
      file2Open.IsOpen = .true.
    endif
  end subroutine
  
!--------------------------------------------------------------------------
!subroutine: closeFileObject
!--------------------------------------------------------------------------
  subroutine closeFileObject (file2Close)
    implicit none
!arguments    
    type (file), pointer :: file2Close
    
    close (file2Close.unit)
    file2Close.isOpen = .false.
  
  end subroutine
  
!--------------------------------------------------------------------------
!function: fileExists
!--------------------------------------------------------------------------
  function fileExists (inputFile)
    implicit none
!function name    
    logical :: fileExists
!arguments    
    type (file), pointer :: inputFile
!local variables    
    logical :: ex
    
    inquire (file=inputFile.name, exist=ex)
    fileExists = ex
    
    return
  end function
  
!--------------------------------------------------------------------------
!function: isFileUnitOpen
!--------------------------------------------------------------------------
function isFileUnitOpen (fileUnit) result (fileIsOpened)
    implicit none
!function name    
    logical :: fileIsOpened
!arguments    
    integer :: fileUnit

    inquire (unit = fileUnit, opened = fileIsOpened)
    return
  end function
 
!--------------------------------------------------------------------------
!subroutine: findFreeUnit
!--------------------------------------------------------------------------
  function findFreeUnit (minUnit, maxUnit) result (fileUnit)
    implicit none
!function definition    
    integer (kind = 4) :: fileUnit
!arguments    
    integer (kind = 4), optional :: minUnit, maxUnit
!local variables    
    logical :: unitIsOpen
    integer (kind = 4) :: unitNo
    integer (kind = 4) :: minUnitTmp = 1
    integer (kind = 4) :: maxUnitTmp = 10000
    
    unitIsOpen = .false.
    
    if (present (minUnit)) minUnitTmp = minUnit
    if (present (maxUnit)) maxUnitTmp = maxUnitTmp
    
    if (minUnitTmp <= maxUnitTmp) maxUnitTmp = minUnitTmp + 10
    
    returnFreeUnit: do unitNo = minUnitTmp, maxUnitTmp
    
      unitIsOpen = isFileUnitOpen (unitNo)
      
      if (unitIsOpen .AND. unitNo == maxUnitTmp) then
        write(*,*) 'No free unit available!'
        fileUnit = -1
        exit returnFreeUnit
      elseif ( .NOT. (unitIsOpen)) then
        fileUnit = unitNo
        exit returnFreeUnit
      endif      
    enddo returnFreeUnit
    
    return
  end function
    
end module
 