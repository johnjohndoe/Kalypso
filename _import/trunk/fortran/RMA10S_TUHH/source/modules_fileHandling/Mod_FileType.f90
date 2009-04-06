module mod_fileType

  type file
    integer (kind = 4)  :: unit = 0
    character (len = 96) :: name = ''
    logical :: IsOpen = .false.
    character (len = 20) :: status = 'NEW'
  end type

contains

  function newFile (unitNo, fileName, fileStatus)
  
    !function name
    type (file), pointer :: newFile
    !arguments
    integer (kind = 4) :: unitNo
    character (len = *) :: fileName
    character (len = *), optional :: fileStatus
    !file
    type (file), pointer :: new
    
    !create the file
    allocate (new) 
    new.unit = unitNo
    new.name = fileName
    if (present (fileStatus)) new.status = fileStatus
    
    newFile => new
    return
  end function

    
end module
 