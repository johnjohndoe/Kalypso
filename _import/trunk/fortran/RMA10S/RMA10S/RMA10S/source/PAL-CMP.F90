!!**********************************************************************************

! Example of calling the Win32 API routine GetOpenFileName
! This can be used from any application type, including Console
!
! GetSaveFileName is very similar.
!
subroutine fileopen(fnam0)
use comdlg32
use user32 ! Interface for GetForegroundWindow
use dflib  ! In case QuickWin is used
implicit none

! Declare structure used to pass and receive attributes
!
type(T_OPENFILENAME) ofn

! Declare filter specification.  This is a concatenation of
! pairs of null-terminated strings.  The first string in each pair
! is the file type name, the second is a semicolon-separated list
! of file types for the given name.  The list ends with a trailing
! null-terminated empty string.
!
character*(*),parameter :: filter_spec = &
  "RMA-10 Files"C//"*.r10"C// &
  "All Files"C//"*.*;*.f"C//""C

! Declare string variable to return the file specification.
! Initialize with an initial filespec, if any - null string
! otherwise
!
character*96  :: fnam0
character*512 :: file_spec = ""C
integer status,ilen
ofn%lStructSize = SIZEOF(ofn)
ofn%hwndOwner = GetForegroundWindow()
ofn%hInstance = NULL  ! For Win32 applications, you
                      ! can set this to the appropriate
                      ! hInstance
                      !
ofn%lpstrFilter = loc(filter_spec)
ofn%lpstrCustomFilter = NULL
ofn%nMaxCustFilter = 0
ofn%nFilterIndex = 1 ! Specifies initial filter value
ofn%lpstrFile = loc(file_spec)
ofn%nMaxFile = sizeof(file_spec)
ofn%nMaxFileTitle = 0
ofn%lpstrInitialDir = NULL  ! Use Windows default directory
ofn%lpstrTitle = loc(""C)
ofn%Flags = OFN_PATHMUSTEXIST
ofn%lpstrDefExt = loc("txt"C)
ofn%lpfnHook = NULL
ofn%lpTemplateName = NULL

! Call GetOpenFileName and check status
! 
status = GetOpenFileName(ofn)
if (status .eq. 0) then
  type *,'No file name specified'
else
  ! Get length of file_spec by looking for trailing NUL
  ilen = INDEX(file_spec,CHAR(0))
  type *,'Filespec is ',file_spec(1:ilen-1)
  ! Example of how to see if user said "Read Only"
  !  
  if (IAND(ofn%flags,OFN_READONLY) /= 0) &
    type *,'Readonly was requested'
end if
fnam0=file_spec(1:96)
return
end 
