
! Function starproc
!
! See header above for function description
!
logical function starproc (fnam0,fnam1)
use dfwin
!ipk intel  use ifwin
implicit none
character*(MAX_PATH) path_to_this_exe
character*195 anam
character*96 fnam0,fnam1
type (T_STARTUPINFO) :: StartupInfo
type (T_PROCESS_INFORMATION) :: ProcessInfo
integer ret, path_len,nl,n1,LEN1,LEN2
integer lenstr
character*1 cret
integer*2 status

starproc = .false.


WRITE(*,'(A)') FNAM0


! Get path to this exe
!
path_len = GetModuleFileName (NULL, path_to_this_exe, &
  len(path_to_this_exe))


! Initialize StartupInfo - we won't use any of its fields
!
StartupInfo = T_STARTUPINFO(SIZEOF(StartupInfo),&
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


! Create a new process to run this executable, passing "Stall -s"
! as the command line (anything that has two or more tokens) 
! and specifying that it should inherit the handles
! (including console) of this process.
!
  
  LEN1=LENSTR(FNAM0,1)
  LEN2=LENSTR(FNAM1,1)
  anam=fnam0(1:LEN1)//' '//fnam1(1:LEN2)//' <RETURNPAR'
  WRITE(*,'(A)') ANAM
!ret = CreateProcess (path_to_this_exe,  & ! Application Name
ret = CreateProcess (NULL,  & ! Application Name
!         "Stall -s"C, &  ! Command line
         anam, &  ! Command line
!         NULL, &  ! Command line
         NULL_SECURITY_ATTRIBUTES, &
         NULL_SECURITY_ATTRIBUTES, &
         TRUE, &        ! InheritHandles
         0, &       ! CreationFlags
         NULL, & ! Environment variables
         NULL_CHARACTER, & ! Current directory
         StartupInfo, &
         ProcessInfo)


if (ret == 0) then
  ret = GetLastError ()
  write (*,'(A,I0)') "Create process failed with error ",ret
else
  ! CreateProcess succeeded.  Wait for the process to finish
  !
  ret = WaitForSingleObject (ProcessInfo%hProcess, INFINITE)


  ! Close handles, otherwise resources are lost
  !
  ret = CloseHandle (ProcessInfo%hThread)
  ret = CloseHandle (ProcessInfo%hProcess)
  starproc = .true.

  end if


return

end function starproc
