// ProcessHelper.cpp: Implementierung der Klasse CProcessHelper.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commResource.h"
#include "tlist.h"

#include "ProcessHelper.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


/////////////////
// Operationen //
/////////////////

void CProcessHelper::StartExternProcess( const CString& appName )
// startet ein externes Programm, falls es nicht schon gestartet ist
// Parameter:
//      const CString& appName: Der Name der Anwendung: die Exe wird anhand des Namens aus 
//                              der Registry unter "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\" 
//                              herausgesucht
{
  if( !IsTaskActive( appName ) )
  {
    CString path;
    
    CString regPlace = TEXT("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\") + appName;
    
    HKEY key;
    LONG error = RegOpenKeyEx( HKEY_LOCAL_MACHINE, regPlace, 0, KEY_EXECUTE, &key );
    if( error == ERROR_SUCCESS )
    {
      DWORD dw = 0;
      DWORD dwType = 0;
      error = RegQueryValueEx( key, NULL, NULL, &dwType, NULL, &dw );
      if (error == ERROR_SUCCESS)
      {
        ASSERT( dwType == REG_SZ );
        LPTSTR lpsz = path.GetBufferSetLength(dw);
        error = RegQueryValueEx( key, NULL, NULL, &dwType, (BYTE*)lpsz, &dw );
        path.ReleaseBuffer();
      }
    }; // if error == ERROR_SUCCES
    
    if( !path.IsEmpty() && error == ERROR_SUCCESS )
    {
      STARTUPINFO sui;
      PROCESS_INFORMATION pi;
      
      TCHAR buffer[1000];
      _tcscpy( buffer, appName );
      
      ::GetStartupInfo( &sui );
      sui.lpReserved = NULL;
      sui.lpTitle = buffer;
      sui.dwFlags |= STARTF_USESHOWWINDOW;
      sui.wShowWindow = SW_SHOWNORMAL;
      if( ::CreateProcess( path, NULL, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi ) )
      {
        // jetzt darauf warten, dass der Process auftaucht
        
        WaitForInputIdle(pi.hProcess, INFINITE);
        CloseHandle( pi.hProcess );
        CloseHandle( pi.hThread );
        return; // alles ok also raus
      };
    }; // if !path.IsEmpty
    
    // wenn man hier ankommt, gabs nen Fehler -> Fehlermeldung
    CString message( CString( MAKEINTRESOURCE( IDS_CANT_FIND_APP ) ) );
    message += path;
    AfxMessageBox( message, MB_OK | MB_ICONEXCLAMATION );
  }; // if !IsTaksActive
  
  return;
}; // StartExternProgram