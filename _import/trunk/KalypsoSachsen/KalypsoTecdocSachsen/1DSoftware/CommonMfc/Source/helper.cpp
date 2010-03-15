// Helper.cpp: Implementierung der Klasse CHelper.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "Helper.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

/* static */
void CHelper::AppendMenu( HINSTANCE hInstance, HMENU hMainMenu, const int menuID )
{
  HMENU hMenu = ::LoadMenu ( hInstance, MAKEINTRESOURCE( menuID ) );
  if( hMenu )
  {
    HMENU hSubMenu = GetSubMenu( hMenu, 0 );

    TCHAR buffer[1024];
    GetMenuString( hMenu, 0, buffer, 1024, MF_BYPOSITION );

    BOOL bReturn = ::AppendMenu( hMainMenu, MF_POPUP, (UINT)hSubMenu, buffer );
  }
};

/* static */ BOOL CHelper::DeleteFileToDustbin( const CString& filePath )
// verschiebt die angegebene Datei oder Verzeichnis in den Papierkorb
// Rückgabewert:
//            FALSE, falls nicht gelöscht werden konnte
{
  // die angegebene Datei löschen
  LPSTR files = (LPSTR)malloc( filePath.GetLength() + 2 );
  strcpy( files, (LPCSTR)filePath );
  files[ filePath.GetLength() + 1 ] = '\0'; // mit einer doppel 0 abschliessen
  
  SHFILEOPSTRUCT foStruct;
  foStruct.hwnd = NULL;
  foStruct.wFunc = FO_DELETE;
  foStruct.pFrom = files;
  foStruct.pTo = NULL;
  foStruct.fFlags = FOF_ALLOWUNDO | FOF_NOCONFIRMATION;
  foStruct.lpszProgressTitle = NULL;
  
  int bResult = SHFileOperation( &foStruct );
  DWORD error = GetLastError();
  
  free( files ); 
  
  return bResult;
} // DeleteFileToDustbin


void* CVoidStorer::m_data = 0;