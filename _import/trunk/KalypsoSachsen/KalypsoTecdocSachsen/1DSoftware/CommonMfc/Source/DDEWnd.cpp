// DDEWnd.cpp: Implementierung der Klasse IDDEWnd.
//
//////////////////////////////////////////////////////////////////////

#include "stdAfx.h"

#include "DDEWnd.h"

////////////////////////////////
// Konstruktion / Destruktion //
////////////////////////////////

IDDEWnd::IDDEWnd( CWnd* pWindow )
// Parameter:
//        CWnd* pWindow: ein Fenster, dieses muss bei aufrufen von ExecuteDDECommand
//        gültige SafeHwnds zurückgeben
{
  ASSERT( pWindow != NULL );
  m_pWindow = pWindow;
  m_bInDDEInitiate = FALSE;
  m_ddeServer = NULL;
}


/////////////////
// Operationen //
/////////////////

LRESULT IDDEWnd::DDEAcknowledge( WPARAM wParam, LPARAM lParam )
{
  HWND hwndServer = (HWND)wParam;

  if( m_bInDDEInitiate ) // Initialisierungsanfrage: das Fenster merken
    m_ddeServer = (HWND)wParam;
  
	return 0L;
}; // OnDDEAcknowledge

LRESULT IDDEWnd::DDETerminate( WPARAM wParam, LPARAM lParam )
{
	return 0L;
}; // OnDDETerminate

void IDDEWnd::ExecuteDDECommand( const CString& applicationStr, const CString& topicStr, const CString& commandStr )
{
  // nur arbeiten, falls nicht gerade ander DDE - Commandos unterwegs sind
  if( m_bInDDEInitiate || m_pWindow == NULL )
    return;

  HWND hWnd = m_pWindow->GetSafeHwnd();
  if( hWnd == NULL )
    return;

  // das DDE-Init auslösen
    
  ATOM atomApplication = GlobalAddAtom( applicationStr ); 
  ATOM atomTopic = GlobalAddAtom( topicStr ); 
 
  m_bInDDEInitiate = TRUE;
  ::SendMessage( HWND_BROADCAST, WM_DDE_INITIATE, (WPARAM)hWnd, MAKELONG(atomApplication, atomTopic ) );
  m_bInDDEInitiate = FALSE;

  // jetzt das Kommando schicken, falls ein server gefunden wurde
	if( m_ddeServer )
  {
    HGLOBAL hCommand = ::GlobalAlloc( GMEM_MOVEABLE | GMEM_DDESHARE, commandStr.GetLength() + 1 );
    if( hCommand )
    {
      LPSTR szCommand = (LPSTR)::GlobalLock( hCommand );
      if( szCommand )
      {
        lstrcpy( szCommand, (LPCSTR)commandStr );
				::GlobalUnlock( hCommand ); 

        if( !::PostMessage( m_ddeServer, WM_DDE_EXECUTE, (WPARAM)hWnd, PackDDElParam( WM_DDE_EXECUTE, 0, (UINT)hCommand) ) )
          ::GlobalFree( hCommand ); // sonst macht dies der Server
      }
      else
        ::GlobalFree( hCommand );
    }; // if hCommand

    // zuletzt die Verbindung zum Server schliessen
    ::PostMessage( m_ddeServer, WM_DDE_TERMINATE, (WPARAM)hWnd, 0 );
    m_ddeServer = NULL; // vorsichtsheitshalber auch hier noch löschen
  }; // if m_ddeServer
}; // ExecuteDDECommand