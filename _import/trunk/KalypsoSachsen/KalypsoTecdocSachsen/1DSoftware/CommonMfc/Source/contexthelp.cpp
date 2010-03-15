// ContextHelp.cpp: Implementierung der Klasse CContextHelp.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "processHelper.h"
#include "DDEWnd.h"
#include "MapUIntToString.h"

#include "ContextHelp.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CContextHelp::CContextHelp()
{
  // den Urzustand herstellen
  DeleteContents();
};

void CContextHelp::DeleteContents()
// zerst�rt alle Inhalte
{
  m_ddeWnd = NULL;
  m_helpFile = TEXT("");
  m_idMap.RemoveAll();
}

BOOL CContextHelp::Init( IDDEWnd* pDDEWnd, const CString& helpFile, const CMapUIntToString& idMap )
// Initialisiert das Objekt mit aktueller Hilfedatei und Zuordnungstabelle
// Parameter:
//        IDDEWnd* pDDEWnd: Ein Fenster, welches die Schnittstelle IDDEWnd implementiert
//        const CString& helpFile: die Hilfsdatei ( eine PDF - Datei )
//        const CMap<UINT, UINT, CString, LPCTSTR>& idMap: die Zuordnung id zu Hyperref
//        const CString& defaultRef: Default Sprungziel, wenn die ID nicht existiert
// R�ckgabewert:
//        TRUE, falls erfolgreich initialisiert werden konnte
// Bemerkung:
//      Die IdMap enth�lt zu jeder m�glichen ID ( d.h. die ID der Control bei der das Ereignis 
//      ausgel�st wurde, eine Sprungadresse. Die Sprungadresse ist ein Zielname im PDF Dokument.
//      Der 0 sollte immer ein Standardtext zugeordnet sein.
{
  // erstmal alles l�schen
  DeleteContents();

  if( pDDEWnd == NULL )
    return FALSE;
  m_ddeWnd = pDDEWnd;

  // Testen, ob die Datei �berhaupt existiert
  CFileStatus fileStatus;
  if( !CFile::GetStatus( helpFile, fileStatus ) )
    return FALSE;
  m_helpFile = helpFile;

  // die idMap kopieren
  m_idMap.Add( idMap );

  // schein alles ok zu sein
  return TRUE;
};

/////////////////
// Operationen //
/////////////////

void CContextHelp::ShowHelp( DWORD dwData, UINT nCmd )
// �ffnet den AcrobatReader mit der Hilfedatei und f�hrt die angegebene Aktion aus.
// Parameter: siehe Dokumentation von WinHelp in der Windows API
// Unterst�tzt sind zur Zeit:
// HELP_CONTENTS: zeigt das Inhaltsverzeichnis an
// HELP_CONTEXT: springt z uder in der idMap angegebenen Referenz
{
  BOOL bStartAcrobat = TRUE;
  CString targetRef;

  UINT targetID = (UINT)LOWORD(dwData);
  switch( nCmd )
  {
  case HELP_CONTENTS:
    m_idMap.Lookup( 0, targetRef ); // der erste ist default
    break;

  case HELP_CONTEXT:
    // das Target aus der Map raussuchen
    if( !m_idMap.Lookup( targetID, targetRef ) )
      m_idMap.Lookup( 0, targetRef ); // ansonsten den allersersten Eintrag w�hlen ( default )
    break;

  default:
    break;
  } // switch nCmd

  // falls gew�nscht den Acrobat Reader starten ( fast immer, ausser bei HELP_QUIT )
  if( bStartAcrobat )
    CProcessHelper::StartExternProcess( "AcroRd32.exe" );

  // jetzt die Hilfsdatei �ffnen und an die entsprechende Stelle springen
  CString appStr = TEXT("acroview");
  CString topicStr = TEXT("control");
  CString helpFile = TEXT( "\"" ) + m_helpFile + TEXT( "\"" ); // Muss in Anf�hrungszeichen stehen, da es Pfade mit Leerzeichen geben kann

  if( m_ddeWnd == NULL )
    AfxMessageBox( "Kontexthilfe wurde nicht initialisiert", MB_OK | MB_ICONSTOP ); 
  else
  {
    m_ddeWnd->ExecuteDDECommand( appStr, topicStr, TEXT("[DocOpen(") + helpFile + TEXT(")]") ); // damit die andern Doc... Befehlefunktionieren
    m_ddeWnd->ExecuteDDECommand( appStr, topicStr, TEXT("[FileOpen(") + helpFile + TEXT(")]") );
    m_ddeWnd->ExecuteDDECommand( appStr, topicStr, TEXT("[DocGoToNameDest(")  + helpFile + TEXT(",") + targetRef +")]" );
  }
}; // CallHelp