// DDEWnd.h: Schnittstelle für die Klasse IDDEWnd.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DDEWND_H__586A12E4_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
#define AFX_DDEWND_H__586A12E4_940F_11D6_B2FA_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////
// Interface zur Implementation von DDE Kommandos                  //
// Damit eine MFC-Anwendung DDE-Kommandos schicken kann,           //
// sollte ein Fenster ( meistens der MainFrame ) dieses Interface  //
// implementieren und zusätzlich die beiden Windows-Messages       //
// WM_DDE_ACK, WM_DDE_TERMINATE implementieren ( d.h. In Mess. Map //
// und Handling Funktionen  ) und direkt nach IDDEWnd::DDEAckknow. //
// etc. weiterleiten ( können leider nicht direkt benutzt werden,  //
// da es it der Klasscasterei dann Schwierigkeiten gibt )          //
//                                                                 //
/////////////////////////////////////////////////////////////////////
class IDDEWnd  
{
  ////////////////////////////////
  // Konstruktion / Destruktion //
  ////////////////////////////////

private:
  IDDEWnd(); // diesen Konstruktor verbieten, damit immer ein pWindow definiert ist

public:
  IDDEWnd( CWnd* pWindow );

///////////////
// Attribute //
///////////////

private:
	BOOL m_bInDDEInitiate;
  HWND m_ddeServer;

  CWnd* m_pWindow;


  /////////////////
  // Operationen //
  /////////////////

public:
void ExecuteDDECommand( const CString& applicationStr, const CString& topicStr, const CString& commandStr );

public:
	LRESULT DDEAcknowledge( WPARAM wParam, LPARAM lParam );
	LRESULT DDETerminate( WPARAM wParam, LPARAM lParam );
};

#endif // !defined(AFX_DDEWND_H__586A12E4_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
