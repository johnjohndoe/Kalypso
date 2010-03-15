// WspDlg.cpp : Legt die Initialisierungsroutinen für die DLL fest.
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include <cderr.h>
#include "stdafx.h"

#include "resource.h"

#include "bce/include/wspfeatures.h"

#include "progdlg.h"

#include "WspDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

int CWspDlgApp::m_nProgressCancelMsg = ::RegisterWindowMessage(_T("WspDlgProgressCancelMessage"));
int CWspDlgApp:: nUeberfallbeiwertUpdateMsg = ::RegisterWindowMessage(_T("WspDlgUeberfallbeiwertUpdateMsg"));
int CWspDlgApp:: nRauheitswertUpdateMsg = ::RegisterWindowMessage(_T("WspDlgRauheitswertUpdateMsg"));
int CWspDlgApp:: nBewuchswertUpdateMsg = ::RegisterWindowMessage(_T("WspDlgBewuchswertUpdateMsg"));

/////////////////////////////////////////////////////////////////////////////
// CWspDlgApp

BEGIN_MESSAGE_MAP(CWspDlgApp, CWinApp)
	//{{AFX_MSG_MAP(CWspDlgApp)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWspDlgApp Konstruktion

CWspDlgApp::CWspDlgApp() : CWinApp(_T("WSPWIN"))
{
	pDlg = NULL;
	datDlg = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Das einzige CWspDlgApp-Objekt

CWspDlgApp theApp;

BOOL CWspDlgApp::InitInstance() 
{
	char szDefaultLanguage[64];
	LANGID langidDefault;

	// Obtain the locale from the system and set the current locale to the
	// obtained locale.
	langidDefault = GetSystemDefaultLangID();
	GetLocaleInfo(langidDefault, LOCALE_SENGLANGUAGE, szDefaultLanguage, 64);
	setlocale(LC_ALL, szDefaultLanguage);
  // ausser für Numerische Werte, da soll stets der Punkt das Dezimaltrennzeichen bleiben!
  setlocale( LC_NUMERIC, "English" );



  // Pfad zum Programmverzeichnis holen
	CString path = m_pszHelpFilePath;
	int i = path.ReverseFind('\\');
	path = path.Left(i);

	/******* Change the name of the .HLP file. **********/
	path += "\\WSPWIN.HLP";
	//First free the string allocated by MFC at CWinApp startup.
	//The string is allocated before InitInstance is called.
	free((void*)m_pszHelpFilePath);
	//The CWinApp destructor will free the memory.
	m_pszHelpFilePath=_tcsdup(path);

	/******* Change the name of the .INI file. **********/
	path = "WSPWIN.INI";
	free((void*)m_pszProfileName);
	//The CWinApp destructor will free the memory.
	m_pszProfileName=_tcsdup(path);

	return CWinApp::InitInstance();
}

int CWspDlgApp::ExitInstance() 
{
	delete pDlg;

	return CWinApp::ExitInstance();
}
