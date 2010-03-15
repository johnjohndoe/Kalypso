// WSPMap.h : Haupt-Header-Datei für die Anwendung WSPMAP
//

#if !defined(AFX_WSPMAP_H__27A882C3_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
#define AFX_WSPMAP_H__27A882C3_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // Hauptsymbole

class CMapProject;
class CFeatureInfo;
class CContextHelp;
class CVersion;


/////////////////////////////////////////////////////////////////////////////
// CWSPMapApp:
// Siehe WSPMap.cpp für die Implementierung dieser Klasse
//

#define APP_SETTINGS_SECTION   TEXT( "Settings" )
#define APP_SETTINGS_MAX       TEXT( "Maximized" )
#define APP_SETTINGS_RECT      TEXT( "FrameRect" )
#define APP_SETTINGS_OVERVIEW  TEXT( "ShowOverviewBar" )


class CWSPMapApp : public CWinApp
{
public:
	CWSPMapApp();
	
	// Überladungen
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
	//{{AFX_VIRTUAL(CWSPMapApp)
public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	virtual BOOL OnIdle(LONG lCount);
	//}}AFX_VIRTUAL
	

protected:
	CMapProject* m_pMapProject; // zeiger auf das (einzige) MapProject
	CMultiDocTemplate* m_pMapDocTemplate;  // zeiger auf das Template der MapDocs
	CString m_licence1;
	CString m_licence2;
	CString m_version;
	CString m_copyright;
	CString m_appDir; // Verzeichniss, in welchem die Applikation liegt
	
public:
	BOOL OpenProject( const CString& projectPath );
	BOOL CloseProject();
	
	CMapProject* GetMapProject() { return m_pMapProject; };
	CMultiDocTemplate* GetMapDocTemplate() { return m_pMapDocTemplate; };
	void DoProjectManager( CWnd* parentWnd );
	CString GetAppDir() const { return m_appDir; };
	CString GetNutzklassDir() const;

  CString GetVersionString() const;
  CVersion* GetVersion();
  CString GetVersionDate() const;
	
private:
	void CWSPMapApp::SetDefaultIniEntries();

protected:
	void InitFeatureInfo();

  CContextHelp* m_pContextHelp;
  CVersion* m_pVersion;
	
	//{{AFX_MSG(CWSPMapApp)
	afx_msg void OnAppAbout();
	//}}AFX_MSG
  afx_msg void WinHelp( DWORD dwData, UINT nCmd );

	DECLARE_MESSAGE_MAP()
};

// globale definitionen
extern CWSPMapApp theApp;
extern HPALETTE _AfxInitPaletteEx(VOID);

enum MapUnitConstants
{
	muDecimalDegrees = 0,
		muFeet = 1,
		muMeters = 2,
};

enum ScaleUnitConstants
{
	suMiles = 0,
		suFeet = 1,
		suMeters = 2,
		suKM = 3,
};

enum ScreenUnitConstants
{
	suInches = 0,
		suCentimeters = 1,
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // !defined(AFX_WSPMAP_H__27A882C3_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)



