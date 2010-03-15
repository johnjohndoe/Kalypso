// WSPMap.cpp : Legt das Klassenverhalten für die Anwendung fest.
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "resource.h"

#include "bce\include\WSPFeatures.h"
#include "bce\include\wspwin_regentries.h"


#include "commonMfc\include\version.h"
#include "commonMfc\include\contextHelp.h"
#include "commonMfc/include/mfcHelper.h"

#include "splash.h"
#include "childfrm.h"
#include "mainfrm.h"
#include "mapview.h"
#include "mapproj.h"
#include "MapperHelpMap.h"

#include "wspmap.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

VARIANT nullVariant = { NULL };

HPALETTE _AfxInitPaletteEx(VOID)
{
	
	NPLOGPALETTE pPal; //Local Palette
	HPALETTE pal;
	
	
	//Allocates space for QcQp Palette 'pPal'
	VERIFY( (UINT) (pPal = (NPLOGPALETTE)::LocalAlloc (LPTR,
		(sizeof (LOGPALETTE) +
		(sizeof (PALETTEENTRY) * (NCOLORS))))));
	
	//Initialize 'pPal' fields
	pPal->palNumEntries = NCOLORS;
	pPal->palVersion = 0x300;
	
	//Inits Every color of our palette
	pPal->palPalEntry[BLACK].peRed        =   0;
	pPal->palPalEntry[BLACK].peGreen      =   0;
	pPal->palPalEntry[BLACK].peBlue       =   0;
	pPal->palPalEntry[BLACK].peFlags      =   (BYTE) 0;
	
	pPal->palPalEntry[WHITE].peRed        =   255;
	pPal->palPalEntry[WHITE].peGreen      =   255;
	pPal->palPalEntry[WHITE].peBlue       =   255;
	pPal->palPalEntry[WHITE].peFlags      =   (BYTE) 0;
	
	pPal->palPalEntry[RED].peRed          =   255;
	pPal->palPalEntry[RED].peGreen        =   0;
	pPal->palPalEntry[RED].peBlue         =   0;
	pPal->palPalEntry[RED].peFlags        =   (BYTE) 0;
	
	pPal->palPalEntry[GREEN].peRed        =   0;
	pPal->palPalEntry[GREEN].peGreen      =   255;
	pPal->palPalEntry[GREEN].peBlue       =   0;
	pPal->palPalEntry[GREEN].peFlags      =   (BYTE) 0;
	
	pPal->palPalEntry[BLUE].peRed         =   0;
	pPal->palPalEntry[BLUE].peGreen       =   0;
	pPal->palPalEntry[BLUE].peBlue        =   255;
	pPal->palPalEntry[BLUE].peFlags       =   (BYTE) 0;
	
	pPal->palPalEntry[YELLOW].peRed       =   255;
	pPal->palPalEntry[YELLOW].peGreen     =   255;
	pPal->palPalEntry[YELLOW].peBlue      =   0;
	pPal->palPalEntry[YELLOW].peFlags     =   (BYTE) 0;
	
	pPal->palPalEntry[MAGENTA].peRed      =   255;
	pPal->palPalEntry[MAGENTA].peGreen    =   0;
	pPal->palPalEntry[MAGENTA].peBlue     =   255;
	pPal->palPalEntry[MAGENTA].peFlags    =   (BYTE) 0;
	
	pPal->palPalEntry[CYAN].peRed         =   0;
	pPal->palPalEntry[CYAN].peGreen       =   255;
	pPal->palPalEntry[CYAN].peBlue        =   255;
	pPal->palPalEntry[CYAN].peFlags       =   (BYTE) 0;
	
	pPal->palPalEntry[GRAY].peRed         =   128;
	pPal->palPalEntry[GRAY].peGreen       =   128;
	pPal->palPalEntry[GRAY].peBlue        =   128;
	pPal->palPalEntry[GRAY].peFlags       =   (BYTE) 0;
	
	pPal->palPalEntry[LIGHTGRAY].peRed    =   192;
	pPal->palPalEntry[LIGHTGRAY].peGreen  =   192;
	pPal->palPalEntry[LIGHTGRAY].peBlue   =   192;
	pPal->palPalEntry[LIGHTGRAY].peFlags  =   (BYTE) 0;
	
	pPal->palPalEntry[DARKRED].peRed      =   128;
	pPal->palPalEntry[DARKRED].peGreen    =   0;
	pPal->palPalEntry[DARKRED].peBlue     =   0;
	pPal->palPalEntry[DARKRED].peFlags    =   (BYTE) 0;
	
	pPal->palPalEntry[DARKGREEN].peRed    =   0;
	pPal->palPalEntry[DARKGREEN].peGreen  =   128;
	pPal->palPalEntry[DARKGREEN].peBlue   =   0;
	pPal->palPalEntry[DARKGREEN].peFlags  =   (BYTE) 0;
	
	pPal->palPalEntry[DARKBLUE].peRed     =   0;
	pPal->palPalEntry[DARKBLUE].peGreen   =   0;
	pPal->palPalEntry[DARKBLUE].peBlue    =   128;
	pPal->palPalEntry[DARKBLUE].peFlags   =   (BYTE) 0;
	
	pPal->palPalEntry[LIGHTBROWN].peRed   =   128;
	pPal->palPalEntry[LIGHTBROWN].peGreen =   128;
	pPal->palPalEntry[LIGHTBROWN].peBlue  =   0;
	pPal->palPalEntry[LIGHTBROWN].peFlags =   (BYTE) 0;
	
	pPal->palPalEntry[DARKMAGENTA].peRed  =   128;
	pPal->palPalEntry[DARKMAGENTA].peGreen=   0;
	pPal->palPalEntry[DARKMAGENTA].peBlue =   128;
	pPal->palPalEntry[DARKMAGENTA].peFlags=   (BYTE) 0;
	
	pPal->palPalEntry[DARKCYAN].peRed     =   0;
	pPal->palPalEntry[DARKCYAN].peGreen   =   128;
	pPal->palPalEntry[DARKCYAN].peBlue    =   128;
	pPal->palPalEntry[DARKCYAN].peFlags   =   (BYTE) 0;
	
	//Creates the logical palette
	pal = ::CreatePalette((LPLOGPALETTE)pPal);
	VERIFY(pal);
	
	//Free allocated memory
	VERIFY(::LocalFree((HLOCAL)pPal) == NULL);
	
	return pal;
}



/////////////////////////////////////////////////////////////////////////////
// CAboutDlg-Dialogfeld für Anwendungsbefehl "Info"

class CAboutDlg : public CDialog
{
public:
	CAboutDlg( CString version, CString copyright, CString licence1, CString licence2 );
	
	// Dialogfelddaten
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	CString	m_licence1;
	CString	m_licence2;
	CString	m_version;
	CString	m_copyright;
	//}}AFX_DATA
	
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
	//{{AFX_VIRTUAL(CAboutDlg)
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL
	
	// Implementierung
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg( CString version, CString copyright, CString licence1, 
					 CString licence2 ) : CDialog( CAboutDlg::IDD )
{
	//{{AFX_DATA_INIT(CAboutDlg)
	m_licence1 = _T("");
	m_licence2 = _T("");
	m_version = _T("");
	m_copyright = _T("");
	//}}AFX_DATA_INIT

	m_version = version;
	m_copyright = copyright;

  m_licence1 = licence1;
  m_licence2 = licence2;
} // Konstruktor

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	DDX_Text(pDX, IDC_STATIC3, m_version);
	DDX_Text(pDX, IDC_ABOUT_COPYRIGHT, m_copyright);
	//}}AFX_DATA_MAP

  DDX_Text(pDX, IDC_STATIC1, m_licence1);
  DDX_Text(pDX, IDC_STATIC2, m_licence2);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
//{{AFX_MSG_MAP(CAboutDlg)
// Keine Nachrichten-Handler
//}}AFX_MSG_MAP
END_MESSAGE_MAP()


BOOL CAboutDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	CString strFreeDiskSpace;
	CString strFreeMemory;
	CString strFmt;
	
	// Verfügbaren Speicher abrufen
	MEMORYSTATUS MemStat;
	MemStat.dwLength = sizeof(MEMORYSTATUS);
	GlobalMemoryStatus(&MemStat);
	strFmt.LoadString(IDS_PHYSICAL_MEM);
	strFreeMemory.Format(strFmt, MemStat.dwTotalPhys / 1024L);
	
	SetDlgItemText(IDC_PHYSICAL_MEM, strFreeMemory);
	
	// Informationen über Festplattenspeicher abrufen
	struct _diskfree_t diskfree;
	int nDrive = _getdrive(); // aktuelles Standardlaufwerk verwenden
	if (_getdiskfree(nDrive, &diskfree) == 0)
	{
		strFmt.LoadString(IDS_DISK_SPACE);
		strFreeDiskSpace.Format(strFmt,
			(DWORD)diskfree.avail_clusters *
			(DWORD)diskfree.sectors_per_cluster *
			(DWORD)diskfree.bytes_per_sector / (DWORD)1024L,
			nDrive-1 + _T('A'));
	}
	else
		strFreeDiskSpace.LoadString(IDS_DISK_SPACE_UNAVAIL);
	
	SetDlgItemText(IDC_DISK_SPACE, strFreeDiskSpace);
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}


/////////////////////////////////////////////////////////////////////////////
// CWSPMapApp

/////////////////////////////////////////////////////////////////////////////
// Das einzige CWSPMapApp-Objekt

CWSPMapApp theApp;


/////////////////////////////////////////////////////////////////////////////
// CWSPMapApp Konstruktion

CWSPMapApp::CWSPMapApp()
{
	m_pMapProject = NULL;
	m_pContextHelp = NULL;
  m_pVersion = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CWSPMapApp Befehlbehandlungsroutinen

BEGIN_MESSAGE_MAP(CWSPMapApp, CWinApp)
//{{AFX_MSG_MAP(CWSPMapApp)
ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CWSPMapApp::OnAppAbout()
{
	CAboutDlg aboutDlg( m_version, m_copyright, m_licence1, m_licence2 );
	aboutDlg.DoModal();
}

BOOL CWSPMapApp::OnIdle(LONG lCount) 
{
	if ( CWinApp::OnIdle( lCount ) )
		return TRUE; // solange CWinApp noch was zu tun hat, selbst nix tun

	return FALSE; // auf nächstes Commando warten
}; // OnIdle


BOOL CWSPMapApp::OpenProject( const CString& projektPath )
// Öffnet ein CMapProject
{
	CWaitCursor wait;
	
	// aktuelles Projekt schliessen
	CloseProject();
	
	// neues Projekt öffnen
	Project* pProject = new Project( projektPath );
	if( !pProject->Load() )
	{
		delete pProject;
		return FALSE;
	};
	
  // es kann sein, dass das Projekt inzwischen in einem anderen Pfad liegt
  // die .wpp soll aber dennoch gefunden werden
  // wir nehmen deshalb einfach die erste im Map-Verzeichnis, d.h. es darf immer nur eine geben
  CString mapDir = pProject->GetMapDir();
  CString projDir = pProject->GetDir();

  CString wppPath;
  CFileFind finder;
  if( finder.FindFile( mapDir + "*.wpp" ) )
  {
    while( TRUE )
    {
      BOOL bNotLast = finder.FindNextFile();// while 

      wppPath = finder.GetFilePath();
      if( wppPath.GetLength() > 0 || !bNotLast )
        break;
    }
  }; // if finder.Findfile

  // falls keine .wpp da ist eine neue anlegen
  if( wppPath.GetLength() == 0 )
    wppPath = mapDir + "mapper.wpp";
    
  m_pMapProject = (CMapProject*)OpenDocumentFile( wppPath );
  if( !m_pMapProject )
  {
    delete pProject;
    return FALSE;
  }
  
	m_pMapProject->SetProject( pProject );
	
	// das Projekt laden
	if ( !m_pMapProject->ReloadProject() )
	{
    delete pProject;
		delete m_pMapProject;
		m_pMapProject = NULL;
		return FALSE;
	};
	
	if ( m_pMapProject->IsModified() )
		m_pMapProject->OnSaveDocument( wppPath ); // neue Projekte gleich speichern ohne zu fragen -> OnFileSave und OnFileclose funktionieren wie erwartet
	
	m_pMapProject->SetOverviewBar( ((CMainFrame*)m_pMainWnd)->GetOverviewBar() );
	m_pMapProject->UpdateOverview();
	m_pMapProject->SetTitle( projektPath );
	
	((CMainFrame*)m_pMainWnd)->OnUpdateFrameTitle( TRUE );
	
	return TRUE;
};

BOOL CWSPMapApp::CloseProject() 
// schliesst das aktuelle Projekt
// Rückgabewert:
//        FALSE, falls eines der dokumente nicht geschlossen werden wollte
{
	if ( m_pMapProject )
	{
		if ( !m_pMapProject->CloseMaps() ) // Karten schliessen und speichern, falls erforderlich
			return FALSE;
		
		m_pMapProject->DoFileSave(); // Abspeichern ohne zu fragen ( Änderungen können durch nichtspeichern
		// sowieso nicht rückgängig gemacht werden
		
		delete m_pMapProject;
		m_pMapProject = NULL;

    if( m_pMainWnd )
    {
      ((CMainFrame*)m_pMainWnd)->GetOverviewBar()->UpdateOverviewMap();
		
      // set the mainframe window title
      ((CMainFrame*)m_pMainWnd)->OnUpdateFrameTitle(TRUE);
    }
	}; // if m_pMapProject
	return TRUE;
};

/////////////////////////////////////////////////////////////////////////////
// CWSPMapApp Initialisierung

BOOL CWSPMapApp::InitInstance()
{
	// die Versionsnummer aus der exe auslesen
	m_pVersion = new CVersion( m_hInstance );

	char szDefaultLanguage[64];
	LANGID langidDefault;
	
	// Obtain the locale from the system and set the current locale to the
	// obtained locale.
	langidDefault = GetSystemDefaultLangID();
	GetLocaleInfo(langidDefault, LOCALE_SENGLANGUAGE, szDefaultLanguage, 64);
	setlocale(LC_ALL, szDefaultLanguage);
	
	setlocale(LC_NUMERIC, "English"); // generell Dezimalzeichenbehandlung auf english setzen (d.h. der . ist Dezimaltrennzeichen)
	
	// Applikationsverzeichnis global zugägnlich machen
	m_appDir = BCE::MfcHelper::GetFileDirectory( m_pszHelpFilePath ); // m_pszHelpFilePath wird von Framework initialisiert
	
	
	AfxEnableControlContainer();
	
	// Ändern des Registrierungsschlüssels, unter dem unsere Einstellungen gespeichert sind.
	// Sie sollten dieser Zeichenfolge einen geeigneten Inhalt geben
	// wie z.B. den Namen Ihrer Firma oder Organisation.
	SetRegistryKey(_T("BCE"));

	SetDefaultIniEntries();

	LoadStdProfileSettings();  // Standard-INI-Dateioptionen einlesen (einschließlich MRU)
	
	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);

	InitFeatureInfo();  //Standardeinstellungen und Versionsnummer überprüfen
	
	// Versionsinformationen initialisieren
	m_licence1 = WSPFeatures::Instance()->GetDataStr("HEAD","LICENCE_TEXT1");
	m_licence2 = WSPFeatures::Instance()->GetDataStr("HEAD","LICENCE_TEXT2");
	
	m_version = CString(WSPFeatures::Instance()->GetDataStr("MAPPER","PRODUCT_NAME")) + ", Version " + GetVersionString();
  
	m_copyright = CString( "Copyright © BCE, Koblenz, "  + GetVersionDate());
	
	CSplashWnd splash( m_version, m_copyright, m_licence1, m_licence2 );
	BOOL bSplash = cmdInfo.m_bShowSplash;
	if (!cmdInfo.m_bRunEmbedded)
	{
		switch (m_nCmdShow)
		{
		case SW_HIDE:
		case SW_SHOWMINIMIZED:
		case SW_MINIMIZE:
		case SW_SHOWMINNOACTIVE:
			break;
			
		case SW_RESTORE:
		case SW_SHOW:
		case SW_SHOWDEFAULT:
		case SW_SHOWNA:
		case SW_SHOWNOACTIVATE:
		case SW_SHOWNORMAL:
		case SW_SHOWMAXIMIZED:
      {
        if( GetProfileInt( APP_SETTINGS_SECTION, APP_SETTINGS_MAX, TRUE ) )
          m_nCmdShow = SW_SHOWMAXIMIZED;
      }
			break;
		}
	}
	else
	{
		//Excel 4 will start OLE servers minimized
		m_nCmdShow = SW_SHOWNORMAL;
	}
	clock_t goal, wait = (clock_t)2*CLOCKS_PER_SEC;
	if (bSplash)
	{
		// only show splash if not embedded
		splash.Create(NULL);
		splash.ShowWindow(SW_SHOW);
		splash.UpdateWindow();
		goal = wait + clock();
	}
	
	// Dokumentvorlagen der Anwendung registrieren. Dokumentvorlagen
	//  dienen als Verbindung zwischen Dokumenten, Rahmenfenstern und Ansichten.
	
	m_pMapDocTemplate = 
		new CMultiDocTemplate( IDR_MAPTYPE,
		RUNTIME_CLASS(CMapDoc),
		RUNTIME_CLASS(CChildFrame), // Benutzerspezifischer MDI-Child-Rahmen
		RUNTIME_CLASS(CMapView) );
	AddDocTemplate( m_pMapDocTemplate );
	
	CProjectDocTemplate* pMapProjectTemplate = 
		new CProjectDocTemplate(IDR_MAPPROJECTTYPE,
		RUNTIME_CLASS(CMapProject),
		NULL, // Benutzerspezifischer MDI-Child-Rahmen
		NULL);
	
	AddDocTemplate(pMapProjectTemplate);
	
	// destroy splash window
	if (bSplash)
	{
		// wait for 2 seconds
		while( goal > clock() )
			;
		splash.DestroyWindow();
	}
	
	// Haupt-MDI-Rahmenfenster erzeugen
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	pMainFrame->SetTitle(WSPFeatures::Instance()->GetDataStr("MAPPER","PRODUCT_NAME"));
	pMainFrame->OnUpdateFrameTitle( FALSE );
	m_pMainWnd = pMainFrame;
	
	
	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileNew)
		cmdInfo.m_nShellCommand = CCommandLineInfo::FileNothing;
	
	// Verteilung der in der Befehlszeile angegebenen Befehle
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
	
	// Das Hauptfenster ist initialisiert und kann jetzt angezeigt und aktualisiert werden.
	pMainFrame->ShowWindow(m_nCmdShow);
	pMainFrame->UpdateWindow();
	
	// die ContextHilfe initialisieren
  CMapUIntToString idMap;
  CMapperHelpMap::FillHelpMap( idMap );
  CString helpFileName = m_appDir + TEXT( "\\WspMap.pdf" );
  m_pContextHelp = new CContextHelp();
  m_pContextHelp->Init( pMainFrame, helpFileName, idMap );
	
	// stets als erstes den Projektmanager öffnen
	GetMainWnd()->PostMessage( WM_COMMAND, ID_FILE_MANAGER, 0 );
	
	return TRUE;
}; // InitInstance

int CWSPMapApp::ExitInstance() 
{
  delete m_pMapProject;
  m_pMapProject = NULL;
	
	delete m_pContextHelp;
  m_pContextHelp = NULL;
  
  delete m_pVersion;
  m_pVersion = NULL;

	return CWinApp::ExitInstance();
}

void CWSPMapApp::SetDefaultIniEntries()
{
	// Den Installationspfad (=Pfad dieser Exe) in die Registry schreiben
	// wird vom WspWin für den Aufruf vom Mapper benutzt
	const CString installPath = GetVersion()->GetFileName();
	if( WriteProfileString( WspWin::REG_KEY_SECTION_INFO, WspWin::REG_KEY_NAME_FILEPATH, LPCTSTR(installPath) ) )
			TRACE( "Wrote install path to registry: " + installPath );
		else
		TRACE( "Could not write install path to registry." );
}

void CWSPMapApp::InitFeatureInfo()
{	// kim
	// ersetzt die FeatureMap durch Vorgabewerte (Demoeinstellung) wenn
	// - die Versionsnummer nicht gültig ist oder
	// - die Registry nicht gelesen werden kann
	// - das Demo-Date erreicht ist

	CString exe_ver = GetVersionString();

	WSPFeatures* features = WSPFeatures::Instance();

	if( WSPFeatures::Instance()->RegLoaded())
	{
		if( features->isEnabled( "MAPPER", "map_nodemo" ) )
		{
			if (!features->CheckDemoDate(features->GetDataStr("MAPPER","DEMO_DATE")))
			{
				AfxMessageBox("Die aktuelle Lizensierung ist abgelaufen.\nDas Programm wird im DEMO-Modus gestartet.");
				features->SetDataStr("MAPPER","DEMO_DATE","01.01.2222");//Damit diese Meldung nur einmal angezeigt wird
				features->DisableFeature("MAPPER", "map_nodemo");
			}
			
			if (!features->CheckRegVer(features->GetDataStr("MAPPER","VERSIONS_NR"), exe_ver))
			{
				AfxMessageBox("Die vorhandene Lizenz ist nicht für diese Version gültig.\nDas Programm wird im DEMO-Modus gestartet.");
				features->SetDataStr("MAPPER","VERSIONS_NR",exe_ver);//Damit diese Meldung wird nur einmal angezeigt wird
				features->DisableFeature("MAPPER", "map_nodemo");
			}
		}
		else
		{
			AfxMessageBox( "Das Programm ist nicht registriert und wird im Demo-Modus gestartet." );
			return;
		}
	}
	else
	{
		AfxMessageBox( "Das Programm ist nicht registriert und wird im Demo-Modus gestartet." );
		return;
	}

	if(features->isEnabled("MAPPER","PRODUCT_NAME")) 
		return;
	
	if(features->AddFeature("MAPPER","PRODUCT_NAME"))
	{
		features->SetDataStr("MAPPER","PRODUCT_NAME","Mapper");
		features->EnableFeature("MAPPER","PRODUCT_NAME");
	}

	AfxMessageBox( "Die Produktregistrierung ist fehlgeschlagen. Das Programm wird im Demo-Modus gestartet." );
};

void CWSPMapApp::DoProjectManager( CWnd* parentWnd )
// öffnet den ProjetkManager und führt die von ihm gestellten commandos aus
// wie z.B. Karte öffnen, Karte erstellen oder Programm schliessen
{
  CloseProject(); // das Projekt immer schliessen, bevor der Projekmanagerdialog aufgemacht wird
  
  ProjectManagerDialog dlg( parentWnd );
  if ( dlg.DoModal() == IDOK )
  {
    NMPROJECTMNG* command = dlg.GetCommand();
    if ( command )
    {
      BOOL bErfolg = FALSE; // ob das commando erfolgreich ausgeführt werden konnte -> falls nein
      // den Projektmanager wieder neu aufmachen
      switch ( command->type )
      {
      case NMPROJECTMNG::closeProgram:
        m_pMainWnd->SendMessage( WM_CLOSE );
        break;
        
      default:
        if ( !CString(command->projectPath).IsEmpty() )
        {
          bErfolg = OpenProject( command->projectPath );
          if ( m_pMapProject )
            bErfolg = m_pMapProject->ParseCommand( command );
        };
      }; // switch
      
      delete command;
      command = NULL;
    }; // if command
  }; // if dlg.DoModal == IdOK
}; // DoProjectManager

void CWSPMapApp::WinHelp( DWORD dwData, UINT nCmd )
// ruft die Hilfe auf
{
  if( m_pContextHelp != NULL )
    m_pContextHelp->ShowHelp( dwData, nCmd );
} // WinHelp

CString CWSPMapApp::GetNutzklassDir() const
{
  CString path;
  
  LPTSTR buffer = NULL;
  UINT length = GetWindowsDirectory( buffer, 0 );
  if( length > 0 )
  {
    buffer = (LPTSTR)malloc( length * sizeof( TCHAR ) );
    
    UINT ret = GetWindowsDirectory( buffer, length );
    if( ret != 0 && ret < length )
      path = CString( buffer ) + "\\" + "Wsp-Nutzklassen";
    
    free( buffer ); // den Buffer immer wieder freigeben
  }; // if length > 0
  
  return path;
}; // GetNutzklassDir

CString CWSPMapApp::GetVersionString() const
{
  if( m_pVersion != NULL )
    return m_pVersion->GetProductNr();
  else
    return CString();
}

CVersion* CWSPMapApp::GetVersion()
{
	return m_pVersion;
}

CString CWSPMapApp::GetVersionDate() const
{
  if( m_pVersion != NULL )
    return m_pVersion->GetTimeString();
  else
    return CString();
}