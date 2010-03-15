// MainFrm.cpp : Implementierung der Klasse CMainFrame
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "resource.h"

#include "wspprj\wspprj.h"
#include "commonMFc\include\helper.h"

#include "mapproj.h"
#include "mainfrm.h"
#include "wspmap.h"
#include "nutzklass.h"

extern CWSPMapApp theApp;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

int CMainFrame::m_nUpdateProjectMsg = RegisterWindowMessage(_T("MapperUpdateProjektMsg"));
int CMainFrame::m_nUpdateProjectCompleteMsg = RegisterWindowMessage(_T("MapperUpdateProjektCompleteMsg"));

static UINT indicators[] =
{
	ID_SEPARATOR,           // Statusleistenanzeige
	ID_INDICATOR_LAYER,
	ID_INDICATOR_XCOORD,
	ID_INDICATOR_YCOORD
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame Konstruktion/Destruktion

CMainFrame::CMainFrame() : IDDEWnd( this )
{
  m_statusProgressCtrl = NULL;
}

CMainFrame::~CMainFrame()
{
  if( m_statusProgressCtrl )
    DestroyStatusBarProgress();
}; // Destruktor

IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_CLOSE()
	ON_WM_DESTROY()
	ON_COMMAND(ID_FILE_MANAGER, OnFileManager)
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
	// Globale Hilfebefehle

	ON_COMMAND(ID_HELP_FINDER, OnHelpFinder)
	ON_COMMAND(ID_HELP, OnHelp)
	ON_COMMAND(ID_CONTEXT_HELP, OnContextHelp)
	ON_COMMAND(ID_DEFAULT_HELP, OnHelpFinder)
	

  ON_COMMAND_EX(ID_VIEW_MAPBAR, OnBarCheck)
  ON_UPDATE_COMMAND_UI(ID_VIEW_MAPBAR, OnUpdateControlBarMenu)
  ON_COMMAND_EX(ID_VIEW_OVERVIEW_MAP, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_OVERVIEW_MAP, OnUpdateControlBarMenu)
	ON_MESSAGE(WM_DDE_ACK, OnDDEAcknowledge)
	ON_MESSAGE(WM_DDE_TERMINATE, OnDDETerminate)
	ON_REGISTERED_MESSAGE(m_nUpdateProjectCompleteMsg, OnUpdateProjectComplete)

  ON_COMMAND_EX( ID_EXTRAS_EDIT_NUTZUNG, OnExtras )
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CMainFrame Nachrichten-Handler

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

  CMenu* mainMenu = new CMenu();
  mainMenu->CreateMenu();

  HMENU hMainMenu = mainMenu->GetSafeHmenu();
  HINSTANCE hInstance = theApp.m_hInstance;
  
    // Ansicht
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_MAINFRAME );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_MAINMAP );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_MAINVIEW );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_EXTRAS );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_HELP );

  SetMenu( mainMenu );

  // Toolbars und CControlBars

  EnableDocking( CBRS_ALIGN_TOP );
  
  CToolBar* lastBar = NULL; // für dockControlBarLeftOf, weil nicht immer alle Toolbars da sind
  // Map-Bar
  if (!m_mapBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY ) ||
    !m_mapBar.LoadToolBar( IDR_MAPBAR ) )
  {
    TRACE0("Failed to create toolbar\n");
    return -1;      // Fehler beim Erzeugen
  }
  m_mapBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBar( &m_mapBar, AFX_IDW_DOCKBAR_TOP );
  lastBar = &m_mapBar;

  // FileBar
	if (!m_fileBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY,
                           IDR_FILEBAR ) ||
		!m_fileBar.LoadToolBar( IDR_FILEBAR ) )
	{
		TRACE0("Failed to create mapbar\n");
		return -1;      // Fehler beim Erzeugen
	}
	m_fileBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBarLeftOf( &m_fileBar, lastBar );
  lastBar = &m_fileBar;

  // Tools-Bar
  if (!m_toolsBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY,
    IDR_TOOLSBAR ) ||
    !m_toolsBar.LoadToolBar( IDR_TOOLSBAR ) )
  {
    TRACE0("Failed to create mapbar\n");
    return -1;      // Fehler beim Erzeugen
  }
  m_toolsBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBarLeftOf( &m_toolsBar, lastBar );
  lastBar = &m_toolsBar;
  
  // ThemeBar
  if (!m_themeBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY,
    IDR_THEMEBAR ) ||
    !m_themeBar.LoadToolBar( IDR_THEMEBAR ) )
  {
    TRACE0("Failed to create mapbar\n");
    return -1;      // Fehler beim Erzeugen
  }
  m_themeBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBarLeftOf( &m_themeBar, lastBar );
  lastBar = &m_themeBar;

  // Object-Bar
  if (!m_objectBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY,
    IDR_OBJECTBAR ) ||
    !m_objectBar.LoadToolBar( IDR_OBJECTBAR ) )
  {
    TRACE0("Failed to create mapbar\n");
    return -1;      // Fehler beim Erzeugen
  }
  m_objectBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBarLeftOf( &m_objectBar, lastBar );
  lastBar = &m_objectBar;

  // Help-Bar
  if (!m_helpBar.Create( this, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY,
    IDR_HELPBAR ) ||
    !m_helpBar.LoadToolBar( IDR_HELPBAR ) )
  {
    TRACE0("Failed to create mapbar\n");
    return -1;      // Fehler beim Erzeugen
  }
  m_helpBar.EnableDocking( CBRS_ALIGN_TOP );
  DockControlBarLeftOf( &m_helpBar, lastBar );
  lastBar = &m_helpBar;

  // Übersichtsfenster ( CMapProject )
	if (!m_wndOverviewBar.Create( this, IDD_OVERVIEWBAR, WS_VISIBLE | WS_THICKFRAME | CBRS_LEFT | 
                                CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC, IDW_OVERVIEW_BAR ) )
	{
		TRACE0("Failed to create overviewbar\n");
		return -1;      // Fehler beim Erzeugen
	}

  // StatusBar

	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;      // Fehler beim Erzeugen
	};

  // Fenster ausblenden, welche nicht sichtbar sein sollen
  BOOL bShowOverview = theApp.GetProfileInt( APP_SETTINGS_SECTION, APP_SETTINGS_OVERVIEW, TRUE );
  if ( !bShowOverview )
    m_wndOverviewBar.ShowWindow( SW_HIDE );

	return 0;
}

void CMainFrame::OnClose() 
{
  if( m_lpfnCloseProc != NULL && !(*m_lpfnCloseProc)(this) )
    return;

  WINDOWPLACEMENT wndPLc;
  GetWindowPlacement( &wndPLc );
  BOOL bMax = wndPLc.showCmd == SW_SHOWMAXIMIZED;
  CRect frameRect( wndPLc.rcNormalPosition );

  // Fenstergrösse und Zustand speichern
	theApp.WriteProfileInt( APP_SETTINGS_SECTION, APP_SETTINGS_MAX, bMax );
	theApp.WriteProfileBinary( APP_SETTINGS_SECTION, APP_SETTINGS_RECT, (BYTE*)&frameRect,  sizeof(CRect) );
  theApp.WriteProfileInt( APP_SETTINGS_SECTION, APP_SETTINGS_OVERVIEW, GetOverviewBar()->IsWindowVisible() );

  // das Projekt schliessen
  if ( ((CWSPMapApp*)AfxGetApp())->CloseProject() )
    CMDIFrameWnd::OnClose();
}; // OnClose

LONG CMainFrame::OnUpdateProjectComplete(UINT wParam, LONG lParam)
{
	m_nWSPWINCount--;

	return 0L;
}

void CMainFrame::OnFileManager() 
{
  theApp.DoProjectManager( this );
}

BOOL CMainFrame::OnCmdMsg(UINT nID, int nCode, void* pExtra,
                          AFX_CMDHANDLERINFO* pHandlerInfo)
{
  // falls Projekt offen ist, Commandos an dieses Weiterschicken ( hat kein eigenen FrameWnd, welches ihm die Commands schickt )
  CMapProject* mapProject = theApp.GetMapProject();
  if ( mapProject && mapProject->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
    return TRUE;

  // falls Nachricht nicht behandelt wurde, selbst erledigen
  return CMDIFrameWnd::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);
}

void CMainFrame::OnUpdateFrameTitle(BOOL bAddToTitle)
{
	if ((GetStyle() & FWS_ADDTOTITLE) == 0)
		return;     // leave it alone!

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to set the title (used for OLE support)
	if (m_pNotifyHook != NULL)
	{
		CMDIFrameWnd::OnUpdateFrameTitle(bAddToTitle);
		return;
	}
#endif

  CString str;

  CMapProject* mapProject = theApp.GetMapProject();
	if ( mapProject && mapProject->GetProject() )
	{
    str = mapProject->GetProject()->GetDir();
    str.Format("[%s]", str);
	}

	CMDIChildWnd* pActiveChild;
	CDocument* pDocument = GetActiveDocument();
	if (bAddToTitle &&
	  (pActiveChild = MDIGetActive()) != NULL &&
	  (pActiveChild->GetStyle() & WS_MAXIMIZE) == 0 &&
	  (pDocument != NULL ||
	   (pDocument = pActiveChild->GetActiveDocument()) != NULL))
	{
		str += " - ";
		str += pDocument->GetTitle();
	}
	if (!str.IsEmpty())
		UpdateFrameTitleForDocument(str);
	else
		UpdateFrameTitleForDocument(NULL);
}

BOOL CMainFrame::OnExtras( UINT nID )
// CommandHandler für Menu-'Extras'
{
  switch( nID )
  {
  case ID_EXTRAS_EDIT_NUTZUNG:
    {
      CNutzKlass dlg( theApp.GetNutzklassDir(), this );
      dlg.DoModal();
    };
    break;


  default:
    return FALSE; // Kommando wurde nicht bearbeitet
  }; // switch

  return TRUE;
}; // OnExtras

LRESULT CMainFrame::OnDDEAcknowledge( WPARAM wParam, LPARAM lParam )
{
  return IDDEWnd::DDEAcknowledge( wParam, lParam );
}

LRESULT CMainFrame::OnDDETerminate( WPARAM wParam, LPARAM lParam )
{
  return IDDEWnd::DDETerminate( wParam, lParam );
}; // OnDDETerminate



//////////////////////////////////////////////////////////////////////////
// Überladungen

BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
// stellt das Fenster so dar, wie es vorher war, lädt die Einstellungen dazu aus der INI
{
	BOOL bRes = CMDIFrameWnd::PreCreateWindow(cs);

  // Maximiert?
  BOOL bMaximized = theApp.GetProfileInt( APP_SETTINGS_SECTION, APP_SETTINGS_MAX, TRUE );
  if( bMaximized )
    cs.style |= WS_MAXIMIZE;

  BYTE* pb = NULL;
  UINT nLen = 0;
  CRect frameRect;
	if( theApp.GetProfileBinary( APP_SETTINGS_SECTION, APP_SETTINGS_RECT, &pb, &nLen ) )
	{
		ASSERT( nLen == sizeof(CRect) );
		memcpy( &frameRect, pb, sizeof(CRect) );
		delete pb;
	}
	else
		frameRect.SetRect(0,0,0,0);

	if( frameRect.Width() > 0 && frameRect.Height() > 0 )
	{
		// make sure window will be visible
		CDC dc;
		dc.CreateIC( _T("DISPLAY" ), NULL, NULL, NULL );
		CRect rectDisplay( 0, 0, dc.GetDeviceCaps(HORZRES), dc.GetDeviceCaps(VERTRES) );
		if( rectDisplay.PtInRect( frameRect.TopLeft() ) && rectDisplay.PtInRect( frameRect.BottomRight() ) )
		{
			cs.x = frameRect.left;
			cs.y = frameRect.top;
			cs.cx = frameRect.Width();
			cs.cy = frameRect.Height();
		}
	}
	
  return bRes;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame Diagnose

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CMDIFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CMDIFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////
// Operationen

void CMainFrame::DockControlBarLeftOf( CToolBar* Bar, CToolBar* LeftOf )
// von http://www.codeproject.com/docking/toolbar_docking.asp; Author: Kirk Stowell 
// wie der Name schon sagt...
// Parameter:
//        CToolBar* Bar: die zu dockende Bar
//        CToolBar* LeftOf: links zu dieser soll gedockt werden, falls NULL wird oben angedockt
{
	CRect rect;
	DWORD dw;
	UINT n;
	
  if ( LeftOf == NULL )
  {
    DockControlBar( Bar, AFX_IDW_DOCKBAR_TOP );
    return;
  };
	// get MFC to adjust the dimensions of all docked ToolBars
	// so that GetWindowRect will be accurate
	RecalcLayout(TRUE);
	
	LeftOf->GetWindowRect(&rect);
	rect.OffsetRect(1,0);
	dw=LeftOf->GetBarStyle();
	n = 0;
	n = (dw&CBRS_ALIGN_TOP) ? AFX_IDW_DOCKBAR_TOP : n;
	n = (dw&CBRS_ALIGN_BOTTOM && n==0) ? AFX_IDW_DOCKBAR_BOTTOM : n;
	n = (dw&CBRS_ALIGN_LEFT && n==0) ? AFX_IDW_DOCKBAR_LEFT : n;
	n = (dw&CBRS_ALIGN_RIGHT && n==0) ? AFX_IDW_DOCKBAR_RIGHT : n;
	
	// When we take the default parameters on rect, DockControlBar will dock
	// each Toolbar on a seperate line. By calculating a rectangle, we
	// are simulating a Toolbar being dragged to that location and docked.
	DockControlBar(Bar,n,&rect);
}; // DockControlBarLeftOf

//////////////////////////////////
// StatusBarProgress-Control
//////////////////////////////////

CProgressCtrl* CMainFrame::CreateStatusBarProgress( const CString& text )
// erzeugt eine ProgressCtrl in der StatusBar des MainFrame. In der StatusBar erscheint der Text
// gefolgt von der StatusBarCtrl
// Parameter:
//      const CString& text: der vor der ProgressCtrl angezeigte Text
// Rückgabewert:
//          CProgressCtrl: ein Zeiger auf die ProgressCtrl; darf nur benutzt werden für SetRange etc.; darf
//          nicht zerstört werden
// Bemerkung: Attribut Step der ProgressCtrl wird per default auf 1 gesetzt, die Range auf 0-100 und die Position auf 0
{
	if( !m_statusProgressCtrl && m_wndStatusBar.GetSafeHwnd() && m_wndStatusBar.IsWindowVisible() )
	{
    CRect statusRect( 0, 0, 0, 0 ); // erstmal unsichtbar
		m_statusProgressCtrl = new CProgressCtrl;
		if( m_statusProgressCtrl->Create( WS_CHILD | WS_VISIBLE, statusRect, &m_wndStatusBar, 100 ) )
    {
			m_statusProgressCtrl->SetStep( 1 );
      m_statusProgressCtrl->SetRange( 0, 100 );
      m_statusProgressCtrl->SetPos( 0 );
    }
		else
		{
			delete m_statusProgressCtrl;
			m_statusProgressCtrl = NULL;
		};

    // den Text setzen und die ProgressCtrl positionieren
    SetStatusBarProgressText( text );

		return m_statusProgressCtrl;
	}
	return NULL;
}; // CreateStatusBarProgress

void CMainFrame::SetStatusBarProgressText( const CString& text )
// setzt den Text der 0. Tafel der StatusBar und setzt eine evtl. vorhandene Fortschritsanzeige rechts daneben
{
  m_wndStatusBar.SetPaneText( 0, text, TRUE );

  // falls die ProgressCtrl existiert, diese neben den Text setzen
  if( m_statusProgressCtrl && m_statusProgressCtrl->GetSafeHwnd() )
  {
    CRect statusRect;
    m_wndStatusBar.GetStatusBarCtrl().GetRect( 0, &statusRect );
    CDC* pDC = m_wndStatusBar.GetDC();
    if ( !pDC )
      return;

    // rausfinden wie gross der Text ist
    CFont* pOldFont;
    CFont* pFont = m_wndStatusBar.GetFont();
    if( pFont )
      pOldFont = pDC->SelectObject( pFont );
    statusRect.left = min( pDC->GetOutputTextExtent( text ).cx + 10, statusRect.right );
    if( pOldFont )
      pDC->SelectObject( pOldFont );
    m_wndStatusBar.ReleaseDC( pDC );

    statusRect.right = min( statusRect.left + 160, statusRect.right );
    m_statusProgressCtrl->MoveWindow( statusRect, TRUE );
  }; // if m_statusProgressCtrl

  m_wndStatusBar.UpdateWindow();
}; // SetStatusBarProgressText

void CMainFrame::DestroyStatusBarProgress()
// zerstört die ProgressCtrl in der StatusBar, falls vorhanden
{
	if( m_statusProgressCtrl )
	{
		m_statusProgressCtrl->DestroyWindow();
		delete m_statusProgressCtrl;
		m_statusProgressCtrl = NULL;
		m_wndStatusBar.SetPaneText( 0, CString(MAKEINTRESOURCE(AFX_IDS_IDLEMESSAGE)), TRUE );
		m_wndStatusBar.UpdateWindow();
	}
}; // DestroyStatusBarProgress

void CMainFrame::OnDestroy() 
{
	CMDIFrameWnd::OnDestroy();
	
  delete GetMenu();
}
