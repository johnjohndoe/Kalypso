// StApp.cpp : Defines the class behaviors for the application.
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)


#include "stdafx.h"

#include "commonMfc\include\contextHelp.h"
#include "commonMfc\include\version.h"

#include "splash.h"
#include "stpldoc.h"
#include "splitfrm.h"
#include "stplview.h"
#include "stplfrm.h"
#include "stempelhelpmap.h"
#include "BCE\include\WSPFeatures.h"

#include "stempel.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStempelApp

BEGIN_MESSAGE_MAP(CStempelApp, CDrawApp)
	//{{AFX_MSG_MAP(CStempelApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
	ON_COMMAND(ID_HELP, OnHelp)
	ON_COMMAND(ID_CONTEXT_HELP, OnContextHelp)
	ON_COMMAND(ID_HELP_FINDER, OnHelpFinder)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStempelApp construction

CStempelApp::CStempelApp() : CDrawApp()
{
	// add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CStempelApp object

CStempelApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CStempelApp initialization

BOOL CStempelApp::InitInstance()
{
	if (!CDrawApp::InitInstance())
		return FALSE;

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);
	if (cmdInfo.m_nShellCommand==CCommandLineInfo::FileNew)
		cmdInfo.m_nShellCommand = CCommandLineInfo::FileNothing;

	CSplashWnd splash;
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
				if (m_bMaximized)
					m_nCmdShow = SW_SHOWMAXIMIZED;
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
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CMultiDocTemplate* pDocTemplate;
	pDocTemplate = new CMultiDocTemplate(
#ifdef _DEBUG
		IDR_STEMPELTYPE_DEBUG,
#else
		IDR_STEMPELTYPE,
#endif
		RUNTIME_CLASS(CStempelDoc),
		RUNTIME_CLASS(CSplitFrame), // custom MDI child frame
		RUNTIME_CLASS(CStempelView));
	pDocTemplate->SetContainerInfo(IDR_STEMPELTYPE_CNTR_IP);
	AddDocTemplate(pDocTemplate);

	// destroy splash window
	if (bSplash)
	{
		// wait for 2 seconds
		while( goal > clock() )
			;
		splash.DestroyWindow();
	}

	// create main MDI Frame window
	CStempelFrame* pStempelFrame = new CStempelFrame;
	if (!pStempelFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pStempelFrame;

  // Kontexthilfe initialisieren
  CMapUIntToString idMap;
  CStempelHelpMap::FillHelpMap( idMap );

  CString helpFileName = m_execDirectory + "Plotter.pdf";
  m_pContextHelp->Init( pStempelFrame, helpFileName, idMap );

	EnableShellOpen();

	// enable file manager drag/drop and DDE Execute open
	m_pMainWnd->DragAcceptFiles();
	RegisterShellFileTypes(FALSE);

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
     
	// The main window has been initialized, so show and update it.
	pStempelFrame->ShowWindow(m_nCmdShow);
	pStempelFrame->UpdateWindow();

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CStempelApp commands

void CStempelApp::OnFileNew()
{
	CWinApp::OnFileNew();
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CAboutDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

  SetWindowText( CString( MAKEINTRESOURCE(IDS_ABOUT_TITLE) ) );

  if( WSPFeatures::Instance()->isEnabled( "PLOTTER","plot_nodemo" ) )  
	GetDlgItem( IDC_DEMO_TEXT)->SetWindowText( TEXT("") );

  CString version = TEXT("WspWin Stempel Editor, Version ") + GETDRAWAPP->GetVersion()->GetProductNr();
  GetDlgItem( IDC_STATIC3 )->SetWindowText( version );

	
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

// App command to run the dialog
void CStempelApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}
