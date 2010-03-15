// mainfrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"

#include "global.h"

#include <dde.h>

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNCREATE(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_COMMAND(ID_HELP, OnHelpFinder)
	ON_WM_DROPFILES()
	ON_WM_QUERYNEWPALETTE()
	ON_WM_PALETTECHANGED()
	ON_WM_DEVMODECHANGE()
	ON_COMMAND(ID_FILE_OPENPROJEKT, OnFileOpenprojekt)
	ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
	ON_COMMAND(ID_FILE_CLOSEPROJEKT, OnFileCloseprojekt)
	ON_UPDATE_COMMAND_UI(ID_FILE_CLOSEPROJEKT, OnUpdateFileCloseprojekt)
	ON_COMMAND(ID_HELP_INDEX, OnHelpFinder)
	//}}AFX_MSG_MAP
	// Global help commands
	ON_COMMAND(ID_DEFAULT_HELP, OnHelpFinder)
	ON_REGISTERED_MESSAGE(CEditorApp::m_nOpenMsg, OnOpenMsg)
	ON_COMMAND_EX(ID_VIEW_STATUS_BAR, OnBarCheck)
	ON_COMMAND_EX(ID_VIEW_TOOLBAR, OnBarCheck)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// arrays of IDs used to initialize control bars

static UINT BASED_CODE indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
	m_hIconDoc = theApp.LoadIcon(IDI_ICON_DOC);
	m_hIconText = theApp.LoadIcon(IDI_ICON_TEXT);
	m_hIconWrite = theApp.LoadIcon(IDI_ICON_WRITE);
}

CMainFrame::~CMainFrame()
{
}

BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs) 
{
	WNDCLASS wndcls;

	BOOL bRes = CMDIFrameWnd::PreCreateWindow(cs);
	HINSTANCE hInst = AfxGetInstanceHandle();

	// see if the class already exists
	if (!::GetClassInfo(hInst, szWSPEditorClass, &wndcls))
	{
		// get default stuff
		::GetClassInfo(hInst, cs.lpszClass, &wndcls);
		wndcls.style &= ~(CS_HREDRAW|CS_VREDRAW);
		// register a new class
		wndcls.lpszClassName = szWSPEditorClass;
		wndcls.hIcon = ::LoadIcon(hInst, MAKEINTRESOURCE(IDR_MAINFRAME));
		ASSERT(wndcls.hIcon != NULL);
		if (!AfxRegisterClass(&wndcls))
			AfxThrowResourceException();
	}
	cs.lpszClass = szWSPEditorClass;
	CRect rect = theApp.m_rectInitialFrame;
	if (rect.Width() > 0 && rect.Height() > 0)
	{
		// make sure window will be visible
		CDisplayIC dc;
		CRect rectDisplay(0, 0, dc.GetDeviceCaps(HORZRES), 
			dc.GetDeviceCaps(VERTRES));
		if (rectDisplay.PtInRect(rect.TopLeft()) && 
			rectDisplay.PtInRect(rect.BottomRight()))
		{
			cs.x = rect.left;
			cs.y = rect.top;
			cs.cx = rect.Width();
			cs.cy = rect.Height();
		}
	}
	return bRes;
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	if (!CreateToolBar())
		return -1;

	if (!CreateStatusBar())
		return -1;

	EnableDocking(CBRS_ALIGN_ANY);

	m_wndToolBar.EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_wndToolBar);

	CWnd* pView = GetDlgItem(AFX_IDW_PANE_FIRST);
	if (pView != NULL)	
	{
		pView->SetWindowPos(&wndBottom, 0, 0, 0, 0, 
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOACTIVATE);
	}

	return 0;
}

BOOL CMainFrame::CreateToolBar()
{
	if (!m_wndToolBar.Create(this, 
		WS_CHILD|WS_VISIBLE|CBRS_TOP|CBRS_TOOLTIPS|CBRS_FLYBY|CBRS_SIZE_DYNAMIC)||
		!m_wndToolBar.LoadToolBar(IDR_MAINFRAME))
	{
		TRACE0("Failed to create toolbar\n");
		return FALSE;      // fail to create
	}

	CString str;
	str.LoadString(IDS_TITLE_TOOLBAR);
	m_wndToolBar.SetWindowText(str);
	return TRUE;
}

BOOL CMainFrame::CreateStatusBar()
{
	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return FALSE;      // fail to create
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame Operations

HICON CMainFrame::GetIcon(int nDocType)
{
	switch (nDocType)
	{
		case RD_WINWORD6:
		case RD_WORDPAD:
		case RD_EMBEDDED:
		case RD_RICHTEXT:
			return m_hIconDoc;
		case RD_TEXT:
		case RD_OEMTEXT:
			return m_hIconText;
		case RD_WRITE:
			return m_hIconWrite;
	}
	return m_hIconDoc;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

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

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

void CMainFrame::OnDevModeChange(LPTSTR lpDeviceName) 
{
	theApp.NotifyPrinterChanged();
	CMDIFrameWnd::OnDevModeChange(lpDeviceName); //sends message to descendants
}

void CMainFrame::ActivateFrame(int nCmdShow) 
{
	CMDIFrameWnd::ActivateFrame(nCmdShow);
	// make sure and display the toolbar, ruler, etc while loading a document.
	OnIdleUpdateCmdUI();
	UpdateWindow();
}

void CMainFrame::OnSize(UINT nType, int cx, int cy) 
{
	CMDIFrameWnd::OnSize(nType, cx, cy);
	theApp.m_bMaximized = (nType == SIZE_MAXIMIZED);
	if (nType == SIZE_RESTORED)
		GetWindowRect(theApp.m_rectInitialFrame);
}

void CMainFrame::OnMove(int x, int y) 
{
	CMDIFrameWnd::OnMove(x, y);
	WINDOWPLACEMENT wp;
	wp.length = sizeof(wp);
	GetWindowPlacement(&wp);
	theApp.m_rectInitialFrame = wp.rcNormalPosition;
	CView* pView = GetActiveView();
	if (pView != NULL)
		pView->SendMessage(WM_MOVE);
}

LONG CMainFrame::OnOpenMsg(UINT, LONG lParam)
{
	TCHAR szAtomName[256];
	szAtomName[0] = NULL;
	GlobalGetAtomName((ATOM)lParam, szAtomName, 256);
	CEditorDoc* pDoc = (CEditorDoc*)GetActiveDocument();
	if (szAtomName[0] != NULL && pDoc != NULL)
	{
		if (lstrcmpi(szAtomName, pDoc->GetPathName()) == 0)
			return TRUE;
	}
	return FALSE;
}

void CMainFrame::OnHelpFinder() 
{
	theApp.WinHelp(0, HELP_FINDER);
}

void CMainFrame::OnDropFiles(HDROP hDropInfo) 
{
	TCHAR szFileName[_MAX_PATH];
	::DragQueryFile(hDropInfo, 0, szFileName, _MAX_PATH);
	::DragFinish(hDropInfo);
	theApp.OpenDocumentFile(szFileName);
}

BOOL CMainFrame::OnQueryNewPalette() 
{
	CView* pView = GetActiveView();
	if (pView != NULL)
		return pView->SendMessage(WM_QUERYNEWPALETTE);
	return FALSE;
}

void CMainFrame::OnPaletteChanged(CWnd* pFocusWnd) 
{
	CView* pView = GetActiveView();
	if (pView != NULL)
		pView->SendMessage(WM_PALETTECHANGED, (WPARAM)pFocusWnd->GetSafeHwnd());
}

void CMainFrame::OnFileOpenprojekt() 
{
	OpenProjektDialog dlg(this);
	CString oldProject;

	if (dlg.DoModal()==IDOK)
	{
		if (theApp.m_pProject!=NULL)
		{
			oldProject = theApp.m_pProject->GetDir();
			if (oldProject.CompareNoCase(dlg.m_dir)!=0)
				oldProject.Empty();
		}
		if (oldProject.IsEmpty())
		{
			CloseProject();
			theApp.LoadProject(dlg.m_dir);
			OnFileOpen();
		}
	}
}

void CMainFrame::OnFileOpen() 
{
	OpenDialog dlg(this);
	CString path;
	int i;
	
	if (theApp.m_pProject==NULL)
	{
		theApp.OnFileOpen();
		return;
	}
	if (dlg.DoModal()==IDOK)
	{
		for (i=0; i<dlg.m_filenames.GetSize(); i++)
		{
			if (theApp.m_pProject->FileIsOEM(dlg.m_filenames[i]))
				theApp.m_bForceOEM = TRUE;
			path = dlg.m_directories[i] + dlg.m_filenames[i];
			theApp.OpenDocumentFile(path);
			theApp.m_bForceOEM = FALSE;
		}
	}
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

	if (theApp.m_pProject!=NULL)
	{
		str = theApp.m_pProject->GetDir();
		str.Format("[%s]", str);
	}
	else
		str.Empty();

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

void CMainFrame::CloseProject()
{
	CFrameWnd *pWnd;
	BOOL bMaximized;
	CEditorDoc *pDoc;

	if (theApp.m_pProject!=NULL)
	{
		delete theApp.m_pProject;
		theApp.m_pProject = NULL;
		// set the mainframe window title
		OnUpdateFrameTitle(TRUE);
		pWnd = MDIGetActive(&bMaximized);
		while (pWnd!=NULL)
		{
			CFrameWnd *pNextWnd = pWnd->m_pNextFrameWnd;
			pDoc = (CEditorDoc*)pWnd->GetActiveDocument();
			if (pDoc!=NULL)
				pWnd->SendMessage(WM_CLOSE);
			pWnd = pNextWnd;
		}
	}
}

void CMainFrame::OnFileCloseprojekt() 
{
	CloseProject();
}

void CMainFrame::OnUpdateFileCloseprojekt(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_pProject!=NULL);
}
