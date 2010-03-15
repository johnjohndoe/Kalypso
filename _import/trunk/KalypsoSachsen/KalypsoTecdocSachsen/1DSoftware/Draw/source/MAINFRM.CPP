// MainFrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"

#include "drawdoc.h"
#include "draw.h"

#include "mainfrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
	ON_COMMAND_EX(ID_VIEW_ZEICHNEN_BAR, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ZEICHNEN_BAR, OnUpdateControlBarMenu)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_SIZE()
	ON_COMMAND(ID_WINDOW_CLOSEALL, OnWindowCloseAll)
	//}}AFX_MSG_MAP

  ON_MESSAGE( WM_DDE_ACK, OnDDEAcknowledge )
  ON_MESSAGE( WM_DDE_TERMINATE, OnDDETerminate )
END_MESSAGE_MAP()

static UINT indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

#pragma warning( disable : 4355 )
CMainFrame::CMainFrame() : IDDEWnd( this )
{
	m_bEditing = FALSE;
	m_pProgressCtrl = NULL;
}
#pragma warning( default : 4355 )

CMainFrame::~CMainFrame()
{
	DestroyStatusBarProgress();
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	CString str;
	
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	if (!m_wndMainToolBar.Init(this,TRUE))
	{
		TRACE0("Failed to create editbar\n");
		return -1;      // fail to create
	}
	if (!m_wndZeichnenToolBar.Create(this, WS_CHILD | WS_VISIBLE | CBRS_LEFT | CBRS_HIDE_INPLACE,
		ID_VIEW_ZEICHNEN_BAR) ||
		!m_wndZeichnenToolBar.LoadToolBar(IDR_ZEICHNEN))
	{
		TRACE0("Failed to create drawing toolbar\n");
		return -1;      // fail to create
	}

	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;      // fail to create
	}

	// Remove this if you don't want tool tips or a resizeable toolbar
	m_wndZeichnenToolBar.SetBarStyle(m_wndZeichnenToolBar.GetBarStyle() |
		CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC);

	str.LoadString(IDS_MAINTOOLBAR);
	m_wndMainToolBar.SetWindowText(str);
	m_wndMainToolBar.EnableDocking(CBRS_ALIGN_ANY);
	str.LoadString(IDS_DRAWTOOLBAR);
	m_wndMainToolBar.SetWindowText(str);
	m_wndZeichnenToolBar.EnableDocking(CBRS_ALIGN_ANY);

	EnableDocking(CBRS_ALIGN_ANY);

	DockControlBar(&m_wndMainToolBar);
	DockControlBar(&m_wndZeichnenToolBar);

	return 0;
}

BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	BOOL bRes = CMDIFrameWnd::PreCreateWindow(cs);

	CRect rect = GETDRAWAPP->m_rectInitialFrame;
	if (rect.Width() > 0 && rect.Height() > 0)
	{
		// make sure window will be visible
		CDC dc;
		dc.CreateIC(_T("DISPLAY"), NULL, NULL, NULL);
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

BOOL CMainFrame::PreTranslateMessage(MSG* pMsg) 
{
	// if we are editing we don't want to use accelerator keys!
	if (m_bEditing && pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
		return FALSE;
	return CMDIFrameWnd::PreTranslateMessage(pMsg);
}

BOOL CMainFrame::CreateStatusBarProgress(CString& str, int nLower, int nUpper)
{
	CRect rect;
	CDC *pDC;
	CFont *pFont, *pOldFont;
	
	if (m_pProgressCtrl==NULL && m_wndStatusBar.GetSafeHwnd()!=NULL
		&& m_wndStatusBar.IsWindowVisible())
	{
		m_wndStatusBar.SetPaneText(0, str, TRUE);
		m_wndStatusBar.GetStatusBarCtrl().GetRect(0, &rect);
		pDC = m_wndStatusBar.GetDC();
		if (pDC==NULL)
			return FALSE;
		pFont = m_wndStatusBar.GetFont();
		if (pFont!=NULL)
			pOldFont = pDC->SelectObject(pFont);
		rect.left = min(pDC->GetOutputTextExtent(str).cx+10, rect.right);
		if (pFont!=NULL)
			pDC->SelectObject(pOldFont);
		m_wndStatusBar.ReleaseDC(pDC);
		rect.right = min(rect.left+160, rect.right);
		m_pProgressCtrl = new CProgressCtrl;
		if (m_pProgressCtrl->Create(WS_CHILD | WS_VISIBLE, rect, &m_wndStatusBar, 100))
		{
			m_pProgressCtrl->SetRange(nLower, nUpper);
			m_pProgressCtrl->SetStep(1);
		}
		else
		{
			delete m_pProgressCtrl;
			m_pProgressCtrl = NULL;
		}
		m_wndStatusBar.UpdateWindow();
		return TRUE;
	}
	return FALSE;
}

void CMainFrame::IncStatusBarProgress()
{
	if (m_pProgressCtrl!=NULL)
		m_pProgressCtrl->StepIt();
}

void CMainFrame::DestroyStatusBarProgress()
{
	if (m_pProgressCtrl!=NULL)
	{
		m_pProgressCtrl->DestroyWindow();
		delete m_pProgressCtrl;
		m_pProgressCtrl = NULL;
		m_wndStatusBar.SetPaneText(0, "", TRUE);
		m_wndStatusBar.UpdateWindow();
	}
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

void CMainFrame::OnSize(UINT nType, int cx, int cy) 
{
	CMDIFrameWnd::OnSize(nType, cx, cy);
	GETDRAWAPP->m_bMaximized = (nType == SIZE_MAXIMIZED);
	if (nType == SIZE_RESTORED)
		GetWindowRect(GETDRAWAPP->m_rectInitialFrame);
}

void CMainFrame::OnWindowCloseAll() 
{
	CFrameWnd *pWnd;
	CDrawDoc *pDoc;
	BOOL bMaximized;
	int nRet;

	nRet = AfxMessageBox(IDS_CLOSEWITHSAVE, MB_YESNOCANCEL | MB_ICONQUESTION);
	if (nRet==IDCANCEL)
		return;

	while ((pWnd = MDIGetActive(&bMaximized))!=NULL)
	{
		pDoc = (CDrawDoc*)pWnd->GetActiveDocument();
		if (pDoc!=NULL)
		{
			if (nRet==IDYES)
			{
				DWORD wParam;

				wParam = MAKELONG(ID_FILE_SAVE, 0);
				pWnd->SendMessage(WM_COMMAND, wParam, 0);
			}
			if (nRet==IDNO)
				pDoc->SetModifiedFlag(FALSE);
		}
		pWnd->SendMessage(WM_CLOSE);
	}
}

LRESULT CMainFrame::OnDDEAcknowledge( WPARAM wParam, LPARAM lParam )
{
  return IDDEWnd::DDEAcknowledge( wParam, lParam );
}

LRESULT CMainFrame::OnDDETerminate( WPARAM wParam, LPARAM lParam )
{
  return IDDEWnd::DDETerminate( wParam, lParam );
}; // OnDDETerminate
