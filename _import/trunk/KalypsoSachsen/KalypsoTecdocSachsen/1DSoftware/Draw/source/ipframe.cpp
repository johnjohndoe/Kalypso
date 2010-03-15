// ipframe.cpp : implementation of the CInPlaceFrame class
//

#include "stdafx.h"

#include "ipframe.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInPlaceFrame

IMPLEMENT_DYNCREATE(CInPlaceFrame, COleIPFrameWnd)

BEGIN_MESSAGE_MAP(CInPlaceFrame, COleIPFrameWnd)
	//{{AFX_MSG_MAP(CInPlaceFrame)
	ON_WM_CREATE()
	ON_UPDATE_COMMAND_UI(ID_CONTEXT_HELP, OnUpdateContextHelp)
	ON_MESSAGE(WM_RECALCPARENT, OnRecalcParent)
	//}}AFX_MSG_MAP
	// Global help commands
	ON_COMMAND(ID_HELP_INDEX, COleIPFrameWnd::OnHelpIndex)
	ON_COMMAND(ID_HELP_USING, COleIPFrameWnd::OnHelpUsing)
	ON_COMMAND(ID_HELP, COleIPFrameWnd::OnHelp)
	ON_COMMAND(ID_DEFAULT_HELP, COleIPFrameWnd::OnHelpIndex)
	ON_COMMAND(ID_CONTEXT_HELP, COleIPFrameWnd::OnContextHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// arrays of IDs used to initialize control bars

// toolbar buttons - IDs are command buttons
static UINT BASED_CODE main_buttons[] =
{
	// same order as in the bitmap 'toolbar.bmp'
	ID_EDIT_CUT,
	ID_EDIT_COPY,
	ID_EDIT_PASTE,
		ID_SEPARATOR,
	ID_APP_ABOUT,
};

static UINT BASED_CODE draw_buttons[] =
{
	// same order as in the bitmap 'itool2.bmp'
	ID_DRAW_SELECT,
	ID_DRAW_TEXT,
	ID_DRAW_LINE,
	ID_DRAW_RECT,
	ID_DRAW_ELLIPSE,
	ID_DRAW_POLYLINE,
	ID_DRAW_POLYGON,
};

/////////////////////////////////////////////////////////////////////////////
// CInPlaceFrame construction/destruction

CInPlaceFrame::CInPlaceFrame()
{
}

CInPlaceFrame::~CInPlaceFrame()
{
}

#if _MFC_VER<0x0421
LRESULT CInPlaceFrame::OnRecalcParent(WPARAM, LPARAM lParam)
{
	// Override default implementation and return FALSE.
	// Unfortunately the default behaviour for MFC 4.0 causes
	// CScrollView::UpdateBars to use a nonsense value for the
	// size of the scroll bars in an in-place frame when both scroll
	// bars are needed! This is a bug in MFC 4.0 which has been resolved
	// in MFC 4.2.
	return FALSE;
}
#endif

int CInPlaceFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (COleIPFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	if (!m_wndResizeBar.Create(this))
	{
		TRACE0("Failed to create resize bar\n");
		return -1;      // fail to create
	}

	m_dropTarget.Register(this);
	return 0;
}

// OnCreateControlBars is called by the framework to create control
//  bars on the container application's windows.
BOOL CInPlaceFrame::OnCreateControlBars(CFrameWnd* pWndFrame,
	CFrameWnd* /*pWndDoc*/)
{
	// create toolbar on client's frame window
	if (!m_wndMainToolBar.Create(pWndFrame) ||
		!m_wndMainToolBar.LoadBitmap(IDR_PLOTTERTYPE_SRVR_IP) ||
		!m_wndMainToolBar.SetButtons(main_buttons,
		  sizeof(main_buttons)/sizeof(UINT)))
	{
		TRACE0("Failed to create main toolbar\n");
		return FALSE;
	}

	if (!m_wndZeichnenToolBar.Create(pWndFrame) ||
		!m_wndZeichnenToolBar.LoadBitmap(IDR_ZEICHNEN) ||
		!m_wndZeichnenToolBar.SetButtons(draw_buttons,
		  sizeof(draw_buttons)/sizeof(UINT)))
	{
		TRACE0("Failed to create drawing toolbar\n");
		return FALSE;
	}

	// set owner to this window, so messages are delivered to correct app
	m_wndMainToolBar.EnableDocking(CBRS_ALIGN_ANY);
	m_wndMainToolBar.SetBarStyle(m_wndMainToolBar.GetBarStyle() | CBRS_SIZE_DYNAMIC);
	pWndFrame->EnableDocking(CBRS_ALIGN_ANY);
	pWndFrame->DockControlBar(&m_wndMainToolBar);
	m_wndMainToolBar.SetOwner(this);

	// set owner to this window, so messages are delivered to correct app
	m_wndZeichnenToolBar.EnableDocking(CBRS_ALIGN_ANY);
	m_wndZeichnenToolBar.SetBarStyle(m_wndZeichnenToolBar.GetBarStyle() | CBRS_SIZE_DYNAMIC);
	pWndFrame->EnableDocking(CBRS_ALIGN_ANY);
	pWndFrame->DockControlBar(&m_wndZeichnenToolBar);
	m_wndZeichnenToolBar.SetOwner(this);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CInPlaceFrame diagnostics

#ifdef _DEBUG
void CInPlaceFrame::AssertValid() const
{
	COleIPFrameWnd::AssertValid();
}

void CInPlaceFrame::Dump(CDumpContext& dc) const
{
	COleIPFrameWnd::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CInPlaceFrame commands

void CInPlaceFrame::OnUpdateContextHelp(CCmdUI* pCmdUI)
{
	pCmdUI->SetCheck(!!m_bHelpMode);
}
