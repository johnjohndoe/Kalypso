// mtoolbar.cpp : implementation file
//

#include "stdafx.h"

#include "buttons.h" // liegt entweder in stempel ODER in plotter

#include "draw.h"
#include "mainfrm.h"

#include "mtoolbar.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

#define COMBOBOX_INDEX 11
#define COMBOBOX_WIDTH 50
#define COMBOBOX_HEIGHT 150
#define COMBOBOX_BITMAP 6

/////////////////////////////////////////////////////////////////////////////
// CMainToolBar

CMainToolBar::CMainToolBar()
{
}

CMainToolBar::~CMainToolBar()
{
}

BOOL CMainToolBar::Init(CWnd* pParentWnd, BOOL bToolTips)
{
	m_bVertical = FALSE;

	// start out with no borders
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_SIZE_DYNAMIC;
	if (bToolTips)
		dwStyle |= (CBRS_TOOLTIPS | CBRS_FLYBY);
	if (!Create(pParentWnd, dwStyle, AFX_IDW_TOOLBAR))
	{
		return FALSE;
	}
	
	if (!LoadBitmap(IDR_MAINFRAME))
		return FALSE;

	if(!SetButtons(MainButtons, sizeof(MainButtons)/sizeof(UINT)))
		return FALSE;
	
	CIntIRect rect(-COMBOBOX_WIDTH, -COMBOBOX_HEIGHT, 0, 0);
	// The ID of the ComboBox is important for two reasons.  One, so you
	// can receive notifications from the control.  And also for ToolTips.
	// During HitTesting if the ToolBar sees that the mouse is on a child
	// control, the toolbar will lookup the controls ID and search for a
	// string in the string table with the same ID to use for ToolTips
	// and StatusBar info.
	if (!m_ZoomBox.Create(WS_CHILD | CBS_DROPDOWN |
		CBS_AUTOHSCROLL | WS_VSCROLL | CBS_HASSTRINGS, rect, this,
		IDC_TOOLBAR_ZOOMBOX))
	{
		return FALSE;
	}

	GETDRAWAPP->m_hWndZoomBox = m_ZoomBox.GetSafeHwnd();

	CFrameWnd *pWnd;
	CMainFrame *pFrame = (CMainFrame*)GETDRAWAPP->m_pMainWnd;
	BOOL bMaximized;
	
	if (pFrame!=NULL)
	{
		pWnd = pFrame->MDIGetActive(&bMaximized);
		if (pWnd==NULL)
			m_ZoomBox.EnableWindow(FALSE);
		else
			m_ZoomBox.SendMessage(GETDRAWAPP->m_nZoomFactorChangedMsg, 0, 0);
	}
	else
		m_ZoomBox.EnableWindow(FALSE);

	HFONT hFont = (HFONT)GetStockObject(DEFAULT_GUI_FONT);
	if (hFont == NULL)
		hFont = (HFONT)GetStockObject(ANSI_VAR_FONT);

  	m_ZoomBox.SendMessage(WM_SETFONT, (WPARAM)hFont);

	if(!SetHorizontal())
		return FALSE;

	return TRUE;
}

BOOL CMainToolBar::SetHorizontal()
{
	m_bVertical = FALSE;

	SetBarStyle(GetBarStyle() | CBRS_ALIGN_TOP);

	SetButtonInfo(COMBOBOX_INDEX, IDC_TOOLBAR_ZOOMBOX, TBBS_SEPARATOR, COMBOBOX_WIDTH);

	if (m_ZoomBox.m_hWnd != NULL)
	{
		CRect rect;
		GetItemRect(COMBOBOX_INDEX, rect);
		
		m_ZoomBox.SetWindowPos(NULL, rect.left, rect.top, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOCOPYBITS);
		m_ZoomBox.ShowWindow(SW_SHOW);
	}
	
	return TRUE;
}

BOOL CMainToolBar::SetVertical()
{
	m_bVertical = TRUE;

	SetButtonInfo(COMBOBOX_INDEX, ID_EDIT_ZOOM, TBBS_BUTTON, COMBOBOX_BITMAP);

	if (m_ZoomBox.m_hWnd != NULL)
		m_ZoomBox.ShowWindow(SW_HIDE);

	return TRUE;
}

CSize CMainToolBar::CalcDynamicLayout(int nLength, DWORD dwMode)
{
	// if we're committing set the buttons appropriately
	if (dwMode & LM_COMMIT)
	{
		if (dwMode & LM_VERTDOCK)
		{
			if (!m_bVertical)
				SetVertical();
		}
		else
		{
			if (m_bVertical)
				SetHorizontal();
		}

		return CToolBar::CalcDynamicLayout(nLength, dwMode);
	}
	else
	{
		BOOL bOld = m_bVertical;
		BOOL bSwitch = (dwMode & LM_HORZ) ? bOld : !bOld;

		if (bSwitch)
		{
			if (bOld)
				SetHorizontal();
			else
				SetVertical();
		}
		

		CSize sizeResult = CToolBar::CalcDynamicLayout(nLength, dwMode);
		
		if (bSwitch)
		{
			if (bOld)
				SetHorizontal();
			else
				SetVertical();
		}

		return sizeResult;
	}
}

BEGIN_MESSAGE_MAP(CMainToolBar, CToolBar)
	//{{AFX_MSG_MAP(CMainToolBar)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CMainToolBar message handlers

