// zoombx.cpp : implementation file
//

#include "stdafx.h"

#include "drawdoc.h"
#include "drawvw.h"
#include "draw.h"
#include "mainfrm.h"

#include "zoombx.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CZoomBox

CZoomBox::CZoomBox()
{
}

CZoomBox::~CZoomBox()
{
}

BEGIN_MESSAGE_MAP(CZoomBox, CComboBox)
	//{{AFX_MSG_MAP(CZoomBox)
	ON_WM_CREATE()
	ON_CONTROL_REFLECT(CBN_SELCHANGE, OnUpdateZoom)
	ON_CONTROL_REFLECT(CBN_KILLFOCUS, OnUpdateZoom)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	ON_REGISTERED_MESSAGE(CDrawApp::m_nZoomFactorChangedMsg, OnZoomChanged)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CZoomBox message handlers

BOOL CZoomBox::PreTranslateMessage(MSG* pMsg)
{
	if ((pMsg->message != WM_KEYDOWN) || (pMsg->wParam != VK_RETURN))
		return CComboBox::PreTranslateMessage(pMsg);

	// when the enter key is hit in the ComboBox we want to update
	// the zoom factor
	if ((pMsg->lParam & 0x40000000) == 0)   // Not a repeat.
	{
		OnUpdateZoom();
	}
	return TRUE;
}

LONG CZoomBox::OnZoomChanged(UINT, LONG)
{
	CDrawView *pView;
	CDrawDoc *pDoc;
	CFrameWnd *pWnd;
	CMainFrame *pFrame = (CMainFrame*)GETDRAWAPP->m_pMainWnd;
	BOOL bMaximized;
	POSITION pos;
	CString str;
	
	pWnd = pFrame->MDIGetActive(&bMaximized);
	if (pWnd!=NULL)
	{
		pDoc = (CDrawDoc*)pWnd->GetActiveDocument();
		if (pDoc!=NULL)
		{
			pos = pDoc->GetFirstViewPosition();
			if (pos!=NULL)
			{
				pView = (CDrawView*)pDoc->GetNextView(pos);
				str.Format("%d%%", pView->m_nZoomFactor);
				SetWindowText(str);
			}
		}
	}
	return 0;
}


int CZoomBox::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	int i, zooms[6] = { 50, 100, 150, 200, 300, 400 };
	CString str;

	if (CComboBox::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	for (i=0; i<6; i++)
	{
		str.Format("%d%%", zooms[i]);
		AddString(str);
	}

	return 0;
}

void CZoomBox::OnUpdateZoom() 
{
	CDrawView *pView;
	CDrawDoc *pDoc;
	CFrameWnd *pWnd;
	CMainFrame *pFrame = (CMainFrame*)GETDRAWAPP->m_pMainWnd;
	BOOL bMaximized;
	POSITION pos;
	CString str;
	int factor, i;
	
	i = GetCurSel();
	if (i==-1)
		GetWindowText(str);
	else
		GetLBText(i, str);
	factor = atoi(str);
	if (factor<10 || factor>4000)
	{
		str.FormatMessage(AFX_IDP_PARSE_INT_RANGE, "10", "4000");
		AfxMessageBox(str, MB_OK | MB_ICONEXCLAMATION);
		OnZoomChanged(0, 0);
		return;
	}
	pWnd = pFrame->MDIGetActive(&bMaximized);
	if (pWnd!=NULL)
	{
		pDoc = (CDrawDoc*)pWnd->GetActiveDocument();
		if (pDoc!=NULL)
		{
			pos = pDoc->GetFirstViewPosition();
			while (pos!=NULL)
			{
				pView = (CDrawView*)pDoc->GetNextView(pos);
				if (pView->m_nZoomFactor!=factor)
				{
					pView->m_nZoomFactor = factor;
					pView->ResyncScrollSizes();
					pView->Invalidate();
				}
			}
			str.Format("%d%%", factor);
			SetWindowText(str);
		}
	}
}

void CZoomBox::OnDestroy() 
{
	CComboBox::OnDestroy();
	
	GETDRAWAPP->m_hWndZoomBox = NULL;
}
