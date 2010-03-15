// ZoomDlg.cpp : implementation file
//

#include "stdafx.h"

#include "drawvw.h"
#include "draw.h"

#include "zoomdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CZoomDialog dialog

CZoomDialog::CZoomDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CZoomDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CZoomDialog)
	m_factor = _T("");
	//}}AFX_DATA_INIT
	m_pView = (CDrawView*)pParent;
}

void CZoomDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CZoomDialog)
	DDX_CBString(pDX, IDC_COMBO1, m_factor);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CZoomDialog, CDialog)
	//{{AFX_MSG_MAP(CZoomDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CZoomDialog message handlers

BOOL CZoomDialog::OnInitDialog() 
{
	CString str;
	
	CDialog::OnInitDialog();
	
	str.Format("%d", m_pView->m_nZoomFactor);
	m_factor = str;
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CZoomDialog::OnOK() 
{
	int factor;
	CString str;

	if (!UpdateData(TRUE))
		return;
	factor = atoi(m_factor);
	if (factor<10 || factor>4000)
	{
		str.FormatMessage(AFX_IDP_PARSE_INT_RANGE, "10", "4000");
		AfxMessageBox(str, MB_OK | MB_ICONEXCLAMATION);
		return;
	}
	m_pView->m_nZoomFactor = factor;
	m_pView->ResyncScrollSizes();
	m_pView->Invalidate();
	if (GETDRAWAPP->m_hWndZoomBox!=NULL)
		::SendMessage(GETDRAWAPP->m_hWndZoomBox, CDrawApp::m_nZoomFactorChangedMsg, 0, 0);
	CDialog::OnOK();
}
