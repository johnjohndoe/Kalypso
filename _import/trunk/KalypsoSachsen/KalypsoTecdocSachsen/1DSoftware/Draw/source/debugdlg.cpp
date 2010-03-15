// debugdlg.cpp : implementation file
//

#include "stdafx.h"

#include "drawvw.h"

#include "debugdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#ifdef _DEBUG
/////////////////////////////////////////////////////////////////////////////
// CDebugDialog dialog


CDebugDialog::CDebugDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CDebugDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDebugDialog)
		m_bBreakInDraw = FALSE;
		m_bTraceUndo = FALSE;
		m_bShowPosInfo = FALSE;
		m_bBreakWhenIntersected = FALSE;
	//}}AFX_DATA_INIT
	m_pView = (CDrawView*)pParent;
}


void CDebugDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDebugDialog)
	DDX_Check(pDX, IDC_CHECK1, m_bBreakInDraw);
	DDX_Check(pDX, IDC_CHECK2, m_bTraceUndo);
	DDX_Check(pDX, IDC_CHECK3, m_bShowPosInfo);
	DDX_Check(pDX, IDC_CHECK4, m_bBreakWhenIntersected);
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDebugDialog, CDialog)
	//{{AFX_MSG_MAP(CDebugDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDebugDialog message handlers

BOOL CDebugDialog::OnInitDialog() 
{
	CString str;
	POSITION pos;
	
	CDialog::OnInitDialog();
	
	m_bTraceUndo = m_pView->m_bTraceUndo;
	m_bShowPosInfo = m_pView->m_bShowPosInfo;
	pos = m_pView->m_selection.GetHeadPosition();
	while (pos!=NULL)
	{
		CDrawObj *pObj;

		pObj = m_pView->m_selection.GetNextObject( pos );
		str.Format("%x", pObj);
		m_list.AddString(str);
		m_bBreakInDraw = pObj->m_bBreakInDraw;
		m_bBreakWhenIntersected = pObj->m_bBreakWhenIntersected;
	}
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CDebugDialog::OnOK() 
{
	POSITION pos;

	UpdateData(TRUE);
	pos = m_pView->m_selection.GetHeadPosition();
	while (pos!=NULL)
	{
		CDrawObj *pObj;
		pObj = m_pView->m_selection.GetNextObject( pos );
		pObj->m_bBreakInDraw = m_bBreakInDraw;
		pObj->m_bBreakWhenIntersected = m_bBreakWhenIntersected;
	}
	m_pView->m_bTraceUndo = m_bTraceUndo;
	m_pView->m_bShowPosInfo = m_bShowPosInfo;
	CDialog::OnOK();
}

#endif // _DEBUG
