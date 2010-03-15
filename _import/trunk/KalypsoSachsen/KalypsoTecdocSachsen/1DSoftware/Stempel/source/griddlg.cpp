// griddlg.cpp : implementation file
//

#include "stdafx.h"

#include "drawvw.h"
#include "drawdoc.h"
#include "draw.h"

#include "griddlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGridDialog dialog


CGridDialog::CGridDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CGridDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CGridDialog)
	m_snap = FALSE;
	m_width = 0;
	m_height = 0;
	//}}AFX_DATA_INIT
	m_pView = (CDrawView*)pParent;
}


void CGridDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGridDialog)
	DDX_Check(pDX, IDC_CHECK1, m_snap);
	DDX_Text(pDX, IDC_EDIT1, m_width);
	DDV_MinMaxInt(pDX, m_width, 0, 20);
	DDX_Text(pDX, IDC_EDIT2, m_height);
	DDV_MinMaxInt(pDX, m_height, 0, 20);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CGridDialog, CDialog)
	//{{AFX_MSG_MAP(CGridDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGridDialog message handlers


BOOL CGridDialog::OnInitDialog() 
{
	CSize sizeGrid;

	CDialog::OnInitDialog();
	ASSERT(m_pView!=NULL);
	
	m_snap = m_pView->GetDocument()->m_pData->m_bSnapToGrid;
	sizeGrid = m_pView->GetDocument()->m_pData->m_sizeGrid;
	m_width = (int)(sizeGrid.cx/MM_FACTOR);
	m_height = (int)(sizeGrid.cy/MM_FACTOR);
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CGridDialog::OnOK() 
{
	if (!UpdateData(TRUE))
		return;
	m_pView->GetDocument()->m_pData->m_bSnapToGrid = m_snap;
	m_pView->GetDocument()->m_pData->m_sizeGrid.cx = m_width*MM_FACTOR;
	m_pView->GetDocument()->m_pData->m_sizeGrid.cy = m_height*MM_FACTOR;
	m_pView->Invalidate();
	CDialog::OnOK();
}
