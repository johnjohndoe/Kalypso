// styppage.cpp : implementation file
//

#include "stdafx.h"

#include "drawvw.h"
#include "draw.h"

#include "styppage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSelectTypePage property page

IMPLEMENT_DYNCREATE(CSelectTypePage, CPropertyPage)

CSelectTypePage::CSelectTypePage() : CPropertyPage(CSelectTypePage::IDD)
{
	//{{AFX_DATA_INIT(CSelectTypePage)
	m_select = -1;
	m_tolerance = 0.0;
	//}}AFX_DATA_INIT
}

CSelectTypePage::~CSelectTypePage()
{
}

void CSelectTypePage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSelectTypePage)
	DDX_Radio(pDX, IDC_RADIO1, m_select);
	DDX_Text(pDX, IDC_EDIT1, m_tolerance);
	DDV_MinMaxDouble(pDX, m_tolerance, 1, 5);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CSelectTypePage, CPropertyPage)
	//{{AFX_MSG_MAP(CSelectTypePage)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSelectTypePage message handlers

BOOL CSelectTypePage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	m_select = GETDRAWAPP->m_nSelectType;
	m_tolerance = GETDRAWAPP->m_nSelectTol/MM_FACTOR;
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CSelectTypePage::OnOK() 
{
	if (!UpdateData(TRUE))
		return;
	GETDRAWAPP->m_nSelectType = m_select;
	GETDRAWAPP->m_nSelectTol = (int)(m_tolerance*MM_FACTOR);
	
	CPropertyPage::OnOK();
}
