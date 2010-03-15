// onvertDialog.cpp : implementation file
//

#include "stdafx.h"

#include "Global.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// ConvertDialog dialog


ConvertDialog::ConvertDialog(CString& filename, CWnd* pParent /*=NULL*/)
	: CDialog(ConvertDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(ConvertDialog)
	m_nType = -1;
	m_text = _T("");
	//}}AFX_DATA_INIT
	m_filename = filename;
}


void ConvertDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ConvertDialog)
	DDX_Radio(pDX, IDC_RADIO1, m_nType);
	DDX_Text(pDX, IDC_STATIC1, m_text);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ConvertDialog, CDialog)
	//{{AFX_MSG_MAP(ConvertDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// ConvertDialog message handlers

BOOL ConvertDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_text.FormatMessage(IDS_SAVE_FORMAT_TEXT, m_filename);
	m_nType = 0;
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void ConvertDialog::OnOK() 
{
	// TODO: Add extra validation here
	
	CDialog::OnOK();
}
