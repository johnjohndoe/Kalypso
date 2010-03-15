// distdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "distdlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDistanceDialog 


CDistanceDialog::CDistanceDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CDistanceDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDistanceDialog)
	m_distance = _T("");
	m_XDistance = _T("");
	m_YDistance = _T("");
	//}}AFX_DATA_INIT
}


void CDistanceDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDistanceDialog)
	DDX_Text(pDX, IDC_STATIC1, m_distance);
	DDX_Text(pDX, IDC_STATIC2, m_XDistance);
	DDX_Text(pDX, IDC_STATIC3, m_YDistance);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDistanceDialog, CDialog)
	//{{AFX_MSG_MAP(CDistanceDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CDistanceDialog 

BOOL CDistanceDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}
