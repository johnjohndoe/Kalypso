// prjprdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "prjprdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectPrintDialog 


ProjectPrintDialog::ProjectPrintDialog(CWnd* pParent /*=NULL*/, BOOL bNoSelection /*=FALSE*/)
	: CDialog(ProjectPrintDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(ProjectPrintDialog)
	m_type = 0;
	//}}AFX_DATA_INIT
	m_bNoSelection = bNoSelection;
}


void ProjectPrintDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ProjectPrintDialog)
	DDX_Radio(pDX, IDC_RADIO1, m_type);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ProjectPrintDialog, CDialog)
	//{{AFX_MSG_MAP(ProjectPrintDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ProjectPrintDialog 

BOOL ProjectPrintDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	if (m_bNoSelection)
		GetDlgItem(IDC_RADIO2)->EnableWindow(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}
