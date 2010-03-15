// progdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "progdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProgressDialog 


ProgressDialog::ProgressDialog(CWnd* pParent /*=NULL*/)
	: CDialog(ProgressDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(ProgressDialog)
	m_text = _T("");
	//}}AFX_DATA_INIT
	m_pParent = pParent;
}


void ProgressDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ProgressDialog)
	DDX_Control(pDX, IDC_PROGRESS1, m_progress);
	DDX_Text(pDX, IDC_STATIC1, m_text);
	//}}AFX_DATA_MAP
}

void ProgressDialog::IncProgress()
{
	MSG msg;

	m_progress.StepIt();
    while (PeekMessage(&msg, GetSafeHwnd(),  0, 0, PM_REMOVE)) 
		DispatchMessage(&msg);
}

void ProgressDialog::SetText(CString& text)
{
	m_text = text;
	UpdateData(FALSE);
}


BEGIN_MESSAGE_MAP(ProgressDialog, CDialog)
	//{{AFX_MSG_MAP(ProgressDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ProgressDialog 

BOOL ProgressDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_progress.SetRange(0, 100);
	m_progress.SetStep(1);
	CenterWindow();
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void ProgressDialog::OnCancel() 
{
	if (m_pParent!=NULL)
		m_pParent->SendMessage(theApp.m_nProgressCancelMsg, 0, 0);
	DestroyWindow();
}


void ProgressDialog::PostNcDestroy() 
{
	CDialog::PostNcDestroy();

	theApp.pDlg = NULL;
	delete this;
}
