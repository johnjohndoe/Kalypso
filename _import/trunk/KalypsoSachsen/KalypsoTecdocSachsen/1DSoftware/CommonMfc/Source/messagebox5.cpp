// messagebox5.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "commresource.h"

#include "messagebox5.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBox5 


CMessageBox5::CMessageBox5( const CString& title, const CString& text, CWnd* pParent /*=NULL*/ )
	: CDialog(CMessageBox5::IDD, pParent)
{
  m_title = title;
  m_text = text;

	//{{AFX_DATA_INIT(CMessageBox5)
	//}}AFX_DATA_INIT
}


void CMessageBox5::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMessageBox5)
	DDX_Control(pDX, IDC_TEXT, m_textCtrl);
	DDX_Control(pDX, IDYES, m_yesButton);
	DDX_Control(pDX, IDNO, m_noButton);
	DDX_Control(pDX, IDNEVER, m_neverButton);
	DDX_Control(pDX, IDCANCEL, m_cancelButton);
	DDX_Control(pDX, IDALWAYS, m_alwaysButton);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMessageBox5, CDialog)
	//{{AFX_MSG_MAP(CMessageBox5)
	ON_BN_CLICKED(IDALWAYS, OnAlways)
	ON_BN_CLICKED(IDNEVER, OnNever)
	ON_BN_CLICKED(IDNO, OnNo)
	ON_BN_CLICKED(IDYES, OnYes)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMessageBox5 

void CMessageBox5::OnAlways() 
{
  EndDialog( IDALWAYS );
}

void CMessageBox5::OnCancel() 
{
  EndDialog( IDCANCEL );
}

void CMessageBox5::OnNever() 
{
  EndDialog( IDNEVER );
}

void CMessageBox5::OnNo() 
{
	EndDialog( IDNO );
}

void CMessageBox5::OnYes() 
{
	EndDialog( IDYES );
}

BOOL CMessageBox5::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  // die Dialogtexte setzen
  SetWindowText( m_title );
  m_textCtrl.SetWindowText( m_text );
  
  m_yesButton.SetWindowText( CString( MAKEINTRESOURCE(IDS_YES) ) );
  m_alwaysButton.SetWindowText( CString( MAKEINTRESOURCE(IDS_ALWAYS) ) );
  m_noButton.SetWindowText( CString( MAKEINTRESOURCE(IDS_NO) ) );
  m_neverButton.SetWindowText( CString( MAKEINTRESOURCE(IDS_NEVER) ) );
  m_cancelButton.SetWindowText( CString( MAKEINTRESOURCE(IDS_CANCEL) ) );
	
	return TRUE;  // return TRUE unless you set the focus to a control
} // OnInitDialog
