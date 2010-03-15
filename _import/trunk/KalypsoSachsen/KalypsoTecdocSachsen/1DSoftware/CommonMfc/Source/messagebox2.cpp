// messagebox2.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "commresource.h"

#include "messagebox2.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBox2 


CMessageBox2::CMessageBox2( const CString& title, const CString& message, const CString& okText, const CString& cancelText,const BOOL bFocusCancel, CWnd* pParent /*=NULL*/ )
	: CDialog( CMessageBox2::IDD, pParent )
{
	//{{AFX_DATA_INIT(CMessageBox2)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

  m_title = title;
  m_message = message;
  m_okText = okText;
  m_cancelText = cancelText;
  m_bFocusCancel = bFocusCancel;
}


void CMessageBox2::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMessageBox2)
		// HINWEIS: Der Klassen-Assistent fügt hier DDX- und DDV-Aufrufe ein
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMessageBox2, CDialog)
	//{{AFX_MSG_MAP(CMessageBox2)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMessageBox2 

BOOL CMessageBox2::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// Texte setzen
  SetWindowText( m_title );
  GetDlgItem( IDC_MESSAGE_BOX_2_TEXT )->SetWindowText( m_message );
  GetDlgItem( IDOK )->SetWindowText( m_okText );
  
  CWnd* pCancelWnd = GetDlgItem( IDCANCEL );
  pCancelWnd->SetWindowText( m_cancelText );


  if( m_bFocusCancel == TRUE )
  {
    pCancelWnd->SetFocus();
    return FALSE;
  }

	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

/*
void CMessageBox2::OnOK() 
{
	// TODO: Zusätzliche Prüfung hier einfügen
	
	CDialog::OnOK();
}

void CMessageBox2::OnCancel() 
{
	// TODO: Zusätzlichen Bereinigungscode hier einfügen
	
	CDialog::OnCancel();
}
*/