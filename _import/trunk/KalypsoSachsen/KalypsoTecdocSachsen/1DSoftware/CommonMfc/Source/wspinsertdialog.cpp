// wspinsertdialog.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "commResource.h"

#include "..\Include\wspinsertdialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CWSPInsertDialog 


CWSPInsertDialog::CWSPInsertDialog( CWnd* pParent /*=NULL*/ )	: CDialog( IDD_WSP_INSERT, pParent )
{
	//{{AFX_DATA_INIT(CWSPInsertDialog)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

  m_bDurchst = TRUE;
  m_strAbflussFormat = TEXT( "( für Q = %1 m³/s )" );
}


void CWSPInsertDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CWSPInsertDialog)
	DDX_Control(pDX, IDC_WSP_STATIC_ABFLUSS, m_abflussStatic);
	DDX_Control(pDX, IDC_WSP_CONSTRAINT_EDIT, m_abflussEdit);
	DDX_Control(pDX, IDC_WSP_CONSTRAINT_CHECK, m_constraintCheck);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CWSPInsertDialog, CDialog)
	//{{AFX_MSG_MAP(CWSPInsertDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CWSPInsertDialog 

BOOL CWSPInsertDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_abflussEdit.SetWindowText( m_strAbflussFormat );
  m_abflussStatic.SetWindowText( CString( MAKEINTRESOURCE( IDS_WSP_STATIC_ABFLUSS ) ) );
  
  m_constraintCheck.SetCheck( m_bDurchst );
  m_constraintCheck.SetWindowText( CString( MAKEINTRESOURCE( IDS_WSP_CONSTRAINT ) ) );


  SetWindowText( CString( MAKEINTRESOURCE( IDS_WSP_INSERT ) ) );

	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CWSPInsertDialog::OnOK() 
{
	m_bDurchst = m_constraintCheck.GetCheck();
  m_abflussEdit.GetWindowText( m_strAbflussFormat );
	
	CDialog::OnOK();
} // OnOK
