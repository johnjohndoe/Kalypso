// messageboxwelcher.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "commresource.h"

#include "messageboxwelcher.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBoxWelcher 

CMessageBoxWelcher::CMessageBoxWelcher( const CString& title, const CString& text, const CString& firstName, const CString& secondName, CWnd* pParent /* = NULL */ )
	: CDialog( IDD, pParent )
{
	//{{AFX_DATA_INIT(CMessageBoxWelcher)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

  m_title = title;
  m_text = text;
  m_names.Add( firstName );
  m_names.Add( secondName );
}


void CMessageBoxWelcher::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMessageBoxWelcher)
	DDX_Control(pDX, IDC_TEXT, m_textCtrl);
	DDX_Control(pDX, IDCANCEL, m_cancelButton);
	DDX_Control(pDX, IDSECOND_YES, m_secondYesButton);
	DDX_Control(pDX, IDSECOND_ALWAYS, m_secondAlwaysButton);
	DDX_Control(pDX, IDNO, m_noButton);
	DDX_Control(pDX, IDNEVER, m_neverButton);
	DDX_Control(pDX, IDFIRST_YES, m_firstYesButton);
	DDX_Control(pDX, IDFIRST_ALYWAS, m_firstAlwaysButton);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMessageBoxWelcher, CDialog)
	//{{AFX_MSG_MAP(CMessageBoxWelcher)
	ON_BN_CLICKED(IDFIRST_ALYWAS, OnFirstAlywas)
	ON_BN_CLICKED(IDFIRST_YES, OnFirstYes)
	ON_BN_CLICKED(IDNEVER, OnNever)
	ON_BN_CLICKED(IDNO, OnNo)
	ON_BN_CLICKED(IDSECOND_ALWAYS, OnSecondAlways)
	ON_BN_CLICKED(IDSECOND_YES, OnSecondYes)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMessageBoxWelcher 

BOOL CMessageBoxWelcher::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  SetWindowText( m_title );
  m_textCtrl.SetWindowText( m_text );

  m_firstYesButton.SetWindowText( m_names[0] );
  m_firstAlwaysButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_ALWAYS) ) );

  m_secondYesButton.SetWindowText( m_names[1] );
  m_secondAlwaysButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_ALWAYS) ) );

  m_noButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NO) ) );
  m_neverButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NEVER) ) );

  m_cancelButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_CANCEL) ) );
	
	return TRUE;  // return TRUE unless you set the focus to a control
}

void CMessageBoxWelcher::OnFirstAlywas() 
{
  m_result = m_names[0];
  EndDialog( IDALWAYS );
}

void CMessageBoxWelcher::OnFirstYes() 
{
  m_result = m_names[0];
  EndDialog( IDYES );
}

void CMessageBoxWelcher::OnNever() 
{
  EndDialog( IDNEVER );
}

void CMessageBoxWelcher::OnNo() 
{
	EndDialog( IDNO );
}

void CMessageBoxWelcher::OnSecondAlways() 
{
	m_result = m_names[1];
  EndDialog( IDALWAYS );
}

void CMessageBoxWelcher::OnSecondYes() 
{
	m_result = m_names[1];
  EndDialog( IDYES );
}

void CMessageBoxWelcher::OnCancel() 
{
  EndDialog( IDCANCEL );
}
