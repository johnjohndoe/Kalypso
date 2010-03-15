///////////////////
// stateDlg.cpp: //
///////////////////

#include "stdafx.h"

#include "..\..\commonMfc\include\commResource.h"

#include "stateDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld StateDialog
/////////////////////////////////////////////////////////////////////////////

StateDialog::StateDialog( WORD maske, CWnd* pParent /* = NULL */ ) : CDialog( IDD, pParent )
// Standardkonstruktor für StateDialog
// Parameter:
//        WORD maske: legt fest welche Felder editiert werden dürfen
//                    Kombination aus den STATE_DLG_MASK_...
//        CWnd* pParent: siehe CDialog
{
  //{{AFX_DATA_INIT(StateDialog)
	m_date = _T("");
	m_name = _T("");
	m_staticDate = _T("");
	m_staticName = _T("");
	m_staticWater = _T("");
	m_water = _T("");
	//}}AFX_DATA_INIT
  m_mask = maske;
};

void StateDialog::DoDataExchange( CDataExchange* pDX )
{
  CDialog::DoDataExchange(pDX);
  //{{AFX_DATA_MAP(StateDialog)
	DDX_Control(pDX, IDC_WATER_NAME, m_waterCtrl);
	DDX_Control(pDX, IDC_STATE_NAME, m_nameCtrl);
	DDX_Control(pDX, IDC_DATE, m_dateCtrl);
	DDX_Text(pDX, IDC_DATE, m_date);
	DDX_Text(pDX, IDC_STATE_NAME, m_name);
	DDV_MaxChars(pDX, m_name, 10);
	DDX_Text(pDX, IDC_STATIC_DATE, m_staticDate);
	DDX_Text(pDX, IDC_STATIC_NAME, m_staticName);
	DDX_Text(pDX, IDC_STATIC_WATERNAME, m_staticWater);
	DDX_Text(pDX, IDC_WATER_NAME, m_water);
	DDV_MaxChars(pDX, m_water, 8);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(StateDialog, CDialog)
//{{AFX_MSG_MAP(StateDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten StateDialog 

BOOL StateDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();

  m_date = m_oleDate.Format( "%x" );

  // wegen versch. Sprachen, Textfelder setzen
  m_staticWater.LoadString( IDS_WATERWAY );
  m_staticDate.LoadString( IDS_DATUM );
  m_staticName.LoadString( IDS_STATE );

  if ( ( m_mask & STATE_DLG_MASK_NAME ) == 0 )
    m_nameCtrl.EnableWindow( FALSE );
  else
    m_nameCtrl.SetFocus();

  if ( ( m_mask & STATE_DLG_MASK_DATE ) == 0 )
    m_dateCtrl.EnableWindow( FALSE );
  else
    m_dateCtrl.SetFocus();
  
  if ( ( m_mask & STATE_DLG_MASK_WATER ) == 0 )
    m_waterCtrl.EnableWindow( FALSE );
  else
    m_waterCtrl.SetFocus();
	
  UpdateData( FALSE );
		
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void StateDialog::OnOK() 
{
  if ( !UpdateData() )
   return;

  if (m_water.IsEmpty())
  {
    AfxMessageBox(IDS_NOWATERNAME, MB_OK | MB_ICONEXCLAMATION);
    return;
  }
  if ( m_name.IsEmpty() )
  {
    AfxMessageBox(IDS_NOSTATENAME, MB_OK | MB_ICONEXCLAMATION);
    return;
  }
  
  if ( m_name.FindOneOf( "\\/#()!´`^~|" ) != -1 )
  {
    AfxMessageBox( IDS_BADSTATECHARACTER, MB_OK | MB_ICONEXCLAMATION );
    return;
  };

  if ( !m_oleDate.ParseDateTime( m_date, VAR_DATEVALUEONLY ) )
  {
    AfxMessageBox( IDS_NOVALIDDATE, MB_OK | MB_ICONEXCLAMATION );
    return;
  };
	
	CDialog::OnOK();
}
