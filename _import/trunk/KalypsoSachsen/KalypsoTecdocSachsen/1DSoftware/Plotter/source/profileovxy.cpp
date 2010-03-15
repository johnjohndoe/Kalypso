// profileovxy.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "profileovxy.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CProfileOvXY 
/////////////////////////////////////////////////////////////////////////////
// ein kleiner Dialog zum Abfragen von Höhe und Breite der Profilübersicht
/////////////////////////////////////////////////////////////////////////////
// im Standardkonstruktor werden die Maximale Höhe und Breite übergeben
/////////////////////////////////////////////////////////////////////////////

CProfileOvXY::CProfileOvXY( UINT width, UINT height, UINT maxWidth, UINT maxHeight, CWnd* pParent )
	: CDialog(CProfileOvXY::IDD, pParent)
{
  this->maxWidth = max( 1, maxWidth ); // mindestens 1
  this->width = min( width, this->maxWidth ); // höchstens maxWidth
  this->width = max( 1, this->width ); // mindestens 1
  
  this->maxHeight = max( 1, maxHeight );
  this->height = min( height, this->maxHeight );
  this->height = max( 1, this->height );

	//{{AFX_DATA_INIT(CProfileOvXY)
	m_heightText = _T("");
	m_widthText = _T("");
	//}}AFX_DATA_INIT
}


void CProfileOvXY::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProfileOvXY)
	DDX_Control(pDX, IDC_COMBO_WIDTH, m_widthCombo);
	DDX_Control(pDX, IDC_COMBO_HEIGHT, m_heightCombo);
	DDX_Text(pDX, IDC_STATIC_HEIGHT, m_heightText);
	DDX_Text(pDX, IDC_STATIC_WIDTH, m_widthText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProfileOvXY, CDialog)
	//{{AFX_MSG_MAP(CProfileOvXY)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CProfileOvXY 

BOOL CProfileOvXY::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// Texte setzen
  SetWindowText( CString( MAKEINTRESOURCE( IDS_PROFIL_OV_XY_TITLE ) ) );
  m_heightText.LoadString( IDS_PROFIL_OV_XY_HEIGHT );
  m_widthText.LoadString( IDS_PROFIL_OV_XY_WIDTH );

  // Comboboxern füllen und selektieren
  for( UINT i = 0; i < maxWidth; i++ )
  {
    CString numberStr;
    numberStr.Format( "%d", i + 1 );
    m_widthCombo.AddString( numberStr );
  }; // for i
  m_widthCombo.SetCurSel( width - 1 );

  for( i = 0; i < maxHeight; i++ )
  {
    CString numberStr;
    numberStr.Format( "%d", i + 1 );
    m_heightCombo.AddString( numberStr );
  }; // for i
  m_heightCombo.SetCurSel( height - 1 );

  m_widthCombo.SetFocus();

  UpdateData( FALSE );

	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}; // OnInitDialog

void CProfileOvXY::OnOK() 
{
	if ( !UpdateData( TRUE ) )
    return;

  height = m_heightCombo.GetCurSel() + 1;
  width = m_widthCombo.GetCurSel() + 1;
	
	CDialog::OnOK();
};
