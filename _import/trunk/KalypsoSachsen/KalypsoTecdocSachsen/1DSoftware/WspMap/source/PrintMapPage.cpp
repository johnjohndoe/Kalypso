// PrintMapPage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRectMap.h"

#include "PrintMapPage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CPrintMapPage 

CPrintMapPage::CPrintMapPage( UINT captionID, CPrintRectMap* mapRect ) : CPropertyPage( CPrintMapPage::IDD, captionID )
{
	//{{AFX_DATA_INIT(CPrintMapPage)
	m_scale = _T("");
  m_adjust = 0;
	m_alignText = _T("");
	//}}AFX_DATA_INIT

  m_mapRect = mapRect;
  
  m_scale.Format( "%d", mapRect->GetScale() );

  m_alignText = CString( MAKEINTRESOURCE(IDS_PRINT_Map_ALIGN_TEXT) );

  m_logFont = mapRect->GetFontLogfont();
  m_color = mapRect->GetFontColor();
  m_ptSize = mapRect->GetFontPtSize();
} // Standardkonstruktor

CPrintMapPage::~CPrintMapPage()
{
}

void CPrintMapPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintMapPage)
	DDX_Control(pDX, IDC_PRINT_MAP_FONT_BUTTON, m_fontButton);
	DDX_Control(pDX, IDC_PRINT_MAP_ALIGN_COMBO, m_alignCombo);
	DDX_Control(pDX, IDC_PRINT_MAP_SCALETEXT, m_scaleStatic);
	DDX_CBString(pDX, IDC_PRINT_MAP_SCALECOMBO, m_scale);
  DDX_Radio(pDX, IDC_PRINT_MAP_RADIO_EXTENT, m_adjust);
	DDX_Text(pDX, IDC_PRINT_MAP_TEXT, m_alignText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintMapPage, CPropertyPage)
	//{{AFX_MSG_MAP(CPrintMapPage)
	ON_CBN_EDITCHANGE(IDC_PRINT_MAP_SCALECOMBO, OnChangeData)
	ON_BN_CLICKED(IDC_PRINT_MAP_RADIO_EXTENT, OnChangeData)
	ON_BN_CLICKED(IDC_PRINT_MAP_RADIO_PRINTRECT, OnChangeData)
	ON_CBN_SELCHANGE(IDC_PRINT_MAP_SCALECOMBO, OnChangeData)
	ON_CBN_SELCHANGE(IDC_PRINT_MAP_ALIGN_COMBO, OnChangeData)
	ON_BN_CLICKED(IDC_PRINT_MAP_FONT_BUTTON, OnPrintMapFontButton)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintMapPage 

BOOL CPrintMapPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	m_scaleStatic.SetWindowText( CString(MAKEINTRESOURCE(IDS_PRINT_MAP_SCALE)));

  SetDlgItemText( IDC_PRINT_MAP_RADIO_EXTENT, CString(MAKEINTRESOURCE(IDS_PRINT_MAP_RADION_EXTENT)) );
  SetDlgItemText( IDC_PRINT_MAP_RADIO_PRINTRECT, CString(MAKEINTRESOURCE(IDS_PRINT_MAP_RADION_PRINTRECT)) );

  // die AlignCombo füllen
  int index = m_alignCombo.AddString( CString( MAKEINTRESOURCE( IDS_PRINT_MAP_ALIGN_NONE ) ) );
  m_alignCombo.SetItemData( index, -1 );

  index = m_alignCombo.AddString( CString( MAKEINTRESOURCE( IDS_PRINT_MAP_ALIGN_NW ) ) );
  m_alignCombo.SetItemData( index, DT_SINGLELINE | DT_TOP | DT_LEFT );

  index = m_alignCombo.AddString( CString( MAKEINTRESOURCE( IDS_PRINT_MAP_ALIGN_NE ) ) );
  m_alignCombo.SetItemData( index, DT_SINGLELINE | DT_TOP | DT_RIGHT );

  index = m_alignCombo.AddString( CString( MAKEINTRESOURCE( IDS_PRINT_MAP_ALIGN_SW ) ) );
  m_alignCombo.SetItemData( index, DT_SINGLELINE | DT_BOTTOM | DT_LEFT );

  index = m_alignCombo.AddString( CString( MAKEINTRESOURCE( IDS_PRINT_MAP_ALIGN_SE ) ) );
  m_alignCombo.SetItemData( index, DT_SINGLELINE | DT_BOTTOM | DT_RIGHT );

  // und den richtigen selektieren
  m_alignCombo.SetCurSel( 0 ); // Default: nichts anzeigen
  for( int i = 0; i < m_alignCombo.GetCount(); i++ )
  {
    UINT align = m_alignCombo.GetItemData( i );
    if( align == m_mapRect->GetFontAlign() )
    {
      m_alignCombo.SetCurSel( i );
      break;
    } // if align
  } // for i


  return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CPrintMapPage::OnOK() 
{
  UpdateData( FALSE );

  int scale = atoi( m_scale );
  scale = max( scale, 25 ); // mindestens 1:25!

  m_mapRect->SetFontLogfont( m_logFont );
  m_mapRect->SetFontColor( m_color );
  m_mapRect->SetFontPtSize( m_ptSize );
  m_mapRect->SetFontAlign( m_alignCombo.GetItemData( m_alignCombo.GetCurSel() ) );

	m_mapRect->SetScale( scale, m_adjust == 1 );
  m_scale.Format( "%d", m_mapRect->GetScale() );

	CPropertyPage::OnOK();
}

void CPrintMapPage::OnChangeData() 
// wird aufgerufen, wenn sich eine der Ctrls geändert hat -> bewirkt, dass der Übernehmen Knopf
// aktiviert wird
{
  SetModified( TRUE );
}

void CPrintMapPage::OnPrintMapFontButton() 
{
  CFontDialog dlg( &m_logFont, CF_EFFECTS | CF_BOTH, NULL, this );
  dlg.m_cf.rgbColors = m_color;
  
  if( dlg.DoModal() == IDOK )
  {
    dlg.GetCurrentFont( &m_logFont );
    // set CLIP_LH_ANGLES to ensure the coordinate system for all devices is the same
    m_logFont.lfClipPrecision = (BYTE)(m_logFont.lfClipPrecision | CLIP_LH_ANGLES);
    
    m_color = dlg.GetColor();
    m_ptSize = dlg.GetSize();
    
    SetModified(TRUE);
  };
} // OnPrintMapFontButton
