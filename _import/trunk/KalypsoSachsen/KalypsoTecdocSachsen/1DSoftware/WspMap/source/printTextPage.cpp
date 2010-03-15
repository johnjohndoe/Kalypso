// printtextpage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRectText.h"

#include "printtextpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CPrintTextPage 

CPrintTextPage::CPrintTextPage( UINT captionID, CPrintRectText* textRect ) : CPropertyPage(CPrintTextPage::IDD, captionID )
{
  ASSERT(textRect); // darf nicht NULL sein

	//{{AFX_DATA_INIT(CPrintTextPage)
	m_radio = -1;
	m_text = _T("");
	//}}AFX_DATA_INIT

  m_textRect = textRect;
  
  m_text = textRect->GetText();
  m_logFont = textRect->GetFontLogfont();
  m_color = textRect->GetFontColor();
  m_ptSize = textRect->GetFontPtSize();
  
  switch( textRect->GetFontAlign() )
  {
  case DT_LEFT:
    m_radio = 0;
    break;

  case DT_CENTER:
    m_radio = 1;
    break;

  case DT_RIGHT:
    m_radio = 2;
  } // switch GetAlign
} // Konstruktor

CPrintTextPage::~CPrintTextPage()
{
}

void CPrintTextPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintTextPage)
	DDX_Control(pDX, IDC_STATIC_ALLIGN, m_staticAlign);
	DDX_Control(pDX, IDC_STATIC_TEXT, m_staticText);
	DDX_Radio(pDX, IDC_RADIO_LEFT, m_radio);
	DDX_Text(pDX, IDC_PRINT_TEXT_EDIT, m_text);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintTextPage, CPropertyPage)
	//{{AFX_MSG_MAP(CPrintTextPage)
	ON_BN_CLICKED(IDC_PRINT_TEXT_SCHRIFT, OnSchrift)
	ON_EN_CHANGE(IDC_PRINT_TEXT_EDIT, OnChangeEdit)
	ON_BN_CLICKED(IDC_RADIO_LEFT, OnRadio)
	ON_BN_CLICKED(IDC_RADIO_CENTER, OnRadio)
	ON_BN_CLICKED(IDC_RADIO_RIGHT, OnRadio)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintTextPage 

void CPrintTextPage::OnOK() 
{
  // die Daten vom Dialog übertragen
  UpdateData( FALSE );

  // und neu setzen
  m_textRect->SetText( m_text );

  UINT align;
  switch( m_radio )
  {
  case 0:
    align = DT_LEFT;
    break;

  case 2:
    align = DT_RIGHT;
    break;

  default:
    align = DT_CENTER;
  } // switch m_radio

  m_textRect->SetFontAlign( align );
  m_textRect->SetFontLogfont( m_logFont );
  m_textRect->SetFontColor( m_color );
  m_textRect->SetFontPtSize( m_ptSize );
	
	CPropertyPage::OnOK();
}

BOOL CPrintTextPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

  GetDlgItem(IDC_STATIC_ALLIGN)->SetWindowText( CString( MAKEINTRESOURCE(IDC_STATIC_ALLIGN) ) );
  GetDlgItem(IDC_PRINT_TEXT_SCHRIFT)->SetWindowText( CString( MAKEINTRESOURCE(IDC_PRINT_TEXT_SCHRIFT) ) );
  GetDlgItem(IDC_RADIO_LEFT)->SetWindowText( CString( MAKEINTRESOURCE(IDC_RADIO_LEFT) ) );
  GetDlgItem(IDC_RADIO_CENTER)->SetWindowText( CString( MAKEINTRESOURCE(IDC_RADIO_CENTER) ) );
  GetDlgItem(IDC_RADIO_RIGHT)->SetWindowText( CString( MAKEINTRESOURCE(IDC_RADIO_RIGHT) ) );
  GetDlgItem(IDC_STATIC_TEXT)->SetWindowText( CString( MAKEINTRESOURCE(IDC_STATIC_TEXT) ) );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}


void CPrintTextPage::OnSchrift() 
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
} // OnSchrift

void CPrintTextPage::OnChangeEdit() 
{
  SetModified( TRUE ); // nur da, um den Übernehmen Knopf zu aktivieren
}

void CPrintTextPage::OnRadio() 
{
  SetModified( TRUE ); // nur da, um den Übernehmen Knopf zu aktivieren
}
