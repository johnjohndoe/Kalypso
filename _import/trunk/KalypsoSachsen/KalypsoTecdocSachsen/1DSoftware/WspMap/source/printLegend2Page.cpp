// printlegend2page.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRectLegend2.h"

#include "printlegend2page.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintLegend2Page 


CPrintLegend2Page::CPrintLegend2Page( UINT captionID, CPrintRectLegend2* pPrintRectLegend2 )
	: CPropertyPage( CPrintLegend2Page::IDD, captionID ), m_pPrintRectLegend2( pPrintRectLegend2 )
{
  ASSERT( pPrintRectLegend2 );

	//{{AFX_DATA_INIT(CPrintLegend2Page)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

  m_logFont = pPrintRectLegend2->GetFontLogfont();
  m_color = pPrintRectLegend2->GetFontColor();
  m_ptSize = pPrintRectLegend2->GetFontPtSize();
}


void CPrintLegend2Page::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintLegend2Page)
		// HINWEIS: Der Klassen-Assistent fügt hier DDX- und DDV-Aufrufe ein
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintLegend2Page, CDialog)
	//{{AFX_MSG_MAP(CPrintLegend2Page)
	ON_BN_CLICKED(IDC_PRINT_LEGEND2_PAGE_SCHRIFT, OnSchrift)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintLegend2Page 

void CPrintLegend2Page::OnSchrift() 
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

void CPrintLegend2Page::OnOK() 
{
  // die Daten vom Dialog übertragen
  UpdateData( FALSE );

  m_pPrintRectLegend2->SetFontLogfont( m_logFont );
  m_pPrintRectLegend2->SetFontColor( m_color );
  m_pPrintRectLegend2->SetFontPtSize( m_ptSize );
  m_pPrintRectLegend2->NotifyListeners( m_pPrintRectLegend2->GetBounds() );
	
	CPropertyPage::OnOK();
}

BOOL CPrintLegend2Page::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

  GetDlgItem(IDC_PRINT_LEGEND2_PAGE_SCHRIFT)->SetWindowText( CString( MAKEINTRESOURCE(IDC_PRINT_TEXT_SCHRIFT) ) );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}
