// printborderpage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRect.h"

#include "printborderpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CPrintBorderPage 

IMPLEMENT_DYNCREATE(CPrintBorderPage, CPropertyPage)

CPrintBorderPage::CPrintBorderPage( CPrintRect* printRect /* = NULL */ ) : CPropertyPage(CPrintBorderPage::IDD, IDS_PRINT_BORDER_PAGE )
// Bemerkung:
//    erster Parameter hat standardwert NULL für IMPLEMENT_DYNCREATE
{
  ASSERT( printRect );
  
	//{{AFX_DATA_INIT(CPrintBorderPage)
	m_width = 0;
	//}}AFX_DATA_INIT
  m_printRect = printRect;

  m_width = printRect->GetBorderWidth();
}

CPrintBorderPage::~CPrintBorderPage()
{
}

void CPrintBorderPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintBorderPage)
	DDX_Control(pDX, IDC_PRINT_BORDER_WIDTH, m_widthCtrl);
	DDX_Control(pDX, IDC_PRINT_BORDER_STYLE, m_styleCombo);
	DDX_Control(pDX, IDC_PRINT_BORDER_SPIN, m_widthSpin);
	DDX_Control(pDX, IDC_PRINT_BORDER_COLOR, m_colorButton);
	DDX_Text(pDX, IDC_PRINT_BORDER_WIDTH, m_width);
	DDV_MinMaxInt(pDX, m_width, 1, 10);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintBorderPage, CPropertyPage)
	//{{AFX_MSG_MAP(CPrintBorderPage)
	ON_BN_CLICKED(IDC_PRINT_BORDER_COLOR, OnPrintBorderColor)
	ON_NOTIFY(UDN_DELTAPOS, IDC_PRINT_BORDER_SPIN, OnDeltaposPrintBorderSpin)
	ON_EN_CHANGE(IDC_PRINT_BORDER_WIDTH, OnChange)
	ON_CBN_SELCHANGE(IDC_PRINT_BORDER_STYLE, OnSelchangePrintBorderStyle)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintBorderPage 

void CPrintBorderPage::OnOK() 
{
  UpdateData( TRUE );


  m_printRect->SetBorderStyle( m_styleCombo.GetCurSel() );
  m_printRect->SetBorderWidth( (int)m_width );
  m_printRect->SetBorderColor( m_colorButton.colGetFaceColor() );
  
	CPropertyPage::OnOK();
} // OnOK

void CPrintBorderPage::OnPrintBorderColor() 
{
  CColorDialog dlg( m_colorButton.colGetFaceColor(), 0, this );

  if( dlg.DoModal() == IDOK )
  {
    m_colorButton.SetFaceColor( dlg.GetColor() );
		m_colorButton.RedrawWindow();
    SetModified( TRUE );
  } // if IDOK
} // OnPrintBorderColor

void CPrintBorderPage::OnDeltaposPrintBorderSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

  // Width darf Werte zwischen 0 und 10 annehmen
  m_width = min( 10, max( 1, m_width - pNMUpDown->iDelta ) );

	UpdateData( FALSE );
	
  SetModified( TRUE );

	*pResult = 0;
}

void CPrintBorderPage::OnChange() 
{
	SetModified( TRUE );
}

BOOL CPrintBorderPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

  // Internationale Texte
  GetDlgItem( IDC_PRINT_BORDER_STYLE_STATIC )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_BORDER_STYLE_STATIC) ) );
  GetDlgItem( IDC_PRINT_BORDER_WIDTH_STATIC )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_BORDER_WIDTH_STATIC) ) );
  GetDlgItem( IDC_PRINT_BORDER_COLOR_STATIC )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_BORDER_COLOR_STATIC) ) );
	
	m_styleCombo.Init( TRUE ); // auch PS_NULL ist zugelassen
  m_styleCombo.SetCurSel( m_printRect->GetBorderStyle() );
  OnSelchangePrintBorderStyle();

  m_colorButton.SetFaceColor( m_printRect->GetBorderColor() );

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CPrintBorderPage::OnSelchangePrintBorderStyle() 
{
  // nur die druchgezogene Linie kann eine Linienstärke != 1 haben
  // also die Breite entsprechend ändern
  int style = m_styleCombo.GetCurSel();
  BOOL bEnable =  style == PS_SOLID;

  m_widthCtrl.EnableWindow( bEnable );
  m_widthSpin.EnableWindow( bEnable );

  if( bEnable )
    m_width = m_printRect->GetBorderWidth();
  else
    m_width = 1;

  m_colorButton.EnableWindow( style != PS_NULL );

  UpdateData( FALSE );


  SetModified( TRUE );
}

