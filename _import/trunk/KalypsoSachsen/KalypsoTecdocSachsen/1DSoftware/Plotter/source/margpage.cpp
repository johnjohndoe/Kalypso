// margpage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\wspprj\wspprj.h"

#include "template.h"
#include "plotdoc.h"
#include "plotdocdata.h"
#include "drawvw.h"
#include "propdlg.h"
#include "plotter.h"

#include "margpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CMarginsPage 


UINT CMarginsPage::alignMap[][2] = { 
  { IDS_MARGINS_ALIGN_NW, CPlotArranger::NW },
  { IDS_MARGINS_ALIGN_NE, CPlotArranger::NE },
  { IDS_MARGINS_ALIGN_SW, CPlotArranger::SW },
  { IDS_MARGINS_ALIGN_SE, CPlotArranger::SE }
};
UINT CMarginsPage::alignCount = 4;

CMarginsPage::CMarginsPage( CPropertyDialog* pParent, CPlotterDoc* pDoc /*=NULL*/ ) : CPropertyPage( CMarginsPage::IDD, IDS_MARGINS_TITLE )
{
	//{{AFX_DATA_INIT(CMarginsPage)
	m_editLeft = 0.0;
	m_editRight = 0.0;
	m_editTop = 0.0;
	m_editBottom = 0.0;
	m_staticBottom = _T("");
	m_staticDist = _T("");
	m_staticHint = _T("");
	m_staticLeft = _T("");
	m_staticProfil = _T("");
	m_staticRight = _T("");
	m_staticStempel = _T("");
	m_staticTop = _T("");
	m_stampScale = 0;
	m_staticStampScale = _T("");
	m_staticHor = _T("");
	m_staticStamp = _T("");
	m_staticProfile = _T("");
	m_editHor = 0.0;
	m_editVert = 0.0;
	m_staticVert = _T("");
	//}}AFX_DATA_INIT


  // Internationale Texte setzen
  m_staticTop = CString( MAKEINTRESOURCE( IDS_MARGINS_TOP ) );
  m_staticLeft = CString( MAKEINTRESOURCE( IDS_MARGINS_LEFT ) );
  m_staticRight = CString( MAKEINTRESOURCE( IDS_MARGINS_RIGHT ) );
  m_staticBottom = CString( MAKEINTRESOURCE( IDS_MARGINS_BOTTOM ) );
  m_staticDist = CString( MAKEINTRESOURCE( IDS_MARGINS_DIST ) );
  m_staticHint = CString( MAKEINTRESOURCE( IDS_MARGINS_HINT ) );
  m_staticProfil = CString( MAKEINTRESOURCE( IDS_MARGINS_PROFIL ) );
  m_staticStempel = CString( MAKEINTRESOURCE( IDS_MARGINS_STEMPEL ) );
  m_staticStampScale = CString( MAKEINTRESOURCE( IDS_MARGINS_STAMPSCALE ) );
  m_staticProfile = CString( MAKEINTRESOURCE( IDS_MARGINS_PROFIL ) );
  m_staticStamp = CString( MAKEINTRESOURCE( IDS_MARGINS_STEMPEL ) );
  m_staticHor = CString( MAKEINTRESOURCE( IDS_MARGINS_HOR ) );
  m_staticVert = CString( MAKEINTRESOURCE( IDS_MARGINS_VERT ) );

	m_pParent = pParent;
	m_pDoc = pDoc;
	m_pTemp = NULL;
}

void CMarginsPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMarginsPage)
	DDX_Control(pDX, IDC_MARGINS_CHECK_HOR, m_checkHor);
	DDX_Control(pDX, IDC_MARGINS_CHECK_STEMPEL, m_stampCheckbox);
	DDX_Control(pDX, IDC_MARGINS_COMBO_PROFIL, m_profilCombo);
	DDX_Text(pDX, IDC_MARGINS_EDIT_LEFT, m_editLeft);
	DDV_MinMaxDouble(pDX, m_editLeft, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_EDIT_RIGHT, m_editRight);
	DDV_MinMaxDouble(pDX, m_editRight, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_EDIT_TOP, m_editTop);
	DDV_MinMaxDouble(pDX, m_editTop, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_EDIT_BOTTOM, m_editBottom);
	DDV_MinMaxDouble(pDX, m_editBottom, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_STATIC_BOTTOM, m_staticBottom);
	DDX_Text(pDX, IDC_MARGINS_STATIC_DISC, m_staticDist);
	DDX_Text(pDX, IDC_MARGINS_STATIC_HINT, m_staticHint);
	DDX_Text(pDX, IDC_MARGINS_STATIC_LEFT, m_staticLeft);
	DDX_Text(pDX, IDC_MARGINS_STATIC_PROFIL, m_staticProfil);
	DDX_Text(pDX, IDC_MARGINS_STATIC_RIGHT, m_staticRight);
	DDX_Text(pDX, IDC_MARGINS_STATIC_STEMPEL, m_staticStempel);
	DDX_Text(pDX, IDC_MARGINS_STATIC_TOP, m_staticTop);
	DDX_Text(pDX, IDC_MARGINS_EDIT_STAMPSCALE, m_stampScale);
	DDV_MinMaxUInt(pDX, m_stampScale, 10, 1000);
	DDX_Text(pDX, IDC_MARGINS_STATIC_STAMPSCALE, m_staticStampScale);
	DDX_Text(pDX, IDC_MARGINS_STATIC_HOR, m_staticHor);
	DDX_Text(pDX, IDC_MARGINS_STAMP_STATIC, m_staticStamp);
	DDX_Text(pDX, IDC_MARGINS_PROFIL_STATIC, m_staticProfile);
	DDX_Text(pDX, IDC_MARGINS_EDIT_HOR, m_editHor);
	DDV_MinMaxDouble(pDX, m_editHor, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_EDIT_VERT, m_editVert);
	DDV_MinMaxDouble(pDX, m_editVert, 0., 1000.);
	DDX_Text(pDX, IDC_MARGINS_STATIC_VERT, m_staticVert);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMarginsPage, CPropertyPage)
	//{{AFX_MSG_MAP(CMarginsPage)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_BOTTOM, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_DIST, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_LEFT, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_RIGHT, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_TOP, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_STAMPSCALE, OnChange)
	ON_BN_CLICKED(IDC_MARGINS_CHECK_STEMPEL, OnChange)
	ON_CBN_SELCHANGE(IDC_MARGINS_COMBO_PROFIL, OnChange)
	ON_EN_CHANGE(IDC_MARGINS_EDIT_HOR, OnChange)
	ON_BN_CLICKED(IDC_MARGINS_CHECK_HOR, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMarginsPage 

BOOL CMarginsPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

  // Internationale Texte setzen
  GetDlgItem( IDC_MARGINS_MARGINS )->SetWindowText( CString( MAKEINTRESOURCE( IDS_MARGINS_MARGINS ) ) );
  GetDlgItem( IDC_MARGINS_ALIGN )->SetWindowText( CString( MAKEINTRESOURCE( IDS_MARGINS_ALIGN ) ) );
  m_stampCheckbox.SetWindowText( CString( MAKEINTRESOURCE( IDS_MARGINS_STEMPEL_ALIGN ) ) );
  m_checkHor.SetWindowText( CString( MAKEINTRESOURCE( IDS_MARGINS_CHECK_HOR ) ) );


  // die ComboBoxen füllen
  for( int i = 0; i < alignCount; i++ )
  {
    CString text( MAKEINTRESOURCE( alignMap[i][0] ) );
    UINT data = alignMap[i][1];

    int id = m_profilCombo.AddString( text );
    m_profilCombo.SetItemData( id, data );
  }; // for i

	m_pParent->m_nActivePages++;
	Update();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CMarginsPage::Update()
{
	CPlotterDoc* pDoc;

	if (GetSafeHwnd()==NULL)
		return;

	if( m_pTemp != NULL )
		pDoc = m_pTemp;
	else
		pDoc = m_pDoc;
	
  CIntIRect borderGaps( pDoc->GetBorderGaps() );
	m_editTop = borderGaps.top / MM_FACTOR;
	m_editLeft = borderGaps.left / MM_FACTOR;
	m_editRight = borderGaps.right / MM_FACTOR;
	m_editBottom = borderGaps.bottom / MM_FACTOR;

  CSize stampMargins = pDoc->GetStempel()->GetMargins();
  m_editHor = stampMargins.cx / MM_FACTOR;
  m_editVert = stampMargins.cy / MM_FACTOR;
  
  m_stampScale = pDoc->GetStempel()->GetZoomFaktor();
  m_stampCheckbox.SetCheck( pDoc->GetStempel()->GetAlignToProfil() );
  m_checkHor.SetCheck( pDoc->GetStempel()->GetHorizontal() );

  for( int i = 0; i < alignCount; i++ )
  {
    UINT data = m_profilCombo.GetItemData( i );
    if( data == pDoc->GetProfil()->GetAlign() )
      m_profilCombo.SetCurSel( i );
  } // for i

	UpdateData( FALSE );
}

void CMarginsPage::ApplyTemplate(CTemplate *pTemp)
{
	m_pTemp = pTemp;
	Update();
	m_pTemp = NULL;
}

void CMarginsPage::OnChange() 
{
	m_pParent->m_nFormat = 1;
	SetModified( TRUE );
}

BOOL CMarginsPage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}

void CMarginsPage::OnOK()
{
	if (!UpdateData(TRUE))
		return;

  CIntIRect borderGaps( int( m_editLeft * MM_FACTOR ), int( m_editTop * MM_FACTOR ), int( m_editRight * MM_FACTOR ), int( m_editBottom * MM_FACTOR ) );
  m_pDoc->SetBorderGaps( borderGaps );

  CSize stampMargins( int( m_editHor * MM_FACTOR ), int( m_editVert * MM_FACTOR ) );
  m_pDoc->GetStempel()->SetMargins( stampMargins );
  m_pDoc->GetStempel()->SetHorizontal( m_checkHor.GetCheck() );
  m_pDoc->GetStempel()->SetZoomFaktor( m_stampScale );
  m_pDoc->GetProfil()->SetAlign( (CPlotArranger::Align)m_profilCombo.GetItemData( m_profilCombo.GetCurSel() ) );
  m_pDoc->GetStempel()->SetAlignToProfil( m_stampCheckbox.GetCheck() );

	CPropertyPage::OnOK();
}
