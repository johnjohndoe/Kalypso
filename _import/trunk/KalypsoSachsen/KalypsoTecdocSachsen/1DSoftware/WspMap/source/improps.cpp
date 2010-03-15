#include "stdafx.h"

#include "resource.h"

#include "imlayer.h"

#include "improps.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CImageProperties 


CImageProperties::CImageProperties( CImageLayer* pLayer, CWnd* pParent /*=NULL*/ ) : CDialog( CImageProperties::IDD, pParent )
{
	//{{AFX_DATA_INIT(CImageProperties)
	m_transparent = FALSE;
	m_name = _T("");
	m_file = _T("");
	m_staticColor = _T("");
	m_staticFile = _T("");
	m_staticName = _T("");
	//}}AFX_DATA_INIT

  m_staticColor = CString( MAKEINTRESOURCE( IDS_IMAGE_PROPS_COLOR ) );
  m_staticFile = CString( MAKEINTRESOURCE( IDS_IMAGE_PROPS_FILE ) );
  m_staticName = CString( MAKEINTRESOURCE( IDS_IMAGE_PROPS_NAME ) );


  m_pLayer = pLayer;
}


void CImageProperties::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CImageProperties)
	DDX_Control(pDX, IDC_IMAGE_PROPS_BUTTON, m_color );
	DDX_Check(pDX, IDC_IMAGE_PROPS_CHECK_TRANSPARENT, m_transparent );
	DDX_Text(pDX, IDC_IMAGE_PROPS_EDIT, m_name );
	DDX_Text(pDX, IDC_IMAGE_PROPS_EDIT_FILE, m_file);
	DDX_Text(pDX, IDC_IMAGE_PROPS_STATIC_COLOR, m_staticColor);
	DDX_Text(pDX, IDC_IMAGE_PROPS_STATIC_FILE, m_staticFile);
	DDX_Text(pDX, IDC_IMAGE_PROPS_STATIC_NAME, m_staticName);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CImageProperties, CDialog)
	//{{AFX_MSG_MAP(CImageProperties)
	ON_BN_CLICKED(IDC_IMAGE_PROPS_BUTTON, OnTransparentColor)
	ON_BN_CLICKED(IDC_IMAGE_PROPS_CHECK_TRANSPARENT, OnTransparent)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CImageProperties 

BOOL CImageProperties::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  SetWindowText( CString( MAKEINTRESOURCE( IDS_IMAGE_PROPS_TITLE ) ) );

	m_name = m_pLayer->GetName();

	m_transparent = m_pLayer->GetTransparent();
	m_color.SetFaceColor( m_pLayer->GetTransparentColor() );
  m_file = m_pLayer->GetFullGeoDatasetName();

  GetDlgItem( IDC_IMAGE_PROPS_CHECK_TRANSPARENT )->SetWindowText( CString( MAKEINTRESOURCE( IDS_IMAGE_PROPS_TRANSPARENT ) ) );

	UpdateData( FALSE );

	OnTransparent();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CImageProperties::OnTransparentColor() 
{
	CColorDialog dlg( m_color.colGetFaceColor(), 0, this );

	if( dlg.DoModal() == IDOK )
	{
		m_color.SetFaceColor( dlg.GetColor() );
		m_color.RedrawWindow();
	}
}

void CImageProperties::OnTransparent() 
{
	if( ( (CButton*)GetDlgItem( IDC_IMAGE_PROPS_CHECK_TRANSPARENT) )->GetCheck() == 1 )
		m_color.EnableWindow( TRUE );
	else
		m_color.EnableWindow( FALSE );
}

void CImageProperties::OnOK() 
{
	if( !UpdateData() )
		return;

	m_pLayer->SetName( m_name );
	m_pLayer->SetTransparent( m_transparent );
	m_pLayer->SetTransparentColor( m_color.colGetFaceColor() );
	
	CDialog::OnOK();
}
