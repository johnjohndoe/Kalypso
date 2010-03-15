// multiprops.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "draw.h"
#include "multiDoc.h"

#include "multiprops.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMultiProps 


CMultiProps::CMultiProps( CMultiDoc* pDoc, CWnd* pParent /*=NULL*/ )	: CDialog(CMultiProps::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMultiProps)
	m_xDist = 0;
	m_distXStatic = _T("");
	m_yDist = 0;
	m_distYStatic = _T("");
	m_title = _T("");
	m_titleStatic = _T("");
	//}}AFX_DATA_INIT

  ASSERT( pDoc );
  m_pDoc = pDoc;
  
  m_title = pDoc->GetPageTitle();;
  m_xDist = pDoc->GetGaps().cx / MM_FACTOR;
  m_yDist = pDoc->GetGaps().cy / MM_FACTOR;

  // Internationale Texte setzen
  m_distXStatic.LoadString( IDS_MULTI_PROPS_DISTX );
  m_distYStatic.LoadString( IDS_MULTI_PROPS_DISTY );
  m_titleStatic.LoadString( IDS_MULTI_PROPS_TITLE );
}


void CMultiProps::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMultiProps)
	DDX_Control(pDX, IDC_MULTI_PROPS_APPLY, m_applyButton);
	DDX_Control(pDX, IDC_MULTI_PROPS_DIST_STATIC, m_distPanel);
	DDX_Text(pDX, IDC_MULTI_PROPS_DIST_X_EDIT, m_xDist);
	DDV_MinMaxUInt(pDX, m_xDist, 0, 1000);
	DDX_Text(pDX, IDC_MULTI_PROPS_DIST_X_STATIC, m_distXStatic);
	DDX_Text(pDX, IDC_MULTI_PROPS_DIST_Y_EDIT, m_yDist);
	DDV_MinMaxUInt(pDX, m_yDist, 0, 1000);
	DDX_Text(pDX, IDC_MULTI_PROPS_DIST_Y_STATIC, m_distYStatic);
	DDX_Text(pDX, IDC_MULTI_PROPS_TITLE_EDIT, m_title);
	DDX_Text(pDX, IDC_MULTI_PROPS_TITLE_STATIC, m_titleStatic);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMultiProps, CDialog)
	//{{AFX_MSG_MAP(CMultiProps)
	ON_BN_CLICKED(IDC_MULTI_PROPS_APPLY, OnApply)
	ON_EN_CHANGE(IDC_MULTI_PROPS_DIST_X_EDIT, OnChange)
	ON_EN_CHANGE(IDC_MULTI_PROPS_DIST_Y_EDIT, OnChange)
	ON_EN_CHANGE(IDC_MULTI_PROPS_TITLE_EDIT, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMultiProps 

BOOL CMultiProps::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// noch ein paar internationale String
  SetWindowText( CString( MAKEINTRESOURCE( IDS_MULTI_PROPS_DLGTITLE ) ) );
  m_distPanel.SetWindowText( CString( MAKEINTRESOURCE( IDS_MULTI_PROPS_DISTPANEL ) ) );
  m_applyButton.SetWindowText( CString( MAKEINTRESOURCE( IDS_MULTI_PROPS_APPLY ) ) );

  
  SetModified( FALSE );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CMultiProps::OnOK() 
{
  OnApply();
	
	
	CDialog::OnOK();
}

void CMultiProps::OnApply()
{
  if( m_bModified )
  {
    UpdateData( TRUE );

    
    m_pDoc->SetPageTitle( m_title );
    m_pDoc->SetGaps( CSize( m_xDist * MM_FACTOR, m_yDist * MM_FACTOR ) );
    
    m_pDoc->UpdateDrawing();

    SetModified( FALSE );
  };
};

void CMultiProps::OnChange() 
{
  SetModified( TRUE );
}


void CMultiProps::SetModified( const BOOL bMod )
{
  m_bModified = bMod;
  if( m_applyButton.GetSafeHwnd() )
    m_applyButton.EnableWindow( bMod );
};