#include "stdafx.h"

#include "brkssblp.h"
#include "snglsblp.h"
#include "valsblp.h"
#include "stdlabp.h"
#include "advlabp.h"
#include "maplayer.h"

#include "mlpdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


CMapLayerPropertyDlg::CMapLayerPropertyDlg(CMapLayer* pLayer, long type, CWnd* pParentWnd, UINT iSelectPage )
	:CPropertySheet( IDS_MAP_LAYER_PROPS_TITLE, pParentWnd, iSelectPage)
{
  AddPage( new CSingleSymbolPage( this, pLayer, type ) );
	AddPage( new CValueSymbolPage( this, pLayer, type ) );
	AddPage( new CBreaksSymbolPage( this, pLayer, type ) );
	AddPage( new CStandardLabelPage( this, pLayer ) );
	AddPage( new CAdvancedLabelPage( this, pLayer ) );

  SetWizardMode();

	m_name = pLayer->GetName();
  m_pLayer = pLayer;
}

CMapLayerPropertyDlg::~CMapLayerPropertyDlg()
{
  for( int i = 0; i < GetPageCount(); i++ )
    delete GetPage( i );
}


BEGIN_MESSAGE_MAP(CMapLayerPropertyDlg, CPropertySheet)
	//{{AFX_MSG_MAP(CMapLayerPropertyDlg)
		// HINWEIS - Der Klassen-Assistent fügt hier Zuordnungsmakros ein und entfernt diese.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMapLayerPropertyDlg 

BOOL CMapLayerPropertyDlg::OnInitDialog() 
{
	BOOL bResult = CPropertySheet::OnInitDialog();
	
	SetWizardButtons( PSWIZB_FINISH );
	SetFinishText( "OK" );
  SetTitle( m_pLayer->GetFullGeoDatasetName() );
	
	return bResult;
}


void CMapLayerPropertyDlg::OnChangeRenderType( CPropertyPage* page ) 
{
	int nPage;

	if( ((CButton*)page->GetDlgItem( IDC_RADIO1 ) )->GetCheck() )
		nPage = 0;
	else if( ((CButton*)page->GetDlgItem( IDC_RADIO2 ) )->GetCheck() )
		nPage = 1;
	else if( ((CButton*)page->GetDlgItem( IDC_RADIO3 ) )->GetCheck() )
		nPage = 2;
	else if( ((CButton*)page->GetDlgItem( IDC_RADIO4 ) )->GetCheck() )
		nPage = 3;
	else if( ((CButton*)page->GetDlgItem( IDC_RADIO5 ) )->GetCheck() )
		nPage = 4;

	SetActivePage( nPage );

  // neu setzen, wird von SetActivePage überschrieben
  SetTitle( m_pLayer->GetFullGeoDatasetName() );
}
