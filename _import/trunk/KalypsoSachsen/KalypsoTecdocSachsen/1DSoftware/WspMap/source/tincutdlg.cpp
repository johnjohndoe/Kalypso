// tincutdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "mapdocdata.h"
#include "maplayer.h"
#include "tincutdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDgmDlg 

CDgmDlg::CDgmDlg(CLayerArray* layers, CWnd* pParent /*=NULL*/)
	: CDialog(CDgmDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDgmDlg)
	m_path = _T("");
	//}}AFX_DATA_INIT
  m_layers = layers;
  m_layer = NULL;
}


void CDgmDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDgmDlg)
	DDX_Control(pDX, IDC_HMO_COMBO, m_hmoCombo);
	DDX_Text(pDX, IDC_HMO_PATH, m_path);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDgmDlg, CDialog)
	//{{AFX_MSG_MAP(CDgmDlg)
	ON_CBN_SELCHANGE(IDC_HMO_COMBO, OnSelchangeHmoCombo)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CDgmDlg 

BOOL CDgmDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

  // hmo-combo initialisieren
  POSITION hmoPos = NULL;
  CMapLayer* hmoLayer = m_layers->FindFirstLayer( CLayer::hmo, &hmoPos );
  while (hmoLayer)
  {
    int index = m_hmoCombo.AddString(hmoLayer->GetName());
    m_hmoCombo.SetItemDataPtr( index, hmoLayer );
    hmoLayer = m_layers->FindNextLayer( CLayer::hmo, &hmoPos );
  };

  if( m_hmoCombo.GetCount() == 0 )
    OnCancel(); // gleich den Dialog abbrechen

  m_hmoCombo.SetCurSel(0);
  OnSelchangeHmoCombo();

  if( m_hmoCombo.GetCount() == 1 )
    OnOK();

  return FALSE;
}; // OnInitDialog

void CDgmDlg::OnSelchangeHmoCombo() 
{
	CMapLayer* hmoLayer = (CMapLayer*)m_hmoCombo.GetItemDataPtr(m_hmoCombo.GetCurSel());
  if( hmoLayer )
  {
    m_path = hmoLayer->GetTag();
    m_layer = hmoLayer;
    UpdateData( FALSE );
  };
};
