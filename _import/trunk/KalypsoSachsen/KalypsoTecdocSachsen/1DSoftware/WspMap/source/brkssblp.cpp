// brkssblp.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "maphelper.h"

#include "mlpdlg.h"
#include "mapLayer.h"

#include "brkssblp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CBreaksSymbolPage::CBreaksSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer,	long type ) : CPropertyPage( CBreaksSymbolPage::IDD )
{
	//{{AFX_DATA_INIT(CBreaksSymbolPage)
	m_nRenderType = 2;
	m_name = _T("");
	m_breakCount = 5;
	m_removeOutline = FALSE;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pLayer = pLayer;
	m_nShapeType = type;
	
  // initialise temporary map layer
	m_pMapLayer = new CMapLayer( m_pLayer->GetBaseDirectory() );
	m_pMapLayer->SetGeoDatasetName( m_pLayer->GetGeoDatasetName() );
	m_pMapLayer->SetName( m_pLayer->GetName() );
	m_pMapLayer->SetColor( m_pLayer->GetColor() );
	m_pMapLayer->SetStyle( m_pLayer->GetStyle() );
	m_pMapLayer->SetOutlineColor( m_pLayer->GetOutlineColor() );
	m_pMapLayer->SetSize( m_pLayer->GetSize() );
	m_pRenderer = new CMapRenderer( *m_pLayer->GetMapRenderer() );
	m_pMapLayer->SetMapRenderer( m_pRenderer );
	if( m_pRenderer->GetRendererType() != CMapRenderer::classBreaks )
	{
		m_pRenderer->SetRendererType( CMapRenderer::classBreaks );
		m_pRenderer->SetConst( 1, m_pLayer->GetSize() );
		m_pRenderer->SetConst( 2, m_pLayer->GetSize() );
	}
}

CBreaksSymbolPage::~CBreaksSymbolPage()
{
  delete m_pMapLayer;
}

void CBreaksSymbolPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CBreaksSymbolPage)
	DDX_Control(pDX, IDC_BUTTON2, m_endColor);
	DDX_Control(pDX, IDC_BUTTON1, m_startColor);
	DDX_Control(pDX, IDC_COMBO3, m_endSize);
	DDX_Control(pDX, IDC_COMBO2, m_startSize);
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_COMBO1, m_textField);
	DDX_Radio(pDX, IDC_RADIO1, m_nRenderType);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDX_Text(pDX, IDC_EDIT2, m_breakCount);
	DDV_MinMaxInt(pDX, m_breakCount, 3, 10);
	DDX_Control(pDX, IDC_LEGEND1, m_legend);
	DDX_Control(pDX, IDC_MAP1, m_map);
	DDX_Check(pDX, IDC_CHECK1, m_removeOutline);
	//}}AFX_DATA_MAP
}

void CBreaksSymbolPage::UpdateLegend()
{
	BOOL bShow = FALSE;

	m_pMapLayer->ApplyRenderer( m_map, TRUE );

	m_legend.RemoveAll();
	if( !m_pRenderer->GetField(0).IsEmpty() )
		m_legend.LoadLegend( &bShow );
}

BEGIN_MESSAGE_MAP(CBreaksSymbolPage, CPropertyPage)
	//{{AFX_MSG_MAP(CBreaksSymbolPage)
	ON_BN_CLICKED(IDC_RADIO1, OnChangeRenderType)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeName)
	ON_BN_CLICKED(IDC_BUTTON1, OnStartColor)
	ON_BN_CLICKED(IDC_BUTTON2, OnEndColor)
	ON_CBN_KILLFOCUS(IDC_COMBO1, OnKillfocusCombo1)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeCombo1)
	ON_CBN_SELCHANGE(IDC_COMBO2, OnChangeCombo2)
	ON_CBN_EDITCHANGE(IDC_COMBO3, OnChangeCombo3)
	ON_EN_KILLFOCUS(IDC_EDIT2, OnChangeEdit2)
	ON_BN_CLICKED(IDC_RADIO2, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO3, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO4, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO5, OnChangeRenderType)
	ON_CBN_KILLFOCUS(IDC_COMBO2, OnChangeCombo2)
	ON_CBN_KILLFOCUS(IDC_COMBO3, OnChangeCombo3)
	ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN1, OnChangeEdit2)
	ON_BN_CLICKED(IDC_CHECK1, OnCheck1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CBreaksSymbolPage 

BOOL CBreaksSymbolPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

  m_nRenderType = 2;
	m_name = m_pParent->m_name;

	CMoRecordset recs( m_pLayer->GetRecords() );
	CMoFields fields( recs.GetFields() );
	
  int i = 0;
  int sel1 = -1;
	FOR_EACH_IN( CMoField, field, fields )
		if( field.GetType() == moLong || field.GetType() == moDouble )
		{
			CString str = field.GetName();
			m_textField.AddString( str );
			if( str == m_pRenderer->GetField( 0 ) )
				sel1 = i;
			i++;
		}
	END_FOR

	if( m_nShapeType == moLine )
		GetDlgItem( IDC_CHECK1 )->ShowWindow( FALSE );


	BOOL bShow = FALSE;

	m_textField.SetCurSel( sel1 );
	m_breakCount = m_pRenderer->GetConst( 0 );
	m_spin.SetBuddy( GetDlgItem( IDC_EDIT2 ) );
	m_spin.SetRange( 3, 10 );
	m_spin.SetBase( 10 );
	m_spin.SetPos( m_breakCount );
	m_startColor.SetFaceColor( m_pRenderer->GetColor( 0 ) );
	m_endColor.SetFaceColor( m_pRenderer->GetColor( 1 ) );
	
  i = m_pRenderer->GetConst( 1 );
	if( i > 0 )
		i--;
	else
		i = 0;
	
  if( i > 9 )
		i = 9;
  m_startSize.SetCurSel( i );
	
  i = m_pRenderer->GetConst( 2 );
	if( i > 0 )
		i--;
	else
		i = 0;
	
  if( i > 9 )
		i = 9;
	m_endSize.SetCurSel( i );

  m_removeOutline = m_pRenderer->GetBool( 0 );
	
	LPDISPATCH lpDispatch = GetIDispatchFromCWnd( &m_map );
	if( lpDispatch )
		m_legend.setMapSource((LPDISPATCH*)&lpDispatch);

	m_legend.SetEnableDragDrop(FALSE);
	CMoLayers layers(m_map.GetLayers());
	layers.Add(m_pMapLayer->GetDispatch());

	UpdateLegend();

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CBreaksSymbolPage::OnSetActive() 
{
	m_nRenderType = 2;
	m_name = m_pParent->m_name;

	return CPropertyPage::OnSetActive();
}

void CBreaksSymbolPage::OnChangeName() 
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_pParent->m_name);
}

void CBreaksSymbolPage::OnChangeRenderType() 
{
  m_pParent->OnChangeRenderType( this );
}

void CBreaksSymbolPage::OnStartColor() 
{
	CColorDialog dlg(m_startColor.colGetFaceColor(), 0, this);

	if (dlg.DoModal()==IDOK)
	{
		m_startColor.SetFaceColor(dlg.GetColor());
		m_startColor.RedrawWindow();
		m_pRenderer->SetColor(0, dlg.GetColor());
		UpdateLegend();
		SetModified(TRUE);
	}
}

void CBreaksSymbolPage::OnEndColor() 
{
	CColorDialog dlg(m_endColor.colGetFaceColor(), 0, this);

	if (dlg.DoModal()==IDOK)
	{
		m_endColor.SetFaceColor(dlg.GetColor());
		m_endColor.RedrawWindow();
		m_pRenderer->SetColor(1, dlg.GetColor());
		UpdateLegend();
		SetModified(TRUE);
	}
}

BOOL CBreaksSymbolPage::OnWizardFinish() 
{
	CString str;

	if (!UpdateData())
		return FALSE;
	
	m_pLayer->SetName(m_name);

	m_textField.GetWindowText(str);
	m_pRenderer->SetField(0, str);

	if (m_pRenderer->GetField(0).IsEmpty())
		m_pRenderer->SetRendererType(CMapRenderer::singleSymbol);
	else
	{
		m_pRenderer->SetConst(0, m_breakCount);
		m_pRenderer->SetColor(0, m_startColor.colGetFaceColor());
		m_pRenderer->SetColor(1, m_endColor.colGetFaceColor());
		m_pRenderer->SetConst(1, m_startSize.GetCurSel()+1);
		m_pRenderer->SetConst(2, m_endSize.GetCurSel()+1);
		m_pRenderer->SetBool(0, m_removeOutline);
	}

	m_pLayer->SetMapRenderer( new CMapRenderer( *m_pRenderer ) );

	return CPropertyPage::OnWizardFinish();
}

void CBreaksSymbolPage::OnKillfocusCombo1() 
{
	CString str, comp;
	int i;

	GetDlgItem(IDC_COMBO1)->GetWindowText(str);
	if (!str.IsEmpty())
	{
		m_textField.SelectString(-1, str);
		for (i=0; i<m_textField.GetCount(); i++)
		{
			m_textField.GetLBText(i, comp);
			if (comp.CompareNoCase(str)==0)
			{
				m_textField.SetCurSel(i);
				break;
			}
		}
		m_textField.GetWindowText(str);
		if (m_pRenderer->GetField(0)!=str)
		{
			OnSelchangeCombo1();
			SetModified(TRUE);
		}
	}
	else if (!m_pRenderer->GetField(0).IsEmpty())
	{
		OnSelchangeCombo1();
		SetModified(TRUE);
	}
}

void CBreaksSymbolPage::OnSelchangeCombo1() 
{
	CString str;
	int i;

	i = m_textField.GetCurSel();
	if (i!=-1)
		m_textField.GetLBText(i, str);
	m_pRenderer->SetField(0, str);

	UpdateLegend();
}

void CBreaksSymbolPage::OnChangeEdit2() 
{
	CString str;
	int n;

	GetDlgItem(IDC_EDIT2)->GetWindowText(str);
	n = atoi(str);
	if (n<3)
		n = 3;
	if (n>10)
		n = 10;
	if (n!=atoi(str))
	{
		str.Format("%d", n);
		GetDlgItem(IDC_EDIT2)->SetWindowText(str);
	}
	m_pRenderer->SetConst(0, n);
	UpdateLegend();
}

void CBreaksSymbolPage::OnChangeCombo2() 
{
	CString str;
	int n;

	GetDlgItem(IDC_COMBO2)->GetWindowText(str);
	n = atoi(str);
	if (n<1)
		n = 1;
	if (n>10)
		n = 10;
	if (n!=atoi(str))
	{
		str.Format("%d", n);
		GetDlgItem(IDC_COMBO2)->SetWindowText(str);
	}
	m_pRenderer->SetConst(1, n);
	UpdateLegend();
}

void CBreaksSymbolPage::OnChangeCombo3() 
{
	CString str;
	int n;

	GetDlgItem(IDC_COMBO3)->GetWindowText(str);
	n = atoi(str);
	if (n<1)
		n = 1;
	if (n>10)
		n = 10;
	if (n!=atoi(str))
	{
		str.Format("%d", n);
		GetDlgItem(IDC_COMBO3)->SetWindowText(str);
	}
	m_pRenderer->SetConst(2, n);
	UpdateLegend();
}

void CBreaksSymbolPage::OnCheck1() 
{
	m_pRenderer->SetBool(0, ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck()==1);
	UpdateLegend();
}
