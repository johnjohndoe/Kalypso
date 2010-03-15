// valsblp.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "maphelper.h"

#include "mapLayer.h"
#include "mlpdlg.h"

#include "valsblp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CValueSymbolPage::CValueSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer, long type ) : CPropertyPage( CValueSymbolPage::IDD )
{
	//{{AFX_DATA_INIT(CValueSymbolPage)
	m_nRenderType = 1;
	m_name = _T("");
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
	if( m_pRenderer->GetRendererType() != CMapRenderer::valueMap )
		m_pRenderer->SetRendererType( CMapRenderer::valueMap );
}

CValueSymbolPage::~CValueSymbolPage()
{
	delete m_pMapLayer;
}

void CValueSymbolPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CValueSymbolPage)
	DDX_Control(pDX, IDC_COMBO1, m_textField);
	DDX_Radio(pDX, IDC_RADIO1, m_nRenderType);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDX_Control(pDX, IDC_LEGEND1, m_legend);
	DDX_Check(pDX, IDC_CHECK1, m_removeOutline);
	DDX_Control(pDX, IDC_MAP1, m_map);
	//}}AFX_DATA_MAP
}


void CValueSymbolPage::UpdateLegend()
{
	BOOL bShow = FALSE;

	m_pMapLayer->ApplyRenderer( m_map, TRUE );

	m_legend.RemoveAll();
	if (!m_pRenderer->GetField(0).IsEmpty())
		m_legend.LoadLegend(&bShow);
}

BEGIN_MESSAGE_MAP(CValueSymbolPage, CPropertyPage)
	//{{AFX_MSG_MAP(CValueSymbolPage)
	ON_BN_CLICKED(IDC_RADIO1, OnChangeRenderType)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeName)
	ON_CBN_KILLFOCUS(IDC_COMBO1, OnKillfocusCombo1)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeCombo1)
	ON_BN_CLICKED(IDC_RADIO2, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO3, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO4, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO5, OnChangeRenderType)
	ON_BN_CLICKED(IDC_CHECK1, OnCheck1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CValueSymbolPage 

BOOL CValueSymbolPage::OnInitDialog() 
{
	CString str;
	BOOL bShow = FALSE;
	int i, sel1 = -1;
	
	CPropertyPage::OnInitDialog();
	
	m_nRenderType = 1;
	m_name = m_pParent->m_name;

	CMoRecordset recs(m_pLayer->GetRecords());
	CMoFields fields(recs.GetFields());
	i = 0;
	FOR_EACH_IN(CMoField, field, fields)
		str = field.GetName();
		if (str!=MO2_FIELD_SHAPE)
		{
			m_textField.AddString(str);
			if (str==m_pRenderer->GetField(0))
				sel1 = i;
			i++;
		}
	END_FOR

	if (m_nShapeType==moLine)
		GetDlgItem(IDC_CHECK1)->ShowWindow(FALSE);

	m_textField.SetCurSel(sel1);
	m_removeOutline = m_pRenderer->GetBool(0);
	
	LPDISPATCH lpDispatch = GetIDispatchFromCWnd(&m_map);
	if (lpDispatch!=NULL)
		m_legend.setMapSource((LPDISPATCH*)&lpDispatch);

	m_legend.SetEnableDragDrop(FALSE);
	CMoLayers layers(m_map.GetLayers());
	layers.Add(m_pMapLayer->GetDispatch());

	UpdateLegend();
	
	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CValueSymbolPage::OnSetActive() 
{
	m_nRenderType = 1;
	m_name = m_pParent->m_name;

	return CPropertyPage::OnSetActive();
}

void CValueSymbolPage::OnChangeName() 
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_pParent->m_name);
}

void CValueSymbolPage::OnChangeRenderType() 
{
  m_pParent->OnChangeRenderType( this );
}

BOOL CValueSymbolPage::OnWizardFinish() 
{
	CString str;
	short i;

	if (!UpdateData())
		return FALSE;
	
	m_pLayer->SetName(m_name);

	m_textField.GetWindowText(str);
	m_pRenderer->SetField(0, str);

	if (m_pRenderer->GetField(0).IsEmpty())
		m_pRenderer->SetRendererType(CMapRenderer::singleSymbol);
	else
	{
		CArray<COLORREF, COLORREF> valCols;
		CMoValueMapRenderer renderer(m_pMapLayer->GetRenderer());

		for (i=0; i<renderer.GetValueCount(); i++)
		{
			CMoSymbol symbol(renderer.GetSymbol(i));
			valCols.Add(symbol.GetColor());
		}
		m_pRenderer->SetValueColors(valCols);
		m_pRenderer->SetBool(0, m_removeOutline);
	}

  m_pLayer->SetMapRenderer( new CMapRenderer( *m_pRenderer ) );

	return CPropertyPage::OnWizardFinish();
}

void CValueSymbolPage::OnKillfocusCombo1() 
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

void CValueSymbolPage::OnSelchangeCombo1() 
{
	CString str;
	int i;

	i = m_textField.GetCurSel();
	if (i!=-1)
		m_textField.GetLBText(i, str);
	if (str!=m_pRenderer->GetField(0))
	{
		m_pRenderer->SetField(0, str);
		m_pRenderer->ClearValueColors();
	}

	UpdateLegend();
}

BEGIN_EVENTSINK_MAP(CValueSymbolPage, CPropertyPage)
    //{{AFX_EVENTSINK_MAP(CImagePage)
	ON_EVENT(CValueSymbolPage, IDC_LEGEND1, 2 /* RenderClick */, OnRenderClickLegend1, VTS_PI2 VTS_PI2 VTS_PVARIANT VTS_PVARIANT)
	//}}AFX_EVENTSINK_MAP
END_EVENTSINK_MAP()

void CValueSymbolPage::OnRenderClickLegend1(short FAR* LayerIndex, short FAR* BreakIndex, VARIANT FAR* val1, VARIANT FAR* val2) 
{
	CMoValueMapRenderer renderer(m_pMapLayer->GetRenderer());

	if (*BreakIndex>=0 && *BreakIndex<renderer.GetValueCount())
	{
		CMoSymbol symbol(renderer.GetSymbol(*BreakIndex));
		CColorDialog dlg(symbol.GetColor(), 0, this);

		if (dlg.DoModal()==IDOK)
		{
			BOOL bShow = FALSE;
			
			symbol.SetColor(dlg.GetColor());
			m_legend.RemoveAll();
			m_legend.LoadLegend(&bShow);
			SetModified(TRUE);
		}
	}
}

void CValueSymbolPage::OnCheck1() 
{
	m_pRenderer->SetBool(0, ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck()==1);
	UpdateLegend();
}
