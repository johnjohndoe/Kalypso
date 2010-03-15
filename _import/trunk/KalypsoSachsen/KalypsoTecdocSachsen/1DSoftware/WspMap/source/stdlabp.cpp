#include "stdafx.h"

#include "resource.h"

#include "maphelper.h"
#include "mapLayer.h"
#include "mlpdlg.h"

#include "stdlabp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CStandardLabelPage::CStandardLabelPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer ) : CPropertyPage( CStandardLabelPage::IDD )
{
	//{{AFX_DATA_INIT(CStandardLabelPage)
	m_nRenderType = 3;
	m_name = _T("");
	m_drawBackground = FALSE;
	m_allowDuplicates = FALSE;
	m_splinedText = FALSE;
	m_flip = FALSE;
	m_rotation = 0;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pLayer = pLayer;

	m_pRenderer = new CMapRenderer( *pLayer->GetMapRenderer() );
	if( m_pRenderer->GetRendererType() != CMapRenderer::standardLabel )
		m_pRenderer->SetRendererType( CMapRenderer::standardLabel );
}

CStandardLabelPage::~CStandardLabelPage()
{
	delete m_pRenderer;
}

void CStandardLabelPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStandardLabelPage)
	DDX_Control(pDX, IDC_SLIDER1, m_slider);
	DDX_Control(pDX, IDC_COMBO6, m_fittedField);
	DDX_Control(pDX, IDC_COMBO5, m_YOffsetField);
	DDX_Control(pDX, IDC_COMBO4, m_XOffsetField);
	DDX_Control(pDX, IDC_COMBO3, m_vertAlign);
	DDX_Control(pDX, IDC_COMBO2, m_horzAlign);
	DDX_Control(pDX, IDC_COMBO1, m_textField);
	DDX_Radio(pDX, IDC_RADIO1, m_nRenderType);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDX_Check(pDX, IDC_CHECK1, m_drawBackground);
	DDX_Check(pDX, IDC_CHECK2, m_allowDuplicates);
	DDX_Check(pDX, IDC_CHECK3, m_splinedText);
	DDX_Check(pDX, IDC_CHECK4, m_flip);
	DDX_Text(pDX, IDC_EDIT2, m_rotation);
	DDV_MinMaxInt(pDX, m_rotation, 0, 359);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStandardLabelPage, CPropertyPage)
	//{{AFX_MSG_MAP(CStandardLabelPage)
	ON_BN_CLICKED(IDC_RADIO1, OnChangeRenderType)
	ON_BN_CLICKED(IDC_BUTTON1, OnFont)
	ON_WM_HSCROLL()
	ON_CBN_KILLFOCUS(IDC_COMBO1, OnKillfocusCombo1)
	ON_CBN_KILLFOCUS(IDC_COMBO4, OnKillfocusCombo4)
	ON_CBN_KILLFOCUS(IDC_COMBO5, OnKillfocusCombo5)
	ON_CBN_KILLFOCUS(IDC_COMBO6, OnKillfocusCombo6)
	ON_BN_CLICKED(IDC_RADIO2, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO3, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO4, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO5, OnChangeRenderType)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeName)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CStandardLabelPage 

BOOL CStandardLabelPage::OnInitDialog() 
{
	CString str;
	int i, sel1, sel2, sel3, sel4;
	
	CPropertyPage::OnInitDialog();
	
	m_nRenderType = 3;
	m_name = m_pParent->m_name;

	sel1 = sel2 = sel3 = sel4 = -1;
	CMoRecordset recs(m_pLayer->GetRecords());
	CMoFields fields(recs.GetFields());
	i = 0;
	FOR_EACH_IN(CMoField, field, fields)
		str = field.GetName();
		if (str!=MO2_FIELD_SHAPE)
		{
			m_textField.AddString(str);
			m_XOffsetField.AddString(str);
			m_YOffsetField.AddString(str);
			m_fittedField.AddString(str);
			if (str==m_pRenderer->GetField(0))
				sel1 = i;
			if (str==m_pRenderer->GetField(1))
				sel2 = i;
			if (str==m_pRenderer->GetField(2))
				sel3 = i;
			if (str==m_pRenderer->GetField(3))
				sel4 = i;
			i++;
		}
	END_FOR
	
	m_slider.SetRange(0, 359, TRUE);
	m_slider.SetTicFreq(15);

	m_textField.SetCurSel(sel1);
	m_XOffsetField.SetCurSel(sel2);
	m_YOffsetField.SetCurSel(sel3);
	m_fittedField.SetCurSel(sel4);

	switch( m_pRenderer->GetConst( 0 ) )
	{
    // obskur: Mapobjects vertausch oben/unten und rechts/links ??!!
		case moAlignRight:
			i = 0;
			break;

		case moAlignCenter:
			i = 1;
			break;

		case moAlignLeft:
			i = 2;
			break;

		default:
			i = 1;
			break;
	}
	m_horzAlign.SetCurSel( i );

	switch( m_pRenderer->GetConst( 1 ) )
	{
		case moAlignBottom:
			i = 0;
			break;

		case moAlignCenter:
			i = 1;
			break;

		case moAlignTop:
			i = 2;
			break;

		case moAlignBaseline:
			i = 3;

		default:
			i = 1;
			break;
	}
	m_vertAlign.SetCurSel( i );

	m_drawBackground = m_pRenderer->GetBool(0);
	m_allowDuplicates = m_pRenderer->GetBool(1);
	m_splinedText = m_pRenderer->GetBool(2);
	m_flip = m_pRenderer->GetBool(3);
	m_rotation = m_pRenderer->GetConst(2);
	m_slider.SetPos(m_rotation);

	m_logfont = m_pRenderer->GetFont();
	m_colorText = m_pRenderer->GetColor(0);

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CStandardLabelPage::OnSetActive() 
{
	m_nRenderType = 3;
	m_name = m_pParent->m_name;

	return CPropertyPage::OnSetActive();
}

void CStandardLabelPage::OnChangeName() 
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_pParent->m_name);
}

void CStandardLabelPage::OnChangeRenderType() 
{
  m_pParent->OnChangeRenderType( this );
}

BOOL CStandardLabelPage::OnWizardFinish() 
{
	CString str;
	int i;

	if( !UpdateData() )
		return FALSE;
	
	m_pLayer->SetName(m_name);

	m_textField.GetWindowText(str);
	m_pRenderer->SetField(0, str);

	if (m_pRenderer->GetField(0).IsEmpty())
		m_pRenderer->SetRendererType(CMapRenderer::singleSymbol);
	else
	{
		m_XOffsetField.GetWindowText(str);
		m_pRenderer->SetField(1, str);
		m_YOffsetField.GetWindowText(str);
		m_pRenderer->SetField(2, str);
		m_fittedField.GetWindowText(str);
		m_pRenderer->SetField(3, str);
		switch (m_horzAlign.GetCurSel())
		{
      // obskur: Mapobjects vertausch oben/unten und rechts/links ??!!
		case 0:
			i = moAlignRight;
			break;
			
		case 1:
			i = moAlignCenter;
			break;
			
		case 2:
			i = moAlignLeft;
			break;
			
		default:
			i = moAlignCenter;
			break;
		}
		m_pRenderer->SetConst(0, i);
		switch (m_vertAlign.GetCurSel())
		{
		case 0:
			i = moAlignBottom;
			break;
			
		case 1:
			i = moAlignCenter;
			break;
			
		case 2:
			i = moAlignTop;
			break;
			
		case 3:
			i = moAlignBaseline;
			break;
			
		default:
			i = moAlignCenter;
			break;
		}
		m_pRenderer->SetConst(1, i);
		m_pRenderer->SetBool(0, m_drawBackground);
		m_pRenderer->SetBool(1, m_allowDuplicates);
		m_pRenderer->SetBool(2, m_splinedText);
		m_pRenderer->SetBool(3, m_flip);
		m_pRenderer->SetConst(2, m_rotation);
		m_pRenderer->SetFont(m_logfont);
		m_pRenderer->SetColor(0, m_colorText);
	}

	m_pLayer->SetMapRenderer( new CMapRenderer( *m_pRenderer ) );

	return CPropertyPage::OnWizardFinish();
}

void CStandardLabelPage::OnFont() 
{
	CFontDialog dlg(&m_logfont, CF_EFFECTS | CF_BOTH, NULL, this);
	dlg.m_cf.rgbColors = m_colorText;

	if (dlg.DoModal() != IDOK)
		return;

	m_logfont = dlg.m_lf;
	m_colorText = dlg.m_cf.rgbColors;
	SetModified(TRUE);
}

void CStandardLabelPage::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	SetModified(TRUE);

	CString str;
	int n;

	GetDlgItem(IDC_EDIT2)->GetWindowText(str);
	n = atoi(str);
	if (n!=m_slider.GetPos())
	{
		str.Format("%d", m_slider.GetPos());
		GetDlgItem(IDC_EDIT2)->SetWindowText(str);
	}
	
	CPropertyPage::OnHScroll(nSBCode, nPos, pScrollBar);
}

void CStandardLabelPage::OnKillfocusCombo1() 
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
			SetModified(TRUE);
	}
	else if (!m_pRenderer->GetField(0).IsEmpty())
		SetModified(TRUE);
}

void CStandardLabelPage::OnKillfocusCombo4() 
{
	CString str, comp;
	int i;

	GetDlgItem(IDC_COMBO4)->GetWindowText(str);
	if (!str.IsEmpty())
	{
		m_XOffsetField.SelectString(-1, str);
		for (i=0; i<m_XOffsetField.GetCount(); i++)
		{
			m_XOffsetField.GetLBText(i, comp);
			if (comp.CompareNoCase(str)==0)
			{
				m_XOffsetField.SetCurSel(i);
				break;
			}
		}
		m_XOffsetField.GetWindowText(str);
		if (m_pRenderer->GetField(1)!=str)
			SetModified(TRUE);
	}
	else if (!m_pRenderer->GetField(1).IsEmpty())
		SetModified(TRUE);
}

void CStandardLabelPage::OnKillfocusCombo5() 
{
	CString str, comp;
	int i;

	GetDlgItem(IDC_COMBO5)->GetWindowText(str);
	if (!str.IsEmpty())
	{
		m_YOffsetField.SelectString(-1, str);
		for (i=0; i<m_YOffsetField.GetCount(); i++)
		{
			m_YOffsetField.GetLBText(i, comp);
			if (comp.CompareNoCase(str)==0)
			{
				m_YOffsetField.SetCurSel(i);
				break;
			}
		}
		m_YOffsetField.GetWindowText(str);
		if (m_pRenderer->GetField(2)!=str)
			SetModified(TRUE);
	}
	else if (!m_pRenderer->GetField(2).IsEmpty())
		SetModified(TRUE);
}

void CStandardLabelPage::OnKillfocusCombo6() 
{
	CString str, comp;
	int i;

	GetDlgItem(IDC_COMBO6)->GetWindowText(str);
	if (!str.IsEmpty())
	{
		m_fittedField.SelectString(-1, str);
		for (i=0; i<m_fittedField.GetCount(); i++)
		{
			m_fittedField.GetLBText(i, comp);
			if (comp.CompareNoCase(str)==0)
			{
				m_fittedField.SetCurSel(i);
				break;
			}
		}
		m_fittedField.GetWindowText(str);
		if (m_pRenderer->GetField(3)!=str)
			SetModified(TRUE);
	}
	else if (!m_pRenderer->GetField(3).IsEmpty())
		SetModified(TRUE);
}
