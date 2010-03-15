// advlabp.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "maphelper.h"
#include "mlpdlg.h"
#include "mapRend.h"
#include "mapLayer.h"

#include "advlabp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CAdvancedLabelPage 

CAdvancedLabelPage::CAdvancedLabelPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer ) : CPropertyPage( CAdvancedLabelPage::IDD )
{
	//{{AFX_DATA_INIT(CAdvancedLabelPage)
	m_nRenderType = 4;
	m_position = 0;
	m_drawBackground = FALSE;
	m_allowDuplicates = FALSE;
	m_bScale = FALSE;
	m_bMask = FALSE;
	m_name = _T("");
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pLayer = pLayer;

	m_pRenderer = new CMapRenderer( *pLayer->GetMapRenderer() );
	if( m_pRenderer->GetRendererType() != CMapRenderer::advancedLabel )
		m_pRenderer->SetRendererType( CMapRenderer::advancedLabel );
}

CAdvancedLabelPage::~CAdvancedLabelPage()
{
	delete m_pRenderer;
}

void CAdvancedLabelPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAdvancedLabelPage)
	DDX_Control(pDX, IDC_SLIDER3, m_scale);
	DDX_Control(pDX, IDC_SLIDER2, m_size);
	DDX_Control(pDX, IDC_BUTTON2, m_maskColor);
	DDX_Control(pDX, IDC_SLIDER1, m_distance);
	DDX_Control(pDX, IDC_COMBO1, m_textField);
	DDX_Radio(pDX, IDC_RADIO1, m_nRenderType);
	DDX_Radio(pDX, IDC_RADIO6, m_position);
	DDX_Check(pDX, IDC_CHECK1, m_drawBackground);
	DDX_Check(pDX, IDC_CHECK2, m_allowDuplicates);
	DDX_Check(pDX, IDC_CHECK4, m_bScale);
	DDX_Check(pDX, IDC_CHECK3, m_bMask);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CAdvancedLabelPage, CPropertyPage)
	//{{AFX_MSG_MAP(CAdvancedLabelPage)
	ON_BN_CLICKED(IDC_RADIO1, OnChangeRenderType)
	ON_BN_CLICKED(IDC_BUTTON1, OnFont)
	ON_BN_CLICKED(IDC_BUTTON2, OnMaskColor)
	ON_BN_CLICKED(IDC_CHECK4, OnScale)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeName)
	ON_CBN_KILLFOCUS(IDC_COMBO1, OnKillfocusCombo1)
	ON_BN_CLICKED(IDC_RADIO2, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO3, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO4, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO5, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO6, OnRadioPosition)
	ON_BN_CLICKED(IDC_RADIO7, OnRadioPosition)
	ON_BN_CLICKED(IDC_RADIO8, OnRadioPosition)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CAdvancedLabelPage 

BOOL CAdvancedLabelPage::OnInitDialog() 
{
	CString str;
	int i, sel1 = -1;

	CPropertyPage::OnInitDialog();
	
	m_nRenderType = 4;
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
	m_distance.SetRange(0, 72, TRUE);
	m_distance.SetTicFreq(6);
	m_size.SetRange(-100, 0);
	m_size.SetTicFreq(10);
	m_scale.SetRange(-100, 0);
	m_scale.SetTicFreq(10);

	m_textField.SetCurSel(sel1);
	m_drawBackground = m_pRenderer->GetBool(0);
	m_allowDuplicates = m_pRenderer->GetBool(1);

	m_logfont = m_pRenderer->GetFont();
	m_colorText = m_pRenderer->GetColor(0);

	if (m_pRenderer->GetBool(4))
		m_position = 0;
	else if (m_pRenderer->GetBool(5))
		m_position = 1;
	else
		m_position = 2;
	m_distance.SetPos(m_pRenderer->GetConst(0));
	m_bMask = m_pRenderer->GetBool(2);
	m_maskColor.SetFaceColor(m_pRenderer->GetColor(1));
	m_maskColor.RedrawWindow();
	m_size.SetPos(-m_pRenderer->GetConst(1));
	m_bScale = m_pRenderer->GetBool(3);
	m_scale.SetPos(-m_pRenderer->GetConst(2));

	UpdateData(FALSE);

	OnScale();

	SetModified(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CAdvancedLabelPage::OnSetActive() 
{
	m_nRenderType = 4;
	m_name = m_pParent->m_name;

	return CPropertyPage::OnSetActive();
}

void CAdvancedLabelPage::OnChangeName() 
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_pParent->m_name);
}

void CAdvancedLabelPage::OnChangeRenderType() 
{
  m_pParent->OnChangeRenderType( this );
}

void CAdvancedLabelPage::OnFont() 
{
	CFontDialog dlg(&m_logfont, CF_EFFECTS | CF_BOTH, NULL, this);
	dlg.m_cf.rgbColors = m_colorText;

	if (dlg.DoModal() != IDOK)
		return;

	m_logfont = dlg.m_lf;
	m_colorText = dlg.m_cf.rgbColors;
	SetModified(TRUE);
}

void CAdvancedLabelPage::OnMaskColor() 
{
	CColorDialog dlg(m_maskColor.colGetFaceColor(), 0, this);

	if (dlg.DoModal()==IDOK)
	{
		m_maskColor.SetFaceColor(dlg.GetColor());
		m_maskColor.RedrawWindow();
		SetModified(TRUE);
	}
}

void CAdvancedLabelPage::OnScale() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK4))->GetCheck()==1)
	{
		GetDlgItem(IDC_SLIDER2)->EnableWindow(FALSE);
		GetDlgItem(IDC_SLIDER3)->EnableWindow(TRUE);
	}
	else
	{
		GetDlgItem(IDC_SLIDER2)->EnableWindow(TRUE);
		GetDlgItem(IDC_SLIDER3)->EnableWindow(FALSE);
	}
	SetModified(TRUE);
}

BOOL CAdvancedLabelPage::OnWizardFinish() 
{
	CString str;

	if( !UpdateData() )
		return FALSE;
	
	m_pLayer->SetName( m_name );

	m_textField.GetWindowText( str );
	m_pRenderer->SetField( 0, str );

	if( m_pRenderer->GetField(0).IsEmpty() )
		m_pRenderer->SetRendererType( CMapRenderer::singleSymbol );
	else
	{
		m_pRenderer->SetBool( 0, m_drawBackground );
		m_pRenderer->SetBool( 1, m_allowDuplicates );
		m_pRenderer->SetFont( m_logfont );
		m_pRenderer->SetColor( 0, m_colorText );

    m_pRenderer->SetBool( 4, m_position == 0 );
    m_pRenderer->SetBool( 5, m_position == 1 );
    m_pRenderer->SetBool( 6, m_position == 2 );

		m_pRenderer->SetConst( 0, m_distance.GetPos() );
		m_pRenderer->SetBool( 2, m_bMask );
		m_pRenderer->SetColor( 1, m_maskColor.colGetFaceColor() );
		
		m_pRenderer->SetConst( 1, -m_size.GetPos() );
		m_pRenderer->SetBool( 3, m_bScale );
		m_pRenderer->SetConst( 2, -m_scale.GetPos() );
	}

	m_pLayer->SetMapRenderer( new CMapRenderer( *m_pRenderer ) );
	
	return CPropertyPage::OnWizardFinish();
}

void CAdvancedLabelPage::OnKillfocusCombo1() 
{
	CString str, comp;

	GetDlgItem(IDC_COMBO1)->GetWindowText( str );
	if (!str.IsEmpty())
	{
		m_textField.SelectString( -1, str );
		for( int i = 0; i < m_textField.GetCount(); i++ )
		{
			m_textField.GetLBText( i, comp );
			if( comp.CompareNoCase( str ) == 0 )
			{
				m_textField.SetCurSel( i );
				break;
			}
		}
		m_textField.GetWindowText( str );
		if( m_pRenderer->GetField( 0 ) != str )
			SetModified( TRUE );
	}
	else if( !m_pRenderer->GetField( 0 ).IsEmpty() )
		SetModified( TRUE );
}

void CAdvancedLabelPage::OnRadioPosition() 
{
	SetModified( TRUE );
}
