// snglsblp.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "maplayer.h"
#include "mlpdlg.h"

#include "snglsblp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CSingleSymbolPage::CSingleSymbolPage( CMapLayerPropertyDlg* pParent, CMapLayer* pLayer, long type ) : CPropertyPage( CSingleSymbolPage::IDD )
{
	//{{AFX_DATA_INIT(CSingleSymbolPage)
	m_nRenderType = 0;
	m_name = _T("");
	m_size = 0;
	m_bOutline = FALSE;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pLayer = pLayer;
	m_nShapeType = type;
}

CSingleSymbolPage::~CSingleSymbolPage()
{
  int i;
  for ( i = 0; i < m_patterns.GetSize(); i++ )
    delete m_patterns[i];
  for ( i = 0; i < m_logbrushes.GetSize(); i++ )
    delete m_logbrushes[i];
}

void CSingleSymbolPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSingleSymbolPage)
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_BUTTON2, m_border);
	DDX_Control(pDX, IDC_BUTTON1, m_color);
	DDX_Radio(pDX, IDC_RADIO1, m_nRenderType);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDX_Text(pDX, IDC_EDIT2, m_size);
	DDV_MinMaxInt(pDX, m_size, 0, 20);
	DDX_Check(pDX, IDC_SINGLE_SYMBOL_CHECK, m_bOutline);
	//}}AFX_DATA_MAP
	switch (m_nShapeType)
	{
		case moPointSymbol:
			DDX_Control(pDX, IDC_COMBO1, m_ptStyle);
			break;

		case moLineSymbol:
			DDX_Control(pDX, IDC_COMBO1, m_lnStyle);
			break;

		case moFillSymbol:
			DDX_Control(pDX, IDC_COMBO1, m_flStyle);
			break;
	}
}


BEGIN_MESSAGE_MAP(CSingleSymbolPage, CPropertyPage)
	//{{AFX_MSG_MAP(CSingleSymbolPage)
	ON_BN_CLICKED(IDC_RADIO1, OnChangeRenderType)
	ON_BN_CLICKED(IDC_BUTTON1, OnColor)
	ON_BN_CLICKED(IDC_BUTTON2, OnBorder)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeName)
	ON_BN_CLICKED(IDC_RADIO2, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO3, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO4, OnChangeRenderType)
	ON_BN_CLICKED(IDC_RADIO5, OnChangeRenderType)
	ON_BN_CLICKED(IDC_SINGLE_SYMBOL_CHECK, OnSingleSymbolCheck)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CSingleSymbolPage 

BOOL CSingleSymbolPage::OnInitDialog() 
{
	int i;
	
	CPropertyPage::OnInitDialog();
	
	m_nRenderType = 0;
	m_name = m_pParent->m_name;

	CMoSymbol symbol( m_pLayer->GetSymbol() );
	m_color.SetFaceColor(symbol.GetColor());

	switch( m_nShapeType )
	{
		case moPointSymbol:
			m_ptStyle.Init();
			m_ptStyle.SetCurSel(symbol.GetStyle());
      m_bOutline = symbol.GetOutline();
      m_border.SetFaceColor( symbol.GetOutlineColor() );
			break;

		case moLineSymbol:
			GetDlgItem(IDC_BUTTON2)->ShowWindow(FALSE);
			GetDlgItem(IDC_STATIC1)->ShowWindow(FALSE);
      GetDlgItem(IDC_SINGLE_SYMBOL_CHECK)->ShowWindow(FALSE);
			m_lnStyle.Init();
			m_lnStyle.SetCurSel(symbol.GetStyle());
			break;

		case moFillSymbol:
      for ( i = 0; i < 11; i++ )
      {
        CBrush* pBrush;
        CBitmap* pBitmap;
        
        switch ( i )
        {
        case 0:
          pBrush = new CBrush( RGB( 0, 0, 0 ) );
          break;

        case 1:
          pBrush = new CBrush( RGB( 255, 255, 255 ) );
          break;

        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
          pBrush = new CBrush( i - 2, RGB( 0, 0, 0 ) );
          break;

        case 8:
          pBitmap = new CBitmap;
          pBitmap->LoadBitmap( IDB_PATTRN01 );
          m_patterns.Add( pBitmap );
          pBrush = new CBrush( pBitmap );
          break;

        case 9:
          pBitmap = new CBitmap;
          pBitmap->LoadBitmap( IDB_PATTRN02 );
          m_patterns.Add( pBitmap );
          pBrush = new CBrush( pBitmap );
          break;

        case 10:
          pBitmap = new CBitmap;
          pBitmap->LoadBitmap( IDB_PATTRN03 );
          m_patterns.Add( pBitmap );
          pBrush = new CBrush( pBitmap );
          break;

        default:
          pBrush = new CBrush( RGB( 0, 0, 0 ) );
        }; // switch i
        m_logbrushes.SetAtGrow( i, new LOGBRUSH );
        pBrush->GetLogBrush( m_logbrushes[i] );
        m_flStyle.AddLogBrush( m_logbrushes[i] );
        delete pBrush;
      };

			m_flStyle.Init();
			m_flStyle.SetCurSel( symbol.GetStyle() );
			m_border.SetFaceColor( symbol.GetOutlineColor() );
      m_bOutline = symbol.GetOutline();
			break;
	}

	m_size = symbol.GetSize();
	m_spin.SetBuddy(GetDlgItem(IDC_EDIT2));
	m_spin.SetRange(0, 20);
	m_spin.SetBase(10);
	m_spin.SetPos(m_size);

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CSingleSymbolPage::OnSetActive() 
{
	m_nRenderType = 0;
	m_name = m_pParent->m_name;

	return CPropertyPage::OnSetActive();
}

void CSingleSymbolPage::OnChangeName() 
{
	GetDlgItem(IDC_EDIT1)->GetWindowText(m_pParent->m_name);
}

void CSingleSymbolPage::OnChangeRenderType() 
{
  m_pParent->OnChangeRenderType( this );
}

void CSingleSymbolPage::OnColor() 
{
	CColorDialog dlg(m_color.colGetFaceColor(), 0, this);

	if (dlg.DoModal()==IDOK)
	{
		m_color.SetFaceColor(dlg.GetColor());
		m_color.RedrawWindow();
		SetModified(TRUE);
	}
}

void CSingleSymbolPage::OnBorder() 
{
	CColorDialog dlg( m_border.colGetFaceColor(), 0, this );

	if( dlg.DoModal() == IDOK )
	{
		m_border.SetFaceColor( dlg.GetColor() );
		m_border.RedrawWindow();
		SetModified( TRUE );
	}
}

BOOL CSingleSymbolPage::OnWizardFinish() 
{
	if (!UpdateData())
		return FALSE;

	m_pLayer->SetName(m_name);

	CMoSymbol symbol( m_pLayer->GetSymbol() );

	symbol.SetColor( m_color.colGetFaceColor() );
	switch (m_nShapeType)
	{
		case moPointSymbol:
			symbol.SetStyle(m_ptStyle.GetCurSel());
      symbol.SetOutline( m_bOutline );
      symbol.SetOutlineColor( m_border.colGetFaceColor() );
			break;

		case moLineSymbol:
			symbol.SetStyle( m_lnStyle.GetCurSel() );
			break;

		case moFillSymbol:
			symbol.SetStyle( m_flStyle.GetCurSel() );
      symbol.SetOutline( m_bOutline );
			symbol.SetOutlineColor( m_border.colGetFaceColor() );
			break;
	}

	symbol.SetSize( m_size );

	m_pLayer->GetMapRenderer()->SetRendererType( CMapRenderer::singleSymbol );
  
	return CPropertyPage::OnWizardFinish();
}

void CSingleSymbolPage::OnSingleSymbolCheck() 
{
	SetModified( TRUE );
}
