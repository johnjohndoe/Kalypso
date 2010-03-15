// formatpg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "template.h"
#include "plotdoc.h"
#include "plotdocdata.h"
#include "propdlg.h"

#include "formatpg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CFormatPage 

CFormatPage::CFormatPage( CPropertyDialog *pParent, CPlotterDoc* pDoc /*=NULL*/ ) : CPropertyPage( CFormatPage::IDD )
{
	//{{AFX_DATA_INIT(CFormatPage)
	m_profilFormat = -1;
	m_tableFormat = -1;
	m_XValueFormat = -1;
	m_YValueFormat = -1;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pDoc = pDoc;
	m_pTemp = NULL;
}

CFormatPage::~CFormatPage()
{
}

void CFormatPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CFormatPage)
	DDX_Radio(pDX, IDC_RADIO1, m_profilFormat);
	DDX_Radio(pDX, IDC_RADIO4, m_tableFormat);
	DDX_Radio(pDX, IDC_RADIO7, m_XValueFormat);
	DDX_Radio(pDX, IDC_RADIO11, m_YValueFormat);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CFormatPage, CPropertyPage)
	//{{AFX_MSG_MAP(CFormatPage)
	ON_BN_CLICKED(IDC_RADIO7, OnChangeXFormat)
	ON_BN_CLICKED(IDC_RADIO11, OnChangeYFormat)
	ON_BN_CLICKED(IDC_RADIO8, OnChangeXFormat)
	ON_BN_CLICKED(IDC_RADIO9, OnChangeXFormat)
	ON_BN_CLICKED(IDC_RADIO10, OnChangeXFormat)
	ON_BN_CLICKED(IDC_RADIO12, OnChangeYFormat)
	ON_BN_CLICKED(IDC_RADIO13, OnChangeYFormat)
	ON_BN_CLICKED(IDC_RADIO14, OnChangeYFormat)
	ON_BN_CLICKED(IDC_RADIO1, OnChange)
	ON_BN_CLICKED(IDC_RADIO2, OnChange)
	ON_BN_CLICKED(IDC_RADIO3, OnChange)
	ON_BN_CLICKED(IDC_RADIO4, OnChange)
	ON_BN_CLICKED(IDC_RADIO5, OnChange)
	ON_BN_CLICKED(IDC_RADIO6, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CFormatPage 

BOOL CFormatPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	m_pParent->m_nActivePages++;
	Update();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CFormatPage::Update()
{
	CPlotterDoc* pDoc;

	if (GetSafeHwnd()==NULL)
		return;

	if (m_pTemp!=NULL)
		pDoc = m_pTemp;
	else
		pDoc = m_pDoc;

	m_tableFormat = pDoc->GetTableFormat();
	m_profilFormat = pDoc->GetProfilFormat();
	m_XValueFormat = pDoc->GetXValueFormat();
	m_YValueFormat = pDoc->GetYValueFormat();
	
	UpdateData(FALSE);

	DisableXButton();
	DisableYButton();
}

void CFormatPage::ApplyTemplate(CTemplate *pTemp)
{
	m_pTemp = pTemp;
	Update();
	m_pTemp = NULL;
}

BOOL CFormatPage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}

void CFormatPage::OnOK() 
{
	if (!UpdateData(TRUE))
		return;

  m_pDoc->SetTableFormat( (CTable::TableFormat)m_tableFormat );
	m_pDoc->SetProfilFormat( m_profilFormat );
	m_pDoc->SetXValueFormat( m_XValueFormat );
	m_pDoc->SetYValueFormat( m_YValueFormat );

	CPropertyPage::OnOK();
}

void CFormatPage::DisableYButton()
{
	if (((CButton*)GetDlgItem(IDC_RADIO7))->GetCheck()==1)
		GetDlgItem(IDC_RADIO11)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO11)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO8))->GetCheck()==1)
		GetDlgItem(IDC_RADIO12)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO12)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO9))->GetCheck()==1)
		GetDlgItem(IDC_RADIO13)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO13)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO10))->GetCheck()==1)
		GetDlgItem(IDC_RADIO14)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO14)->EnableWindow(TRUE);
}

void CFormatPage::DisableXButton()
{
	if (((CButton*)GetDlgItem(IDC_RADIO11))->GetCheck()==1)
		GetDlgItem(IDC_RADIO7)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO7)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO12))->GetCheck()==1)
		GetDlgItem(IDC_RADIO8)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO8)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO13))->GetCheck()==1)
		GetDlgItem(IDC_RADIO9)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO9)->EnableWindow(TRUE);

	if (((CButton*)GetDlgItem(IDC_RADIO14))->GetCheck()==1)
		GetDlgItem(IDC_RADIO10)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_RADIO10)->EnableWindow(TRUE);
}

void CFormatPage::OnChangeXFormat() 
{
	OnChange();

	DisableYButton();
}

void CFormatPage::OnChangeYFormat()
{
	OnChange();

	DisableXButton();
}

void CFormatPage::OnChange() 
{
	SetModified(TRUE);
	m_pParent->m_nFormat = 1;
}
