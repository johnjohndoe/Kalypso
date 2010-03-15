// calcpg4.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "wspdlg.h"
#include "calcpg4.h"
#include "calcsht.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite LWACalcPage4 

IMPLEMENT_DYNCREATE(LWACalcPage4, CPropertyPage)

LWACalcPage4::LWACalcPage4(CalcData* pCD /*=NULL*/) : CPropertyPage(LWACalcPage4::IDD)
{
	//{{AFX_DATA_INIT(LWACalcPage4)
	m_zmax = 0;
	m_nNN = FALSE;
	m_nDat = FALSE;
	m_nDr = FALSE;
	m_nFP = 0;
	m_hmo = FALSE;
	m_wsfq = FALSE;
	m_wsfl = FALSE;
	m_dhwmax = 0.0;
	m_vfmax = 0.0;
	m_hzvmax = 0.0;
	m_ffmax = 0.0;
	m_faklhg = 0.0;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
}

LWACalcPage4::~LWACalcPage4()
{
}

void LWACalcPage4::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT1 );
  LWACalcSheet::replaceChar( this, IDC_EDIT3 );
  LWACalcSheet::replaceChar( this, IDC_EDIT4 );
  LWACalcSheet::replaceChar( this, IDC_EDIT6 );
  LWACalcSheet::replaceChar( this, IDC_EDIT5 );

  CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LWACalcPage4)
	DDX_Text(pDX, IDC_EDIT2, m_zmax);
	DDV_MinMaxInt(pDX, m_zmax, 1, 100);
	DDX_Check(pDX, IDC_CHECK2, m_nNN);
	DDX_Check(pDX, IDC_CHECK1, m_nDat);
	DDX_Check(pDX, IDC_CHECK3, m_nDr);
	DDX_Radio(pDX, IDC_RADIO3, m_nFP);
	DDX_Check(pDX, IDC_CHECK4, m_hmo);
	DDX_Check(pDX, IDC_CHECK8, m_wsfq);
	DDX_Check(pDX, IDC_CHECK9, m_wsfl);
	DDX_Text(pDX, IDC_EDIT1, m_dhwmax);
	DDX_Text(pDX, IDC_EDIT3, m_vfmax);
	DDX_Text(pDX, IDC_EDIT6, m_hzvmax);
	DDX_Text(pDX, IDC_EDIT5, m_ffmax);
	DDX_Text(pDX, IDC_EDIT4, m_faklhg);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(LWACalcPage4, CPropertyPage)
	//{{AFX_MSG_MAP(LWACalcPage4)
	ON_WM_HELPINFO()
	ON_BN_CLICKED(IDC_CHECK9, OnCheck9)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LWACalcPage4 

BOOL LWACalcPage4::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	switch (pHelpInfo->iCtrlId)
	{
		case IDC_RADIO3:
		case IDC_RADIO4:
		case IDC_RADIO5:
			theApp.WinHelp(HID_KAPITEL_6_4_3_1);
			break;

		case IDC_CHECK1:
			theApp.WinHelp(HID_KAPITEL_6_4_3_2);
			break;

		case IDC_EDIT2:
			theApp.WinHelp(HID_KAPITEL_6_4_3_3);
			break;
		
		case IDC_CHECK2:
			theApp.WinHelp(HID_KAPITEL_6_4_3_4);
			break;
		
		case IDC_CHECK9:
		case IDC_CHECK8:
			theApp.WinHelp(HID_KAPITEL_6_4_3_5);
			break;
	}
	
	return CPropertyPage::OnHelpInfo(pHelpInfo);
}

BOOL LWACalcPage4::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL);

	m_nFP = m_pCD->m_nFP;
	m_wsfl = (BOOL)m_pCD->m_nWSFL==1;
	if (m_wsfl)
		m_wsfq = (BOOL)m_pCD->m_nWSFQ==1;
	else
		GetDlgItem(IDC_CHECK8)->EnableWindow(FALSE);
	m_nDat = (BOOL)m_pCD->m_nDat==1;
	m_nNN = (BOOL)m_pCD->m_nNN==1;
	m_nDr = (BOOL)m_pCD->m_nDr==1;
	GetDlgItem(IDC_CHECK4)->ShowWindow(TRUE);
	m_hmo = (BOOL)m_pCD->m_nHMO==1;
	m_zmax = m_pCD->m_nZMax;
	m_dhwmax = m_pCD->m_dDHWMax;;
	m_vfmax = m_pCD->m_dVFMax;
	m_hzvmax = m_pCD->m_dHZVMax;
	m_faklhg = m_pCD->m_dFakLHG;
	m_ffmax = m_pCD->m_dFFMax;
	
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void LWACalcPage4::OnOK() 
{
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	m_pCD->m_nFP = m_nFP;
	m_pCD->m_nWSFL = m_wsfl;
	if (!m_wsfl)
		m_wsfq = FALSE;
	m_pCD->m_nWSFQ = m_wsfq;
	m_pCD->m_nDat = m_nDat;
	m_pCD->m_nNN = m_nNN;
	m_pCD->m_nDr = m_nDr;
	m_pCD->m_nHMO = m_hmo;
	m_pCD->m_nZMax = m_zmax;
	m_pCD->m_dDHWMax = m_dhwmax;
	m_vfmax = m_pCD->m_dVFMax = m_vfmax;
	m_pCD->m_dHZVMax = m_hzvmax;
	m_pCD->m_dFakLHG = m_faklhg;
	m_pCD->m_dFFMax = m_ffmax;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}

BOOL LWACalcPage4::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}

void LWACalcPage4::OnCheck9() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK9))->GetCheck()==1)
		GetDlgItem(IDC_CHECK8)->EnableWindow(TRUE);
	else
	{
		((CButton*)GetDlgItem(IDC_CHECK8))->SetCheck(0);
		m_wsfq = FALSE;
		GetDlgItem(IDC_CHECK8)->EnableWindow(FALSE);
	}
}
