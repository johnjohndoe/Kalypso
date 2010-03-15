// calcpg3.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "calcpg3.h"
#include "calcsht.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite LWACalcPage3 

IMPLEMENT_DYNCREATE(LWACalcPage3, CPropertyPage)

LWACalcPage3::LWACalcPage3(CalcData* pCD /*=NULL*/) : CPropertyPage(LWACalcPage3::IDD)
{
	//{{AFX_DATA_INIT(LWACalcPage3)
	m_ncar = 0;
	m_nfrou = FALSE;
	m_nForm = FALSE;
	m_nposey = 0;
	m_nbeta = FALSE;
	m_epsh = 0.0;
	m_epsv = 0.0;
	m_rny = 0.0;
	m_cwr = 0.0;
	m_sm = _T("");
	//}}AFX_DATA_INIT
	m_pCD = pCD;
}

LWACalcPage3::~LWACalcPage3()
{
}

void LWACalcPage3::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT1 );
  LWACalcSheet::replaceChar( this, IDC_EDIT2 );
  LWACalcSheet::replaceChar( this, IDC_EDIT3 );
  LWACalcSheet::replaceChar( this, IDC_EDIT15 );
  LWACalcSheet::replaceChar( this, IDC_EDIT16 );

	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LWACalcPage3)
	DDX_Radio(pDX, IDC_RADIO1, m_ncar);
	DDX_Check(pDX, IDC_CHECK13, m_nfrou);
	DDX_Check(pDX, IDC_CHECK2, m_nForm);
	DDX_Radio(pDX, IDC_RADIO13, m_nposey);
	DDX_Check(pDX, IDC_CHECK1, m_nbeta);
	DDX_Text(pDX, IDC_EDIT2, m_epsh);
	DDX_Text(pDX, IDC_EDIT3, m_epsv);
	DDX_Text(pDX, IDC_EDIT15, m_rny);
	DDX_Text(pDX, IDC_EDIT16, m_cwr);
	DDX_Text(pDX, IDC_EDIT1, m_sm);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(LWACalcPage3, CPropertyPage)
	//{{AFX_MSG_MAP(LWACalcPage3)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LWACalcPage3 

BOOL LWACalcPage3::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	switch (pHelpInfo->iCtrlId)
	{
		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
		case IDC_RADIO4:
			theApp.WinHelp(HID_KAPITEL_6_4_2_1);
			break;

		case IDC_CHECK13:
			theApp.WinHelp(HID_KAPITEL_6_4_2_2);
			break;

		case IDC_EDIT2:
		case IDC_EDIT3:
		case IDC_EDIT15:
		case IDC_EDIT16:
			theApp.WinHelp(HID_KAPITEL_6_4_2_3);
			break;
		
		case IDC_EDIT1:
			theApp.WinHelp(HID_KAPITEL_6_4_2_4);
			break;
		
		case IDC_CHECK2:
			theApp.WinHelp(HID_KAPITEL_6_4_2_5);
			break;

		case IDC_RADIO13:
		case IDC_RADIO6:
		case IDC_RADIO7:
		case IDC_RADIO8:
			theApp.WinHelp(HID_KAPITEL_6_4_2_6);
			break;

		case IDC_CHECK1:
			theApp.WinHelp(HID_KAPITEL_6_4_2_7);
			break;
	}

	return CPropertyPage::OnHelpInfo(pHelpInfo);
}

BOOL LWACalcPage3::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL);
	
	m_ncar = m_pCD->m_nCar;
	m_nposey = m_pCD->m_nPosey;
	m_nbeta = (BOOL)m_pCD->m_nBeta==1;
	m_nForm = (BOOL)m_pCD->m_nForm==1;
	m_nfrou = (BOOL)m_pCD->m_nFrou==1;
  if( m_pCD->m_dSM == std::numeric_limits<double>::infinity() )
		m_sm.Empty();
	else
		m_sm.Format("%.*g", DBL_DIG, m_pCD->m_dSM);
	m_epsh = m_pCD->m_dEpsh;
	m_epsv = m_pCD->m_dEpsv;
	m_rny = m_pCD->m_dRny;
	m_cwr = m_pCD->m_dCwr;

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void LWACalcPage3::OnOK() 
{
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	if (m_sm.IsEmpty())
    m_pCD->m_dSM = std::numeric_limits<double>::infinity();
	else
	{
		double d;
		if (!AfxSimpleFloatParse(m_sm, d))
		{
			AfxMessageBox(AFX_IDP_PARSE_REAL);
			GetDlgItem(IDC_EDIT1)->SetFocus();
			return;
		}
		m_pCD->m_dSM = d;
	}

	m_pCD->m_nCar = m_ncar;
	m_pCD->m_nPosey = m_nposey;
	m_pCD->m_nBeta = m_nbeta;
	m_pCD->m_nForm = m_nForm;
	m_pCD->m_nFrou = m_nfrou;
	m_pCD->m_dEpsh = m_epsh;
	m_pCD->m_dEpsv = m_epsv;
	m_pCD->m_dRny = m_rny;
	m_pCD->m_dCwr = m_cwr;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}

BOOL LWACalcPage3::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite BCECalcPage3 

IMPLEMENT_DYNCREATE(BCECalcPage3, CPropertyPage)


const double BCECalcPage3::Q_FAKTOR = 100; // Umrechnungsfaktor zwischen Einegegebenen Q-Werten und gesetzten


BCECalcPage3::BCECalcPage3(CalcData* pCD /*=NULL*/) : CPropertyPage(BCECalcPage3::IDD)
{
	//{{AFX_DATA_INIT(BCECalcPage3)
	m_wasserspiegel = 0;
	m_VZVerlust = 0;
	m_RVerlust = 0;
	m_anfangsWSP = 0;
	m_qmin = 0.0;
	m_qstep = 0.0;
	m_qmax = 0.0;
	m_brucke = FALSE;
	m_wehre = FALSE;
	m_gefaelle = 0.0;
	m_WQDateien = FALSE;
	m_ergebnislisten = FALSE;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
}

BCECalcPage3::~BCECalcPage3()
{
}

void BCECalcPage3::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT1 );
  LWACalcSheet::replaceChar( this, IDC_EDIT2 );
  LWACalcSheet::replaceChar( this, IDC_EDIT3 );
  LWACalcSheet::replaceChar( this, IDC_EDIT4 );

	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(BCECalcPage3)
	DDX_Radio(pDX, IDC_RADIO1, m_wasserspiegel);
	DDX_Radio(pDX, IDC_RADIO5, m_VZVerlust);
	DDX_Radio(pDX, IDC_RADIO3, m_RVerlust);
	DDX_Radio(pDX, IDC_RADIO8, m_anfangsWSP);
	DDX_Text(pDX, IDC_EDIT2, m_qmin);
	DDX_Text(pDX, IDC_EDIT3, m_qstep);
	DDX_Text(pDX, IDC_EDIT4, m_qmax);
	DDX_Check(pDX, IDC_CHECK1, m_brucke);
	DDX_Check(pDX, IDC_CHECK2, m_wehre);
	DDX_Text(pDX, IDC_EDIT1, m_gefaelle);
	DDX_Check(pDX, IDC_CHECK8, m_WQDateien);
	DDX_Check(pDX, IDC_CHECK13, m_ergebnislisten);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(BCECalcPage3, CPropertyPage)
	//{{AFX_MSG_MAP(BCECalcPage3)
	ON_BN_CLICKED(IDC_RADIO8, OnAnfangsWSP)
	ON_BN_CLICKED(IDC_RADIO9, OnAnfangsWSP)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten BCECalcPage3 

BOOL BCECalcPage3::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL);
	
	m_wasserspiegel = m_pCD->m_nWerte[4]-1;
	m_RVerlust = m_pCD->m_nWerte[5]-1;
	m_VZVerlust = m_pCD->m_nWerte[6]-1;
	m_anfangsWSP = m_pCD->m_nWerte[7]-2;
	switch (m_anfangsWSP)
	{
		case 1:
			m_gefaelle = m_pCD->m_dGefaelle;
			break;

		default:
			m_gefaelle = 0;
			GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
			break;
	}
	m_brucke = (BOOL)m_pCD->m_nWerte[8];
	m_wehre = (BOOL)m_pCD->m_nWerte[9];
	m_WQDateien = (BOOL)m_pCD->m_nWerte[11];
	m_ergebnislisten = (BOOL)m_pCD->m_nWerte[12];
	m_qmin = m_pCD->m_dQMin / Q_FAKTOR;
	m_qstep = m_pCD->m_dQStep / Q_FAKTOR;
	m_qmax = m_pCD->m_dQMax / Q_FAKTOR;

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void BCECalcPage3::OnAnfangsWSP() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO8))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO9))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
	}
}

void BCECalcPage3::OnOK() 
{
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	if (m_qmin>=m_qmax)
	{
		AfxMessageBox(IDS_QMINQMAX, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_EDIT2)->SetFocus();
		return;
	}
	if (m_qstep>(m_qmax-m_qmin))
	{
		AfxMessageBox(IDS_QSTEP, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_EDIT3)->SetFocus();
		return;
	}
	m_pCD->m_nWerte[4] = m_wasserspiegel+1;
	m_pCD->m_nWerte[5] = m_RVerlust+1;
	m_pCD->m_nWerte[6] = m_VZVerlust+1;
	m_pCD->m_nWerte[7] = m_anfangsWSP+2;
	switch (m_anfangsWSP)
	{
		case 1:
			m_pCD->m_dGefaelle = m_gefaelle;
			break;

		default:
			m_pCD->m_dGefaelle = 0;
			break;
	}
	m_pCD->m_nWerte[8] = m_brucke;
	m_pCD->m_nWerte[9] = m_wehre;
	m_pCD->m_nWerte[11] = m_WQDateien;
	m_pCD->m_nWerte[12] = m_ergebnislisten;
	m_pCD->m_dQMin = m_qmin * Q_FAKTOR;
	m_pCD->m_dQStep = m_qstep * Q_FAKTOR;
	m_pCD->m_dQMax = m_qmax * Q_FAKTOR;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}

BOOL BCECalcPage3::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}
