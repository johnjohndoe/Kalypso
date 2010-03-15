// calcpg1.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "wspdlg.h"
#include "calcsht.h"
#include "calcpg1.h"
#include "calcpg2.h"
#include "calcpg3.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite LWACalcPage1 

IMPLEMENT_DYNCREATE(LWACalcPage1, CPropertyPage)

LWACalcPage1::LWACalcPage1(CalcData* pCD /*=NULL*/) : CPropertyPage(LWACalcPage1::IDD)
{
	//{{AFX_DATA_INIT(LWACalcPage1)
	m_name = _T("");
	m_ausdruck = 0;
	m_wehr = FALSE;
	m_schiess = FALSE;
	m_bedingungen = 0;
	m_hoehe = 0.0;
	m_gefaelle = 0.0;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
	if (pCD!=NULL)
		m_pState = pCD->GetState();
	else
		m_pState = NULL;
}

LWACalcPage1::~LWACalcPage1()
{
}

void LWACalcPage1::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT8 ); 
  LWACalcSheet::replaceChar( this, IDC_EDIT9 ); 
	
  CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LWACalcPage1)
	DDX_Control(pDX, IDC_COMBO6, m_anzahl);
	DDX_Control(pDX, IDC_COMBO5, m_endprofil);
	DDX_Control(pDX, IDC_COMBO4, m_anfangsprofil);
	DDX_Control(pDX, IDC_COMBO2, m_abflussereignis);
	DDX_Control(pDX, IDC_COMBO1, m_fliessgesetz);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDV_MaxChars(pDX, m_name, 60);
	DDX_Radio(pDX, IDC_RADIO7, m_ausdruck);
	DDX_Check(pDX, IDC_CHECK3, m_wehr);
	DDX_Check(pDX, IDC_CHECK4, m_schiess);
	DDX_Radio(pDX, IDC_RADIO3, m_bedingungen);
	DDX_Text(pDX, IDC_EDIT8, m_hoehe);
	DDX_Text(pDX, IDC_EDIT9, m_gefaelle);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(LWACalcPage1, CPropertyPage)
	//{{AFX_MSG_MAP(LWACalcPage1)
	ON_BN_CLICKED(IDC_RADIO3, OnAnfangsbedingungen)
	ON_BN_CLICKED(IDC_RADIO7, OnAusdruck)
	ON_BN_CLICKED(IDC_RADIO5, OnAnfangsbedingungen)
	ON_BN_CLICKED(IDC_RADIO6, OnAnfangsbedingungen)
	ON_BN_CLICKED(IDC_RADIO8, OnAusdruck)
	ON_BN_CLICKED(IDC_RADIO9, OnAusdruck)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LWACalcPage1 

BOOL LWACalcPage1::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	switch (pHelpInfo->iCtrlId)
	{
		case IDC_RADIO3:
		case IDC_RADIO5:
		case IDC_RADIO6:
		case IDC_EDIT8:
		case IDC_EDIT9:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_1);
			break;

		case IDC_COMBO1:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_2);
			break;

		case IDC_COMBO4:
		case IDC_COMBO5:
		case IDC_CHECK3:
		case IDC_CHECK4:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_3);
			break;
		
		case IDC_COMBO2:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_4);
			break;


		case IDC_EDIT1:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_5);
			break;
		
		case IDC_RADIO7:
		case IDC_RADIO8:
		case IDC_RADIO9:
		case IDC_COMBO6:
			theApp.WinHelp(HID_KAPITEL_4_6_1_1_6);
			break;
	}
	
	return CPropertyPage::OnHelpInfo(pHelpInfo);
}

BOOL LWACalcPage1::OnInitDialog() 
{
	Connection *con, *last_con;
	OutFlow *of;
	CString str;
	int nIndex;
	
	CPropertyPage::OnInitDialog();

	ASSERT(m_pCD!=NULL && m_pState!=NULL);
	
	m_bIncreasingStations = TRUE;
	last_con = m_pState->GetLastConnection();
	con = m_pState->GetFirstConnection();
	if (con!=NULL && last_con->GetAnfStation()<con->GetAnfStation())
		m_bIncreasingStations = FALSE;
	while (con!=NULL)
	{
		str.Format("%.6f", con->GetAnfStation());
		m_anfangsprofil.AddString(str);
		m_endprofil.AddString(str);
		last_con = con;
		con = m_pState->GetNextConnection();
	}
	str.Format("%.6f", last_con->GetEndStation());
	m_anfangsprofil.AddString(str);
	m_endprofil.AddString(str);

	of = m_pState->GetFirstOutFlow();
	while (of!=NULL)
	{
		str = of->GetName();
		m_abflussereignis.AddString(str);
		of = m_pState->GetNextOutFlow();
	}

	m_pCD->GetInfo(m_name);
	m_fliessgesetz.SetCurSel(m_pCD->m_nHyd-1);
	// anfang
	str.Format("%.6f", m_pCD->m_dAnfang);
	nIndex = m_anfangsprofil.FindString(-1, str);
	if (nIndex==CB_ERR)
		m_anfangsprofil.SetCurSel(0);
	else
		m_anfangsprofil.SetCurSel(nIndex);
	// ende
	str.Format("%.6f", m_pCD->m_dEnde);
	nIndex = m_endprofil.FindString(-1, str);
	if (nIndex==CB_ERR)
		m_endprofil.SetCurSel(m_endprofil.GetCount()-1);
	else
		m_endprofil.SetCurSel(nIndex);
	// ausdruck parameter	// Check This!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
	m_anzahl.SetCurSel(0);
	if (m_pCD->m_nA==0)
	{
		m_ausdruck = 0;
		GetDlgItem(IDC_COMBO6)->EnableWindow(FALSE);
	}
	else if (m_pCD->m_nA==-1)
	{
		m_ausdruck = 2;
		GetDlgItem(IDC_COMBO6)->EnableWindow(FALSE);
	}
	else
	{
		m_ausdruck = 1;
		m_anzahl.SetCurSel(m_pCD->m_nA-1);
		GetDlgItem(IDC_COMBO6)->EnableWindow(TRUE);
	}
	// Wehr am Anfang
	m_wehr = (BOOL)(m_pCD->m_nWehrAnf);
	// schiessender Abfluss
	m_schiess = (BOOL)m_pCD->m_nSchiess;
	// Anfangsbedingungen
	m_bedingungen = m_pCD->m_nWasser;
	switch (m_bedingungen)
	{
		case 0:
			GetDlgItem(IDC_EDIT8)->EnableWindow(TRUE);
			GetDlgItem(IDC_EDIT9)->EnableWindow(FALSE);
			m_hoehe = m_pCD->m_dHoehe;
			m_gefaelle = 0;
			break;

		case 1:
			GetDlgItem(IDC_EDIT8)->EnableWindow(FALSE);
			GetDlgItem(IDC_EDIT9)->EnableWindow(TRUE);
			m_hoehe = 0;
			m_gefaelle = m_pCD->m_dGefaelle;
			break;

		case 2:
			GetDlgItem(IDC_EDIT8)->EnableWindow(FALSE);
			GetDlgItem(IDC_EDIT9)->EnableWindow(FALSE);
			m_hoehe = 0;
			m_gefaelle = 0;
			break;
	}
	m_abflussereignis.SetCurSel(m_pCD->m_nSelIndex);



	UpdateData(FALSE);
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void LWACalcPage1::OnAnfangsbedingungen() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO3))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT8)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT9)->EnableWindow(FALSE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO5))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT8)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT9)->EnableWindow(TRUE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO6))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT8)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT9)->EnableWindow(FALSE);
	}
}

void LWACalcPage1::OnAusdruck() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO7))->GetCheck()==1)
	{
		GetDlgItem(IDC_COMBO6)->EnableWindow(FALSE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO8))->GetCheck()==1)
	{
		GetDlgItem(IDC_COMBO6)->EnableWindow(TRUE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO9))->GetCheck()==1)
	{
		GetDlgItem(IDC_COMBO6)->EnableWindow(FALSE);
	}
}

void LWACalcPage1::OnOK() 
{
	CString str;
	double anf, end;
	
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	m_anfangsprofil.GetWindowText(str);
	anf = atof(str);
	m_endprofil.GetWindowText(str);
	end = atof(str);
	if (m_bIncreasingStations)
	{
		if (anf>=end)
		{
			AfxMessageBox(IDS_ERROR_START_GRTH_END, MB_OK | MB_ICONEXCLAMATION);
			GetDlgItem(IDC_COMBO4)->SetFocus();
			return;
		}
	}
	else
	{
		if (end>=anf)
		{
			AfxMessageBox(IDS_ERROR_END_GRTH_START, MB_OK | MB_ICONEXCLAMATION);
			GetDlgItem(IDC_COMBO4)->SetFocus();
			return;
		}
	}

	m_pCD->m_dAnfang = anf;
	m_pCD->m_dEnde = end;
	m_pCD->m_nWehrAnf = m_wehr;
	m_pCD->m_nSchiess = m_schiess;
	m_pCD->m_nHyd = m_fliessgesetz.GetCurSel()+1;
	m_pCD->m_nSelIndex = m_abflussereignis.GetCurSel();
	m_abflussereignis.GetLBText(m_pCD->m_nSelIndex, m_pCD->m_strQ);
	m_pCD->SetInfo(m_name);
	m_pCD->m_nWasser = m_bedingungen;
	switch (m_bedingungen)
	{
		case 0:
			m_pCD->m_dHoehe = m_hoehe;
			m_pCD->m_dGefaelle = 0;
			break;

		case 1:
			m_pCD->m_dHoehe = 0;
			m_pCD->m_dGefaelle = m_gefaelle;
			break;

		case 2:
			m_pCD->m_dHoehe = 0;
			m_pCD->m_dGefaelle = 0;
			break;
	}
	switch (m_ausdruck)
	{
		case 0:
			m_pCD->m_nA = 0;
			break;

		case 1:
			m_pCD->m_nA = m_anzahl.GetCurSel()+1;
			break;

		case 2:
			m_pCD->m_nA = -1;
			break;
	}
	m_bOK = TRUE;	
	CPropertyPage::OnOK();
}

BOOL LWACalcPage1::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite BCECalcPage1 

IMPLEMENT_DYNCREATE(BCECalcPage1, CPropertyPage)

BCECalcPage1::BCECalcPage1(CalcData* pCD /*=NULL*/, BCECalcSheet* pParent /*=NULL*/) : CPropertyPage(BCECalcPage1::IDD)
{
	//{{AFX_DATA_INIT(BCECalcPage1)
	m_calctype = 0;
	m_rauheit = 0;
	m_ausgabe = 0;
	m_profilnum = FALSE;
	m_ausgabewerte = FALSE;
	m_name = _T("");
	m_kalmin = FALSE;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
	m_pParent = pParent;
	if (pCD!=NULL)
		m_pState = pCD->GetState();
	else
		m_pState = NULL;
}

BCECalcPage1::~BCECalcPage1()
{
}

void BCECalcPage1::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(BCECalcPage1)
	DDX_Control(pDX, IDC_COMBO2, m_endprofil);
	DDX_Control(pDX, IDC_COMBO1, m_anfangsprofil);
	DDX_Radio(pDX, IDC_RADIO1, m_calctype);
	DDX_Radio(pDX, IDC_RADIO4, m_rauheit);
	DDX_Radio(pDX, IDC_RADIO7, m_ausgabe);
	DDX_Check(pDX, IDC_CHECK1, m_profilnum);
	DDX_Check(pDX, IDC_CHECK2, m_ausgabewerte);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDV_MaxChars(pDX, m_name, 60);
	DDX_Check(pDX, IDC_CHECK8, m_kalmin);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(BCECalcPage1, CPropertyPage)
	//{{AFX_MSG_MAP(BCECalcPage1)
	ON_BN_CLICKED(IDC_RADIO1, OnCalcType)
	ON_BN_CLICKED(IDC_RADIO2, OnCalcType)
	ON_BN_CLICKED(IDC_RADIO3, OnCalcType)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten BCECalcPage1 

BOOL BCECalcPage1::OnInitDialog() 
{
	Connection *con, *last_con;
	CString str;
	int nIndex;

	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL && m_pState!=NULL);
	
	con = m_pState->GetFirstConnection();
	while (con!=NULL)
	{
		str.Format("%.6f", con->GetAnfStation());
		m_anfangsprofil.AddString(str);
		m_endprofil.AddString(str);
		last_con = con;
		con = m_pState->GetNextConnection();
	}
	str.Format("%.6f", last_con->GetEndStation());
	m_anfangsprofil.AddString(str);
	m_endprofil.AddString(str);
	
	m_pCD->GetInfo(m_name);
	// Berechnungstyp
	m_calctype = m_pCD->m_nWerte[0]-1;
	m_rauheit = m_pCD->m_nWerte[1]-1;
	m_ausgabe = m_pCD->m_nWerte[2]-1;
	// anfang
	str.Format("%.6f", m_pCD->m_dAnfang);
	nIndex = m_anfangsprofil.FindString(-1, str);
	if (nIndex==CB_ERR)
		m_anfangsprofil.SetCurSel(0);
	else
		m_anfangsprofil.SetCurSel(nIndex);
	// ende
	str.Format("%.6f", m_pCD->m_dEnde);
	nIndex = m_endprofil.FindString(-1, str);
	if (nIndex==CB_ERR)
		m_endprofil.SetCurSel(m_endprofil.GetCount()-1);
	else
		m_endprofil.SetCurSel(nIndex);
	m_profilnum = (BOOL)m_pCD->m_nWerte[3];
	m_ausgabewerte = (BOOL)m_pCD->m_nWerte[10];
	m_kalmin = (BOOL)m_pCD->m_nKalMin;

	UpdateData(FALSE);
	OnCalcType();

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void BCECalcPage1::OnCalcType() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO1))->GetCheck()==1)
	{	// show pages 1 and 2
		if (m_pParent->GetPageIndex(m_pParent->page2)==-1)
			m_pParent->AddPage(m_pParent->page2);
		if (m_pParent->GetPageIndex(m_pParent->page3)!=-1)
			m_pParent->RemovePage(m_pParent->page3);
		GetDlgItem(IDC_CHECK2)->ShowWindow(FALSE);
    GetDlgItem(IDC_CHECK8)->ShowWindow(FALSE);
	}
	else if (((CButton*)GetDlgItem(IDC_RADIO2))->GetCheck()==1)
	{	// show page 1 only
		if (m_pParent->GetPageIndex(m_pParent->page2)!=-1)
			m_pParent->RemovePage(m_pParent->page2);
		if (m_pParent->GetPageIndex(m_pParent->page3)!=-1)
			m_pParent->RemovePage(m_pParent->page3);
		GetDlgItem(IDC_CHECK2)->ShowWindow(TRUE);
    GetDlgItem(IDC_CHECK8)->ShowWindow(TRUE);
	}
	else if (((CButton*)GetDlgItem(IDC_RADIO3))->GetCheck()==1)
	{	// show pages 1 and 3
		if (m_pParent->GetPageIndex(m_pParent->page2)!=-1)
			m_pParent->RemovePage(m_pParent->page2);
		if (m_pParent->GetPageIndex(m_pParent->page3)==-1)
			m_pParent->AddPage(m_pParent->page3);
		GetDlgItem(IDC_CHECK2)->ShowWindow(TRUE);
    GetDlgItem(IDC_CHECK8)->ShowWindow(TRUE);
	}
}

void BCECalcPage1::OnOK() 
{
	CString str;
	double anf, end;
	
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	m_anfangsprofil.GetWindowText(str);
	anf = atof(str);
	m_endprofil.GetWindowText(str);
	end = atof(str);
	if (anf>=end)
	{
		AfxMessageBox(IDS_ERROR_START_GRTH_END, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_COMBO1)->SetFocus();
		return;
	}

	m_pCD->m_dAnfang = anf;
	m_pCD->m_dEnde = end;
	m_pCD->SetInfo(m_name);
	m_pCD->m_nWerte[0] = m_calctype+1;
	m_pCD->m_nWerte[1] = m_rauheit+1;
	m_pCD->m_nWerte[2] = m_ausgabe+1;
	m_pCD->m_nWerte[3] = m_profilnum;
	m_pCD->m_nWerte[10] = m_ausgabewerte;
	m_pCD->m_nKalMin = m_kalmin;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}


BOOL BCECalcPage1::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}
