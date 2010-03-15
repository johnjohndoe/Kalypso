// calcpg2.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "Alphaops.h"
#include "wspdlg.h"
#include "calcpg2.h"
#include "calcsht.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite LWACalcPage2 

IMPLEMENT_DYNCREATE(LWACalcPage2, CPropertyPage)

LWACalcPage2::LWACalcPage2(CalcData* pCD /*=NULL*/) : CPropertyPage(LWACalcPage2::IDD)
{
	//{{AFX_DATA_INIT(LWACalcPage2)
	m_eichung = 0;
	m_wqbez = FALSE;
	m_qmin = 0.0;
	m_qstep = 0.0;
	m_qmax = 0.0;
	m_nasall = FALSE;
	m_qwv = FALSE;
	m_kalmin = FALSE;
	m_wtau = FALSE;
	m_auto = 0;
	m_he = 0.0;
	m_nasabs = FALSE;
	m_hgralle = FALSE;
	m_normalle = FALSE;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
	if (pCD!=NULL)
		m_pState = pCD->GetState();
	else
		m_pState = NULL;
}

LWACalcPage2::~LWACalcPage2()
{
}

void LWACalcPage2::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT1 ); 
  LWACalcSheet::replaceChar( this, IDC_EDIT2 ); 
  LWACalcSheet::replaceChar( this, IDC_EDIT3 ); 
  LWACalcSheet::replaceChar( this, IDC_EDIT10 ); 

	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LWACalcPage2)
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Radio(pDX, IDC_RADIO1, m_eichung);
	DDX_Check(pDX, IDC_CHECK1, m_wqbez);
	DDX_Text(pDX, IDC_EDIT2, m_qmin);
	DDX_Text(pDX, IDC_EDIT3, m_qstep);
	DDX_Text(pDX, IDC_EDIT10, m_qmax);
	DDX_Check(pDX, IDC_CHECK8, m_nasall);
	DDX_Check(pDX, IDC_CHECK10, m_qwv);
	DDX_Check(pDX, IDC_CHECK11, m_kalmin);
	DDX_Check(pDX, IDC_CHECK9, m_wtau);
	DDX_Radio(pDX, IDC_RADIO10, m_auto);
	DDX_Text(pDX, IDC_EDIT1, m_he);
	DDX_Check(pDX, IDC_CHECK12, m_nasabs);
	DDX_Check(pDX, IDC_CHECK6, m_hgralle);
	DDX_Check(pDX, IDC_CHECK7, m_normalle);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(LWACalcPage2, CPropertyPage)
	//{{AFX_MSG_MAP(LWACalcPage2)
	ON_BN_CLICKED(IDC_CHECK1, OnWQBez)
	ON_BN_CLICKED(IDC_CHECK12, OnNasimKalmin)
	ON_BN_CLICKED(IDC_RADIO1, OnEichung)
	ON_BN_CLICKED(IDC_BUTTON1, OnInsertNasim)
	ON_BN_CLICKED(IDC_BUTTON2, OnDeleteNasim)
	ON_WM_HELPINFO()
	ON_BN_CLICKED(IDC_CHECK11, OnNasimKalmin)
	ON_BN_CLICKED(IDC_RADIO2, OnEichung)
	ON_BN_CLICKED(IDC_RADIO3, OnEichung)
	ON_BN_CLICKED(IDC_CHECK6, OnCheck6)
	ON_BN_CLICKED(IDC_CHECK7, OnCheck7)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LWACalcPage2 

BOOL LWACalcPage2::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	switch (pHelpInfo->iCtrlId)
	{
		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
		case IDC_EDIT1:
			theApp.WinHelp(HID_KAPITEL_6_4_1_1);
			break;

		case IDC_RADIO10:
		case IDC_RADIO11:
		case IDC_RADIO12:
			theApp.WinHelp(HID_KAPITEL_6_4_1_2);
			break;

		case IDC_CHECK6:
			theApp.WinHelp(HID_KAPITEL_6_4_1_3);
			break;
		
		case IDC_CHECK7:
			theApp.WinHelp(HID_KAPITEL_6_4_1_4);
			break;
		
		case IDC_CHECK11:
			theApp.WinHelp(HID_KAPITEL_6_4_1_5);
			break;

		case IDC_CHECK12:
		case IDC_LIST1:
		case IDC_BUTTON1:
		case IDC_BUTTON2:
			theApp.WinHelp(HID_KAPITEL_6_4_1_6);
			break;

		case IDC_CHECK9:
			theApp.WinHelp(HID_KAPITEL_6_4_1_7);
			break;

		case IDC_CHECK10:
			theApp.WinHelp(HID_KAPITEL_6_4_1_8);
			break;

		case IDC_CHECK1:
		case IDC_EDIT2:
		case IDC_EDIT3:
		case IDC_EDIT10:
			theApp.WinHelp(HID_KAPITEL_6_4_1_9);
			break;
	}
	
	return CPropertyPage::OnHelpInfo(pHelpInfo);
}

BOOL LWACalcPage2::OnInitDialog() 
{
	CString str;
	Connection *con, *last_con;

	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL && m_pState!=NULL);
	
	con = m_pState->GetFirstConnection();
	while (con!=NULL)
	{
		str.Format("%.4f\n", con->GetAnfStation());
		m_stations += str;
		last_con = con;
		con = m_pState->GetNextConnection();
	}
	str.Format("%.4f\n", last_con->GetEndStation());
	m_stations += str;

	m_eichung = m_pCD->m_nEich;
	switch (m_eichung)
	{
		case 1:
		case 2:
			m_he = m_pCD->m_dHe;
			break;

		default:
			m_he = 0;
			GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
			break;
	}
	m_auto = m_pCD->m_nAuto;
	m_hgralle = (BOOL)m_pCD->m_nHGralle;
	if (!m_hgralle)
		m_normalle = (BOOL)m_pCD->m_nNormal;
	m_wtau = (BOOL)m_pCD->m_nWTau;
	m_wqbez = (BOOL)m_pCD->m_nWQBez;
	if (!m_wqbez)
	{
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT3)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT10)->EnableWindow(FALSE);
	}
	else
	{
		m_qmin = m_pCD->m_dQMin;
		m_qstep = m_pCD->m_dQStep;
		m_qmax = m_pCD->m_dQMax;
	}
	m_nasall = (BOOL)m_pCD->m_nNasall;
	m_qwv = (BOOL)m_pCD->m_nQWV;
	m_kalmin = (BOOL)m_pCD->m_nKalMin;
	m_nasabs = (BOOL)m_pCD->m_nNasabs;
	if (!m_nasabs && !m_kalmin)
	{
		GetDlgItem(IDC_LIST1)->EnableWindow(FALSE);
		GetDlgItem(IDC_BUTTON1)->EnableWindow(FALSE);
		GetDlgItem(IDC_BUTTON2)->EnableWindow(FALSE);
	}
	FillNasim(0);
	
	UpdateData(FALSE);
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void LWACalcPage2::OnCheck6() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK6))->GetCheck()==1)
		((CButton*)GetDlgItem(IDC_CHECK7))->SetCheck(0);
}

void LWACalcPage2::OnCheck7() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK7))->GetCheck()==1)
		((CButton*)GetDlgItem(IDC_CHECK6))->SetCheck(0);
}

void LWACalcPage2::FillNasim(int nIndex)
{
	int i, width;
	CString str;
	CRect rect;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	Coord *pCrd;

	m_list.SetGridList(TRUE);
	m_list.GetClientRect(&rect);
	width = rect.Width()/2;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_VALUE1);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VALUE2);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	for (i=0; i<m_pCD->m_arNasim.GetSize(); i++)
	{
		pCrd = m_pCD->m_arNasim[i];
		str.Format("%.*g", DBL_DIG, pCrd->dx);
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		m_list.SetItemControl(lvItem.iItem, 0, LCTRLEX_DROPDOWN, m_stations, DOUBLE_FIELD);

		str.Format("%.*g", DBL_DIG, pCrd->dy);
		m_list.SetItemText(lvItem.iItem, 1, str);
		m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWN, m_stations, DOUBLE_FIELD);
	}
	UpdateData(FALSE);
}

void LWACalcPage2::OnInsertNasim() 
{
	CString str;
	LV_ITEM lvItem;
	int i;

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	i = m_stations.Find('\n');
	if (i!=-1)
		str = m_stations.Left(i);
	lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
	lvItem.iSubItem = 0;
	lvItem.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvItem.cchTextMax = str.GetLength();
	m_list.InsertItem(&lvItem);
	m_list.SetItemControl(lvItem.iItem, 0, LCTRLEX_DROPDOWN, m_stations, DOUBLE_FIELD);
	
	m_list.SetItemText(lvItem.iItem, 1, str);
	m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWN, m_stations, DOUBLE_FIELD);
}

void LWACalcPage2::OnDeleteNasim() 
{
	int i;

	for (i=0; i<m_list.GetItemCount(); i++)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
			m_list.DeleteItem(i--);
	}
}

void LWACalcPage2::OnWQBez() 
{
	BOOL bEnable = FALSE;

	if (((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck()==1)
		bEnable = TRUE;
	GetDlgItem(IDC_EDIT2)->EnableWindow(bEnable);
	GetDlgItem(IDC_EDIT3)->EnableWindow(bEnable);
	GetDlgItem(IDC_EDIT10)->EnableWindow(bEnable);
}

void LWACalcPage2::OnNasimKalmin() 
{
	BOOL bEnable = FALSE;

	if (((CButton*)GetDlgItem(IDC_CHECK12))->GetCheck()==1 ||
		((CButton*)GetDlgItem(IDC_CHECK11))->GetCheck()==1)
		bEnable = TRUE;
	GetDlgItem(IDC_LIST1)->EnableWindow(bEnable);
	GetDlgItem(IDC_BUTTON1)->EnableWindow(bEnable);
	GetDlgItem(IDC_BUTTON2)->EnableWindow(bEnable);
}

void LWACalcPage2::OnEichung() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO1))->GetCheck()==1)
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
	else
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
}

void LWACalcPage2::OnOK() 
{
	int i;
	
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	if (m_wqbez)
	{
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
	}

	m_pCD->m_nEich = m_eichung;
	switch (m_eichung)
	{
		case 1:
		case 2:
			m_pCD->m_dHe = m_he;
			break;

		default:
			m_pCD->m_dHe = 0;
			break;
	}
	m_pCD->m_nHGralle = m_hgralle;
	m_pCD->m_nNormal = m_normalle;
	m_pCD->m_nWTau = m_wtau;
	m_pCD->m_nNasall = m_nasall;
	m_pCD->m_nQWV = m_qwv;
	m_pCD->m_nWQBez = m_wqbez;
	if (m_wqbez)
	{
		m_pCD->m_dQMin = m_qmin;
		m_pCD->m_dQStep = m_qstep;
		m_pCD->m_dQMax = m_qmax;
	}
	else
	{
		m_pCD->m_dQMin = 0;
		m_pCD->m_dQStep = 0;
		m_pCD->m_dQMax = 0;
	}
	if (m_list.GetItemCount()==0)
	{
		m_kalmin = 0;
		m_nasabs = 0;
	}
	m_pCD->m_nKalMin = m_kalmin;
	m_pCD->m_nNasabs = m_nasabs;
	for (i=0; i<m_pCD->m_arNasim.GetSize(); i++)
		delete m_pCD->m_arNasim[i];
	m_pCD->m_arNasim.RemoveAll();
	if (m_nasabs || m_kalmin)
	{
		for (i=0; i<m_list.GetItemCount(); i++)
		{
			CString str1, str2;

			str1 = m_list.GetItemText(i, 0);
			str2 = m_list.GetItemText(i, 1);
			if (!str1.IsEmpty() || !str2.IsEmpty())
			{
				Coord *pCrd = new Coord;
				
				pCrd->dx = atof(str1);
				pCrd->dy = atof(str2);
				m_pCD->m_arNasim.Add(pCrd);
			}
		}
	}
	m_pCD->m_nAuto = m_auto;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}

BOOL LWACalcPage2::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite BCECalcPage2 

IMPLEMENT_DYNCREATE(BCECalcPage2, CPropertyPage)

BCECalcPage2::BCECalcPage2(CalcData* pCD /*=NULL*/) : CPropertyPage(BCECalcPage2::IDD)
{
	//{{AFX_DATA_INIT(BCECalcPage2)
	m_wasserspiegel = 0;
	m_VZVerlust = 0;
	m_anfangsWSP = 0;
	m_RVerlust = 0;
	m_brucke = FALSE;
	m_wehre = FALSE;
	m_gefaelle = 0.0;
	m_hoehe = 0.0;
	m_wsfq = FALSE;
	m_wsfl = FALSE;
	//}}AFX_DATA_INIT
	m_pCD = pCD;
	if (pCD!=NULL)
		m_pState = pCD->GetState();
	else
		m_pState = NULL;
}

BCECalcPage2::~BCECalcPage2()
{
}

void BCECalcPage2::DoDataExchange(CDataExchange* pDX)
{
  LWACalcSheet::replaceChar( this, IDC_EDIT1 );
  LWACalcSheet::replaceChar( this, IDC_EDIT2 );

	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(BCECalcPage2)
	DDX_Control(pDX, IDC_COMBO2, m_abflussereignis);
	DDX_Radio(pDX, IDC_RADIO1, m_wasserspiegel);
	DDX_Radio(pDX, IDC_RADIO5, m_VZVerlust);
	DDX_Radio(pDX, IDC_RADIO8, m_anfangsWSP);
	DDX_Radio(pDX, IDC_RADIO3, m_RVerlust);
	DDX_Check(pDX, IDC_CHECK1, m_brucke);
	DDX_Check(pDX, IDC_CHECK2, m_wehre);
	DDX_Text(pDX, IDC_EDIT1, m_gefaelle);
	DDX_Text(pDX, IDC_EDIT2, m_hoehe);
	DDX_Check(pDX, IDC_CHECK5, m_wsfq);
	DDX_Check(pDX, IDC_CHECK8, m_wsfl);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(BCECalcPage2, CPropertyPage)
	//{{AFX_MSG_MAP(BCECalcPage2)
	ON_BN_CLICKED(IDC_RADIO8, OnAnfangsWSP)
	ON_BN_CLICKED(IDC_RADIO9, OnAnfangsWSP)
	ON_BN_CLICKED(IDC_RADIO10, OnAnfangsWSP)
	ON_BN_CLICKED(IDC_CHECK8, OnCheck8)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten BCECalcPage2 

BOOL BCECalcPage2::OnInitDialog() 
{
	OutFlow *of;
	CString str;
	
	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pCD!=NULL && m_pState!=NULL);
	
	of = m_pState->GetFirstOutFlow();
	while (of!=NULL)
	{
		str = of->GetName();
		m_abflussereignis.AddString(str);
		of = m_pState->GetNextOutFlow();
	}

	m_abflussereignis.SetCurSel(m_pCD->m_nAbfluss);
	m_wasserspiegel = m_pCD->m_nWerte[4]-1;
	m_RVerlust = m_pCD->m_nWerte[5]-1;
	m_VZVerlust = m_pCD->m_nWerte[6]-1;
	switch (m_pCD->m_nWerte[7])
	{
		case 1:	// hoehe
			m_gefaelle = 0;
			m_hoehe = m_pCD->m_dHoehe;
			m_anfangsWSP = 2;
			GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
			break;

		case 3:		// gefaelle
			m_gefaelle = m_pCD->m_dGefaelle;
			m_hoehe = 0;
			m_anfangsWSP = 1;
			GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
			break;

		case 2:	// grenztiefe
			m_gefaelle = 0;
			m_hoehe = 0;
			m_anfangsWSP = 0;
			GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
			GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
			break;
	}
	m_brucke = (BOOL)m_pCD->m_nWerte[8];
	m_wehre = (BOOL)m_pCD->m_nWerte[9];
	m_wsfl = (BOOL)m_pCD->m_nWSFL==1;
	if (m_wsfl)
		m_wsfq = (BOOL)m_pCD->m_nWSFQ==1;
	else
		GetDlgItem(IDC_CHECK5)->EnableWindow(FALSE);

	UpdateData(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void BCECalcPage2::OnAnfangsWSP() 
{
	if (((CButton*)GetDlgItem(IDC_RADIO8))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO9))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
	}
	if (((CButton*)GetDlgItem(IDC_RADIO10))->GetCheck()==1)
	{
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT2)->EnableWindow(TRUE);
	}
}

void BCECalcPage2::OnOK() 
{
	m_bOK = FALSE;
	if (!UpdateData())
		return;

	m_pCD->m_nAbfluss = m_abflussereignis.GetCurSel();
	m_abflussereignis.GetLBText(m_pCD->m_nAbfluss, m_pCD->m_strQ);
	m_pCD->m_nWerte[4] = m_wasserspiegel+1;
	m_pCD->m_nWerte[5] = m_RVerlust+1;
	m_pCD->m_nWerte[6] = m_VZVerlust+1;
	switch (m_anfangsWSP)
	{
		case 2:		// hoehe
			m_pCD->m_dGefaelle = 0;
			m_pCD->m_dHoehe = m_hoehe;
			m_pCD->m_nWerte[7] = 1;
			break;

		case 1:		// gefaelle
			m_pCD->m_dGefaelle = m_gefaelle;
			m_pCD->m_dHoehe = 0;
			m_pCD->m_nWerte[7] = 3;
			break;

		case 0:		// grenztiefe
			m_pCD->m_dGefaelle = 0;
			m_pCD->m_dHoehe = 0;
			m_pCD->m_nWerte[7] = 2;
			break;
	}
	m_pCD->m_nWerte[8] = m_brucke;
	m_pCD->m_nWerte[9] = m_wehre;
	m_pCD->m_nWSFL = m_wsfl;
	if (!m_wsfl)
		m_wsfq = FALSE;
	m_pCD->m_nWSFQ = m_wsfq;
	
	m_bOK = TRUE;
	CPropertyPage::OnOK();
}

BOOL BCECalcPage2::OnApply() 
{
	CPropertyPage::OnApply();
	return m_bOK;
}

void BCECalcPage2::OnCheck8() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK8))->GetCheck()==1)
		GetDlgItem(IDC_CHECK5)->EnableWindow(TRUE);
	else
	{
		((CButton*)GetDlgItem(IDC_CHECK5))->SetCheck(0);
		m_wsfq = FALSE;
		GetDlgItem(IDC_CHECK5)->EnableWindow(FALSE);
	}
}