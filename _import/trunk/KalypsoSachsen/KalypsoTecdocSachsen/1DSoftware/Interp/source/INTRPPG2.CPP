// intrppg2.cpp : implementation file
//

#include "stdAfx.h"

#include "intrpsht.h"
#include "interp.h"

#include "intrppg2.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInterpPage2 property page

IMPLEMENT_DYNCREATE(CInterpPage2, CPropertyPage)

CInterpPage2::CInterpPage2(CInterpSheet *pOwner /*=NULL*/) : CPropertyPage(CInterpPage2::IDD)
{
	//{{AFX_DATA_INIT(CInterpPage2)
	m_watername = _T("");
	m_station = 0.0;
	m_vzk = 0;
	m_num = 0;
	m_state = _T("");
	m_pk = _T("0");
	//}}AFX_DATA_INIT
	m_pOwner = pOwner;
	m_nCount = 1;
	m_nDone = 0;
}

CInterpPage2::~CInterpPage2()
{
	int i;

	for (i=0; i<m_profils.GetSize(); i++)
		delete m_profils.GetAt(i);
	m_profils.RemoveAll();
}

void CInterpPage2::DoDataExchange(CDataExchange* pDX)
{
	double st1, st2;
	CString text, defaultLang;

	st1 = m_pOwner->GetMinStation();
	st2 = m_pOwner->GetMaxStation();
	CPropertyPage::DoDataExchange(pDX);
	GetDlgItem(IDC_EDIT2)->GetWindowText(text);
	defaultLang = setlocale(LC_NUMERIC, NULL);
	if (text.Find('.')!=-1)		// try English format
		setlocale(LC_NUMERIC, "English");
	//{{AFX_DATA_MAP(CInterpPage2)
	DDX_Text(pDX, IDC_EDIT1, m_watername);
	DDX_Text(pDX, IDC_EDIT2, m_station);
	DDV_MinMaxDouble(pDX, m_station, st1, st2);
	DDX_Text(pDX, IDC_EDIT4, m_vzk);
	DDX_Text(pDX, IDC_EDIT6, m_num);
	DDX_Text(pDX, IDC_EDIT3, m_state);
	DDX_Text(pDX, IDC_EDIT5, m_pk);
	DDV_MaxChars(pDX, m_pk, 8);
	//}}AFX_DATA_MAP
	setlocale(LC_NUMERIC, defaultLang);
}


BEGIN_MESSAGE_MAP(CInterpPage2, CPropertyPage)
	//{{AFX_MSG_MAP(CInterpPage2)
	ON_EN_KILLFOCUS(IDC_EDIT2, OnKillfocusEdit2)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CInterpPage2::SetProfilNum(int n)
{
	CrossSection *cs;
	CString str, file;
	int dummy, i = theApp.m_pProject->GetCrossSectionCount()+1;
	POSITION pos;

	if (n>m_profils.GetSize())
	{
		while(n>m_profils.GetSize())
		{
			cs = new CrossSection(theApp.m_pProject);
			m_pOwner->GetWaterName(str);
			cs->SetWaterName(str);
			file = str.Left(2);
			m_pOwner->GetStateName(str);
			cs->SetStateName(str);
			while (m_usedProfilNums.Lookup(i, dummy))
				i++;
			cs->SetProfilNr(i);
			str = "0";
			cs->SetVZK(0);
			cs->SetPK(str);
			cs->CreateFileName(&m_usedFileNames);
			m_usedFileNames.AddTail(cs->GetFileName());
			m_profils.Add(cs);
			m_usedProfilNums.SetAt(i, m_profils.GetSize());
		}
	}
	else if (n<m_profils.GetSize())
	{
		while (n<m_profils.GetSize())
		{
			cs = m_profils.GetAt(m_profils.GetSize()-1);
			m_usedProfilNums.RemoveKey(cs->GetProfilNr());
			pos = m_usedFileNames.Find(cs->GetFileName());
			if (pos!=NULL)
				m_usedFileNames.RemoveAt(pos);
			delete cs;
			m_profils.RemoveAt(m_profils.GetSize()-1);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CInterpPage2 message handlers

BOOL CInterpPage2::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CInterpPage2::OnSetActive() 
{
	CString str, temp1, temp2;
	CrossSection *cs;
	CFont *pFont, font;
	LOGFONT logfont;

	m_nDone = max(m_nDone, m_nCount);
	m_pOwner->SetWizardButtons(PSWIZB_BACK | (m_nCount>=m_profils.GetSize() ? PSWIZB_FINISH : PSWIZB_NEXT));
	str.FormatMessage(IDS_PROFIL_DATA, m_nCount);
	GetDlgItem(IDC_STATIC1)->SetWindowText(str);
	pFont = GetDlgItem(IDC_STATIC2)->GetFont();
	pFont->GetLogFont(&logfont);
	logfont.lfWeight = FW_BOLD;
	font.CreateFontIndirect(&logfont);
	GetDlgItem(IDC_STATIC1)->SetFont(&font);
	temp1.Format("%.4f", m_pOwner->GetMinStation());
	temp2.Format("%.4f", m_pOwner->GetMaxStation());
	str.FormatMessage(IDS_STATION_BOUNDS, temp1, temp2);
	GetDlgItem(IDC_STATIC2)->SetWindowText(str);

	cs = m_profils.GetAt(m_nCount-1);
	m_watername = cs->GetWaterName();
	m_station = cs->GetStation();
	m_state = cs->GetStateName();
	m_vzk = cs->GetVZK();
	m_pk = cs->GetPK();
	m_num = cs->GetProfilNr();

	UpdateData(FALSE);
	
	return CPropertyPage::OnSetActive();
}

LRESULT CInterpPage2::OnWizardBack() 
{
	if (!UpdateProfilData())
		return 1;

	if (m_nCount>1)
	{
		m_nCount--;
		OnSetActive();
		return -1;
	}

	return CPropertyPage::OnWizardBack();
}

LRESULT CInterpPage2::OnWizardNext() 
{
	if (!UpdateProfilData())
		return 1;
	m_nCount++;
	
	return CPropertyPage::OnWizardNext();
}

BOOL CInterpPage2::OnWizardFinish() 
{
	if (!UpdateProfilData())
		return FALSE;
	m_pOwner->Interpolate();
	
	return CPropertyPage::OnWizardFinish();
}

BOOL CInterpPage2::UpdateProfilData()
{
	CrossSection *cs;
	CString file, str;
	int n, nmin = theApp.m_pProject->GetCrossSectionCount()+1;

	if (!UpdateData(TRUE))
		return FALSE;
	if (m_pk.IsEmpty())
	{
		AfxMessageBox(IDS_PKEMPTY, MB_OK | MB_ICONEXCLAMATION);
		return FALSE;
	}
	if (m_num<nmin || (m_usedProfilNums.Lookup(m_num, n) && n!=m_nCount))
	{
		AfxMessageBox(IDS_NUMUSED, MB_OK | MB_ICONEXCLAMATION);
		return FALSE;
	}
	if (KeyExists(m_station, m_vzk, m_pk) || theApp.m_pProject->KeyExists(m_station, m_vzk, m_pk))
	{
		AfxMessageBox(IDS_KEYEXISTS, MB_OK | MB_ICONEXCLAMATION);
		return FALSE;
	}
	cs = m_profils.GetAt(m_nCount-1);
	cs->SetVZK(m_vzk);
	cs = m_profils.GetAt(m_nCount-1);
	cs->SetWaterName(m_watername);
	cs->SetStation(m_station);
	cs->SetStateName(m_state);
	cs->SetPK(m_pk);
	m_usedProfilNums.RemoveKey(cs->GetProfilNr());
	cs->SetProfilNr(m_num);
	m_usedProfilNums.SetAt(m_num, m_nCount);
	return TRUE;
}

BOOL CInterpPage2::KeyExists(double station, int vzk, CString pk)
{
	CrossSection *cs;
	int i;
	CString cpk;
	int cvzk;

	for (i=0; i<m_profils.GetSize(); i++)
	{
		if (i<=m_nDone && i!=m_nCount-1)
		{
			cs = m_profils[i];
			cvzk = cs->GetVZK();
			cpk = cs->GetPK();
			if (cs->GetStation()==station && cvzk==vzk && cpk==pk)
				return TRUE;
		}
	}

	return FALSE;
}

void CInterpPage2::OnKillfocusEdit2() 
{
	CString text, defaultLang;
	double value;

	GetDlgItem(IDC_EDIT2)->GetWindowText(text);
	defaultLang = setlocale(LC_NUMERIC, NULL);
	if (text.Find('.')!=-1)		// try English format
		setlocale(LC_NUMERIC, "English");
	value = atof(text);
	setlocale(LC_NUMERIC, defaultLang);
	text.Format("%.4f", value);
	GetDlgItem(IDC_EDIT2)->SetWindowText(text);
}
