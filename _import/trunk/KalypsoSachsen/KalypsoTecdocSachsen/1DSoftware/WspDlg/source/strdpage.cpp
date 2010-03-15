// strdpage.cpp: Implementierungsdatei
//
///////////////////////////////////////////
//
// Optionendialog für WspWin
//
////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "wspdlg.h"

#include "strdpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite StandardPage 

IMPLEMENT_DYNCREATE(StandardPage, CPropertyPage)

StandardPage::StandardPage( const BOOL bFeatureSort /* = TRUE */, const BOOL bBCE /*=FALSE*/ ) : CPropertyPage(StandardPage::IDD)
{
	//{{AFX_DATA_INIT(StandardPage)
	m_auto = FALSE;
	m_type = -1;
	m_sort_type = -1;
	m_laengs_sort_type = -1;
	m_sort_Direction = -1;
	//}}AFX_DATA_INIT
	m_bBCE = bBCE;
  m_bFeatureSort = bFeatureSort;
}

StandardPage::~StandardPage()
{
}

void StandardPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(StandardPage)
	DDX_Check(pDX, IDC_CHECK1, m_auto);
	DDX_Radio(pDX, IDC_RADIO1, m_type);
	DDX_Radio(pDX, IDC_RADIO3, m_sort_type);
	DDX_Radio(pDX, IDC_RADIO5, m_laengs_sort_type);
	DDX_Radio(pDX, IDC_RADIO_VORWAERTS, m_sort_Direction);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(StandardPage, CPropertyPage)
	//{{AFX_MSG_MAP(StandardPage)
	ON_BN_CLICKED(IDC_CHECK1, OnAuto)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten StandardPage 

BOOL StandardPage::OnInitDialog() 
{
	CString rauheit, verzsort, laengsverzsort, sortDirection;
	
	CPropertyPage::OnInitDialog();
	
	rauheit = theApp.GetProfileString("WSPWIN", "AUTORAUHEIT", "NEIN");
	if (rauheit=="KS")
	{
		m_auto = TRUE;
		m_type = 0;
	}
	else if (rauheit=="KST")
	{
		m_auto = TRUE;
		m_type = 1;
	}
	else
	{
		m_auto = FALSE;
		m_type = 0;
		GetDlgItem(IDC_RADIO1)->EnableWindow(FALSE);
		GetDlgItem(IDC_RADIO2)->EnableWindow(FALSE);
	}
	if (!m_bBCE)
	{
		verzsort = theApp.GetProfileString("WSPWIN", "VERZWEIGSORT", "HAND");
		if (verzsort=="AUTO")
			m_sort_type = 1;
		else
			m_sort_type = 0;
        laengsverzsort = theApp.GetProfileString("WSPWIN", "LAENGSVERZWEIGSORT", "STATION");
		if (laengsverzsort=="VERZWEIGUNG")
			m_laengs_sort_type = 1;
		else
			m_laengs_sort_type = 0;

    sortDirection = theApp.GetProfileString("WSPWIN", "SORTRICHTUNG", "VORWAERTS");
    if ( sortDirection == "RUECKWAERTS" )
      m_sort_Direction = 1;
    else
      m_sort_Direction = 0;
	}
	else
	{
    m_sort_Direction = 0; // für BCE immer vorwärts
    m_sort_type = 1;
    m_laengs_sort_type = 0;

		GetDlgItem(IDC_STATIC1)->ShowWindow(FALSE);
		GetDlgItem(IDC_STATIC2)->ShowWindow(FALSE);
		GetDlgItem(IDC_RADIO3)->ShowWindow(FALSE);
		GetDlgItem(IDC_RADIO4)->ShowWindow(FALSE);

    GetDlgItem(IDC_STATIC3)->ShowWindow(FALSE);
		GetDlgItem(IDC_STATIC4)->ShowWindow(FALSE);
		GetDlgItem(IDC_RADIO5)->ShowWindow(FALSE);
		GetDlgItem(IDC_RADIO6)->ShowWindow(FALSE);

    GetDlgItem(IDC_STATIC_SORT_HEAD)->ShowWindow(FALSE);
    GetDlgItem(IDC_STATIC_SORT_DIRECTION)->ShowWindow(FALSE);
    GetDlgItem(IDC_RADIO_VORWAERTS)->ShowWindow(FALSE);
    GetDlgItem(IDC_RADIO_RUECKWAERTS)->ShowWindow(FALSE);
	}

  if( !m_bFeatureSort )
  {
    GetDlgItem(IDC_STATIC_SORT_HEAD)->ShowWindow(FALSE);
    GetDlgItem(IDC_STATIC_SORT_DIRECTION)->ShowWindow(FALSE);
    GetDlgItem(IDC_RADIO_VORWAERTS)->ShowWindow(FALSE);
    GetDlgItem(IDC_RADIO_RUECKWAERTS)->ShowWindow(FALSE);
  };


	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void StandardPage::OnAuto() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck()==1)
	{
		GetDlgItem(IDC_RADIO1)->EnableWindow(TRUE);
		GetDlgItem(IDC_RADIO2)->EnableWindow(TRUE);
	}
	else
	{
		GetDlgItem(IDC_RADIO1)->EnableWindow(FALSE);
		GetDlgItem(IDC_RADIO2)->EnableWindow(FALSE);
	}
}

void StandardPage::OnOK() 
{
	CString rauheit, verzsort, laengsverzsort, sortDirection;
	
	if (!UpdateData())
		return;

	if (!m_auto)
		rauheit = "NEIN";
	else if (m_type==0)
		rauheit = "KS";
	else
		rauheit = "KST";
	theApp.WriteProfileString("WSPWIN", "AUTORAUHEIT", rauheit);

  if (m_sort_type==0)
    verzsort = "HAND";
	else
    verzsort = "AUTO";
	theApp.WriteProfileString("WSPWIN", "VERZWEIGSORT", verzsort);

  if (m_laengs_sort_type==1)
		laengsverzsort = "VERZWEIGUNG";
	else
		laengsverzsort = "STATION";
	theApp.WriteProfileString("WSPWIN", "LAENGSVERZWEIGSORT", laengsverzsort);

  if ( m_sort_Direction == 0 )
    sortDirection = "VORWAERTS";
  else
    sortDirection = "RUECKWAERTS";
  theApp.WriteProfileString("WSPWIN", "SORTRICHTUNG", sortDirection);
	
	CPropertyPage::OnOK();
}
