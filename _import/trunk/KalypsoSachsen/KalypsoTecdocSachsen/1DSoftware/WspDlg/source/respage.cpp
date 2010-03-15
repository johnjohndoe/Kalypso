// respage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "respage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite ResPage 

IMPLEMENT_DYNCREATE(ResPage, CPropertyPage)

ResPage::ResPage() : CPropertyPage(ResPage::IDD)
{
	//{{AFX_DATA_INIT(ResPage)
	m_auto = FALSE;
	m_size = -1;
	m_res = _T("");
	//}}AFX_DATA_INIT
}

ResPage::~ResPage()
{
}

void ResPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ResPage)
	DDX_Check(pDX, IDC_CHECK1, m_auto);
	DDX_Radio(pDX, IDC_RADIO1, m_size);
	DDX_Text(pDX, IDC_STATIC1, m_res);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ResPage, CPropertyPage)
	//{{AFX_MSG_MAP(ResPage)
	ON_BN_CLICKED(IDC_CHECK1, OnAuto)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ResPage 

BOOL ResPage::OnInitDialog() 
{
	int smode;
	
	CPropertyPage::OnInitDialog();
	
	smode = theApp.GetProfileInt("WSPWIN", "SCREENMODE", 0);
	switch (smode)
	{
		case 0:
			m_auto = TRUE;
			m_size = 1;
			GetDlgItem(IDC_RADIO1)->EnableWindow(FALSE);
			GetDlgItem(IDC_RADIO2)->EnableWindow(FALSE);
			break;

		case 640:
		case 800:
			m_auto = FALSE;
			m_size = 0;
			break;

		case 1024:
			m_auto = FALSE;
			m_size = 1;
			break;
	}
	m_res.Format("%d x %d", (int)::GetSystemMetrics(SM_CXSCREEN), (int)::GetSystemMetrics(SM_CYSCREEN));
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void ResPage::OnOK() 
{
	int smode;

	UpdateData();

	if (m_auto)
		smode = 0;
	else
	{
		switch (m_size)
		{
			case 0:
				smode = 800;
				break;

			case 1:
				smode = 1024;
				break;
		}
	}

	theApp.WriteProfileInt("WSPWIN", "SCREENMODE", smode);
	
	CPropertyPage::OnOK();
}

void ResPage::OnAuto() 
{
	if (((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck()==1)
	{
		GetDlgItem(IDC_RADIO1)->EnableWindow(FALSE);
		GetDlgItem(IDC_RADIO2)->EnableWindow(FALSE);
	}
	else
	{
		GetDlgItem(IDC_RADIO1)->EnableWindow(TRUE);
		GetDlgItem(IDC_RADIO2)->EnableWindow(TRUE);
	}
}

BOOL ResPage::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	theApp.WinHelp(HID_KAPITEL_7_6_1);
	
	return CPropertyPage::OnHelpInfo(pHelpInfo);
}
