// dirpage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "dirpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite DirPage 

IMPLEMENT_DYNCREATE(DirPage, CPropertyPage)

DirPage::DirPage() : CPropertyPage(DirPage::IDD)
{
	//{{AFX_DATA_INIT(DirPage)
	//}}AFX_DATA_INIT
}

DirPage::~DirPage()
{
}

void DirPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(DirPage)
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(DirPage, CPropertyPage)
	//{{AFX_MSG_MAP(DirPage)
	ON_BN_CLICKED(IDC_BUTTON1, OnMainDir)
	ON_BN_CLICKED(IDC_BUTTON2, OnEditor)
	ON_BN_CLICKED(IDC_BUTTON3, OnCAD)
	ON_BN_CLICKED(IDC_BUTTON4, OnSpecial)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void DirPage::MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen)
{
	static const _TCHAR szThreeDots[] = _T("\\...");
	int nLength;

	nLength = pDC->GetTextExtent(strLong).cx;
	if(strLong.GetLength() == 0 || nLength <= nStaticLen)
		return;

	CString strShort = strLong;

	while (nLength > nStaticLen)
	{
		int i;
		CString strRight, strLeft, strTemp;

		i = strShort.ReverseFind('\\');
		if (i==-1)
			break;
		strRight = strShort.Right(strShort.GetLength()-i);
		strTemp = strShort.Left(i);
		i = strTemp.ReverseFind('\\');
		if (i==-1)
			break;
		strLeft = strTemp.Left(i);
		strShort = strLeft + szThreeDots + strRight;
		nLength = pDC->GetTextExtent(strShort).cx;
		if (nLength > nStaticLen)
			strShort = strLeft + strRight;
	}

	strLong = strShort;
}

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten DirPage 

BOOL DirPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	CRect rect;
	CDC *pDC = GetDC();
	CFont *pOldFont;
	CString path, str;

	pOldFont = pDC->SelectObject(GetFont());

	str.LoadString(IDS_UNKNOWN);
	m_maindir = theApp.GetProfileString("WSPWIN", "HAUPTPATH", "");
	if (m_maindir.IsEmpty())
		SetDlgItemText(IDC_STATIC1, str);
	else
	{
		GetDlgItem(IDC_STATIC1)->GetClientRect(rect);
		path = m_maindir;
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC1, path);
	}
	
	m_editor = theApp.GetProfileString("WSPWIN", "EDITOR", "");
	if (m_editor.IsEmpty())
		SetDlgItemText(IDC_STATIC2, str);
	else
	{
		GetDlgItem(IDC_STATIC2)->GetClientRect(rect);
		path = m_editor;
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC2, path);
	}
	
	m_CAD = theApp.GetProfileString("WSPWIN", "AUTOSKETCH", "");
	if (m_CAD.IsEmpty())
		SetDlgItemText(IDC_STATIC3, str);
	else
	{
		GetDlgItem(IDC_STATIC3)->GetClientRect(rect);
		path = m_CAD;
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC3, path);
	}
	
	m_special = theApp.GetProfileString("WSPWIN", "SONDERPROGRAMM", "");
	if (m_special.IsEmpty())
		SetDlgItemText(IDC_STATIC4, str);
	else
	{
		GetDlgItem(IDC_STATIC4)->GetClientRect(rect);
		path = m_special;
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC4, path);
	}
	
	pDC->SelectObject(pOldFont);
	ReleaseDC(pDC);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void DirPage::OnMainDir() 
{
	DirectoryDlg dlg( NULL, m_maindir, this);
	if (dlg.DoModal()==IDOK)
	{
		m_maindir = dlg.m_strDir;
		CRect rect;
		CDC *pDC = GetDC();
		CFont *pOldFont;
		CString path = m_maindir;
		
		pOldFont = pDC->SelectObject(GetFont());
		GetDlgItem(IDC_STATIC1)->GetClientRect(rect);
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC1, path);
		pDC->SelectObject(pOldFont);
		ReleaseDC(pDC);
	}
}

void DirPage::OnEditor() 
{
	CFileDialog dlg(TRUE, "*.exe", m_editor, OFN_HIDEREADONLY, "Ausführbare Dateien (*.exe)|*.exe|Alle Dateien (*.*)|*.*||", this);
	if (dlg.DoModal()==IDOK)
	{
		m_editor = dlg.GetPathName();
		CRect rect;
		CDC *pDC = GetDC();
		CFont *pOldFont;
		CString path = m_editor;
		
		pOldFont = pDC->SelectObject(GetFont());
		GetDlgItem(IDC_STATIC2)->GetClientRect(rect);
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC2, path);
		pDC->SelectObject(pOldFont);
		ReleaseDC(pDC);
	}
}

void DirPage::OnCAD() 
{
	CFileDialog dlg(TRUE, "*.exe", m_CAD, OFN_HIDEREADONLY, "Ausführbare Dateien (*.exe)|*.exe|Alle Dateien (*.*)|*.*||", this);
	if (dlg.DoModal()==IDOK)
	{
		m_CAD = dlg.GetPathName();
		CRect rect;
		CDC *pDC = GetDC();
		CFont *pOldFont;
		CString path = m_CAD;
		
		pOldFont = pDC->SelectObject(GetFont());
		GetDlgItem(IDC_STATIC3)->GetClientRect(rect);
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC3, path);
		pDC->SelectObject(pOldFont);
		ReleaseDC(pDC);
	}
}

void DirPage::OnSpecial() 
{
	CFileDialog dlg(TRUE, "exe", m_special, OFN_HIDEREADONLY, "Ausführbare Dateien (*.exe)|*.exe|Alle Dateien (*.*)|*.*||", this);
	if (dlg.DoModal()==IDOK)
	{
		m_special = dlg.GetPathName();
		CRect rect;
		CDC *pDC = GetDC();
		CFont *pOldFont;
		CString path = m_special;
		
		pOldFont = pDC->SelectObject(GetFont());
		GetDlgItem(IDC_STATIC4)->GetClientRect(rect);
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC4, path);
		pDC->SelectObject(pOldFont);
		ReleaseDC(pDC);
	}
}

void DirPage::OnOK() 
{
	if (!m_maindir.IsEmpty())
		theApp.WriteProfileString("WSPWIN", "HAUPTPATH", m_maindir);
	if (!m_editor.IsEmpty())
		theApp.WriteProfileString("WSPWIN", "EDITOR", m_editor);
	if (!m_CAD.IsEmpty())
		theApp.WriteProfileString("WSPWIN", "AUTOSKETCH", m_CAD);
	if (!m_special.IsEmpty())
		theApp.WriteProfileString("WSPWIN", "SONDERPROGRAMM", m_special);
	
	CPropertyPage::OnOK();
}

BOOL DirPage::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	switch (pHelpInfo->iCtrlId)
	{
		case IDC_BUTTON1:
			theApp.WinHelp(HID_KAPITEL_4_2_1);
			break;

		case IDC_BUTTON2:
			theApp.WinHelp(HID_KAPITEL_4_7_3);
			break;

		case IDC_BUTTON3:
			theApp.WinHelp(HID_KAPITEL_6_7_2);
			break;

		case IDC_BUTTON4:
			theApp.WinHelp(HID_KAPITEL_7_6_2);
			break;
	}
	
	return CPropertyPage::OnHelpInfo(pHelpInfo);
}
