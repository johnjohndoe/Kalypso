// intrppg3.cpp: Implementierungsdatei
//

#include "stdAfx.h"

#include "intrpsht.h"
#include "interp.h"

#include "intrppg3.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CInterpPage3 

IMPLEMENT_DYNCREATE(CInterpPage3, CPropertyPage)

CInterpPage3::CInterpPage3(CInterpSheet *pOwner) : CPropertyPage(CInterpPage3::IDD)
{
	//{{AFX_DATA_INIT(CInterpPage3)
	m_wholeProfile = 0;
	m_distance = 0.0;
	m_existing = FALSE;
	//}}AFX_DATA_INIT
	m_pOwner = pOwner;
}

CInterpPage3::~CInterpPage3()
{
}

void CInterpPage3::MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen)
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

void CInterpPage3::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CInterpPage3)
	DDX_Radio(pDX, IDC_RADIO1, m_wholeProfile);
	DDX_Text(pDX, IDC_EDIT1, m_distance);
	DDX_Check(pDX, IDC_CHECK1, m_existing);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CInterpPage3, CPropertyPage)
	//{{AFX_MSG_MAP(CInterpPage3)
	ON_BN_CLICKED(IDC_BUTTON1, OnButton1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CInterpPage3 

BOOL CInterpPage3::OnInitDialog() 
{
	CString str;
	
	CPropertyPage::OnInitDialog();
	
	str = theApp.m_pProject->GetDir();
	GetDlgItem(IDC_STATIC1)->SetWindowText(str);
	str = theApp.m_pState->GetName();
	GetDlgItem(IDC_STATIC2)->SetWindowText(str);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CInterpPage3::OnSetActive() 
{
	m_pOwner->SetWizardButtons(PSWIZB_NEXT);
	
	return CPropertyPage::OnSetActive();
}

void CInterpPage3::OnButton1() 
{
	CString str;
	str.LoadString(IDS_ALLFILES);
	CFileDialog dlg(TRUE, "", m_file, OFN_HIDEREADONLY, str, this);
	if (dlg.DoModal()==IDOK)
	{
		m_file = dlg.GetPathName();
		CRect rect;
		CDC *pDC = GetDC();
		CFont *pOldFont;
		CString path = m_file;
		
		pOldFont = pDC->SelectObject(GetFont());
		GetDlgItem(IDC_STATIC3)->GetClientRect(rect);
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC3, path);
		pDC->SelectObject(pOldFont);
		ReleaseDC(pDC);
	}
}

LRESULT CInterpPage3::OnWizardNext() 
{
	CFileStatus rStatus;
	CString rString;
	
	if (!UpdateData())
		return 1L;

	if (m_file.IsEmpty())
	{
		AfxMessageBox(IDS_NOFILENAME, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_BUTTON1)->SetFocus();
		return 1L;
	}

	if (!CFile::GetStatus(m_file, rStatus))
	{
		AfxMessageBox(IDS_NOFILEEXIST, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_BUTTON1)->SetFocus();
		return 1L;
	}

	gifstream ifs;

	m_pOwner->m_batch.DeleteContents();
	ifs.open(m_file, ios::in);
	if (ifs.fail())
	{
		rString.FormatMessage(IDS_ERROR_READFILE, m_file);
		AfxMessageBox(rString, MB_ERROR);
		return 1L;
	}
	else
	{
		ifs >> m_pOwner->m_batch;
		ifs.close();
	}

	m_pOwner->m_batch.m_bWholeProfile = (BOOL)(m_wholeProfile);
	m_pOwner->m_batch.m_bExisting = m_existing;
	m_pOwner->m_batch.m_dDistance = m_distance;

	return CPropertyPage::OnWizardNext();
}
