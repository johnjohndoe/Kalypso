// newldlg.cpp: Implementierungsdatei
//

#include "stdafx.h"


#include "maplayer.h"
#include "newldlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static int fieldTypeStrings[5] =
{
	IDS_WHOLENUM,
	IDS_DECIMAL,
	IDS_DATE,
	IDS_TEXT,
	IDS_BOOLEAN
};

static int fieldTypes[5] =
{
	moLong,
	moDouble,
	moDate,
	moString,
	moBoolean
};

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNewLayerDialog 

CNewLayerDialog::CNewLayerDialog( CWnd* pParent /*=NULL*/ )
	: CDialog(CNewLayerDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNewLayerDialog)
	m_type = 2;
	m_name = _T("");
	m_fieldCount = 1;
	//}}AFX_DATA_INIT
}


void CNewLayerDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNewLayerDialog)
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Radio(pDX, IDC_RADIO1, m_type);
	DDX_Text(pDX, IDC_EDIT1, m_name);
	DDX_Text(pDX, IDC_EDIT2, m_fieldCount);
	DDV_MinMaxInt(pDX, m_fieldCount, 1, 100);
	//}}AFX_DATA_MAP
}

void CNewLayerDialog::MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen)
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

BEGIN_MESSAGE_MAP(CNewLayerDialog, CDialog)
	//{{AFX_MSG_MAP(CNewLayerDialog)
	ON_BN_CLICKED(IDC_BUTTON1, OnChangeFile)
	ON_EN_CHANGE(IDC_EDIT2, OnChangeFieldCount)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CNewLayerDialog 

BOOL CNewLayerDialog::OnInitDialog() 
{
	CRect rect;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	CString str, data;
	CDC *pDC = GetDC();

	CDialog::OnInitDialog();
	
	m_list.SetFullRowSel(TRUE);
	m_list.SetGridList(TRUE);
	m_list.GetClientRect(&rect);
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = rect.Width()/2;
	str.LoadString(IDS_NAME);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	str.LoadString(IDS_TYPE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
	str.FormatMessage(IDS_VALUEN, lvItem.iItem+1);
	lvItem.iSubItem = 0;
	lvItem.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvItem.cchTextMax = str.GetLength();
	m_list.InsertItem(&lvItem);
	m_list.SetItemControl(lvItem.iItem, 0, LCTRLEX_EDIT, NULL, STRING_FIELD);
	data.LoadString(IDS_FIELDSTRINGS);
	str.LoadString(IDS_DECIMAL);
	m_list.SetItemText(lvItem.iItem, 1, str);
	m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWNLIST, data);

	m_spin.SetBuddy(GetDlgItem(IDC_EDIT2));
	m_spin.SetRange(1, 100);
	m_spin.SetBase(10);
	m_spin.SetPos(1);

	GetDlgItem(IDC_STATIC1)->GetClientRect(rect);
	str = m_file;
	MakeShortPath(pDC, str, rect.Width());
	SetDlgItemText(IDC_STATIC1, str);
	ReleaseDC(pDC);

	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CNewLayerDialog::OnChangeFile() 
{
	CRect rect;
	CDC *pDC = GetDC();
	CString path, str;
	str.LoadString(IDS_SHAPEFILE_OR_ALLFILES);
	CFileDialog dlg(FALSE, "shp", m_file, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		str, this);

	if (dlg.DoModal()==IDOK)
	{
		m_file = dlg.GetPathName();
		GetDlgItem(IDC_STATIC1)->GetClientRect(rect);
		path = m_file;
		MakeShortPath(pDC, path, rect.Width());
		SetDlgItemText(IDC_STATIC1, path);
	}
	ReleaseDC(pDC);
}

void CNewLayerDialog::OnOK() 
{
	int i, j;
	CString str, temp;
	CFileStatus rStatus;
	
	if (!UpdateData())
		return;

	if (m_name.IsEmpty())
	{
		AfxMessageBox(IDS_NONAME, MB_OK | MB_ICONEXCLAMATION);
		GetDlgItem(IDC_EDIT1)->SetFocus();
		return;
	}

	for (i=0; i<m_list.GetItemCount()-1; i++)
	{
		str = m_list.GetItemText(i, 0);
		for (j=i+1; j<m_list.GetItemCount(); j++)
		{
			if (str.Compare(m_list.GetItemText(j, 0))==0)
			{
				AfxMessageBox(IDS_SAMEFIELDS, MB_OK | MB_ICONEXCLAMATION);
				GetDlgItem(IDC_LIST1)->SetFocus();
				return;
			}
		}
	}

	if (CFile::GetStatus(m_file, rStatus))
	{
		if (!::DeleteFile(m_file))
		{
			AfxMessageBox(IDS_FILEINUSE, MB_OK | MB_ICONEXCLAMATION);
			GetDlgItem(IDC_BUTTON1)->SetFocus();
			return;
		}
	}

	for (i=0; i<m_list.GetItemCount(); i++)
	{
		m_fieldNames.Add(m_list.GetItemText(i, 0));
		m_fieldTypes.Add(moDouble);
		str = m_list.GetItemText(i, 1);
		for (j=0; j<5; j++)
		{
			temp.LoadString(fieldTypeStrings[j]);
			if (str==temp)
			{
				m_fieldTypes.SetAt(i, fieldTypes[j]);
				break;
			}
		}
	}
	
	CDialog::OnOK();
}

void CNewLayerDialog::OnChangeFieldCount() 
{
	int n;
	CString str, data;
	LV_ITEM lvItem;

	GetDlgItem(IDC_EDIT2)->GetWindowText(str);
	n = atoi(str);
	n = max(n, 1);
	n = min(n, 100);
	str.Format("%d", n);
	GetDlgItem(IDC_EDIT2)->SetWindowText(str);

	while (m_list.GetItemCount()>n)
		m_list.DeleteItem(m_list.GetItemCount()-1);

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	while (m_list.GetItemCount()<n)
	{
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		str.FormatMessage(IDS_VALUEN, lvItem.iItem+1);
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		m_list.SetItemControl(lvItem.iItem, 0, LCTRLEX_EDIT, NULL, STRING_FIELD);
		data.LoadString(IDS_FIELDSTRINGS);
		str.LoadString(IDS_DECIMAL);
		m_list.SetItemText(lvItem.iItem, 1, str);
		m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWNLIST, data);
	}
}

CString CNewLayerDialog::GetFieldName(int i)
{
	CString name;

	if (i>=0 && i<m_fieldNames.GetSize())
		name = m_fieldNames[i];

	name = name.Left(8);
	return name;
}

short CNewLayerDialog::GetFieldType(int i)
{
	if (i>=0 && i<m_fieldTypes.GetSize())
		return m_fieldTypes[i];

	return moDouble;
}
