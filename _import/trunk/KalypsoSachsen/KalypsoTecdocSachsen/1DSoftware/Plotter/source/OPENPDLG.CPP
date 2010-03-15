// OpenPDlg.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotter.h"

#include "openpdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COpenProjectDialog dialog


COpenProjectDialog::COpenProjectDialog(CWnd* pParent /*=NULL*/)
	: CDialog(COpenProjectDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(COpenProjectDialog)
	//}}AFX_DATA_INIT
}


void COpenProjectDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(COpenProjectDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(COpenProjectDialog, CDialog)
	//{{AFX_MSG_MAP(COpenProjectDialog)
	ON_NOTIFY(NM_DBLCLK, IDC_LIST1, OnDblclkList1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COpenProjectDialog message handlers

BOOL COpenProjectDialog::OnInitDialog() 
{
	CString path, rString, str;
	CString filename, name;
	ifstream ifs;
	char buffer[LINE_SIZE];
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	int width;
	CRect rect;
	CFile file;
	CFileStatus rStatus;

	CDialog::OnInitDialog();
	
	m_list.SetFullRowSel(TRUE);
	m_list.SetImageList( CCommonImageList::GetList( FALSE ), LVSIL_SMALL);
	m_list.GetClientRect(&rect);
	width = rect.Width()/2;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_DIRECTORY);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_NAME);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	lvItem.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	GetWindowsDirectory(path.GetBuffer(MAX_PATH), MAX_PATH);
	path.ReleaseBuffer();
	path += "\\wsp.prj";
	if (!file.GetStatus(path, rStatus))
	{
		AfxFormatString1(rString, IDS_ERROR_READFILE, path);
		AfxMessageBox(rString, MB_OK | MB_ICONSTOP);
		EndDialog(IDCANCEL);
		return TRUE;
	}
	ifs.open(path, ios::in);
	if (ifs.fail())
	{
		AfxFormatString1(rString, IDS_ERROR_READFILE, path);
		AfxMessageBox(rString, MB_OK | MB_ICONSTOP);
		EndDialog(IDCANCEL);
		return TRUE;
	}
	else
	{
		while(!ifs.eof())
		{
			ifs.getline(buffer, LINE_SIZE, '\n');
			path = buffer;
			if (!path.IsEmpty())
			{
				ifstream ifs2;
				lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
				lvItem.iSubItem = 0;
				lvItem.pszText = path.GetBuffer(path.GetLength());
				path.ReleaseBuffer();
				lvItem.cchTextMax = path.GetLength();
				lvItem.iImage = IMAGE_PROJECT;
				m_list.InsertItem(&lvItem);
				name.Empty();
				// try and find project name
				filename = path + "\\prof\\probez.txt";
				if (file.GetStatus(filename, rStatus))
				{
					ifs2.open(filename, ios::in);
					if (!ifs2.fail())
					{
						ifs2.getline(buffer, LINE_SIZE, '\n');
						name = buffer;
						name.TrimLeft();
						name.TrimRight();
						ifs2.close();
					}
				}
				m_list.SetItemText(lvItem.iItem, 1, name);
			}
		}
	}
	m_list.SetItemState(0, LVIS_SELECTED, LVIS_SELECTED);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void COpenProjectDialog::OnOK() 
{
	int i;
	
	if (!UpdateData(TRUE))
		return;
	for (i=0; i<m_list.GetItemCount(); i++)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
		{
			m_dir = m_list.GetItemText(i, 0);
			CDialog::OnOK();
			break;
		}
	}
}

void COpenProjectDialog::OnDblclkList1(NMHDR* /*pNMHDR*/, LRESULT* pResult) 
{
	OnOK();
	
	*pResult = 0;
}
