// OpenDlg.cpp : implementation file
//

#include "stdafx.h"

#include "Global.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static CListCtrl *listCtrl;
//static CTypedPtrMap<CMapWordToPtr, WORD, Section*> *secmap_list;

int CALLBACK ListCompareFunc(LPARAM lParam1, LPARAM lParam2, 
	LPARAM lParamSort)
{
	int nResult = 0;
	CString str1, str2;

	switch(lParamSort)
	{
		case 0:		// column 0
			str1 = listCtrl->GetItemText(lParam1, 3);
			str2 = listCtrl->GetItemText(lParam2, 3);
			nResult = str1.CompareNoCase(str2);
			break;

		case 1:		// column 1
			str1 = listCtrl->GetItemText(lParam1, 3);
			str2 = listCtrl->GetItemText(lParam2, 3);
			nResult = str1.CompareNoCase(str2);
			break;
	}
	return nResult;
}

/////////////////////////////////////////////////////////////////////////////
// OpenDialog dialog

OpenDialog::OpenDialog(CWnd* pParent /*=NULL*/)
	: CDialog(OpenDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(OpenDialog)
	m_project = _T("");
	//}}AFX_DATA_INIT
}


void OpenDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(OpenDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Control(pDX, IDC_TREE1, m_tree);
	DDX_Text(pDX, IDC_PROJEKT, m_project);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(OpenDialog, CDialog)
	//{{AFX_MSG_MAP(OpenDialog)
	ON_NOTIFY(NM_DBLCLK, IDC_LIST1, OnDblclkList1)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_LIST1, OnColumnclickList1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// OpenDialog message handlers

BOOL OpenDialog::OnInitDialog() 
{
	HTREEITEM hTIW, hTIZ, hTIR1, hTIR2, hTIR3;
	Project *proj;
	State *st;
	Calculation *calc;
	CString str, fmt, wname;
	CString fileName,fileName2;
	CRect rect;
	LV_COLUMN lvColumn;
	int width, i, j;
	CFile file;
	CFileStatus rStatus;
	
	CDialog::OnInitDialog();
	
	listCtrl = &m_list;
  m_tree.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	m_list.SetFullRowSel(TRUE);
  m_list.SetImageList( CCommonImageList::GetList( FALSE ), LVSIL_SMALL);
	m_list.GetClientRect(&rect);
	width = rect.Width()/3;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width*2;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	proj = theApp.m_pProject;
	m_project = proj->GetName();
	if (m_project.IsEmpty())
		 m_project = proj->GetDir();
	UpdateData(FALSE);
	// first states and result files
	for (j=0; j<proj->GetWaterCount(); j++)
	{
		wname = proj->GetWaterName(j);
		hTIW = m_tree.InsertItem(wname, IMAGE_WATER, IMAGE_WATER);
		st = proj->GetFirstState();
		while (st!=NULL)
		{
			str = st->GetWaterName();
			if (str==wname)
			{
				BOOL bResults[N_RESULT_TYPES];
				for (i=0; i<N_RESULT_TYPES; i++)
					bResults[i] = FALSE;
				str = st->GetName();
				hTIZ = m_tree.InsertItem(str, IMAGE_STATE, IMAGE_STATE, hTIW);
				calc = st->GetFirstCalculation();
				while (calc!=NULL)
				{
					// check for results files
					for (i=0; i<N_RESULT_TYPES; i++)
					{
						if (calc->GetResultFileName(fileName, i))
							bResults[i] = TRUE;
					}
					calc = st->GetNextCalculation();
				}
				if (bResults[0] || bResults[1])
				{
					str.LoadString(IDS_ER);
					hTIR1 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIZ);
					m_smap_tree.SetAt(hTIR1, st);
					m_rmap_tree.SetAt(hTIR1, RESULT_TYPE_ER);
				}
				BOOL bOK = FALSE;
				for (i=1; i<N_RESULT_TYPES; i++)
				{
					if (bResults[i])
					{
						bOK = TRUE;
						break;
					}
				}
				if (bOK)
				{
					str.LoadString(IDS_RESULTS);
					hTIR1 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIZ);
					bOK = FALSE;
					for (i=RESULT_TYPE_WK; i<=RESULT_TYPE_EX; i++)
					{
						if (bResults[i])
						{
							bOK = TRUE;
							break;
						}
					}
					if (bOK)
					{
						str.LoadString(IDS_LIST);
						hTIR2 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIR1);
						for (i=RESULT_TYPE_WK; i<=RESULT_TYPE_EX; i++)
						{
							if (bResults[i])
							{
								int nID;
							
								switch(i)
								{
									case RESULT_TYPE_WK:
										nID = IDS_WK;
										break;
							
									case RESULT_TYPE_UE:
										nID = IDS_UE;
										break;
							
									case RESULT_TYPE_MA:
										nID = IDS_MA;
										break;
							
									case RESULT_TYPE_EX:
										nID = IDS_EX;
										break;
							
									default:
										ASSERT(FALSE);
								}
								str.LoadString(nID);
								hTIR3 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIR2);
								m_smap_tree.SetAt(hTIR3, st);
								m_rmap_tree.SetAt(hTIR3, i);
							}
						}
					}
					if (bResults[RESULT_TYPE_PR])
					{
						str.LoadString(IDS_PR);
						hTIR2 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIR1);
						m_smap_tree.SetAt(hTIR2, st);
						m_rmap_tree.SetAt(hTIR2, RESULT_TYPE_PR);
					}
					if (bResults[RESULT_TYPE_VG])
					{
						str.LoadString(IDS_VG);
						hTIR2 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3, hTIR1);
						m_smap_tree.SetAt(hTIR2, st);
						m_rmap_tree.SetAt(hTIR2, RESULT_TYPE_VG);
					}
				}
			}
			st = proj->GetNextState();
		}
	}
	// Ergebnisse der Massenberechnung
	fileName = proj->GetCalcDir();
	fileName += "masse.txt";
	if (file.GetStatus(fileName, rStatus))
	{
		str.LoadString(IDS_RESULT_MEASURE);
		hTIR1 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3);
		m_smap_tree.SetAt(hTIR1, NULL);
		m_rmap_tree.SetAt(hTIR1, 0);
	}
	// LOG files
	fileName = proj->GetCalcDir();
	fileName += "*.log";
    fileName2 += "*.err";
	if (file.GetStatus(fileName, rStatus) || file.GetStatus(fileName2, rStatus))
	{
		str.LoadString(IDS_LOG_FILES);
		hTIR1 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3);
		m_smap_tree.SetAt(hTIR1, NULL);
		m_rmap_tree.SetAt(hTIR1, 1);
	}
	else
	{
		fileName = proj->GetDataDir();
		fileName += "*.log";
        fileName2 += "*.err";
		if (file.GetStatus(fileName, rStatus) || file.GetStatus(fileName2, rStatus))
		{
			str.LoadString(IDS_LOG_FILES);
			hTIR1 = m_tree.InsertItem(str, IMAGE_TEXT3, IMAGE_TEXT3);
			m_smap_tree.SetAt(hTIR1, NULL);
			m_rmap_tree.SetAt(hTIR1, 1);
		}
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void OpenDialog::OnOK() 
{
	int iItem;
	CString fileName, fileDir;

	if (!UpdateData(TRUE))
		return;
	m_filenames.RemoveAll();
	for (iItem = 0; iItem<m_list.GetItemCount(); iItem++)
	{
		if (m_list.GetItemState(iItem, LVIS_SELECTED)==LVIS_SELECTED)
		{
			fileDir = theApp.m_pProject->GetCalcDir();
			if (m_otherfiles.GetSize()>0)
			{
				fileName = m_otherfiles[iItem];
				fileDir = m_otherdirs[iItem];
			}
			else
				fileName = m_list.GetItemText(iItem, 1);
			m_directories.SetAtGrow(m_directories.GetSize(), fileDir);
			m_filenames.SetAtGrow(m_filenames.GetSize(), fileName);
		}
	}
	CDialog::OnOK();
}

void OpenDialog::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	Project *proj;
	Calculation *calc;
	State *st;
	CString str, fmt;
	CString fileName;
	int i, image;
	WORD type;
	LV_ITEM lvItem;
	LV_COLUMN lvColumn[2];
	CString text[2];
	
	lvItem.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	proj = theApp.m_pProject;
	m_list.DeleteAllItems();
	m_otherfiles.RemoveAll();
	m_otherdirs.RemoveAll();
	if (m_smap_tree.Lookup(pNMTreeView->itemNew.hItem, st))
	{
		if (m_rmap_tree.Lookup(pNMTreeView->itemNew.hItem, type))
		{
			text[0].LoadString(IDS_FILENAME);
			text[1].LoadString(IDS_DIRECTORY);
			image = IMAGE_TEXT3;
			if (st!=NULL)
			{
				text[0].LoadString(IDS_LSECTION);
				text[1].LoadString(IDS_ORIGINFILE);
				image = IMAGE_LSECTION;
				calc = st->GetFirstCalculation();
				while (calc!=NULL)
				{
					if (calc->GetResultFileName(fileName, type))
					{
						str = calc->GetName();
						lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
						lvItem.iSubItem = 0;
						lvItem.pszText = str.GetBuffer(str.GetLength());
						str.ReleaseBuffer();
						lvItem.cchTextMax = str.GetLength();
						lvItem.iImage = image;
						m_list.InsertItem(&lvItem);
						m_list.SetItemText(lvItem.iItem, 1, fileName);
					}
					if (type==RESULT_TYPE_ER)
					{
						if (calc->GetResultFileName(fileName, RESULT_TYPE_TB))
						{
							str = calc->GetName();
							lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
							lvItem.iSubItem = 0;
							lvItem.pszText = str.GetBuffer(str.GetLength());
							str.ReleaseBuffer();
							lvItem.cchTextMax = str.GetLength();
							lvItem.iImage = image;
							m_list.InsertItem(&lvItem);
							m_list.SetItemText(lvItem.iItem, 1, fileName);
						}
					}
					calc = st->GetNextCalculation();
				}
			}
			else if (type==0)
			{
				str = "masse.txt";
				fileName = proj->GetDir();
				fileName += "\\dath";
				lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
				lvItem.iSubItem = 0;
				lvItem.pszText = str.GetBuffer(str.GetLength());
				str.ReleaseBuffer();
				lvItem.cchTextMax = str.GetLength();
				lvItem.iImage = image;
				m_list.InsertItem(&lvItem);
				m_list.SetItemText(lvItem.iItem, 1, fileName);
				fileName += "\\";
				m_otherdirs.Add(fileName);
				m_otherfiles.Add(str);
			}
			else if (type==1)
			{
				WIN32_FIND_DATA fd;
				HANDLE h;
				CString search;
                int i;
                for(i=0;i<2;i++)
                    {
				// DATH
				search = proj->GetCalcDir();
                switch(i)
                    {
                    case 0:search += "*.log";break;
                    case 1:search += "*.err";break;
                    default:search += "*.log";break;
                    }
				h = ::FindFirstFile(search, &fd);
				if (h!=INVALID_HANDLE_VALUE)
				{
					BOOL bOK = TRUE;
					while (bOK)
					{
						str = fd.cFileName;
						fileName = proj->GetDir();
						fileName += "\\dath";
						lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
						lvItem.iSubItem = 0;
						lvItem.pszText = str.GetBuffer(str.GetLength());
						str.ReleaseBuffer();
						lvItem.cchTextMax = str.GetLength();
						lvItem.iImage = image;
						m_list.InsertItem(&lvItem);
						m_list.SetItemText(lvItem.iItem, 1, fileName);
						fileName += "\\";
						m_otherdirs.Add(fileName);
						m_otherfiles.Add(str);
						bOK = ::FindNextFile(h, &fd);
					}
					::FindClose(h);
				}
				// PROF
				search = proj->GetDataDir();
				search += "*.log";
				h = ::FindFirstFile(search, &fd);
				if (h!=INVALID_HANDLE_VALUE)
				{
					BOOL bOK = TRUE;
					while (bOK)
					{
						str = fd.cFileName;
						fileName = proj->GetDir();
						fileName += "\\prof";
						lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
						lvItem.iSubItem = 0;
						lvItem.pszText = str.GetBuffer(str.GetLength());
						str.ReleaseBuffer();
						lvItem.cchTextMax = str.GetLength();
						lvItem.iImage = image;
						m_list.InsertItem(&lvItem);
						m_list.SetItemText(lvItem.iItem, 1, fileName);
						fileName += "\\";
						m_otherdirs.Add(fileName);
						m_otherfiles.Add(str);
						bOK = ::FindNextFile(h, &fd);
					}
					::FindClose(h);
				}
                    }//for
			}
		}
	}
	for (i=0; i<2; i++)
	{
		lvColumn[i].mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
		lvColumn[i].fmt = LVCFMT_LEFT;
		lvColumn[i].pszText = text[i].GetBuffer(text[i].GetLength());
		text[i].ReleaseBuffer();
		lvColumn[i].iSubItem = i;
		lvColumn[i].cx = m_list.GetColumnWidth(i);
		VERIFY(m_list.SetColumn(i, &lvColumn[i]));
	}
	*pResult = 0;
}

void OpenDialog::OnDblclkList1(NMHDR* /*pNMHDR*/, LRESULT* pResult) 
{
	OnOK();
	*pResult = 0;
}

void OpenDialog::OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	m_list.SortItems(ListCompareFunc, pNMListView->iSubItem);
	
	*pResult = 0;
}

