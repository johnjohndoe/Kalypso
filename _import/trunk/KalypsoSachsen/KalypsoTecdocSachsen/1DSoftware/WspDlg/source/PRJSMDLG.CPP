// prjsmdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "prjprdlg.h"
#include "prjsmdlg.h"

// UNICODE/MBCS abstractions
#ifdef _MBCS
	extern AFX_DATA const BOOL _afxDBCS;
#else
	#define _afxDBCS FALSE
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

PROCESS_LOCAL(_AFX_WIN_STATE, _afxWinState)

static CString underscore = "______________________________________________________________________________________________\r\n";

/////////////////////////////////////////////////////////////////////////////
// Printing Dialog

class CPrintingSummDialog : public CDialog
{
public:
	//{{AFX_DATA(CPrintingSummDialog)
	enum { IDD = AFX_IDD_PRINTDLG };
	//}}AFX_DATA
	CPrintingSummDialog::CPrintingSummDialog(CWnd* pParent)
		{
#ifdef _MAC
			// Note! set m_pView *before* CDialog::Create so that
			// CPrintingSummDialog::OnInitDialog can use it.
			m_pView = pParent;
#endif
			Create(CPrintingSummDialog::IDD, pParent);      // modeless !
			_afxWinState->m_bUserAbort = FALSE;
		}
	virtual ~CPrintingSummDialog() { }

	virtual BOOL OnInitDialog();
	virtual void OnCancel();

protected:
#ifdef _MAC
	CWnd*   m_pView;        // the view being printed

	afx_msg int OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message);
#endif

#ifdef _MAC
	//{{AFX_MSG(CPrintingSummDialog)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
#endif
};

#ifdef _MAC
BEGIN_MESSAGE_MAP(CPrintingSummDialog, CDialog)
	//{{AFX_MSG_MAP(CPrintingSummDialog)
	ON_WM_MOUSEACTIVATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#endif


BOOL CALLBACK _AfxAbortProcEx(HDC, int)
{
	_AFX_WIN_STATE* pWinState = _afxWinState;

#ifdef _MAC
	EventRecord er;
	BOOL fNewMessages;

	// Look for an event, and if we find one, see if WLM cares about it.
	// If so, remove it from the queue (and then ignore it, since we've
	// already told WLM about it by calling QueueEvent). We also remove
	// the event from the queue if it's an activateEvt for a non-WLM
	// window, since this will probably be an activate for the printer
	// driver's status window, which we want to flush from the queue so
	// that it doesn't block events for WLM windows.

	while (EventAvail(everyEvent, &er) &&
		(QueueEvent(&er, &fNewMessages) || er.what == activateEvt))
	{
		GetNextEvent(everyEvent, &er);
	}

	// It's harder for us to depend on a cmd-. or escape keypress getting
	// picked up by the EventAvail, since the user could easily click in
	// the printer status window, producing a non-WLM event that would hide
	// the keypress. Therefore we also explicitly check for the keypress.

	if (GetAsyncKeyState(VK_CANCEL))
	{
		TRACE0("saw an async VK_CANCEL\n");
		pWinState->m_bUserAbort = TRUE;
	}
#endif

	MSG msg;
	while (!pWinState->m_bUserAbort &&
#ifndef _MAC
		::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
#else
		// don't grab any new messages from the Mac event queue
		::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE | PM_NOEVENTS))
#endif
	{
		if (!AfxGetThread()->PumpMessage())
			return FALSE;   // terminate if WM_QUIT received
	}
	return !pWinState->m_bUserAbort;
}


BOOL CPrintingSummDialog::OnInitDialog()
{
#ifdef _MAC
	// prime the state of the VK_CANCEL key
	GetAsyncKeyState(VK_CANCEL);
#endif
	SetWindowText(AfxGetAppName());
#ifndef _MAC
	CenterWindow();
#else
	CenterWindow(m_pView->GetParentFrame());
#endif
	return CDialog::OnInitDialog();
}

void CPrintingSummDialog::OnCancel()
{
	_afxWinState->m_bUserAbort = TRUE;  // flag that user aborted print
	CDialog::OnCancel();
}

#ifdef _MAC
int CPrintingSummDialog::OnMouseActivate(CWnd* pDesktopWnd, UINT, UINT)
{
	UNUSED(pDesktopWnd); // not used in release build
	ASSERT(pDesktopWnd == this);

	// if the printer driver has opened its own status window, we don't
	// want to activate our printing status window when it's clicked

	return MA_NOACTIVATE;
}
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectSummDialog 

#define TYPE_DOUBLE		0
#define TYPE_STRING		1

static CListCtrl *listCtrl;
static int col_types[4];

int CALLBACK ListCompareFunc(LPARAM lParam1, LPARAM lParam2, 
	LPARAM lParamSort)
{
	LV_FINDINFO lvFindInfo;
	int iItem1, iItem2;
	int nResult = 0;
	CString str1, str2;

	lvFindInfo.flags = LVFI_PARAM;
	lvFindInfo.psz = NULL;
	lvFindInfo.lParam = lParam1;
	iItem1 = listCtrl->FindItem(&lvFindInfo);
	lvFindInfo.lParam = lParam2;
	iItem2 = listCtrl->FindItem(&lvFindInfo);

	str1 = listCtrl->GetItemText(iItem1, lParamSort);
	str2 = listCtrl->GetItemText(iItem2, lParamSort);
	if (col_types[lParamSort]==TYPE_DOUBLE)
	{
		double a, b;

		a = atof(str1);
		b = atof(str2);
		if (a<b)
			nResult = -1;
		else if (a>b)
			nResult = 1;
		else
			nResult = 0;
	}
	else
		nResult = str1.CompareNoCase(str2);
	return nResult;
}

ProjectSummDialog::ProjectSummDialog(CWnd* pParent /*=NULL*/)
	: CDialog(ProjectSummDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(ProjectSummDialog)
	//}}AFX_DATA_INIT
	m_pProject = NULL;
	m_hTISel = NULL;
	m_nTabStops = 100;
	m_hPrinterFont = NULL;
	m_hMirrorFont = NULL;
}

ProjectSummDialog::~ProjectSummDialog()
{
	if (m_pProject!=NULL)
		delete m_pProject;
}

void ProjectSummDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ProjectSummDialog)
	DDX_Control(pDX, IDC_TREE1, m_states);
	DDX_Control(pDX, IDC_LIST2, m_data);
	DDX_Control(pDX, IDC_LIST1, m_projects);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ProjectSummDialog, CDialog)
	//{{AFX_MSG_MAP(ProjectSummDialog)
	ON_NOTIFY(NM_CLICK, IDC_LIST1, OnProject)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_LIST1, OnColumnclickList1)
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_LIST2, OnColumnclickList2)
	ON_BN_CLICKED(IDC_BUTTON1, OnPrintSummary)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ProjectSummDialog 

BOOL ProjectSummDialog::OnInitDialog() 
{
	CString path, rString, str;
	CString filename, name;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	int width;
	CRect rect;

	CDialog::OnInitDialog();

	GetDlgItem(IDC_BUTTON1)->EnableWindow(FALSE);

  CImageList* pImageList = CCommonImageList::GetList( FALSE );
	
	m_states.SetImageList( pImageList, TVSIL_NORMAL);
	m_projects.SetFullRowSel(TRUE);
	m_projects.SetImageList( pImageList, LVSIL_SMALL);
	m_projects.GetClientRect(&rect);
	width = rect.Width()/2;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_DIRECTORY);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_projects.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_NAME);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_projects.InsertColumn(1, &lvColumn);
	
	m_data.SetFullRowSel(TRUE);
	m_data.SetImageList( pImageList, LVSIL_SMALL);
	m_data.GetClientRect(&rect);
	width = rect.Width()/4;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 0;
	m_data.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 1;
	m_data.InsertColumn(1, &lvColumn);
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 2;
	m_data.InsertColumn(2, &lvColumn);
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 3;
	m_data.InsertColumn(3, &lvColumn);

	lvItem.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	GetWindowsDirectory(path.GetBuffer(MAX_PATH), MAX_PATH);
	path.ReleaseBuffer();
  path += "\\wsp.prj";
	
	CFileStatus rStatus;
  if (!CFile::GetStatus(path, rStatus))
	{
		AfxFormatString1(rString, IDS_ERROR_READFILE, path);
		AfxMessageBox(rString, MB_OK | MB_ICONSTOP);
		EndDialog(IDCANCEL);
		return TRUE;
	}

  CStdioFile file;
 
  if ( !file.Open( path, CFile::modeRead ) )
	{
		AfxFormatString1(rString, IDS_ERROR_READFILE, path);
		AfxMessageBox(rString, MB_OK | MB_ICONSTOP);
		EndDialog(IDCANCEL);
		return TRUE;
	}
	else
	{
    CString inputStr;;
    while ( file.ReadString( inputStr ) )
		{
			if (!inputStr.IsEmpty())
			{
        CStdioFile file2;

				lvItem.iItem = lvItem.lParam = m_projects.GetItemCount();
				lvItem.iSubItem = 0;
				lvItem.pszText = inputStr.GetBuffer(inputStr.GetLength());
				inputStr.ReleaseBuffer();
				lvItem.cchTextMax = inputStr.GetLength();
				lvItem.iImage = IMAGE_PROJECT;
				m_projects.InsertItem(&lvItem);
				name.Empty();
				// try and find project name
				filename = inputStr + "\\prof\\probez.txt";
        if ( CFile::GetStatus(filename, rStatus ) )
				{
          if ( file2.Open( filename, CFile::modeRead ) )
					{
            CString inStr;
            file2.ReadString( inStr );
						name = inStr;
						name.TrimLeft();
						name.TrimRight();
            file2.Close();
          } // if !file2.Open
        } // if CFile::GetStatus
				m_projects.SetItemText(lvItem.iItem, 1, name);
      } // if !inputStr.IsEmpty
    } // while !file.ReadString
    file.Close();
  } // if file.Open
	m_projects.SetItemState(0, LVIS_SELECTED, LVIS_SELECTED);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void ProjectSummDialog::OnProject(NMHDR* pNMHDR, LRESULT* pResult) 
{
	State *st;
	Calculation *calc;
	BOOL bLS;
	int i;
	CString dir, str, wname, fileTitle;
	HTREEITEM hTIW, hTIZ, hTIL;
	BOOL bCalcs;

	CWaitCursor wait;

	*pResult = 0;
	for (i=0; i<m_projects.GetItemCount(); i++)
	{
		if (m_projects.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
		{
			str = m_projects.GetItemText(i, 0);
			if (m_pProject!=NULL)
			{
				dir = m_pProject->GetDir();
				if (dir.CompareNoCase(str)==0)
					return;	// already loaded
				delete m_pProject;
			}
			m_pProject = new Project(str);
			m_pProject->Load();
			break;
		}
	}

	m_states.DeleteAllItems();
	m_data.DeleteAllItems();
	m_qmap_tree.RemoveAll();
	m_lmap_tree.RemoveAll();
	m_cmap_tree.RemoveAll();
	m_omap_tree.RemoveAll();
	m_hTISel = NULL;
	for (i=0; i<m_pProject->GetWaterCount(); i++)
	{
		wname = m_pProject->GetWaterName(i);
		hTIW = m_states.InsertItem(wname, IMAGE_WATER, IMAGE_WATER);
		st = m_pProject->GetFirstState();
		while (st!=NULL)
		{
			str = st->GetWaterName();
			if (str==wname)
			{
				bCalcs = FALSE;
				str = st->GetName();
				hTIZ = m_states.InsertItem(str, IMAGE_STATE, IMAGE_STATE, hTIW);
				if (st->GetNumCrossSections()>0)
				{
					str.LoadString(IDS_CROSSSECTIONS);
					hTIL = m_states.InsertItem(str, IMAGE_CSECTION, IMAGE_CSECTION, hTIZ);
					m_qmap_tree.SetAt(hTIL, st);
				}
				bLS = FALSE;
				calc = st->GetFirstCalculation();
				while (calc!=NULL)
				{
					if (calc->GetLengthSection()!=NULL)
					{
						bLS = TRUE;
						break;
					}
					calc = st->GetNextCalculation();
				}
				if (bLS)
				{
					str.LoadString(IDS_LENGTHSECTIONS);
					hTIL = m_states.InsertItem(str, IMAGE_LSECTION, IMAGE_LSECTION, hTIZ);
					m_lmap_tree.SetAt(hTIL, st);
				}
				if (st->GetNumCalculations()>0)
				{
					str.LoadString(IDS_CALCULATIONS);
					hTIL = m_states.InsertItem(str, IMAGE_CALC, IMAGE_CALC, hTIZ);
					m_cmap_tree.SetAt(hTIL, st);
				}
				if (st->GetNumOutFlows()>0)
				{
					str.LoadString(IDS_OUTFLOWS);
					hTIL = m_states.InsertItem(str, IMAGE_ABFLUSS, IMAGE_ABFLUSS, hTIZ);
					m_omap_tree.SetAt(hTIL, st);
				}
			}
			st = m_pProject->GetNextState();
		}
	}

	GetDlgItem(IDC_BUTTON1)->EnableWindow(TRUE);
}

void ProjectSummDialog::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	State *st;
	Calculation *calc;
	OutFlow *of;
	CrossSection *cs;
	LengthSection *ls;
	CString str, fmt;
	CString fileTitle, fileExt, path;
	CFile file;
	CFileStatus rStatus;
	int i;
	LV_ITEM lvItem;
	LV_COLUMN lvColumn[4];
	CString text[4];
	
	if (m_pProject==NULL || pNMTreeView->action==0)
		return;
	
	lvItem.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	m_data.DeleteAllItems();
	m_hTISel = pNMTreeView->itemNew.hItem;
	if (m_qmap_tree.Lookup(pNMTreeView->itemNew.hItem, st))
	{
		text[0].LoadString(IDS_STATION);
		text[1].LoadString(IDS_PK);
		text[2].LoadString(IDS_VZK);
		text[3].LoadString(IDS_FILE);
		cs = st->GetFirstCrossSection();
		while (cs!=NULL)
		{
			str.Format("%.4f", cs->GetStation());
			lvItem.iItem = lvItem.lParam = m_data.GetItemCount();
			lvItem.iSubItem = 0;
			lvItem.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvItem.cchTextMax = str.GetLength();
			lvItem.iImage = IMAGE_CSECTION;
			m_data.InsertItem(&lvItem);
			str = cs->GetPK();
			m_data.SetItemText(lvItem.iItem, 1, str);
			str.Format("%d", cs->GetVZK());
			m_data.SetItemText(lvItem.iItem, 2, str);
			fileTitle = cs->GetFileTitle();
			fileExt = cs->GetFileExt();
			str = "prof\\" + fileTitle + '.' + fileExt;
			m_data.SetItemText(lvItem.iItem, 3, str);
			cs = st->GetNextCrossSection();
		}
	}
	if (m_lmap_tree.Lookup(pNMTreeView->itemNew.hItem, st))
	{
		text[0].LoadString(IDS_NAME);
		text[1].LoadString(IDS_START);
		text[2].LoadString(IDS_END);
		text[3].LoadString(IDS_FILE);
		calc = st->GetFirstCalculation();
		while (calc!=NULL)
		{
			ls = calc->GetLengthSection();
			if (ls!=NULL)
			{
				str = ls->GetName();
				lvItem.iItem = lvItem.lParam = m_data.GetItemCount();
				lvItem.iSubItem = 0;
				lvItem.pszText = str.GetBuffer(str.GetLength());
				str.ReleaseBuffer();
				lvItem.cchTextMax = str.GetLength();
				lvItem.iImage = IMAGE_LSECTION;
				m_data.InsertItem(&lvItem);
				str.Format("%.4f", ls->GetStartStation());
				m_data.SetItemText(lvItem.iItem, 1, str);
				str.Format("%.4f", ls->GetEndStation());
				m_data.SetItemText(lvItem.iItem, 2, str);
				fileTitle = ls->GetFileTitle();
				fileExt = ls->GetFileExt();
				str = "dath\\" + fileTitle + '.' + fileExt;
				m_data.SetItemText(lvItem.iItem, 3, str);
			}
			calc = st->GetNextCalculation();
		}
	}
	if (m_cmap_tree.Lookup(pNMTreeView->itemNew.hItem, st))
	{
		text[0].LoadString(IDS_NAME);
		text[1].LoadString(IDS_START);
		text[2].LoadString(IDS_END);
		text[3].LoadString(IDS_FILE);
		calc = st->GetFirstCalculation();
		while (calc!=NULL)
		{
			str = calc->GetName();
			lvItem.iItem = lvItem.lParam = m_data.GetItemCount();
			lvItem.iSubItem = 0;
			lvItem.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvItem.cchTextMax = str.GetLength();
			lvItem.iImage = IMAGE_CALC;
			m_data.InsertItem(&lvItem);
			str.Format("%.4f", calc->GetStartStation());
			m_data.SetItemText(lvItem.iItem, 1, str);
			str.Format("%.4f", calc->GetEndStation());
			m_data.SetItemText(lvItem.iItem, 2, str);
			fileTitle = calc->GetFileTitle();
			fileExt = calc->GetFileExt();
			str = "prof\\" + fileTitle + '.' + fileExt;
			m_data.SetItemText(lvItem.iItem, 3, str);
			calc = st->GetNextCalculation();
		}
	}
	if (m_omap_tree.Lookup(pNMTreeView->itemNew.hItem, st))
	{
		text[0].LoadString(IDS_NAME);
		text[1].LoadString(IDS_WSPFIX);
		text[2].Empty();
		text[3].LoadString(IDS_FILE);
		of = st->GetFirstOutFlow();
		while (of!=NULL)
		{
			str = of->GetName();
			lvItem.iItem = lvItem.lParam = m_data.GetItemCount();
			lvItem.iSubItem = 0;
			lvItem.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvItem.cchTextMax = str.GetLength();
			lvItem.iImage = IMAGE_ABFLUSS;
			m_data.InsertItem(&lvItem);
			if (of->WSPFIsDefined())
				str.LoadString(IDS_DEFINED);
			else
				str.LoadString(IDS_NOTDEFINED);
			m_data.SetItemText(lvItem.iItem, 1, str);
			str = st->GetFileTitle();
			str = "prof\\" + str + ".qwt";
			m_data.SetItemText(lvItem.iItem, 3, str);
			of = st->GetNextOutFlow();
		}
	}
	for (i=0; i<4; i++)
	{
		lvColumn[i].mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
		lvColumn[i].fmt = LVCFMT_LEFT;
		lvColumn[i].pszText = text[i].GetBuffer(text[i].GetLength());
		text[i].ReleaseBuffer();
		lvColumn[i].iSubItem = i;
		lvColumn[i].cx = m_data.GetColumnWidth(i);
		VERIFY(m_data.SetColumn(i, &lvColumn[i]));
	}

	*pResult = 0;
}

void ProjectSummDialog::OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	col_types[0] = TYPE_STRING;
	col_types[1] = TYPE_STRING;
	col_types[2] = TYPE_STRING;
	col_types[3] = TYPE_STRING;
	listCtrl = &m_projects;
	m_projects.SortItems(ListCompareFunc, pNMListView->iSubItem);
	
	*pResult = 0;
}

void ProjectSummDialog::OnColumnclickList2(NMHDR* pNMHDR, LRESULT* pResult) 
{
	State *st;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;

	col_types[0] = TYPE_STRING;
	col_types[1] = TYPE_STRING;
	col_types[2] = TYPE_STRING;
	col_types[3] = TYPE_STRING;
	if (m_qmap_tree.Lookup(m_hTISel, st))
		col_types[0] = TYPE_DOUBLE;
	else if (m_lmap_tree.Lookup(m_hTISel, st))
	{
		col_types[1] = TYPE_DOUBLE;
		col_types[2] = TYPE_DOUBLE;
	}
	else if (m_cmap_tree.Lookup(m_hTISel, st))
	{
		col_types[1] = TYPE_DOUBLE;
		col_types[2] = TYPE_DOUBLE;
	}
	listCtrl = &m_data;
	m_data.SortItems(ListCompareFunc, pNMListView->iSubItem);
	
	*pResult = 0;
}

BOOL ProjectSummDialog::CreateText(BOOL bTotal)
{
	if (m_pProject==NULL)
		return FALSE;

	CString str, wname, temp1, temp2, temp3, fileTitle, fileExt, temp;
	State *st;
	CrossSection *cs;
	Calculation *calc;
	LengthSection *ls;
	OutFlow *of;
	int i, j;

	m_strText.Empty();

	m_strText  = "**********************************************************************************************\r\n";
	m_strText += "*                                                                                            *\r\n";
	temp.LoadString(IDS_PROJ_OVIEW);
	m_strText += temp;
	m_strText += "*                                                                                            *\r\n";
	m_strText += "**********************************************************************************************\r\n\r\n";
	str = m_pProject->GetName();
	temp.LoadString(IDS_PROJ_NAME);
	m_strText += temp + str + "\r\n";
	str = m_pProject->GetDir();
	temp.LoadString(IDS_PROJ_DIR);
	m_strText += temp + str + "\r\n\r\n";
	if (bTotal)
	{
		for (i=0; i<m_pProject->GetWaterCount(); i++)
		{
			wname = m_pProject->GetWaterName(i);
			temp.Format("%d", i+1);
			str.FormatMessage(IDS_WATERWAYN, temp, wname);
			m_strText += str;
			m_strText += underscore + "\r\n";
			st = m_pProject->GetFirstState();
			while (st!=NULL)
			{
				str = st->GetWaterName();
				if (str==wname)
				{
					str = st->GetName();
					temp.LoadString(IDS_STATE);
					m_strText += temp + str + "\r\n\r\n";
					
					if (st->GetNumCrossSections()>0)
					{
						temp.LoadString(IDS_CROSSSECTIONS);
						m_strText += temp + ":\r\n";
						temp.LoadString(IDS_STATION);
						m_strText += temp + "\t";
						temp.LoadString(IDS_PK);
						m_strText += temp + "\t";
						temp.LoadString(IDS_VZK);
						m_strText += temp + "\t";
						temp.LoadString(IDS_FILE);
						m_strText += temp + "\r\n";
						m_strText += underscore;
						cs = st->GetFirstCrossSection();
						while (cs!=NULL)
						{
							temp1 = cs->GetPK();
							fileTitle = cs->GetFileTitle();
							fileExt = cs->GetFileExt();
							temp2 = "prof\\" + fileTitle + '.' + fileExt;
							str.Format("%.4f\t%s\t%d\t%s\r\n", cs->GetStation(), temp1, cs->GetVZK(), temp2);
							m_strText += str;
							cs = st->GetNextCrossSection();
						}
						m_strText += "\r\n\r\n";
					}
					
					if (st->GetNumCalculations()>0)
					{
						temp.LoadString(IDS_LENGTHSECTIONS);
						m_strText += temp + ":\r\n";
						temp.LoadString(IDS_NAME);
						m_strText += temp + "\t";
						temp.LoadString(IDS_START);
						m_strText += temp + "\t";
						temp.LoadString(IDS_END);
						m_strText += temp + "\t";
						temp.LoadString(IDS_FILE);
						m_strText += temp + "\r\n";
						m_strText += underscore;
						calc = st->GetFirstCalculation();
						while (calc!=NULL)
						{
							ls = calc->GetLengthSection();
							if (ls!=NULL)
							{
								temp1 = ls->GetName();
								fileTitle = ls->GetFileTitle();
								fileExt = ls->GetFileExt();
								temp2 = "dath\\" + fileTitle + '.' + fileExt;
								str.Format("%s\t%.4f\t%.4f\t%s\r\n", temp1, ls->GetStartStation(), ls->GetEndStation(), temp2);
								m_strText += str;
							}
							calc = st->GetNextCalculation();
						}
						m_strText += "\r\n\r\n";
						
						temp.LoadString(IDS_CALCULATIONS);
						m_strText += temp + ":\r\n";
						temp.LoadString(IDS_NAME);
						m_strText += temp + "\t";
						temp.LoadString(IDS_START);
						m_strText += temp + "\t";
						temp.LoadString(IDS_END);
						m_strText += temp + "\t";
						temp.LoadString(IDS_FILE);
						m_strText += temp + "\r\n";
						m_strText += underscore;
						calc = st->GetFirstCalculation();
						while (calc!=NULL)
						{
							temp1 = calc->GetName();
							fileTitle = calc->GetFileTitle();
							fileExt = calc->GetFileExt();
							temp2 = "prof\\" + fileTitle + '.' + fileExt;
							str.Format("%s\t%.4f\t%.4f\t%s\r\n", temp1, calc->GetStartStation(), calc->GetEndStation(), temp2);
							m_strText += str;
							calc = st->GetNextCalculation();
						}
						m_strText += "\r\n\r\n";
					}
					
					if (st->GetNumOutFlows()>0)
					{
						temp.LoadString(IDS_OUTFLOWS);
						m_strText += temp + ":\r\n";
						temp.LoadString(IDS_NAME);
						m_strText += temp + "\t";
						temp.LoadString(IDS_WSPFIX);
						m_strText += temp + "\t";
						m_strText += "\t";
						temp.LoadString(IDS_FILE);
						m_strText += temp + "\r\n";
						m_strText += underscore;
						of = st->GetFirstOutFlow();
						while (of!=NULL)
						{
							temp1 = of->GetName();
							if (of->WSPFIsDefined())
								temp2.LoadString(IDS_DEFINED);
							else
								temp2.LoadString(IDS_NOTDEFINED);
							fileTitle = st->GetFileTitle();
							temp3 = "prof\\" + fileTitle + ".qwt";
							str.Format("%s\t%s\t\t%s\r\n", temp1, temp2, temp3);
							m_strText += str;
							of = st->GetNextOutFlow();
						}
						m_strText += "\r\n\r\n";
					}
				}
				st = m_pProject->GetNextState();
			}
		}
	}
	else
	{
		LV_COLUMN lvColumn;
		lvColumn.mask = LVCF_TEXT;
		char cText[20];
		lvColumn.pszText = cText;
		lvColumn.cchTextMax = 20;
		HTREEITEM hTI;
		State *st = NULL;
		int type;

		hTI = m_states.GetSelectedItem();
		if (m_qmap_tree.Lookup(hTI, st))
			type = 0;
		else if (m_lmap_tree.Lookup(hTI, st))
			type = 1;
		else if (m_cmap_tree.Lookup(hTI, st))
			type = 2;
		else if (m_omap_tree.Lookup(hTI, st))
			type = 3;

		if (st==NULL)
			return FALSE;

		str = st->GetWaterName();
		temp.LoadString(IDS_WATERWAY);
		m_strText += temp + str + "\r\n";
		str = st->GetName();
		temp.LoadString(IDS_STATE);
		m_strText += temp + str + "\r\n\r\n";

		switch (type)
		{
			case 0:
				temp.LoadString(IDS_CROSSSECTIONS);
				m_strText += temp + ":\r\n";
				break;

			case 1:
				temp.LoadString(IDS_LENGTHSECTIONS);
				m_strText += temp + ":\r\n";
				break;

			case 2:
				temp.LoadString(IDS_CALCULATIONS);
				m_strText += temp + ":\r\n";
				break;

			case 3:
				temp.LoadString(IDS_OUTFLOWS);
				m_strText += temp + ":\r\n";
				break;
		}

		for (i=0; i<4; i++)
		{
			m_data.GetColumn(i, &lvColumn);
			temp1 = lvColumn.pszText;
			m_strText += temp1 + "\t";
		}
		m_strText += "\r\n";
		m_strText += underscore;
		for (i=0; i<m_data.GetItemCount(); i++)
		{
			if (m_data.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
			{
				for (j=0; j<4; j++)
					m_strText += m_data.GetItemText(i, j) + "\t";
				m_strText += "\r\n";
			}
		}
		m_strText += "\r\n";
	}

	return TRUE;
}

static UINT AFXAPI EndOfLine(LPCTSTR lpszText, UINT nLen, UINT nIndex)
{
	ASSERT(AfxIsValidAddress(lpszText, nLen, FALSE));
	LPCTSTR lpsz = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nLen;
#ifndef _MAC
	while (lpsz < lpszStop && *lpsz != '\r')
#else
	while (lpsz < lpszStop && *lpsz != '\n')
#endif
		++lpsz;
	return lpsz - lpszText;
}

static UINT AFXAPI NextLine(LPCTSTR lpszText, UINT nLen, UINT nIndex)
{
	ASSERT(AfxIsValidAddress(lpszText, nLen, FALSE));
	LPCTSTR lpsz = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nLen;
#ifndef _MAC
	while (lpsz < lpszStop && *lpsz == '\r')
		++lpsz;
#endif
	if (lpsz < lpszStop && *lpsz == '\n')
		++lpsz;
	return lpsz - lpszText;
}

static UINT AFXAPI
ClipLine(CDC* pDC, int aCharWidths[256], int cxLine, int nTabStop,
	LPCTSTR lpszText, UINT nIndex, UINT nIndexEnd)
{
	ASSERT_VALID(pDC);
	ASSERT(nIndex < nIndexEnd);
	ASSERT(AfxIsValidAddress(lpszText, nIndexEnd, FALSE));
	UNUSED_ALWAYS(nTabStop);    // unused in Mac build

	TEXTMETRIC tm;
	::GetTextMetrics(pDC->m_hDC, &tm);

	// make an initial guess on the number of characters that will fit
	int cx = 0;
	LPCTSTR lpszStart = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nIndexEnd;
	LPCTSTR lpsz = lpszStart;
	while (lpsz < lpszStop)
	{
#ifndef _MAC
		if (*lpsz == '\t')
			cx += nTabStop - (cx % nTabStop);
		else
#endif
		{
#ifdef _UNICODE
			if (*lpsz <= 0xFF)
				cx += aCharWidths[(BYTE)*lpsz];
			else
				cx += tm.tmAveCharWidth;
#else //_UNICODE
			if (_afxDBCS && _istlead(*lpsz))
			{
				++lpsz;
				cx += tm.tmAveCharWidth;
			}
			else
				cx += aCharWidths[(BYTE)*lpsz];
#endif //!_UNICODE
		}
		++lpsz;
		if (cx > cxLine)
			break;
	}

	// adjust for errors in the guess
#ifndef _MAC
	cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
#else
	cx = pDC->GetTextExtent(lpszStart, lpsz-lpszStart).cx;
#endif
	if (cx > cxLine)
	{
		// remove characters until it fits
		do
		{
			ASSERT(lpsz != lpszStart);
			if (_afxDBCS)
				lpsz = _tcsdec(lpszStart, lpsz);
			else
				--lpsz;
#ifndef _MAC
			cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
#else
			cx = pDC->GetTextExtent(lpszStart, lpsz-lpszStart).cx;
#endif
		} while (cx > cxLine);
	}
	else if (cx < cxLine)
	{
		// add characters until it doesn't fit
		while (lpsz < lpszStop)
		{
			lpsz = _tcsinc(lpsz);
			ASSERT(lpsz <= lpszStop);
#ifndef _MAC
			cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
#else
			cx = pDC->GetTextExtent(lpszStart, lpsz-lpszStart).cx;
#endif
			if (cx > cxLine)
			{
				if (_afxDBCS)
					lpsz = _tcsdec(lpszStart, lpsz);
				else
					--lpsz;
				break;
			}
		}
	}

	// return index of character just past the last that would fit
	return lpsz - lpszText;
}

BOOL ProjectSummDialog::DoPreparePrinting(CPrintInfo* pInfo)
{
	ASSERT(pInfo != NULL);
	ASSERT(pInfo->m_pPD != NULL);

	pInfo->m_pPD->m_pd.Flags |= PD_NOPAGENUMS | PD_NOSELECTION;

	if (pInfo->m_pPD->m_pd.nMinPage > pInfo->m_pPD->m_pd.nMaxPage)
		pInfo->m_pPD->m_pd.nMaxPage = pInfo->m_pPD->m_pd.nMinPage;

	// don't prompt the user if we're doing print preview, printing directly,
	// or printing via IPrint and have been instructed not to ask

	CWinApp* pApp = AfxGetApp();
	if (pInfo->m_bPreview || pInfo->m_bDirect ||
		(pInfo->m_bDocObject && !(pInfo->m_dwFlags & PRINTFLAG_PROMPTUSER)))
	{
		if (pInfo->m_pPD->m_pd.hDC == NULL)
		{
			// if no printer set then, get default printer DC and create DC without calling
			//   print dialog.
			if (!pApp->GetPrinterDeviceDefaults(&pInfo->m_pPD->m_pd))
			{
				// bring up dialog to alert the user they need to install a printer.
				if (!pInfo->m_bDocObject || (pInfo->m_dwFlags & PRINTFLAG_MAYBOTHERUSER))
					if (pApp->DoPrintDialog(pInfo->m_pPD) != IDOK)
						return FALSE;
			}

			if (pInfo->m_pPD->m_pd.hDC == NULL)
			{
				// call CreatePrinterDC if DC was not created by above
				if (pInfo->m_pPD->CreatePrinterDC() == NULL)
					return FALSE;
			}
		}

		// set up From and To page range from Min and Max
		pInfo->m_pPD->m_pd.nFromPage = (WORD)pInfo->GetMinPage();
		pInfo->m_pPD->m_pd.nToPage = (WORD)pInfo->GetMaxPage();
	}
	else
	{
		// otherwise, bring up the print dialog and allow user to change things
		// preset From-To range same as Min-Max range
		pInfo->m_pPD->m_pd.nFromPage = (WORD)pInfo->GetMinPage();
		pInfo->m_pPD->m_pd.nToPage = (WORD)pInfo->GetMaxPage();

		if (pApp->DoPrintDialog(pInfo->m_pPD) != IDOK)
			return FALSE;       // do not print
	}

	ASSERT(pInfo->m_pPD != NULL);
	ASSERT(pInfo->m_pPD->m_pd.hDC != NULL);
	if (pInfo->m_pPD->m_pd.hDC == NULL)
		return FALSE;

	pInfo->m_nNumPreviewPages = pApp->m_nNumPreviewPages;
	VERIFY(pInfo->m_strPageDesc.LoadString(AFX_IDS_PREVIEWPAGEDESC));
	return TRUE;
}

void ProjectSummDialog::OnBeginPrinting(CDC* pDC, CPrintInfo*)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	// initialize page start vector
	ASSERT(m_aPageStart.GetSize() == 0);
	m_aPageStart.Add(0);
	ASSERT(m_aPageStart.GetSize() > 0);

	if (m_hPrinterFont == NULL)
	{
		// get current screen font object metrics
//		CFont* pFont = GetFont();
		LOGFONT lf;
		int cx;
//		if (pFont == NULL)
//			return;
//		VERIFY(pFont->GetObject(sizeof(LOGFONT), &lf));
		VERIFY(::GetObject(::GetStockObject(SYSTEM_FIXED_FONT), sizeof(LOGFONT),
			&lf));

		cx = pDC->GetTextExtent(underscore).cx;
		lf.lfHeight = ::MulDiv(lf.lfHeight, 4800,
			cx);
		lf.lfWidth = ::MulDiv(lf.lfWidth, 4800,
			cx);
		// map to printer font metrics
		HDC hDCFrom = ::GetDC(NULL);
		lf.lfHeight = ::MulDiv(lf.lfHeight, pDC->GetDeviceCaps(LOGPIXELSY),
			::GetDeviceCaps(hDCFrom, LOGPIXELSY));
		lf.lfWidth = ::MulDiv(lf.lfWidth, pDC->GetDeviceCaps(LOGPIXELSX),
			::GetDeviceCaps(hDCFrom, LOGPIXELSX));
		::ReleaseDC(NULL, hDCFrom);

		// create it, if it fails we just use the printer's default.
		m_hMirrorFont = ::CreateFontIndirect(&lf);
		m_hPrinterFont = m_hMirrorFont;
	}
	ASSERT_VALID(this);
}

BOOL ProjectSummDialog::PaginateTo(CDC* pDC, CPrintInfo* pInfo)
	// attempts pagination to pInfo->m_nCurPage, TRUE == success
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	CRect rectSave = pInfo->m_rectDraw;
	UINT nPageSave = pInfo->m_nCurPage;
	ASSERT(nPageSave > 1);
	ASSERT(nPageSave >= (UINT)m_aPageStart.GetSize());
	VERIFY(pDC->SaveDC() != 0);
	pDC->IntersectClipRect(0, 0, 0, 0);
	pInfo->m_nCurPage = m_aPageStart.GetSize();
	while (pInfo->m_nCurPage < nPageSave)
	{
		ASSERT(pInfo->m_nCurPage == (UINT)m_aPageStart.GetSize());
		OnPrepareDC(pDC, pInfo);
		ASSERT(pInfo->m_bContinuePrinting);
		pInfo->m_rectDraw.SetRect(0, 0,
			pDC->GetDeviceCaps(HORZRES), pDC->GetDeviceCaps(VERTRES));
		pDC->DPtoLP(&pInfo->m_rectDraw);
		OnPrint(pDC, pInfo);
		if (pInfo->m_nCurPage == (UINT)m_aPageStart.GetSize())
			break;
		++pInfo->m_nCurPage;
	}
	BOOL bResult = pInfo->m_nCurPage == nPageSave;
	pDC->RestoreDC(-1);
	pInfo->m_nCurPage = nPageSave;
	pInfo->m_rectDraw = rectSave;
	ASSERT_VALID(this);
	return bResult;
}

void ProjectSummDialog::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	ASSERT(pInfo != NULL);  // overriding OnPaint -- never get this.

	if (pInfo->m_nCurPage > (UINT)m_aPageStart.GetSize() &&
		!PaginateTo(pDC, pInfo))
	{
		// can't paginate to that page, thus cannot print it.
		pInfo->m_bContinuePrinting = FALSE;
	}
	ASSERT_VALID(this);
}

UINT ProjectSummDialog::PrintInsideRect(CDC* pDC, RECT& rectLayout,
	UINT nIndexStart, UINT nIndexStop, UINT nPage)
	// worker function for laying out text in a rectangle.
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	BOOL bWordWrap = (GetStyle() & ES_AUTOHSCROLL) == 0;

	// get buffer and real starting and ending postions
	UINT nLen = m_strText.GetLength();
	if (nIndexStart >= nLen)
		return nLen;
	LPCTSTR lpszText = m_strText.GetBuffer(m_strText.GetLength());
	if (nIndexStop > nLen)
		nIndexStop = nLen;
	ASSERT(nIndexStart < nLen);

	// calculate text & tab metrics
	TEXTMETRIC tm;
	pDC->GetTextMetrics(&tm);
	int cyChar = tm.tmHeight + tm.tmExternalLeading;
#ifndef _MAC
	int nTabStop = m_nTabStops *
		pDC->GetTabbedTextExtent(_T("\t"), 1, 0, NULL).cx / 8 / 4;
#else
	int nTabStop = pDC->GetTextExtent(_T("\t"), 1).cx;
#endif
	int aCharWidths[256];
	pDC->GetCharWidth(0, 255, aCharWidths);

	int y = rectLayout.top;
	UINT cx = rectLayout.right - rectLayout.left;
	UINT nIndex = nIndexStart;

	VERIFY(pDC->SaveDC() != 0);
	BOOL bLayoutOnly = pDC->IntersectClipRect(&rectLayout) == NULLREGION;

	do
	{
		UINT nIndexEnd = EndOfLine(lpszText, nIndexStop, nIndex);
		if (nIndex == nIndexEnd)
		{
			y += cyChar;
		}
		else if (bWordWrap)
		{
			// word-wrap printing
			do
			{
				UINT nIndexWrap = ClipLine(pDC, aCharWidths,
					cx, nTabStop, lpszText, nIndex, nIndexEnd);
				UINT nIndexWord = nIndexWrap;
				if (nIndexWord != nIndexEnd)
				{
					while (nIndexWord > nIndex &&
					  !_istspace(lpszText[nIndexWord]))
					{
						nIndexWord--;
					}
					if (nIndexWord == nIndex)
						nIndexWord = nIndexWrap;
				}
				CRect rect(rectLayout.left, y, rectLayout.right, y+cyChar);
				if (!bLayoutOnly && pDC->RectVisible(rect))
				{
#ifndef _MAC
					pDC->TabbedTextOut(rect.left, y,
						(LPCTSTR)(lpszText+nIndex), nIndexWord-nIndex, 1,
						&nTabStop, rect.left);
#else
					pDC->TextOut(rect.left, y,
						(LPCTSTR)(lpszText+nIndex), nIndexWord-nIndex);
#endif
				}
				y += cyChar;
				nIndex = nIndexWord;
				while (nIndex < nIndexEnd && _istspace(lpszText[nIndex]))
					nIndex++;
			} while (nIndex < nIndexEnd && y+cyChar <= rectLayout.bottom);

			nIndexEnd = nIndex;
		}
		else
		{
			// non-word wrap printing (much easier and faster)
			CRect rect(rectLayout.left, y, rectLayout.right, y+cyChar);
			if (!bLayoutOnly && pDC->RectVisible(rect))
			{
				UINT nIndexClip = ClipLine(pDC, aCharWidths, cx, nTabStop,
					lpszText, nIndex, nIndexEnd);
				if (nIndexClip < nIndexEnd)
				{
					if (_istlead(*(lpszText+nIndexClip)))
						nIndexClip++;
					nIndexClip++;
				}
#ifndef _MAC
				pDC->TabbedTextOut(rect.left, y,
					(LPCTSTR)(lpszText+nIndex), nIndexClip-nIndex, 1,
					&nTabStop, rect.left);
#else
				pDC->TextOut(rect.left, y,
					(LPCTSTR)(lpszText+nIndex), nIndexClip-nIndex);
#endif
			}
			y += cyChar;
		}
		nIndex = NextLine(lpszText, nIndexStop, nIndexEnd);
	}
	while (nIndex < nIndexStop && y+cyChar <= rectLayout.bottom-2*cyChar);

	CString pageDesc;

	pageDesc.Format("%d", nPage);
	pDC->TextOut((int)((rectLayout.left+rectLayout.right)/2), rectLayout.bottom-cyChar, pageDesc);

	pDC->RestoreDC(-1);
	m_strText.ReleaseBuffer();
	ASSERT_VALID(this);

	rectLayout.bottom = y;
	return nIndex;
}

void ProjectSummDialog::OnPrint(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	ASSERT(pInfo != NULL);
	ASSERT(pInfo->m_bContinuePrinting);

	CFont* pOldFont = NULL;
	if (m_hPrinterFont != NULL)
		pOldFont = pDC->SelectObject(CFont::FromHandle(m_hPrinterFont));
	pDC->SetBkMode(TRANSPARENT);

	UINT nPage = pInfo->m_nCurPage;
	ASSERT(nPage <= (UINT)m_aPageStart.GetSize());
	UINT nIndex = m_aPageStart[nPage-1];

	// print as much as possible in the current page.
	nIndex = PrintInsideRect(pDC, pInfo->m_rectDraw, nIndex, m_strText.GetLength(), nPage);

	if (pOldFont != NULL)
		pDC->SelectObject(pOldFont);

	// update pagination information for page just printed
	if (nPage == (UINT)m_aPageStart.GetSize())
	{
		if (nIndex < (UINT)m_strText.GetLength())
			m_aPageStart.Add(nIndex);
	}
	else
	{
		ASSERT(nPage+1 <= (UINT)m_aPageStart.GetSize());
		ASSERT(nIndex == m_aPageStart[nPage+1-1]);
	}
}

void ProjectSummDialog::OnEndPrinting(CDC*, CPrintInfo*)
{
	ASSERT_VALID(this);

	m_aPageStart.RemoveAll();
	if (m_hMirrorFont != NULL && m_hPrinterFont == m_hMirrorFont)
	{
		::DeleteObject(m_hMirrorFont);
		m_hMirrorFont = NULL;
		m_hPrinterFont = NULL;
	}
}

void ProjectSummDialog::OnPrintSummary() 
{
	BOOL bNoSelection = TRUE;
	int i;

	for (i=0; i<m_data.GetItemCount(); i++)
	{
		if (m_data.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
		{
			bNoSelection = FALSE;
			break;
		}
	}

	ProjectPrintDialog prjprDlg(this, bNoSelection);
	if (prjprDlg.DoModal()!=IDOK)
		return;

	if (!CreateText(prjprDlg.m_type==0))
		return;
	
	// get default print info
	CPrintInfo printInfo;
	ASSERT(printInfo.m_pPD != NULL);    // must be set

	printInfo.m_bDirect = FALSE;

	if (DoPreparePrinting(&printInfo))
	{
		ASSERT(printInfo.m_pPD->m_pd.hDC != NULL);

		// gather file to print to if print-to-file selected
		CString strOutput;
		if (printInfo.m_pPD->m_pd.Flags & PD_PRINTTOFILE)
		{
			// construct CFileDialog for browsing
			CString strDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULTEXT));
			CString strPrintDef(MAKEINTRESOURCE(AFX_IDS_PRINTDEFAULT));
			CString strFilter(MAKEINTRESOURCE(AFX_IDS_PRINTFILTER));
			CString strCaption(MAKEINTRESOURCE(AFX_IDS_PRINTCAPTION));
			CFileDialog dlg(FALSE, strDef, strPrintDef,
				OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT, strFilter);
			dlg.m_ofn.lpstrTitle = strCaption;

			if (dlg.DoModal() != IDOK)
				return;

			// set output device to resulting path name
			strOutput = dlg.GetPathName();
		}

		// set up document info and start the document printing process
		CString strTitle;
		strTitle.LoadString(IDS_PROJOV);
		DOCINFO docInfo;
		memset(&docInfo, 0, sizeof(DOCINFO));
		docInfo.cbSize = sizeof(DOCINFO);
		docInfo.lpszDocName = strTitle;
		CString strPortName;
		int nFormatID;
		if (strOutput.IsEmpty())
		{
			docInfo.lpszOutput = NULL;
			strPortName = printInfo.m_pPD->GetPortName();
			nFormatID = AFX_IDS_PRINTONPORT;
		}
		else
		{
			docInfo.lpszOutput = strOutput;
//			AfxGetFileTitle(strOutput,
//				strPortName.GetBuffer(_MAX_PATH), _MAX_PATH);
			nFormatID = AFX_IDS_PRINTTOFILE;
		}

		// setup the printing DC
		CDC dcPrint;
		dcPrint.Attach(printInfo.m_pPD->m_pd.hDC);  // attach printer dc
		dcPrint.m_bPrinting = TRUE;
		OnBeginPrinting(&dcPrint, &printInfo);
		dcPrint.SetAbortProc(_AfxAbortProcEx);

		// disable window while printing & init printing status dialog
		EnableWindow(FALSE);
		CPrintingSummDialog dlgPrintStatus(this);

		CString strTemp;
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_DOCNAME, strTitle);
#ifndef _MAC
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PRINTERNAME,
			printInfo.m_pPD->GetDeviceName());
		AfxFormatString1(strTemp, nFormatID, strPortName);
		dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PORTNAME, strTemp);
#endif
		dlgPrintStatus.ShowWindow(SW_SHOW);
		dlgPrintStatus.UpdateWindow();

		// start document printing process
		if (dcPrint.StartDoc(&docInfo) == SP_ERROR)
		{
			// enable window before proceeding
			EnableWindow(TRUE);

			// cleanup and show error message
			OnEndPrinting(&dcPrint, &printInfo);
			dlgPrintStatus.DestroyWindow();
			dcPrint.Detach();   // will be cleaned up by CPrintInfo destructor
			AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
			return;
		}

		// Guarantee values are in the valid range
		UINT nEndPage = printInfo.GetToPage();
		UINT nStartPage = printInfo.GetFromPage();

		if (nEndPage < printInfo.GetMinPage())
			nEndPage = printInfo.GetMinPage();
		if (nEndPage > printInfo.GetMaxPage())
			nEndPage = printInfo.GetMaxPage();

		if (nStartPage < printInfo.GetMinPage())
			nStartPage = printInfo.GetMinPage();
		if (nStartPage > printInfo.GetMaxPage())
			nStartPage = printInfo.GetMaxPage();

		int nStep = (nEndPage >= nStartPage) ? 1 : -1;
		nEndPage = (nEndPage == 0xffff) ? 0xffff : nEndPage + nStep;

		VERIFY(strTemp.LoadString(AFX_IDS_PRINTPAGENUM));

		// begin page printing loop
		BOOL bError = FALSE;
		for (printInfo.m_nCurPage = nStartPage;
			printInfo.m_nCurPage != nEndPage; printInfo.m_nCurPage += nStep)
		{
			OnPrepareDC(&dcPrint, &printInfo);

			// check for end of print
			if (!printInfo.m_bContinuePrinting)
				break;

			// write current page
			TCHAR szBuf[80];
			wsprintf(szBuf, strTemp, printInfo.m_nCurPage);
			dlgPrintStatus.SetDlgItemText(AFX_IDC_PRINT_PAGENUM, szBuf);

			// set up drawing rect to entire page (in logical coordinates)
			printInfo.m_rectDraw.SetRect(0, 0,
				dcPrint.GetDeviceCaps(HORZRES),
				dcPrint.GetDeviceCaps(VERTRES));
			dcPrint.DPtoLP(&printInfo.m_rectDraw);

			// attempt to start the current page
			if (dcPrint.StartPage() < 0)
			{
				bError = TRUE;
				break;
			}

			// must call OnPrepareDC on newer versions of Windows because
			// StartPage now resets the device attributes.
			OnPrepareDC(&dcPrint, &printInfo);

			ASSERT(printInfo.m_bContinuePrinting);

			// page successfully started, so now render the page
			OnPrint(&dcPrint, &printInfo);
			if (dcPrint.EndPage() < 0 || !_AfxAbortProcEx(dcPrint.m_hDC, 0))
			{
				bError = TRUE;
				break;
			}
		}

		// cleanup document printing process
		if (!bError)
			dcPrint.EndDoc();
		else
			dcPrint.AbortDoc();

		EnableWindow();    // enable window

		OnEndPrinting(&dcPrint, &printInfo);    // clean up after printing
		dlgPrintStatus.DestroyWindow();

		dcPrint.Detach();   // will be cleaned up by CPrintInfo destructor
	}
}
