// PrefDlg.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "stpldoc.h"
#include "template.h"
#include "propdlg.h"
#include "plotter.h"
#include "stempel.h"

#include "prefdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPrefDialog dialog


CPrefDialog::CPrefDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CPrefDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPrefDialog)
	//}}AFX_DATA_INIT
	GETPLOTTERAPP->LoadTemplates();
	GETPLOTTERAPP->LoadStempelFiles();
	for( int stID = 0; stID < GETPLOTTERAPP->m_strStempelFiles.GetSize(); stID++ )
	{
    CStempelDoc* pStpl = CStempelDoc::LoadStempel( GETPLOTTERAPP->m_strStempelFiles[stID] );
		if( pStpl )
		{
      CString str;
			pStpl->GetName( str );
			m_stempels.SetAtGrow( stID, str );
			pStpl->DeleteContents();
			delete pStpl;
		}
		else
			m_stempels.SetAtGrow( stID, GETPLOTTERAPP->m_strStempelFiles[stID] );
  } // for stID

	for( int tempID = 0; tempID < GETPLOTTERAPP->m_templates.GetSize(); tempID++ )
	{
		m_templates.SetAtGrow( tempID, GETPLOTTERAPP->m_templates[tempID] );
		CString str = GETPLOTTERAPP->m_templates[tempID]->GetStempel()->GetFileName();
		m_templatestempels.SetAtGrow( tempID, -1 );
		if( !str.IsEmpty() )
		{
			for( int j = 0; j < GETPLOTTERAPP->m_strStempelFiles.GetSize(); j++ )
			{
        CString stplFile = GETPLOTTERAPP->m_strStempelFiles[j];

        // es kann sein, dass im Template nur der Relative Pfad steht, dadurch wird der
        // Vergleich ein bisschen komplizierter

        int minLength = min( str.GetLength(), stplFile.GetLength() );

        CString s1 = str.Right( minLength );
        CString s2 = stplFile.Right( minLength );

        //			if( str.CompareNoCase( stplFile ) == 0 )
        if( s1.CompareNoCase( s2 ) == 0 )
          m_templatestempels.SetAtGrow( tempID, j );
      } // for j
    } // if str
  } // for tempID
} // Konstruktor

CPrefDialog::~CPrefDialog()
{
	int i, j;
	BOOL bFound;
	CTemplate *pTemp;
	
	// remove left over templates
	for (i=0; i<m_templates.GetSize(); i++)
	{
		pTemp = m_templates.GetAt(i);
		bFound = FALSE;
		for (j=0; j<GETPLOTTERAPP->m_templates.GetSize(); j++)
		{
			if (pTemp==GETPLOTTERAPP->m_templates.GetAt(j))
				bFound = TRUE;
		}
		if (!bFound)
		{
			pTemp->DeleteContents();
			delete pTemp;
		}
	}
	GETPLOTTERAPP->FlushTemplates();
	GETPLOTTERAPP->FlushStempelFiles();
}

void CPrefDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrefDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}

void CPrefDialog::SetCheck(int nIndex, BOOL bChecked)
{
	m_list.SetItemState(nIndex, INDEXTOSTATEIMAGEMASK(bChecked ? 2 : 1), LVIS_STATEIMAGEMASK);
}

BOOL CPrefDialog::GetCheck(int nIndex)
{
	return (BOOL)(m_list.GetItemState(nIndex, LVIS_STATEIMAGEMASK)==INDEXTOSTATEIMAGEMASK(2));
}

BEGIN_MESSAGE_MAP(CPrefDialog, CDialog)
	//{{AFX_MSG_MAP(CPrefDialog)
	ON_BN_CLICKED(IDC_BUTTON1, OnEdit)
	ON_BN_CLICKED(IDC_BUTTON2, OnAdd)
	ON_BN_CLICKED(IDC_BUTTON3, OnDelete)
	ON_NOTIFY(LVN_ENDLABELEDIT, IDC_LIST1, OnEndlabeleditList1)
	ON_NOTIFY(NM_DBLCLK, IDC_LIST1, OnDblclkList1)
	ON_NOTIFY(LVN_ITEMCHANGED, IDC_LIST1, OnItemchangedList1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPrefDialog message handlers

BOOL CPrefDialog::OnInitDialog() 
{
	CString str;
	int i, width;
	CRect rect;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	
	CDialog::OnInitDialog();

	m_list.SetGridList(TRUE);
	m_list.SetStateIcons(TRUE);
	m_list.GetClientRect(&rect);
	width = rect.Width()/2;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_SETTING);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	str.LoadString(IDS_STAMP);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);

	m_StateImageList.Create(IDB_STATEICONS, 16, 1, RGB(255, 0, 0));

	m_list.SetImageList(&m_StateImageList, LVSIL_STATE);

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.stateMask = LVIS_STATEIMAGEMASK;
	lvItem.state = INDEXTOSTATEIMAGEMASK(1);

	m_data.LoadString(IDS_NONE);
	m_data += "\n";
	for (i=0; i<m_stempels.GetSize(); i++)
	{
		m_data += m_stempels[i] + "\n";
	}
	for (i=0; i<m_templates.GetSize(); i++)
	{
		str = m_templates.GetAt(i)->GetName();
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		str.LoadString(IDS_NONE);
		if (m_templatestempels[i]>=0)
			m_list.SetItemText(lvItem.iItem, 1, m_stempels[m_templatestempels[i]]);
		else
			m_list.SetItemText(lvItem.iItem, 1, str);
		m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWNLIST, m_data);
		if (i==GETPLOTTERAPP->m_nTemplate)
			SetCheck(m_list.GetItemCount()-1, 1);
		else
			SetCheck(m_list.GetItemCount()-1, 0);
	}
	GetDlgItem(IDC_BUTTON1)->EnableWindow(FALSE);
	GetDlgItem(IDC_BUTTON3)->EnableWindow(FALSE);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CPrefDialog::OnOK() 
{
	int i, j;
	CTemplate *pTemp;
	BOOL bFound;
	CString str, temp;

	if (!UpdateData(TRUE))
		return;
	// remove any deleted templates
	for (i=0; i<GETPLOTTERAPP->m_templates.GetSize(); i++)
	{
		pTemp = GETPLOTTERAPP->m_templates.GetAt(i);
		bFound = FALSE;
		for (j=0; j<m_templates.GetSize(); j++)
		{
			if (pTemp==m_templates.GetAt(j))
				bFound = TRUE;
		}
		if (!bFound)
		{
			pTemp->DeleteContents();
			delete pTemp;
		}
	}
	// update the template list
	GETPLOTTERAPP->m_templates.RemoveAll();
	GETPLOTTERAPP->m_nTemplate = -1;
	for (i=0; i<m_templates.GetSize(); i++)
	{
		str = m_list.GetItemText(i, 0);
		m_templates.GetAt(i)->SetName(str);
		str = m_list.GetItemText(i, 1);
		temp.LoadString(IDS_NONE);
		if (str==temp)
			str.Empty();
		else
		{
			for (j=0; j<m_stempels.GetSize(); j++)
			{
				if (str==m_stempels[j])
				{
					str = GETPLOTTERAPP->m_strStempelFiles[j];
					break;
				}
			}
		}
		m_templates.GetAt(i)->GetStempel()->SetFileName( str );
		GETPLOTTERAPP->m_templates.SetAtGrow(i, m_templates.GetAt(i));
		if (GetCheck(i))
			GETPLOTTERAPP->m_nTemplate = i;
	}
	GETPLOTTERAPP->SaveTemplates();
	
	CDialog::OnOK();
}

void CPrefDialog::OnEdit() 
{
	int i, j;
	BOOL bFound;
	CTemplate *pTemp;
	CString str;

	for (i=0; i<m_list.GetItemCount(); i++)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
			break;
	}
	if (i<m_templates.GetSize())
	{
		pTemp = m_templates.GetAt(i);
		str = pTemp->GetName();
		CPropertyDialog dlg( pTemp, str, 0, TRUE );
		if (dlg.DoModal()==IDOK)
		{
			bFound = FALSE;
			for (j=0; j<GETPLOTTERAPP->m_templates.GetSize(); j++)
			{
				if (pTemp==GETPLOTTERAPP->m_templates.GetAt(j))
					bFound = TRUE;
			}
			if (bFound)		// we have changed an already existing template
			{
				str.LoadString(IDS_CLOSE);
				GetDlgItem(IDCANCEL)->SetWindowText(str);
			}
		}
	}
}

void CPrefDialog::OnAdd() 
{
	CTemplate *pTemp;
	CString str, temp;
	int i;
	LV_ITEM lvItem;

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.stateMask = LVIS_STATEIMAGEMASK;
	lvItem.state = INDEXTOSTATEIMAGEMASK(1);

	pTemp = new CTemplate;
	temp.Format("%d", m_list.GetItemCount());
	str.FormatMessage(IDS_PROPS_N, temp);
	pTemp->SetName(str);
	pTemp->CreateDrawing();
	i = m_templates.GetSize();
	m_templates.SetAtGrow(i, pTemp);
	m_templatestempels.SetAtGrow(i, -1);
	str = pTemp->GetName();
	lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
	lvItem.iSubItem = 0;
	lvItem.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvItem.cchTextMax = str.GetLength();
	m_list.InsertItem(&lvItem);
	str.LoadString(IDS_NONE);
	m_list.SetItemText(lvItem.iItem, 1, str);
	m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_DROPDOWNLIST, m_data);
}

void CPrefDialog::OnDelete() 
{
	int i;
	
	for (i=0; i<m_list.GetItemCount(); i++)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
			break;
	}
	if (i<m_templates.GetSize())
	{
		m_templates.RemoveAt(i);
		m_templatestempels.RemoveAt(i);
		m_list.DeleteItem(i);
	}
}

void CPrefDialog::OnEndlabeleditList1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	LV_DISPINFO* pDispInfo = (LV_DISPINFO*)pNMHDR;
	CString str;
	
	str = pDispInfo->item.pszText;
	m_templates.GetAt(pDispInfo->item.iItem)->SetName(str);
	*pResult = 1;
}

void CPrefDialog::OnDblclkList1(NMHDR* /*pNMHDR*/, LRESULT* pResult) 
{
	OnEdit();
	*pResult = 0;
}

void CPrefDialog::OnItemchangedList1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	int i;
	BOOL bEnable = FALSE;

	if (GetCheck(pNMListView->iItem))
	{
		for (i=0; i<m_list.GetItemCount(); i++)
		{
			if (i!=pNMListView->iItem)
				SetCheck(i, 0);
		}
	}

	for (i=0; i<m_list.GetItemCount(); i++)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
		{
			bEnable = TRUE;
			break;
		}
	}

	GetDlgItem(IDC_BUTTON1)->EnableWindow(bEnable);
	GetDlgItem(IDC_BUTTON3)->EnableWindow(bEnable);
	*pResult = 0;
}

void CPrefDialog::OnCancel() 
{
	CDialog::OnCancel();
}
