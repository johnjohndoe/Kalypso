// intrppg1.cpp : implementation file
//

#include "stdAfx.h"

#include "intrpsht.h"
#include "interp.h"

#include "intrppg1.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInterpPage1 property page

IMPLEMENT_DYNCREATE(CInterpPage1, CPropertyPage)

CInterpPage1::CInterpPage1(CInterpSheet *pOwner /*=NULL*/) : CPropertyPage(CInterpPage1::IDD)
{
	//{{AFX_DATA_INIT(CInterpPage1)
	m_nProfilNum = 0;
	//}}AFX_DATA_INIT
	m_pOwner = pOwner;
	sec[0] = sec[1] = NULL;
}

CInterpPage1::~CInterpPage1()
{
}

void CInterpPage1::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CInterpPage1)
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Text(pDX, IDC_EDIT1, m_nProfilNum);
	DDV_MinMaxInt(pDX, m_nProfilNum, 1, 10);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CInterpPage1, CPropertyPage)
	//{{AFX_MSG_MAP(CInterpPage1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CInterpPage1 message handlers

BOOL CInterpPage1::OnInitDialog() 
{
	CrossSection *cs;
	int width;
	CString str;
	CRect rect;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	CTypedPtrList<CObList, CrossSection*> profils;
	POSITION pos;

	CPropertyPage::OnInitDialog();
	
	m_list.SetFullRowSel(TRUE);
	m_list.GetClientRect(&rect);
	width = rect.Width()/6;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width*2;
	str.LoadString(IDS_STATION);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_PK);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VZK);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 2;
	m_list.InsertColumn(2, &lvColumn);
	lvColumn.cx = width*2;
	str.LoadString(IDS_URDATEI);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 3;
	m_list.InsertColumn(3, &lvColumn);
	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	cs = theApp.m_pState->GetFirstCrossSection();
	while (cs!=NULL)
	{
		profils.AddTail(cs);
		cs = theApp.m_pState->GetNextCrossSection();
	}
	OrderListByStation(profils);
	pos = profils.GetHeadPosition();
	while (pos!=NULL)
	{
		cs = profils.GetNext(pos);
		str.Format("%.4f", cs->GetStation());
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		//				lvItem.iImage = image;
		m_list.InsertItem(&lvItem);
		m_list.SetItemText(lvItem.iItem, 1, cs->GetPK());
		str.Format("%d", cs->GetVZK());
		m_list.SetItemText(lvItem.iItem, 2, str);
		m_list.SetItemText(lvItem.iItem, 3, cs->GetFileName());
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CInterpPage1::OnSetActive() 
{
	CString file1, file2, str;
	int i;

	str.LoadString(IDS_INTERP);
	m_pOwner->SetWindowText(str);
	m_pOwner->SetWizardButtons(PSWIZB_NEXT);
	m_nProfilNum = m_pOwner->GetProfilNum();
	if (sec[0]!=NULL && sec[1]!=NULL)
	{
		file1 = sec[0]->GetFileName();
		file2 = sec[1]->GetFileName();
		for (i=0; i<m_list.GetItemCount(); i++)
		{
			str = m_list.GetItemText(i, 3);
			if (str==file1 || str==file2)
				m_list.SetItemState(i, LVIS_SELECTED, LVIS_SELECTED);
		}
	}
	UpdateData(FALSE);
	
	return CPropertyPage::OnSetActive();
}

LRESULT CInterpPage1::OnWizardNext() 
{
	int i;
	CArray<int, int> m_sels;
	CString file, str;
	CrossSection *cs;

	if (UpdateData(TRUE))
	{
		for (i=0; i<m_list.GetItemCount(); i++)
		{
			if (m_list.GetItemState(i, LVIS_SELECTED)==LVIS_SELECTED)
				m_sels.Add(i);
		}
		if (m_sels.GetSize()==2)
		{
			for (i=0; i<2; i++)
			{
				file = m_list.GetItemText(m_sels[i], 3);
				cs = theApp.m_pProject->GetFirstCrossSection();
				while (cs!=NULL)
				{
					if (file == cs->GetFileName())
					{
						sec[i] = cs;
						break;
					}
					cs = theApp.m_pProject->GetNextCrossSection();
				}
			}
			m_pOwner->SetProfilNum(m_nProfilNum);

			return CPropertyPage::OnWizardNext();
		}
		else
		{
			AfxMessageBox(IDS_SELECT2ITEMS, MB_OK | MB_ICONEXCLAMATION);
		}
	}

	return 1L;
}