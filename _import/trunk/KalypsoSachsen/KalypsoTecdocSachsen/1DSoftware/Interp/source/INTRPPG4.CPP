// intrppg4.cpp: Implementierungsdatei
//

#include "stdAfx.h"

#include "intrpsht.h"

#include "intrppg4.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CInterpPage4 

IMPLEMENT_DYNCREATE(CInterpPage4, CPropertyPage)

CInterpPage4::CInterpPage4(CInterpSheet *pOwner) : CPropertyPage(CInterpPage4::IDD)
{
	//{{AFX_DATA_INIT(CInterpPage4)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
	m_pOwner = pOwner;
}

CInterpPage4::~CInterpPage4()
{
}

void CInterpPage4::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CInterpPage4)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CInterpPage4, CPropertyPage)
	//{{AFX_MSG_MAP(CInterpPage4)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CInterpPage4 

BOOL CInterpPage4::OnInitDialog() 
{
	CRect rect;
	LV_COLUMN lvColumn;
	int width;
	CString str;

	CPropertyPage::OnInitDialog();

	m_list.SetFullRowSel(TRUE);
	m_list.GetClientRect(&rect);
	width = rect.Width()/6;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_STATION);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_SOHLHOEHE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	lvColumn.cx = width*2;
	str.LoadString(IDS_LBVHOEHE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 2;
	m_list.InsertColumn(2, &lvColumn);
	lvColumn.cx = width*2;
	str.LoadString(IDS_RBVHOEHE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 3;
	m_list.InsertColumn(3, &lvColumn);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

BOOL CInterpPage4::OnSetActive() 
{
	LV_ITEM lvItem;
	CString str;
	int i;

	m_pOwner->SetWizardButtons(PSWIZB_BACK | PSWIZB_FINISH);
	
	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	m_list.DeleteAllItems();
	for (i=0; i<m_pOwner->m_batch.GetNumEntries(); i++)
	{
		str.Format("%.4f", m_pOwner->m_batch.GetStation(i));
		lvItem.iItem = lvItem.lParam = i;
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		str.Format("%.4f", m_pOwner->m_batch.GetBedHeight(i));
		m_list.SetItemText(i, 1, str);
		str = m_pOwner->m_batch.GetLBankString(i);
		m_list.SetItemText(i, 2, str);
		str = m_pOwner->m_batch.GetRBankString(i);
		m_list.SetItemText(i, 3, str);
	}
	
	return CPropertyPage::OnSetActive();
}

BOOL CInterpPage4::OnWizardFinish() 
{
	m_pOwner->Interpolate();
	
	return CPropertyPage::OnWizardFinish();
}
