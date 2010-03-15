// lossdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "lossdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LossDialog 


LossDialog::LossDialog( CWnd* pParent /*=NULL*/, State* pState /*=NULL*/ )
	: CDialog(LossDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(LossDialog)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
	m_pState = pState;
}


void LossDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LossDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(LossDialog, CDialog)
	//{{AFX_MSG_MAP(LossDialog)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LossDialog 

BOOL LossDialog::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	theApp.WinHelp( HID_KAPITEL_4_5_2 );
	
	return CDialog::OnHelpInfo( pHelpInfo );
}

BOOL LossDialog::OnInitDialog() 
{
	int i, width, type;
	CString str, data;
	CRect rect;
	LV_COLUMN lvColumn;
	LV_ITEM lvItem;
	CrossSection *cs;
	Loss *loss;

	ASSERT(m_pState!=NULL);
	
	CDialog::OnInitDialog();

	m_list.SetFullRowSel(TRUE);
	m_list.SetGridList(TRUE);
	m_list.GetClientRect(&rect);
	width = rect.Width()/11;
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = width;
	str.LoadString(IDS_STATION);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VART1);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VWERT1);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 2;
	m_list.InsertColumn(2, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VART2);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 3;
	m_list.InsertColumn(3, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VWERT2);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 4;
	m_list.InsertColumn(4, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VART3);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 5;
	m_list.InsertColumn(5, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VWERT3);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 6;
	m_list.InsertColumn(6, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VART4);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 7;
	m_list.InsertColumn(7, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VWERT4);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 8;
	m_list.InsertColumn(8, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VART5);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 9;
	m_list.InsertColumn(9, &lvColumn);
	lvColumn.cx = width;
	str.LoadString(IDS_VWERT5);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 10;
	m_list.InsertColumn(10, &lvColumn);

	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	cs = m_pState->GetFirstCrossSection();
	while (cs!=NULL)
	{
		loss = m_pState->FindLoss(cs->GetStation());

		str.Format("%.4f", cs->GetStation());
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		m_list.SetItemColor(lvItem.iItem, 0, ::GetSysColor(COLOR_BTNTEXT),
			::GetSysColor(COLOR_BTNFACE));
		
		
		str.LoadString(IDS_INFLOW);
		data = str + "\n";
		str.LoadString(IDS_KRUEMMER);
		data += str + "\n";
		str.LoadString(IDS_EXTRAV);
		data += str + "\n";
		str.LoadString(IDS_CALCD);
		data += str + "\n";
		str.LoadString(IDS_OUTFLOW);
		data += str + "\n";
		for (i=0; i<N_VLTYPES; i++)
		{
			if (loss==NULL)
				type = i;
			else
				type = loss->GetType(i);
			switch (type)
			{
				case VL_EINLAUF:
					str.LoadString(IDS_INFLOW);
					break;

				case VL_KRUEMMER:
					str.LoadString(IDS_KRUEMMER);
					break;

				case VL_ZUSATZ:
					str.LoadString(IDS_EXTRAV);
					break;

				case VL_RECHEN:
					str.LoadString(IDS_CALCD);
					break;

				case VL_AUSLAUF:
					str.LoadString(IDS_OUTFLOW);
					break;

			}
			m_list.SetItemText(lvItem.iItem, i*2+1, str);
			m_list.SetItemControl(lvItem.iItem, i*2+1, LCTRLEX_DROPDOWNLIST, data);
			if (loss==NULL || !loss->ValueDefined(i))
				str.Empty();
			else
				str.Format("%.*g", DBL_DIG, loss->GetValue(i));
			m_list.SetItemText(lvItem.iItem, i*2+2, str);
			m_list.SetItemControl(lvItem.iItem, i*2+2, LCTRLEX_EDIT, NULL, EMPTYDOUBLE_FIELD);
		}

		cs = m_pState->GetNextCrossSection();
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void LossDialog::OnOK() 
{
	int i, j;
	double station;
	int type[N_VLTYPES];
	double value[N_VLTYPES];
	CString str, temp;
	Loss *loss;

	if (!m_list.DeactivateControl())
		return;

	for (i=0; i<m_list.GetItemCount(); i++)
	{
		BOOL bDefined = FALSE;

		str = m_list.GetItemText(i, 0);
		station = atof(str);
		loss = m_pState->FindLoss(station);

		for (j=0; j<N_VLTYPES; j++)
		{
			str = m_list.GetItemText(i, j*2+1);
			temp.LoadString(IDS_INFLOW);
			if (str==temp)
				type[j] = VL_EINLAUF;
			temp.LoadString(IDS_KRUEMMER);
			if (str==temp)
				type[j] = VL_KRUEMMER;
			temp.LoadString(IDS_EXTRAV);
			if (str==temp)
				type[j] = VL_ZUSATZ;
			temp.LoadString(IDS_CALCD);
			if (str==temp)
				type[j] = VL_RECHEN;
			temp.LoadString(IDS_OUTFLOW);
			if (str==temp)
				type[j] = VL_AUSLAUF;
			str = m_list.GetItemText(i, j*2+2);
      value[j] = std::numeric_limits<double>::infinity();
			if (!str.IsEmpty())
			{
				value[j] = atof(str);
				bDefined = TRUE;
			}
		}
		if (loss==NULL)
		{
			if (bDefined)
			{
				loss = new Loss(m_pState->GetProject(), m_pState);
				loss->SetStation(station);
				m_pState->AddLoss(loss);
			}
		}
		else
		{
			if (!bDefined)
			{
				m_pState->RemoveLoss(loss);
				delete loss;
			}
		}
		if (bDefined)
		{
			for (j=0; j<N_VLTYPES; j++)
			{
				loss->SetType(j, type[j]);
				loss->SetValue(j, value[j]);
			}
		}
	}
	
	CDialog::OnOK();
}