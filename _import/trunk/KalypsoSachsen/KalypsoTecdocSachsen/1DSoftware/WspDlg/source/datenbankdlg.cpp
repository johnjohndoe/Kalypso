// DatenbankDlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "wspdlg.h"
#include "datenbankdlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld DatenbankDlg 


DatenbankDlg::DatenbankDlg(CWnd* pParent /*=NULL*/,CString start_path,int dlgtyp)
	: CDialog(DatenbankDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(DatenbankDlg)
	// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
	pFileName=start_path;
	m_DlgTyp=dlgtyp;
	m_pParent=pParent;
	switch(m_DlgTyp)
	{
	case ueberfallbeiwert:
		pFileName+="db_mue.txt";
		break;
    case rauheit_ks:
        pFileName+="db_ks.txt";
		break;
    case rauheit_kst:
		pFileName+="db_kst.txt";
		break;
    case bewuchs:
		pFileName+="db_bew.txt";
		break;
	default:
		pFileName+="db_mue.txt";
	}
}


void DatenbankDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(DatenbankDlg)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(DatenbankDlg, CDialog)
	//{{AFX_MSG_MAP(DatenbankDlg)
	ON_WM_DESTROY()
	ON_BN_CLICKED(IDC_BUTTON1, OnNeu)
	ON_BN_CLICKED(IDC_BUTTON2, OnDel)
	ON_BN_CLICKED(IDC_BUTTON3, OnUpdate)
	ON_WM_KEYDOWN()
    ON_NOTIFY_EX( TTN_NEEDTEXT, 0, OnToolTipNotify )
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten DatenbankDlg 

BOOL DatenbankDlg::OnInitDialog() 
{
    int width,i;
    CString str, data;
    CRect rect;
    LV_COLUMN lvColumn;
    LV_ITEM lvItem;
    BOOL ok;
	
	
	CDialog::OnInitDialog();
	
    lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
    lvItem.state = 0;
    lvItem.stateMask = 0;
	
	Datenbank dat(m_DlgTyp);
	
	dat.SetDatabankName(pFileName);
	ok=dat.Load();
	
	m_list.SetGridList(TRUE);
	m_list.GetClientRect(&rect);
	switch(m_DlgTyp)
	{
		case ueberfallbeiwert:
		{
			width = rect.Width()*3/4;
			lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
			lvColumn.fmt = LVCFMT_LEFT;
			lvColumn.cx = width/12;
			str.LoadString(IDS_NUM);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 0;
			m_list.InsertColumn(0, &lvColumn);
			
			lvColumn.cx = width;
			str.LoadString(IDS_KRONENFORM);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 1;
			m_list.InsertColumn(1, &lvColumn);
			
			lvColumn.cx = width/4;
			str.LoadString(IDS_UEBERFALLBEIWERT);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 2;
			m_list.InsertColumn(2, &lvColumn);
			if(ok)
			{
				for(i=0;i < dat.GetSize();i++)
				{
					lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
					data.Format("%d",lvItem.iItem);
					lvItem.iSubItem = 0;
					lvItem.pszText = data.GetBuffer(data.GetLength());
					data.ReleaseBuffer();
					lvItem.cchTextMax = data.GetLength();
					m_list.InsertItem(&lvItem);
					dat.GetName(i,data);
					m_list.SetItemText(lvItem.iItem, 1, data);
					data.Format("%.4lf",dat.GetVar1(i));
					m_list.SetItemText(lvItem.iItem, 2, data);
					m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_EDIT, NULL, STRING_FIELD);
					m_list.SetItemControl(lvItem.iItem, 2, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
				}
			}
		}
        break;

		case rauheit_ks:
		case rauheit_kst:
		{
			width = rect.Width()*3/4;
			lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
			lvColumn.fmt = LVCFMT_LEFT;
			lvColumn.cx = width/12;
			str.LoadString(IDS_NUM);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 0;
			m_list.InsertColumn(0, &lvColumn);
			
			lvColumn.cx = width;
			str.LoadString(IDS_COMMENT);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 1;
			m_list.InsertColumn(1, &lvColumn);
			
			lvColumn.cx = width/4;
			str.LoadString(IDS_VALUE);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 2;
			m_list.InsertColumn(2, &lvColumn);
			if(ok)
			{
				for(i=0;i < dat.GetSize();i++)
				{
					lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
					data.Format("%d",lvItem.iItem);
					lvItem.iSubItem = 0;
					lvItem.pszText = data.GetBuffer(data.GetLength());
					data.ReleaseBuffer();
					lvItem.cchTextMax = data.GetLength();
					m_list.InsertItem(&lvItem);
					dat.GetName(i,data);
                    data.TrimLeft();
                    data.TrimRight();
					m_list.SetItemText(lvItem.iItem, 1, data);
					data.Format("%.4lf",dat.GetVar1(i));
					m_list.SetItemText(lvItem.iItem, 2, data);
					m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_EDIT, NULL, STRING_FIELD);
					m_list.SetItemControl(lvItem.iItem, 2, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
				}
			}
		}
        break;

		case bewuchs:
		{
			width = rect.Width()*3/4;
			lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
			lvColumn.fmt = LVCFMT_LEFT;
			lvColumn.cx = width/12;
			str.LoadString(IDS_NUM);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 0;
			m_list.InsertColumn(0, &lvColumn);
			
            lvColumn.cx = width*3/5;
			str.LoadString(IDS_COMMENT);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 1;
			m_list.InsertColumn(1, &lvColumn);
			
			lvColumn.cx = width/5;
			str.LoadString(IDS_AX);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 2;
			m_list.InsertColumn(2, &lvColumn);
			
            lvColumn.cx = width/5;
			str.LoadString(IDS_AY);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 3;
			m_list.InsertColumn(3, &lvColumn);
			
            lvColumn.cx = width/5;
			str.LoadString(IDS_DP);
			lvColumn.pszText = str.GetBuffer(str.GetLength());
			str.ReleaseBuffer();
			lvColumn.iSubItem = 4;
			m_list.InsertColumn(4, &lvColumn);
			if(ok)
			{
				for(i=0;i < dat.GetSize();i++)
				{
					lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
					data.Format("%d",lvItem.iItem);
					lvItem.iSubItem = 0;
					lvItem.pszText = data.GetBuffer(data.GetLength());
					data.ReleaseBuffer();
					lvItem.cchTextMax = data.GetLength();
					m_list.InsertItem(&lvItem);
					dat.GetName(i,data);
					m_list.SetItemText(lvItem.iItem, 1, data);
					data.Format("%.4lf",dat.GetVar1(i));
					m_list.SetItemText(lvItem.iItem, 2, data);
                    data.Format("%.4lf",dat.GetVar2(i));
					m_list.SetItemText(lvItem.iItem, 3, data);
                    data.Format("%.4lf",dat.GetVar3(i));
					m_list.SetItemText(lvItem.iItem, 4, data);
					m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_EDIT, NULL, STRING_FIELD);
					m_list.SetItemControl(lvItem.iItem, 2, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
                    m_list.SetItemControl(lvItem.iItem, 3, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
					m_list.SetItemControl(lvItem.iItem, 4, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
					
				}
			}
		}
        break;
	}
	
	
	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void DatenbankDlg::OnDestroy() 
{
	CDialog::OnDestroy();
    if(theApp.datDlg!=NULL)
        theApp.parentDatenbank.Detach();
}

void DatenbankDlg::PostNcDestroy() 
{
	if(theApp.datDlg!=NULL)
		delete this;
	theApp.datDlg  = NULL;
	CDialog::PostNcDestroy();
}

void DatenbankDlg::OnNeu() 
{
    LV_ITEM lvItem;
    CString data;
    lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
    lvItem.state = LVIS_FOCUSED;
    lvItem.stateMask = 0; 
    lvItem.iItem = lvItem.lParam = m_list.GetItemCount();    
    data.Format("%d",lvItem.iItem);
    lvItem.iSubItem = 0;
    lvItem.pszText = data.GetBuffer(data.GetLength());
    data.ReleaseBuffer();
    lvItem.cchTextMax = data.GetLength();
    m_list.InsertItem(&lvItem);
    switch(m_DlgTyp)
	{
		case ueberfallbeiwert:
		case rauheit_ks:
		case rauheit_kst:
			m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_EDIT, NULL, STRING_FIELD);
			m_list.SetItemControl(lvItem.iItem, 2, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
			break;

		case bewuchs:
			m_list.SetItemControl(lvItem.iItem, 1, LCTRLEX_EDIT, NULL, STRING_FIELD);
			m_list.SetItemControl(lvItem.iItem, 2, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
			m_list.SetItemControl(lvItem.iItem, 3, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
			m_list.SetItemControl(lvItem.iItem, 4, LCTRLEX_EDIT, NULL, DOUBLE_FIELD);
			break;
	}
	m_list.EnsureVisible(lvItem.iItem,FALSE); 
	m_list.StartEdit(lvItem.iItem, 1);
}

void DatenbankDlg::OnDel() 
{
    int i;
	CString data;

    for(i=0;i<m_list.GetItemCount();i++)
	{
        if(m_list.GetItemState(i,LVIS_SELECTED) ==LVIS_SELECTED)
            {
            m_list.DeleteItem(i);
            for(i=0;i<m_list.GetItemCount();i++)
                {
                data.Format("%d",i);
                m_list.SetItemText(i, 0, data);
                }
            }
	}
}

void DatenbankDlg::OnUpdate() 
{
	int i;
	CString lese_string;
    typedef struct _Bewuchs
    {
		double ax;
		double ay;
		double dp;
    } Bewuchs;
    Bewuchs bew = { -1.23456789E+10, -1.23456789E+10, -1.23456789E+10 };
	
	switch(m_DlgTyp)
	{
		case ueberfallbeiwert:
      {
        for(i=0;i<m_list.GetItemCount();i++)
          if(m_list.GetItemState(i,LVIS_SELECTED) ==LVIS_SELECTED)
            if (m_pParent!=NULL)
            {
              lese_string = m_list.GetItemText(i,2);
              if (!lese_string.IsEmpty())
              {
                m_pParent->SendMessage(theApp.nUeberfallbeiwertUpdateMsg, 0,(long)lese_string.GetBuffer(lese_string.GetLength()));
                lese_string.ReleaseBuffer();
              }
            }
      }
      break;

		case rauheit_ks:
		case rauheit_kst:
        {
			for(i=0;i<m_list.GetItemCount();i++)
				if(m_list.GetItemState(i,LVIS_SELECTED) ==LVIS_SELECTED)
					if (m_pParent!=NULL)
                    {
						lese_string = m_list.GetItemText(i,2);
						if (!lese_string.IsEmpty())
						{
							m_pParent->SendMessage(theApp.nRauheitswertUpdateMsg, 0,(long)lese_string.GetBuffer(lese_string.GetLength()));
							lese_string.ReleaseBuffer();
						}
                    }
        }
        break;

		case bewuchs:
        {
			for(i=0;i<m_list.GetItemCount();i++)
				if(m_list.GetItemState(i,LVIS_SELECTED) ==LVIS_SELECTED)
					if (m_pParent!=NULL)
                    {
						lese_string = m_list.GetItemText(i,2);
						if (!lese_string.IsEmpty())
							bew.ax = atof(lese_string);
						else
							bew.ax=-1.23456789E+10;
						lese_string = m_list.GetItemText(i,3);
						if (!lese_string.IsEmpty())
							bew.ay = atof(lese_string);
						else
							bew.ay=-1.23456789E+10;
						lese_string = m_list.GetItemText(i,4);
						if (!lese_string.IsEmpty())
							bew.dp = atof(lese_string);
						else
							bew.dp=-1.23456789E+10;
						m_pParent->SendMessage(theApp.nBewuchswertUpdateMsg, 0,(long)&bew);	
                    }
        }
        break;
    }
}

void DatenbankDlg::OnOK() 
{
    int i;
    Datenbank dat(m_DlgTyp);
	CString data;
    
    dat.SetDatabankName(pFileName);
	switch(m_DlgTyp)
	{
		case ueberfallbeiwert:
		case rauheit_ks:
		case rauheit_kst:
        {
			for(i=0;i<m_list.GetItemCount();i++)
            {
				data=m_list.GetItemText(i,1);
				data.TrimLeft();
				data.TrimRight();             
				if(data.GetLength()!=0)
                {
					dat.AddName(data);
					dat.SetVar1(i,atof(m_list.GetItemText(i,2)));
                }
            }
        }
        break;

		case bewuchs:
        {
			for(i=0;i<m_list.GetItemCount();i++)
            {
				data=m_list.GetItemText(i,1);
				data=m_list.GetItemText(i,1);
				data.TrimLeft();
				data.TrimRight();             
				if(data.GetLength()!=0)
                {
					dat.AddName(data);
					dat.SetVar1(i,atof(m_list.GetItemText(i,2)));
					dat.SetVar2(i,atof(m_list.GetItemText(i,3)));
					dat.SetVar3(i,atof(m_list.GetItemText(i,4)));
                }
            }
        }
        break;
    }
    dat.Save();
	
	CDialog::OnOK();
}

void DatenbankDlg::OnCancel() 
{   
	if(theApp.datDlg!=NULL)
		DestroyWindow();
	else
		EndDialog(0);
}

BOOL DatenbankDlg::PreTranslateMessage(MSG* pMsg) 
{
    //m_tooltip.RelayEvent(pMsg);
    //FilterToolTipMessage(pMsg);
	if( IsDialogMessage( pMsg ) ) //für Modeless
		return TRUE;   //für Modeless
    else //für Modeless
	return CDialog::PreTranslateMessage(pMsg);
}

BOOL DatenbankDlg::OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam, LRESULT* pLResult) 
{
	// TODO: Speziellen Code hier einfügen und/oder Basisklasse aufrufen
	if(message==WM_KEYDOWN)// && wParam==VK_TAB)
        int stop=1;
	return CDialog::OnChildNotify(message, wParam, lParam, pLResult);
}

void DatenbankDlg::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	// TODO: Code für die Behandlungsroutine für Nachrichten hier einfügen und/oder Standard aufrufen
	
	CDialog::OnKeyDown(nChar, nRepCnt, nFlags);
}

BOOL DatenbankDlg::OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult) 
{
	// TODO: Speziellen Code hier einfügen und/oder Basisklasse aufrufen
/*	switch (((LPNMHDR)lParam)->code)
            {
             // ON_NOTIFY_EX
               //   ON_NOTIFY_REFLECT 
                case TTN_NEEDTEXT :
                    // Time to display a tool tip text box. Return the ID of
                    // the string in our resource table. The control will
                    // pick it up from there.
                    //((LPTOOLTIPTEXT)lParam)->hinst = ghInstance;
                    //((LPTOOLTIPTEXT)lParam)->lpszText = (LPTSTR)((LPTOOLTIPTEXT)lParam)->hdr.idFrom;
                    ((LPTOOLTIPTEXT)lParam)->lpszText = (LPTSTR)((LPTOOLTIPTEXT)lParam)->hdr.idFrom;
                    break;
            }*/
	return CDialog::OnNotify(wParam, lParam, pResult);
}

BOOL DatenbankDlg::OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult )
{
  /*  TOOLTIPTEXT *pTTT = (TOOLTIPTEXT *)pNMHDR;   
 UINT nID =pNMHDR->idFrom;

    if (pTTT->uFlags & TTF_IDISHWND)   
        {
        // idFrom ist der HWND des Werkzeugs
        nID = ::GetDlgCtrlID((HWND)nID);        
        if(nID)        
            {
            pTTT->lpszText = MAKEINTRESOURCE(nID);
            pTTT->hinst = AfxGetResourceHandle();
            return(TRUE);
        
            }  
        }   */ 
    return(FALSE);
}
