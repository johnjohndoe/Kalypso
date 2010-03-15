// OpenDlg.cpp : implementation file
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"
#include "bce\include\wspfeatures.h"

#include "plotter.h"

#include "opendlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static CListCtrl *listCtrl;
static CTypedPtrMap<CMapWordToPtr, WORD, Section*> *secmap_list;

int CALLBACK ListCompareFunc(LPARAM lParam1, LPARAM lParam2, 
	LPARAM lParamSort)
{
	LV_FINDINFO lvFindInfo;
	int iItem1, iItem2;
	BOOL bProfil = TRUE;
	Section *sec1, *sec2;
	int nResult = 0;
	CString str1, str2;
	CString fileTitle, fileExt;

	sec1 = sec2 = NULL;
	lvFindInfo.flags = LVFI_PARAM;
	lvFindInfo.psz = NULL;
	lvFindInfo.lParam = lParam1;
	iItem1 = listCtrl->FindItem(&lvFindInfo);
	lvFindInfo.lParam = lParam2;
	iItem2 = listCtrl->FindItem(&lvFindInfo);
	secmap_list->Lookup((WORD)lParam1, sec1);
	secmap_list->Lookup((WORD)lParam2, sec2);

	ASSERT(sec1!=NULL && sec2!=NULL);
	
	switch(lParamSort)
	{
		case 0:		// column 0
			if (sec1->GetClassType()==CLASS_TYPE_CSECTION)
			{
				if (((CrossSection*)sec1)->GetStation()<((CrossSection*)sec2)->GetStation())
					nResult = -1;
				if (((CrossSection*)sec1)->GetStation()>((CrossSection*)sec2)->GetStation())
					nResult = 1;
			}
			else
			{
				str1 = listCtrl->GetItemText(iItem1, 3);
				str2 = listCtrl->GetItemText(iItem2, 3);
				nResult = str1.CompareNoCase(str2);
			}
			break;

		case 1:		// column 1
			if (sec1->GetClassType()==CLASS_TYPE_CSECTION)
			{
				str1 = ((CrossSection*)sec1)->GetPK();
				str2 = ((CrossSection*)sec2)->GetPK();
				if (atoi(str1)<atoi(str2))
					nResult = -1;
				if (atoi(str1)>atoi(str2))
					nResult = 1;
			}
			else
			{
				if (((LengthSection*)sec1)->GetStartStation()<((LengthSection*)sec2)->GetStartStation())
					nResult = -1;
				if (((LengthSection*)sec1)->GetStartStation()>((LengthSection*)sec2)->GetStartStation())
					nResult = 1;
			}
			break;

		case 2:		// column 2
			if (sec1->GetClassType()==CLASS_TYPE_CSECTION)
			{
				int vzk1, vzk2;
				vzk1 = ((CrossSection*)sec1)->GetVZK();
				vzk2 = ((CrossSection*)sec2)->GetVZK();
				if (vzk1<vzk2)
					nResult = -1;
				if (vzk1>vzk2)
					nResult = 1;
			}
			else
			{
				if (((LengthSection*)sec1)->GetEndStation()<((LengthSection*)sec2)->GetEndStation())
					nResult = -1;
				if (((LengthSection*)sec1)->GetEndStation()>((LengthSection*)sec2)->GetEndStation())
					nResult = 1;
			}
			break;

		case 3:		// column 3
			str1 = listCtrl->GetItemText(iItem1, 3);
			str2 = listCtrl->GetItemText(iItem2, 3);
			nResult = str1.CompareNoCase(str2);
			break;
	}
	return nResult;
}

/////////////////////////////////////////////////////////////////////////////
// COpenDialog dialog

COpenDialog::COpenDialog( CWnd* pParent, BOOL bInsert, BOOL bDelete, BOOL bMultiPlot )
	: CDialog( IDD_OFFNEN, pParent )
{
	//{{AFX_DATA_INIT(COpenDialog)
	m_project = _T("");
	m_alignLowPoint = FALSE;
	m_alignValue = 0.0;
	m_multiPlotCheck = FALSE;
	//}}AFX_DATA_INIT
	
  m_bInsert = bInsert;
  m_bDelete = bDelete;
  m_bMultiPlot = bMultiPlot;
	m_pState = NULL;
	m_pCurrentSection = NULL;
}


void COpenDialog::DoDataExchange( CDataExchange* pDX )
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(COpenDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Control(pDX, IDC_TREE1, m_tree);
	DDX_Text(pDX, IDC_PROJEKT, m_project);
	//}}AFX_DATA_MAP

  if( m_bMultiPlot )
    DDX_Check( pDX, IDC_OFFNEN_MULTIPLE_PLOT_CHECK, m_multiPlotCheck );

	if( m_bInsert )
	{
		DDX_Check( pDX, IDC_OFFNEN_CHECK_AUSRICHTUNG, m_alignLowPoint );
		DDX_Text( pDX, IDC_OFFNEN_EDIT_AUSRICHTUNG, m_alignValue );
	}
}


BEGIN_MESSAGE_MAP(COpenDialog, CDialog)
	//{{AFX_MSG_MAP(COpenDialog)
	ON_NOTIFY(NM_DBLCLK, IDC_LIST1, OnDblclkList1)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_NOTIFY(LVN_COLUMNCLICK, IDC_LIST1, OnColumnclickList1)
	ON_BN_CLICKED(IDC_OFFNEN_CHECK_AUSRICHTUNG, OnCheckAusrichtung)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COpenDialog message handlers

BOOL COpenDialog::OnInitDialog() 
{
  CPlotterApp* plotApp = GETPLOTTERAPP;
	
	CDialog::OnInitDialog();

  GetDlgItem( IDC_OFFNEN_STATIC_PROJECT )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_STATIC_PROJECT ) ) );
  GetDlgItem( IDC_OFFNEN_STATIC_WATER_STATE )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_STATIC_WATER_STATE ) ) );
  GetDlgItem( IDC_OFFNEN_STATIC_PROFILES )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_STATIC_PROFILES ) ) );
  GetDlgItem( IDC_OFFNEN_MULTIPLE_PLOT_CHECK )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_MULTIPLE_PLOT_CHECK ) ) );
  GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_CHECK_AUSRICHTUNG ) ) );
  GetDlgItem( IDC_OFFNEN_STATIC_AUSRICHTUNG )->SetWindowText( CString( MAKEINTRESOURCE( IDS_OFFNEN_STATIC_AUSRICHTUNG ) ) );

  // erstmal immer verboten
  GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG )->EnableWindow( FALSE );
  GetDlgItem( IDC_OFFNEN_EDIT_AUSRICHTUNG )->EnableWindow( FALSE );

	if( m_bDelete )
    SetWindowText( CString( MAKEINTRESOURCE( IDS_OEFFNEN_TITLE_DELETE ) ) );
  else
    SetWindowText( CString( MAKEINTRESOURCE( IDS_OEFFNEN_TITLE_OPEN ) ) );

  if( m_bInsert )
  {
    SetWindowText( CString( MAKEINTRESOURCE( IDS_OEFFNEN_TITLE_INSERT ) ) );
    GetDlgItem( IDC_OFFNEN_MULTIPLE_PLOT_CHECK )->ShowWindow( FALSE );
  }
  else
  {
    GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG )->ShowWindow( FALSE );
    GetDlgItem( IDC_OFFNEN_EDIT_AUSRICHTUNG )->ShowWindow( FALSE );
    GetDlgItem( IDC_OFFNEN_STATIC_AUSRICHTUNG )->ShowWindow( FALSE );
  }

  if( !m_bMultiPlot )
    GetDlgItem( IDC_OFFNEN_MULTIPLE_PLOT_CHECK )->ShowWindow( FALSE );
  if(!WSPFeatures::Instance()->isEnabled ("PLOTTER","plot_multiplot"))
    GetDlgItem( IDC_OFFNEN_MULTIPLE_PLOT_CHECK )->EnableWindow( FALSE );

	listCtrl = &m_list;
	secmap_list = &m_secmap_list;
	m_tree.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	m_list.SetFullRowSel(TRUE);
	m_list.SetImageList( CCommonImageList::GetList( FALSE ), LVSIL_SMALL);

  CRect rect;
	m_list.GetClientRect( &rect );
  int width = rect.Width() / 6;

  LV_COLUMN lvColumn;
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
	lvColumn.cx = width;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 2;
	m_list.InsertColumn(2, &lvColumn);
	lvColumn.cx = width*2;
	lvColumn.pszText = "";
	lvColumn.iSubItem = 3;
	m_list.InsertColumn(3, &lvColumn);
	
  Project* proj = plotApp->GetProject();
	m_project = proj->GetName();
	if( m_project.IsEmpty() )
		 m_project = proj->GetDir();
	
  UpdateData( FALSE );
	
  for( int i = 0; i < proj->GetWaterCount(); i++ )
	{

  /*
	HTREEITEM hTIW, hTIZ, hTIL;
	State *st;
	Calculation *calc;
	LengthSection *ls;
	CString str, fmt;
	CString fileTitle, fileExt, path;
	CFile file;
	CFileStatus rStatus;
	BOOL bCalcs;
	
	
	int  i;
  */

		CString wname = proj->GetWaterName( i );
		HTREEITEM hTIW = m_tree.InsertItem( wname, IMAGE_WATER, IMAGE_WATER );
		State* st = proj->GetFirstState();
		while( st )
		{
			CString str = st->GetWaterName();
			if( str == wname )
			{
				BOOL bCalcs = FALSE;
				CString str = st->GetName();
				HTREEITEM hTIZ = m_tree.InsertItem(str, IMAGE_STATE, IMAGE_STATE, hTIW);
				str.LoadString( IDS_CROSSSECTIONS );
				HTREEITEM hTIL = m_tree.InsertItem(str, IMAGE_CSECTION, IMAGE_CSECTION, hTIZ);
				m_qmap_tree.SetAt(hTIL, st);
				Calculation* calc = st->GetFirstCalculation();
				while( calc )
				{
					LengthSection* ls = calc->GetLengthSection();
					if( ls )
					{
						CString fileTitle = ls->GetFileTitle();
						if( !bCalcs )
						{
              CString str( MAKEINTRESOURCE( IDS_LENGTHSECTIONS ) );
							HTREEITEM hTIL = m_tree.InsertItem(str, IMAGE_LSECTION, IMAGE_LSECTION, hTIZ);
							m_lmap_tree.SetAt( hTIL, st );
							bCalcs = TRUE;
						}
					}
					calc = st->GetNextCalculation();
				}
			}
			st = proj->GetNextState();
		}
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void COpenDialog::OnOK() 
{
	if( !UpdateData( TRUE ) )
		return;
	
  int count = 0;
	m_Sections.RemoveAll();
	for( int iItem = 0; iItem<m_list.GetItemCount(); iItem++ )
	{
		if( m_list.GetItemState( iItem, LVIS_SELECTED ) == LVIS_SELECTED )
		{
      Section* sec = NULL;
			if( !m_secmap_list.Lookup(iItem, sec))
				break;
			else if( m_pCurrentSection && ( m_pCurrentSection == sec ||
				m_pCurrentSection->GetClassType() != sec->GetClassType() ) )
				break;
			else
				m_Sections.SetAtGrow( count++, sec );
		}
	}
	if( m_Sections.GetSize() == 0 )
		return;
	
  CDialog::OnOK();
}

void COpenDialog::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
  NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;

  CPlotterApp* plotApp = GETPLOTTERAPP;
	
  LV_ITEM lvItem;
	lvItem.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;
	
  Project* proj = plotApp->GetProject();
	m_list.DeleteAllItems();
	m_pState = NULL;
	m_secmap_list.RemoveAll();
	if( m_bInsert )
	{
		GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG )->EnableWindow( FALSE );
		GetDlgItem( IDC_OFFNEN_EDIT_AUSRICHTUNG )->EnableWindow( FALSE );
	}

  CString text[4];
  State* st = NULL;
	if( m_qmap_tree.Lookup( pNMTreeView->itemNew.hItem, st ) )
	{
		m_pState = st;
		
    if( m_bInsert )
		{
			GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG )->EnableWindow( TRUE );
			GetDlgItem( IDC_OFFNEN_EDIT_AUSRICHTUNG )->EnableWindow( TRUE );
			OnCheckAusrichtung();
		}

		text[0].LoadString( IDS_STATION );
		text[1].LoadString( IDS_PK );
		text[2].LoadString( IDS_VZK );
		text[3].LoadString( IDS_ORIGINFILE );
		Section* sec = st->GetFirstCrossSection();
		while (sec!=NULL)
		{
      CString path;
			plotApp->GetDrawingFileName( path, sec );
			int image = IMAGE_CSECTION;
      CFileStatus rStatus;
			if (m_pCurrentSection!=NULL)
			{ 
				if (m_pCurrentSection==sec ||
					m_pCurrentSection->GetClassType()!=sec->GetClassType())
					image = IMAGE_SECTION_DISABLED;
			}
      else if( CFile::GetStatus( path, rStatus ) )
				image = IMAGE_CSECTION_SAVED;

			if( !m_bDelete || image == IMAGE_CSECTION_SAVED )
			{
        CString str;
				str.Format("%.4f", ((CrossSection*)sec)->GetStation());
				lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
				lvItem.iSubItem = 0;
				lvItem.pszText = str.GetBuffer(str.GetLength());
				str.ReleaseBuffer();
				lvItem.cchTextMax = str.GetLength();
				lvItem.iImage = image;
				m_list.InsertItem(&lvItem);
				m_secmap_list.SetAt((WORD)lvItem.lParam, sec);
				str = ((CrossSection*)sec)->GetPK();
				m_list.SetItemText(lvItem.iItem, 1, str);
				str.Format("%d", ((CrossSection*)sec)->GetVZK());
				m_list.SetItemText(lvItem.iItem, 2, str);
				CString fileTitle = sec->GetFileTitle();
				CString fileExt = sec->GetFileExt();
				str = fileTitle + '.' + fileExt;
				m_list.SetItemText(lvItem.iItem, 3, str);
			}
			sec = st->GetNextCrossSection();
		}
	}
	if( m_lmap_tree.Lookup( pNMTreeView->itemNew.hItem, st ) )
	{
		m_pState = st;

		text[0].LoadString(IDS_NAME);
		text[1].LoadString(IDS_BEGIN);
		text[2].LoadString(IDS_END);
		text[3].LoadString(IDS_ORIGINFILE);
		Calculation* calc = st->GetFirstCalculation();
		while( calc )
		{
			Section* sec = calc->GetLengthSection();
			if( sec )
			{
				CString fileTitle = sec->GetFileTitle();
				if (!fileTitle.IsEmpty())
				{
          CString path;
					plotApp->GetDrawingFileName( path, sec );
					int image = IMAGE_LSECTION;
          CFileStatus rStatus;
					if( m_pCurrentSection )
					{ 
						if( m_pCurrentSection == sec ||
							m_pCurrentSection->GetClassType()!=sec->GetClassType())
							image = IMAGE_SECTION_DISABLED;
					}
          else if( CFile::GetStatus( path, rStatus ) )
						image = IMAGE_LSECTION_SAVED;

					if( !m_bDelete || image == IMAGE_LSECTION_SAVED )
					{
						CString str = ((LengthSection*)sec)->GetName();
						lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
						lvItem.iSubItem = 0;
						lvItem.pszText = str.GetBuffer(str.GetLength());
						str.ReleaseBuffer();
						lvItem.cchTextMax = str.GetLength();
						lvItem.iImage = image;
						m_list.InsertItem(&lvItem);
						m_secmap_list.SetAt((WORD)lvItem.lParam, sec);
						str.Format("%.4f", ((LengthSection*)sec)->GetStartStation());
						m_list.SetItemText(lvItem.iItem, 1, str);
						str.Format("%.4f", ((LengthSection*)sec)->GetEndStation());
						m_list.SetItemText(lvItem.iItem, 2, str);
						CString fileTitle = sec->GetFileTitle();
						CString fileExt = sec->GetFileExt();
						str = fileTitle + '.' + fileExt;
						m_list.SetItemText(lvItem.iItem, 3, str);
					}
				}
			}
			calc = st->GetNextCalculation();
		}
	}
	for( int i = 0; i < 4; i++ )
	{
    LV_COLUMN lvColumn;
		lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.pszText = text[i].GetBuffer(text[i].GetLength());
		text[i].ReleaseBuffer();
		lvColumn.iSubItem = i;
		lvColumn.cx = m_list.GetColumnWidth(i);
		
    VERIFY( m_list.SetColumn( i, &lvColumn ) );
	}
	*pResult = 0;
}

void COpenDialog::OnDblclkList1(NMHDR* /*pNMHDR*/, LRESULT* pResult) 
{
	OnOK();
	*pResult = 0;
}

void COpenDialog::OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	m_list.SortItems(ListCompareFunc, pNMListView->iSubItem);
	
	*pResult = 0;
}


void COpenDialog::OnCheckAusrichtung() 
{
	if( m_bInsert )
	{
		BOOL bEnable = !(BOOL)( (CButton*)GetDlgItem( IDC_OFFNEN_CHECK_AUSRICHTUNG ) )->GetCheck();
		GetDlgItem( IDC_OFFNEN_EDIT_AUSRICHTUNG )->EnableWindow( bEnable );
	}
}
