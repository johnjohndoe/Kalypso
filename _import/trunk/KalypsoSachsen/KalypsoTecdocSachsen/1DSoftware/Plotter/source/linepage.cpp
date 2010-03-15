// LinePage.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "drawvw.h"
#include "plotdoc.h"
#include "plotview.h"
#include "template.h"
#include "propdlg.h"
#include "plotter.h"
#include "profil.h"
#include "stempel.h"

#include "linepage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLinePage property page

CLinePage::CLinePage(CPropertyDialog *pParent, CPlotterDoc* pDoc /*=NULL*/ ) : CPropertyPage(CLinePage::IDD)
{
	//{{AFX_DATA_INIT(CLinePage)
	m_lbreite = 0.0;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pDoc = pDoc;
  if( m_pDoc != NULL )
    m_pView = (CPlotterView*)m_pDoc->GetView();
  else
    m_pView = NULL;
	m_pTemp = NULL;
}

CLinePage::~CLinePage()
{
	POSITION pos;
	LPLOGPEN lpLogPen;
	HANDLE hTI;
	
	pos = m_logpens.GetStartPosition();
	while (pos!=NULL)
	{
		m_logpens.GetNextAssoc(pos, hTI, lpLogPen);
		delete lpLogPen;
	}
	m_logpens.RemoveAll();
}

void CLinePage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLinePage)
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_COMBO7, m_lcolor);
	DDX_Control(pDX, IDC_TREE1, m_tree);
	DDX_Control(pDX, IDC_COMBO5, m_ltype);
	DDX_Text(pDX, IDC_EDIT1, m_lbreite);
	DDV_MinMaxDouble(pDX, m_lbreite, 0, 10.0);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CLinePage, CPropertyPage)
	//{{AFX_MSG_MAP(CLinePage)
	ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN1, OnDeltaposSpin1)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_CBN_SELCHANGE(IDC_COMBO5, OnSelchangeType)
	ON_CBN_SELCHANGE(IDC_COMBO7, OnSelchangeColor)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeWidth)
	ON_EN_KILLFOCUS(IDC_EDIT1, OnKillfocusWidth)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLinePage message handlers

BOOL CLinePage::OnInitDialog() 
{
	HPALETTE hPal = GETPLOTTERAPP->m_hPal;

	ASSERT(m_pDoc!=NULL);
	CPropertyPage::OnInitDialog();
	
	m_pParent->m_nActivePages++;
	// initialize controls
	m_ltype.Init();
	m_lcolor.SetPalette(hPal);
	m_spin.SetRange(0, 10*MM_FACTOR);
	// initialize tree image list
	m_tree.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);

  int type = m_ltype.GetCurSel();
  if( type != 0 )
  {
    m_lbreite=0;
    m_spin.EnableWindow(FALSE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
    UpdateData(FALSE);
  }
  else
  {
    m_spin.EnableWindow(TRUE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
  }

	UpdateTree();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CLinePage::UpdateTree()
{
	HTREEITEM hTI, hTI1;
	HANDLE hTreeItem, hTS;
	DataBlock temp(NULL);
	CDrawObj *pObj, *pTempObj;
	LPLOGPEN lpLogPen;
	POSITION pos;
	State *st;
	Section *sec;
	CString str, fmt;
	int i, j;
	CMap<int, int, int, int> types;
	CTypedPtrMap<CMapWordToPtr, int, HANDLE> sections;
	CDrawObjListArray* table = m_pDoc->GetTable();
	CDrawObjList* stempel = m_pDoc->GetStempel();
	CDrawObjList *rahmen = m_pDoc->GetRahmen();
	CProfil* profil = m_pDoc->GetProfil();

	if (GetSafeHwnd()==NULL)
		return;
	// initialize maps and lists
	pos = m_logpens.GetStartPosition();
	while (pos!=NULL)
	{
		m_logpens.GetNextAssoc(pos, hTreeItem, lpLogPen);
		delete lpLogPen;
	}
	m_logpens.RemoveAll();
	m_objects.RemoveAll();
	m_updated.RemoveAll();
	m_items.RemoveAll();
	// initialize tree
	m_tree.DeleteAllItems();
	// add Tabelle
	if (table->GetSize()>0)
	{
		str.LoadString(IDS_TABLE);
		hTI = m_tree.InsertItem(str, IMAGE_TABELLE, IMAGE_TABELLE);
		m_items.AddTail(hTI);
		for (i=0; i<table->GetSize(); i++)
		{
			pos = table->GetAt(i)->GetHeadPosition();
			while (pos!=NULL)
			{
				pObj = table->GetAt(i)->GetNextObject(pos);
				if (!pObj->IsInvisible() && pObj->HasPen())
					break;
			}
		}
		lpLogPen = new LOGPEN;
		if (m_pTemp!=NULL)
		{
			for (i=0; i<m_pTemp->GetTable()->GetSize(); i++)
			{
				pos = m_pTemp->GetTable()->GetAt(i)->GetHeadPosition();
				while (pos!=NULL)
				{
					pTempObj = m_pTemp->GetTable()->GetAt(i)->GetNextObject(pos);
					if (!pTempObj->IsInvisible() && pTempObj->HasPen())
						break;
				}
			}
			*lpLogPen = pTempObj->m_logpen;
		}
		else
			*lpLogPen = pObj->m_logpen;
		m_logpens.SetAt(hTI, lpLogPen);
		m_objects.SetAt(hTI, pObj);
		if (m_pTemp!=NULL)
			m_updated.SetAt(hTI, TRUE);
		else
			m_updated.SetAt(hTI, FALSE);
	}
	// add Stempel
	if (stempel->GetObjectCount()>0)
	{
		str.LoadString(IDS_STAMP);
		hTI = m_tree.InsertItem(str, IMAGE_STEMPEL, IMAGE_STEMPEL);
		m_items.AddTail(hTI);
		pos = stempel->GetHeadPosition();
		while (pos!=NULL)
		{
			pObj = stempel->GetNextObject(pos);
			if (!pObj->IsInvisible() && pObj->HasPen())
				break;
		}
		lpLogPen = new LOGPEN;
		if (m_pTemp!=NULL)
		{
			pos = m_pTemp->GetStempel()->GetHeadPosition();
			while (pos!=NULL)
			{
				pTempObj = m_pTemp->GetStempel()->GetNextObject(pos);
				if (!pTempObj->IsInvisible() && pTempObj->HasPen())
					break;
			}
			*lpLogPen = pTempObj->m_logpen;
		}
		else
			*lpLogPen = pObj->m_logpen;
		m_logpens.SetAt(hTI, lpLogPen);
		m_objects.SetAt(hTI, pObj);
		if (m_pTemp!=NULL)
			m_updated.SetAt(hTI, TRUE);
		else
			m_updated.SetAt(hTI, FALSE);
	}
	// add Rahmen
	if (rahmen->GetObjectCount()>0)
	{
		str.LoadString(IDS_FRAME);
		hTI = m_tree.InsertItem(str, IMAGE_RAHMEN, IMAGE_RAHMEN);
		m_items.AddTail(hTI);
		pos = rahmen->GetHeadPosition();
		while (pos!=NULL)
		{
			pObj = rahmen->GetNextObject(pos);
			if (!pObj->IsInvisible() && pObj->HasPen())
				break;
		}
		lpLogPen = new LOGPEN;
		if (m_pTemp!=NULL)
		{
			pos = m_pTemp->GetRahmen()->GetHeadPosition();
			while (pos!=NULL)
			{
				pTempObj = m_pTemp->GetRahmen()->GetNextObject(pos);
				if (!pTempObj->IsInvisible() && pTempObj->HasPen())
					break;
			}
			*lpLogPen = pTempObj->m_logpen;
		}
		else
			*lpLogPen = pObj->m_logpen;
		m_logpens.SetAt(hTI, lpLogPen);
		m_objects.SetAt(hTI, pObj);
		if (m_pTemp!=NULL)
			m_updated.SetAt(hTI, TRUE);
		else
			m_updated.SetAt(hTI, FALSE);
	}
	// Add Sections
	if (profil->GetObjectCount()>0)
	{
		str.LoadString(IDS_DATA);
		for (j=0; j<m_pDoc->GetSections()->GetSize(); j++)
		{
			st = m_pDoc->GetMState( j );
			sec = m_pDoc->GetMSection( j );
			if (sec!=NULL)
			{
				if (sec->GetClassType()==CLASS_TYPE_CSECTION)
				{
					if (st!=NULL)
					{
						CString wname;
						
						wname = st->GetWaterName();
						str = st->GetName();
						fmt.Format("%.4f", ((CrossSection*)sec)->GetStation());
						str.FormatMessage(IDS_STATION_DESC1, wname, str, fmt);
					}
				}
				else
				{
					str = ((LengthSection*)sec)->GetName();
					if (str.IsEmpty())
						str.LoadString(IDS_DATA);
				}
			}
			hTI = m_tree.InsertItem(str, IMAGE_CSECTION, IMAGE_CSECTION);
			m_items.AddTail(hTI);
			sections.SetAt(j, hTI);
		}
		// Add Datentypen
		pos = profil->GetHeadPosition();
		while (pos!=NULL)
		{
			int type, image, secIndex, dbIndex;
				
			pObj = profil->GetNextObject(pos);
			if (pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon)
				continue;
			type = pObj->GetType();
			secIndex = pObj->GetSectionIndex();
			dbIndex = pObj->GetDataBlockIndex();
			if ((!types.Lookup(type, i) 
				|| i!=(1000*secIndex+dbIndex)) && !pObj->IsInvisible() && pObj->HasPen())
			{	
				types.SetAt(type, 1000*secIndex+dbIndex);
				temp.SetType(type);
				str = temp.GetDesc(0);
				image = GETPLOTTERAPP->GetImageType(type);
				sections.Lookup(secIndex, hTS);
				hTI1 = m_tree.InsertItem(str, image, image, (HTREEITEM)hTS);
				m_items.AddTail(hTI1);
				lpLogPen = new LOGPEN;
				if (m_pTemp!=NULL)
				{
					pTempObj = m_pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type);
					*lpLogPen = pTempObj->m_logpen;
				}
				else
					*lpLogPen = pObj->m_logpen;
				m_logpens.SetAt(hTI1, lpLogPen);
				m_objects.SetAt(hTI1, pObj);
				if (m_pTemp!=NULL)
					m_updated.SetAt(hTI1, TRUE);
				else
					m_updated.SetAt(hTI1, FALSE);
				if (!m_logpens.Lookup(hTS, lpLogPen))
				{
					lpLogPen = new LOGPEN;
					*lpLogPen = pObj->m_logpen;
					m_logpens.SetAt(hTS, lpLogPen);
				}
			}
		}
	}
	// select the first item and update controls accordingly
	hTI = m_tree.GetFirstVisibleItem();
	if (m_logpens.Lookup(hTI, lpLogPen))
	{
		m_tree.SelectItem(hTI);
		m_spin.SetPos(lpLogPen->lopnWidth.x);
		m_lbreite = lpLogPen->lopnWidth.x;
		m_lbreite /= MM_FACTOR;
		m_lcolor.SetColor(lpLogPen->lopnColor);
		m_ltype.SetCurSel(lpLogPen->lopnStyle);
	}
	UpdateData(FALSE);
}

void CLinePage::ApplyTemplate(CTemplate *pTemp)
{
	m_pTemp = pTemp;
	UpdateTree();
	m_pTemp = NULL;
}

void CLinePage::OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	double newWidth;

	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newWidth = pNMUpDown->iDelta;
	newWidth /= MM_FACTOR;
	newWidth += m_lbreite;
	if (newWidth>=0 && newWidth<=10)
		m_lbreite = newWidth;
	UpdateData(FALSE);
	OnChangeWidth();
	
	*pResult = 0;
}

void CLinePage::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	HTREEITEM hTI;
	LPLOGPEN lpLogPen;
	
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	hTI = m_tree.GetSelectedItem();
	if (m_logpens.Lookup(hTI, lpLogPen))
	{
		m_spin.SetPos(lpLogPen->lopnWidth.x);
		m_lbreite = lpLogPen->lopnWidth.x;
		m_lbreite /= MM_FACTOR;
		m_lcolor.SetColor(lpLogPen->lopnColor);
		m_ltype.SetCurSel(lpLogPen->lopnStyle);
	}
  int type = m_ltype.GetCurSel();
  if(type!=0)
  {
    m_lbreite=0;
    m_spin.EnableWindow(FALSE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
    UpdateData(FALSE);
  }
  else
  {
    m_spin.EnableWindow(TRUE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
  }
  UpdateData(FALSE);
	
	*pResult = 0;
}

void CLinePage::UpdateItemAndChildren( HTREEITEM hTI, int format ) 
{
	LPLOGPEN lpLogPen;
	if( m_logpens.Lookup( hTI, lpLogPen ) )
	{
		if( format & TYPE )
			lpLogPen->lopnStyle = m_ltype.GetCurSel();
		if( format & WIDTH )
		{
			lpLogPen->lopnWidth.x = (int)( m_lbreite * MM_FACTOR );
			m_spin.SetPos( lpLogPen->lopnWidth.x );
		}

		if( format & COLOR )
			lpLogPen->lopnColor = m_lcolor.GetColor();
		
    BOOL bUpdated;
    if( m_updated.Lookup( hTI, bUpdated ) )
			m_updated.SetAt( hTI, TRUE );
		
    if( m_tree.ItemHasChildren( hTI ) )
		{
			HTREEITEM hTI1 = m_tree.GetChildItem( hTI );
			while( hTI1 )
			{
				UpdateItemAndChildren( hTI1, format );
				hTI1 = m_tree.GetNextSiblingItem( hTI1 );
			}
		}
	}
}

BOOL CLinePage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}

void CLinePage::OnOK() 
{
	CDrawObj *pObj, *pDrawObj;
	POSITION pos1, pos2;
	int i;
	HTREEITEM hTI;
	LPLOGPEN lpLogPen;
	BOOL bUpdated;
	CDrawObjList *pObjList;		// for undo
	CDrawObjListArray *tableKey1 = m_pDoc->GetTableKey1();
	CDrawObjListArray *tableKey2 = m_pDoc->GetTableKey2();
	CDrawObjListArray *table = m_pDoc->GetTable();
	CDrawObjList *stempel = m_pDoc->GetStempel();
	CDrawObjList *rahmen = m_pDoc->GetRahmen();
	CDrawObjList *profil = m_pDoc->GetProfil();

	if (!UpdateData(TRUE))
		return;
	if (m_pView!=NULL && m_pParent->m_bUndo)
	{
		// add updated objects to undo buffer
		pObjList = new CDrawObjList;
		pos1 = m_items.GetHeadPosition();
		while (pos1!=NULL)
		{
			hTI = (HTREEITEM)m_items.GetNext(pos1);
			if (m_objects.Lookup(hTI, pObj))
			{
				bUpdated = FALSE;
				m_updated.Lookup(hTI, bUpdated);
				if (bUpdated)
				{
					if (table->FindObjectIndex(pObj)!=-1)
					{
						for (i=0; i<table->GetSize(); i++)
						{
							pos2 = table->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = table->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pObjList->AddTailObject(pDrawObj);
							}
						}
						for (i=0; i<tableKey1->GetSize(); i++)
						{
							pos2 = tableKey1->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = tableKey1->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pObjList->AddTailObject(pDrawObj);
							}
						}
						for (i=0; i<tableKey2->GetSize(); i++)
						{
							pos2 = tableKey2->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = tableKey2->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pObjList->AddTailObject(pDrawObj);
							}
						}
					}
					else if (stempel->FindObject(pObj)!=NULL)
					{
						pos2 = stempel->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = stempel->GetNextObject(pos2);
							pObjList->AddTailObject(pDrawObj);
						}
					}
					else if (rahmen->FindObject(pObj)!=NULL)
					{
						pos2 = rahmen->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = rahmen->GetNextObject(pos2);
							pObjList->AddTailObject(pDrawObj);
						}
					}
					else if (profil->FindObject(pObj)!=NULL)
					{
						int secIndex = pObj->GetSectionIndex();
						int dbIndex = pObj->GetDataBlockIndex();
						int type = pObj->GetType();
						pos2 = profil->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = profil->GetNextObject(pos2);
							if (pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pDrawObj)->GetShape()==CDrawPoly::polygon)
								continue;
							if (pDrawObj->GetSectionIndex()==secIndex &&
								pDrawObj->GetDataBlockIndex()==dbIndex &&
								pDrawObj->GetType()==type)
							{
								if (pDrawObj->HasPen())
								{
									CDrawObjList *pConnections;
									
									pObjList->AddTailObject( pDrawObj );
									pConnections = pDrawObj->GetConnections();
									if (pConnections!=NULL)
										pObjList->AddTailObjects( pConnections );
								}
							}
						}
					}
				}
			}
		}
		if (pObjList->GetObjectCount()>0)
			m_pView->AddToUndoBuffer(pObjList, HINT_OBJ_EDIT);
		delete pObjList;
	}

	pos1 = m_items.GetHeadPosition();
	while (pos1!=NULL)
	{
		hTI = (HTREEITEM)m_items.GetNext(pos1);
		if (m_objects.Lookup(hTI, pObj))
		{
			bUpdated = FALSE;
			m_updated.Lookup(hTI, bUpdated);
			if (bUpdated)
			{
				m_updated.SetAt(hTI, FALSE);
				if (m_logpens.Lookup(hTI, lpLogPen))
				{
					if (table->FindObjectIndex(pObj)!=-1)
					{
						for (i=0; i<table->GetSize(); i++)
						{
							pos2 = table->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = table->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pDrawObj->m_logpen = *lpLogPen;
							}
						}
						for (i=0; i<tableKey1->GetSize(); i++)
						{
							pos2 = tableKey1->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = tableKey1->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pDrawObj->m_logpen = *lpLogPen;
							}
						}
						for (i=0; i<tableKey2->GetSize(); i++)
						{
							pos2 = tableKey2->GetAt(i)->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = tableKey2->GetAt(i)->GetNextObject(pos2);
								if (pDrawObj->HasPen() && !pDrawObj->IsConnected())
									pDrawObj->m_logpen = *lpLogPen;
							}
						}
					}
					else if (stempel->FindObject(pObj)!=NULL)
					{
						pos2 = stempel->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = stempel->GetNextObject(pos2);
							if (pDrawObj->HasPen())
								pDrawObj->m_logpen = *lpLogPen;
						}
					}
					else if (rahmen->FindObject(pObj)!=NULL)
					{
						pos2 = rahmen->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = rahmen->GetNextObject(pos2);
							if (pDrawObj->HasPen())
								pDrawObj->m_logpen = *lpLogPen;
						}
					}
					else if (profil->FindObject(pObj)!=NULL)
					{
						int secIndex = pObj->GetSectionIndex();
						int dbIndex = pObj->GetDataBlockIndex();
						int type = pObj->GetType();
						pos2 = profil->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = profil->GetNextObject(pos2);
							if (pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pDrawObj)->GetShape()==CDrawPoly::polygon)
								continue;
							if (pDrawObj->GetSectionIndex()==secIndex &&
								pDrawObj->GetDataBlockIndex()==dbIndex &&
								pDrawObj->GetType()==type)
							{
								if (pDrawObj->HasPen())
								{
									pDrawObj->m_logpen = *lpLogPen;
									if (m_pView!=NULL)
										pDrawObj->Invalidate();
								}
							}
						}
					}
				}
			}
		}
	}
	if (m_pView!=NULL)
		m_pView->Invalidate();

	CPropertyPage::OnOK();
}

void CLinePage::OnSelchangeType() 
{
  int type = m_ltype.GetCurSel();
  if( type != 0 )
  {
    m_lbreite = 0;
    m_spin.EnableWindow( FALSE );
    GetDlgItem( IDC_EDIT1 )->EnableWindow( FALSE );
    UpdateData( FALSE );
  }
  else
  {
    m_spin.EnableWindow( TRUE );
    GetDlgItem( IDC_EDIT1 )->EnableWindow( TRUE );
  }

  HTREEITEM hTI = m_tree.GetSelectedItem();
  UpdateData();
  UpdateItemAndChildren( hTI, TYPE );
	SetModified( TRUE );
}

void CLinePage::OnChangeWidth() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren( hTI, WIDTH );
	SetModified(TRUE);
}

void CLinePage::OnSelchangeColor() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren( hTI, COLOR );
	SetModified(TRUE);
}

void CLinePage::OnKillfocusWidth() 
{
	UpdateData();
	m_spin.SetPos((int)m_lbreite*MM_FACTOR);
}
