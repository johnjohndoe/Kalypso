// solpage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotdoc.h"
#include "template.h"
#include "plotview.h"
#include "propdlg.h"
#include "plotter.h"
#include "profil.h"

#include "solpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CSolidPage 

CSolidPage::CSolidPage(CPropertyDialog *pParent, CPlotterDoc* pDoc /*=NULL*/ ) : CPropertyPage(CSolidPage::IDD)
{
	//{{AFX_DATA_INIT(CSolidPage)
	m_filled = FALSE;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pDoc = pDoc;
  if( pDoc != NULL )
    m_pView = (CPlotterView*)pDoc->GetView();
  else
    m_pView = NULL;
	m_pTemp = NULL;
}

CSolidPage::~CSolidPage()
{
	POSITION pos;
	LPLOGBRUSH lpLogBrush;
	HANDLE hTI;
	
	pos = m_logbrushes.GetStartPosition();
	while (pos!=NULL)
	{
		m_logbrushes.GetNextAssoc(pos, hTI, lpLogBrush);
		delete lpLogBrush;
	}
	m_logbrushes.RemoveAll();
}

void CSolidPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSolidPage)
	DDX_Control(pDX, IDC_COMBO2, m_fpattern);
	DDX_Control(pDX, IDC_TREE1, m_tree);
	DDX_Check(pDX, IDC_CHECK1, m_filled);
	DDX_Control(pDX, IDC_COMBO1, m_fcolor);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CSolidPage, CPropertyPage)
	//{{AFX_MSG_MAP(CSolidPage)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeColor)
	ON_BN_CLICKED(IDC_CHECK1, OnCheck1)
	ON_CBN_SELCHANGE(IDC_COMBO2, OnSelchangePattern)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CSolidPage 

BOOL CSolidPage::OnInitDialog() 
{
	HPALETTE hPal = GETPLOTTERAPP->m_hPal;
	LPLOGBRUSH lpLogBrush;
	int i;

	ASSERT(m_pDoc!=NULL);
	CPropertyPage::OnInitDialog();
	
	m_pParent->m_nActivePages++;
	// initialize controls
	m_fcolor.SetPalette(hPal);
	for (i=0; i<GETPLOTTERAPP->m_logbrushes.GetSize(); i++)
	{
		lpLogBrush = GETPLOTTERAPP->m_logbrushes[i];
		m_fpattern.AddLogBrush(lpLogBrush);
	}
	m_fpattern.Init();
	// initialize tree image list
	m_tree.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	UpdateTree();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CSolidPage::UpdateTree()
{
	HTREEITEM hTI, hTI1;
	HANDLE hTreeItem, hTS;
	DataBlock temp(NULL);
	CDrawObj *pObj, *pTempObj;
	LPLOGBRUSH lpLogBrush;
	BOOL bFill;
	WORD wFill;
	POSITION pos;
	State *st;
	Section *sec;
	CString str, fmt;
	int i, j;
	CMap<int, int, int, int> types;
	CTypedPtrMap<CMapWordToPtr, int, HANDLE> sections;
	CProfil* profil = m_pDoc->GetProfil();

	if (GetSafeHwnd()==NULL)
		return;
	// initialize maps and lists
	pos = m_logbrushes.GetStartPosition();
	while (pos!=NULL)
	{
		m_logbrushes.GetNextAssoc(pos, hTreeItem, lpLogBrush);
		delete lpLogBrush;
	}
	m_logbrushes.RemoveAll();
	m_fills.RemoveAll();
	m_objects.RemoveAll();
	m_updated.RemoveAll();
	m_items.RemoveAll();
	// initialize tree
	m_tree.DeleteAllItems();
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
			if ((pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon) ||
				(pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) && pObj->HasBrush()))
			{
				type = pObj->GetType();
				secIndex = pObj->GetSectionIndex();
				dbIndex = pObj->GetDataBlockIndex();
				if ((!types.Lookup(type, i) 
					|| i!=(1000*secIndex+dbIndex)) && !pObj->IsInvisible() && pObj->HasBrush())
				{	
					types.SetAt(type, 1000*secIndex+dbIndex);
					temp.SetType(type);
					str = temp.GetDesc(0);
					image = GETPLOTTERAPP->GetImageType(type);
					sections.Lookup(secIndex, hTS);
					hTI1 = m_tree.InsertItem(str, image, image, (HTREEITEM)hTS);
					m_items.AddTail(hTI1);
					lpLogBrush = new LOGBRUSH;
					if (m_pTemp!=NULL)
					{
						if (pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)))
							pTempObj = m_pTemp->GetTemplateObj(TMPL_PROFIL_POLYGON, type);
						else
							pTempObj = m_pTemp->GetTemplateObj(TMPL_PROFIL_POLYLINE, type);
						*lpLogBrush = pTempObj->m_logbrush;
					}
					else
						*lpLogBrush = pObj->m_logbrush;
					m_logbrushes.SetAt(hTI1, lpLogBrush);
					bFill = pObj->IsFilled();
					m_fills.SetAt(hTI1, bFill);
					m_objects.SetAt(hTI1, pObj);
					if (m_pTemp!=NULL)
						m_updated.SetAt(hTI1, TRUE);
					else
						m_updated.SetAt(hTI1, FALSE);
					if (!m_logbrushes.Lookup(hTS, lpLogBrush))
					{
						lpLogBrush = new LOGBRUSH;
						*lpLogBrush = pObj->m_logbrush;
						m_logbrushes.SetAt(hTS, lpLogBrush);
						m_fills.SetAt(hTS, bFill);
					}
				}
			}
		}
	}
	// select the first item and update controls accordingly
	hTI = m_tree.GetFirstVisibleItem();
	if (m_logbrushes.Lookup(hTI, lpLogBrush))
	{
		m_tree.SelectItem(hTI);
		m_fcolor.SetColor(lpLogBrush->lbColor);
		m_fpattern.SetLogBrush(lpLogBrush);
		if (m_fills.Lookup(hTI, wFill))
			m_filled = (BOOL)wFill;
	}
	UpdateData(FALSE);
}

void CSolidPage::ApplyTemplate(CTemplate *pTemp)
{
	m_pTemp = pTemp;
	UpdateTree();
	m_pTemp = NULL;
}

BOOL CSolidPage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}

void CSolidPage::OnOK() 
{
	CDrawObj *pObj, *pDrawObj;
	POSITION pos1, pos2;
	HTREEITEM hTI;
	LPLOGBRUSH lpLogBrush;
	WORD wFill;
	BOOL bUpdated;
	CDrawObjList *pObjList;		// for undo
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
					if (profil->FindObject(pObj)!=NULL)
					{
						int secIndex = pObj->GetSectionIndex();
						int dbIndex = pObj->GetDataBlockIndex();
						int type = pObj->GetType();
						pos2 = profil->GetHeadPosition();
						while (pos2!=NULL)
						{
							pDrawObj = profil->GetNextObject(pos2);
							if ((pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pDrawObj)->GetShape()==CDrawPoly::polygon) ||
								(pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) && pDrawObj->HasBrush()))
							{
								if (pDrawObj->GetSectionIndex()==secIndex &&
									pDrawObj->GetDataBlockIndex()==dbIndex &&
									pDrawObj->GetType()==type)
								{
									if (pDrawObj->HasBrush())
										pObjList->AddTailObject( pDrawObj );
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
				if (m_logbrushes.Lookup(hTI, lpLogBrush))
				{
					if (m_fills.Lookup(hTI, wFill))
					{
						if( profil->FindObject( pObj ) != NULL )
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();
							pos2 = profil->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = profil->GetNextObject(pos2);
								if ((pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pDrawObj)->GetShape()==CDrawPoly::polygon) ||
									(pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) && pDrawObj->HasBrush()))
								{
									if (pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex &&
										pDrawObj->GetType()==type)
									{
										if (pDrawObj->HasBrush())
										{
											pDrawObj->m_logbrush = *lpLogBrush;
											pDrawObj->DeletePattern();
											if (wFill)
												pDrawObj->SetFlags(CDrawObj::filled);
											else
												pDrawObj->UnsetFlags(CDrawObj::filled);
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
		}
	}
	if (m_pView!=NULL)
		m_pView->Invalidate();

	CPropertyPage::OnOK();
}

void CSolidPage::UpdateItemAndChildren(HTREEITEM hTI, int format) 
{
	LPLOGBRUSH lpLogBrush, lpLB;
	HTREEITEM hTI1, hTI2;
	BOOL bUpdated;
	WORD wFill;
	COLORREF oldColor;

	if (m_logbrushes.Lookup(hTI, lpLogBrush))
	{
		if (m_fills.Lookup(hTI, wFill))
		{
			if (format & color)
				lpLogBrush->lbColor = m_fcolor.GetColor();
			if (format & pattern)
			{
				oldColor = lpLogBrush->lbColor;
				lpLB = m_fpattern.GetLogBrush();
				*lpLogBrush = *lpLB;
				lpLogBrush->lbColor = oldColor;
			}
			if (format & fill)
			{
				if (m_filled)
					m_fills.SetAt(hTI, TRUE);
				else
					m_fills.SetAt(hTI, FALSE);
				m_fills.Lookup(hTI, wFill);
			}
			if (m_updated.Lookup(hTI, bUpdated))
				m_updated.SetAt(hTI, TRUE);
			if (m_tree.ItemHasChildren(hTI))
			{
				hTI1 = m_tree.GetChildItem(hTI);
				while (hTI1!=NULL)
				{
					UpdateItemAndChildren(hTI1, format);
					hTI2 = m_tree.GetNextSiblingItem(hTI1);
					hTI1 = hTI2;
				}
			}
		}
	}
}

void CSolidPage::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	HTREEITEM hTI;
	LPLOGBRUSH lpLogBrush;
	
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	hTI = m_tree.GetSelectedItem();
	if (m_logbrushes.Lookup(hTI, lpLogBrush))
	{
		m_fcolor.SetColor(lpLogBrush->lbColor);
		m_fpattern.SetLogBrush(lpLogBrush);
	}
	UpdateData(FALSE);
	
	*pResult = 0;
}

void CSolidPage::OnSelchangeColor() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, color);
	SetModified(TRUE);
}

void CSolidPage::OnCheck1() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, fill);
	SetModified(TRUE);
}

void CSolidPage::OnSelchangePattern() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, pattern);
	SetModified(TRUE);
}
