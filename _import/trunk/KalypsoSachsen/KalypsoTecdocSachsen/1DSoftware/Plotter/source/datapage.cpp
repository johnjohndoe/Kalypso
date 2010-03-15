// DataPage.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"
#include "..\..\commonMfc\commonMfc.h"

#include "drawobj.h"
#include "plotview.h"
#include "plotdoc.h"
#include "template.h"
#include "propdlg.h"
#include "plotter.h"
#include "profil.h"

#include "datapage.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define ON_BN_CLICKED_EX(id, memberFxn) \
	{ WM_COMMAND, (WORD)BN_CLICKED, (WORD)id, (WORD)id, AfxSig_bw, \
		(AFX_PMSG)(BOOL (AFX_MSG_CALL CCmdTarget::*)(UINT))&memberFxn },

/////////////////////////////////////////////////////////////////////////////
// CDataPage property page
/////////////////////////////////////////////////////////////////////////////

//////////////////
// Konstruktion //
//////////////////

CDataPage::CDataPage( CPropertyDialog* pParent, CPlotterDoc* pDoc /*=NULL*/ ) : CPropertyPage( CDataPage::IDD )
{
	//{{AFX_DATA_INIT(CDataPage)
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pDoc = pDoc;
	if( pDoc != NULL )
		m_pView = (CPlotterView*)pDoc->GetView();
	else
		m_pView = NULL;
	m_pTemp = NULL;
	m_bUpdated = FALSE;
	m_dragImageList = NULL;
	m_dragItem = NULL;
}

CDataPage::~CDataPage()
{
	if ( m_tree1.GetSafeHwnd() )
		DeleteTree1( m_tree1.GetRootItem() );
};

void CDataPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDataPage)
	DDX_Control(pDX, IDC_TREE2, m_tree2);
	DDX_Control(pDX, IDC_TREE1, m_tree1);
	//}}AFX_DATA_MAP
}

//////////////////////
// Message Handling //
//////////////////////

BEGIN_MESSAGE_MAP(CDataPage, CPropertyPage)
	//{{AFX_MSG_MAP(CDataPage)
	ON_NOTIFY(TVN_BEGINDRAG, IDC_TREE2, OnBegindragTree2)
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONUP()
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED_EX(IDC_BUTTON1, OnChange)
	ON_BN_CLICKED_EX(IDC_BUTTON2, OnChange)
	ON_BN_CLICKED_EX(IDC_BUTTON3, OnChange)
	ON_BN_CLICKED_EX(IDC_BUTTON4, OnChange)
END_MESSAGE_MAP()


BOOL CDataPage::OnInitDialog() 
{
	ASSERT(m_pDoc!=NULL);
	CPropertyPage::OnInitDialog();
	
	m_pParent->m_nActivePages++;
	// initialize image list
	m_tree1.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	m_tree2.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	UpdateTree1();

	return TRUE;
}; // OnInitdialog

BOOL CDataPage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}; // OnApply


void CDataPage::OnOK() 
{
  if ( !UpdateData(TRUE) )
    return;

  CWaitCursor wait;

  if ( m_bUpdated )
  {
    // zuerst die Tabelle sortieren
    SortTable();

    // dann zu ändernden Objekte für Undo holen
    CDrawObjList* pUndoList = new CDrawObjList;
    UpdateDrawing( m_tree1.GetRootItem(), pUndoList );
    if ( m_pView && m_pParent->m_bUndo && pUndoList->GetObjectCount() > 0 )
      m_pView->AddToUndoBuffer( pUndoList, HINT_OBJ_EDIT );
		delete pUndoList;
    pUndoList = NULL; // für alle Fälle

    // und jetzt wirklich die Objekte ändern
    UpdateDrawing( m_tree1.GetRootItem(), NULL );

    m_bUpdated = FALSE;
  }; // if m_bUpdated

	CPropertyPage::OnOK();
}; // OnOk

void CDataPage::OnCancel()
{
	DeleteTree1( m_tree1.GetRootItem() );
  
	CPropertyPage::OnCancel();
}; // OnCancel

BOOL CDataPage::OnChange( UINT nID )
{
	HTREEITEM hTI1 = m_tree1.GetSelectedItem();
	HTREEITEM hTI2 = m_tree2.GetSelectedItem();
	HTREEITEM hTIShow21 = NULL; // ist dieses am Ende != NULL, zeigt der tree2 danach das Item, welches als Daten diese Item hat
	HTREEITEM hTISelect1 = NULL; // dieses Element des 1. Baumes soll selektiert werden 
	HTREEITEM hTISelect2 = NULL; // dieses Element des 2. Baumes soll selektiert werden
	HTREEITEM hTISelect21 = NULL; // das Element des 2. Baumes soll selektiert werden, welche diese element des 1. baumes als Daten hat
  
	switch( nID )
	{
	case IDC_BUTTON1: // AddAll
		hTI1 = m_tree1.GetRootItem();
		hTISelect21 = m_tree1.GetChildItem( hTI1 );
		while ( hTI1 )
		{
			ShowItemAndChildren( hTI1, TRUE );
			hTI1 = m_tree1.GetNextSiblingItem( hTI1 );
		}; // while hTI1
		break;
		
	case IDC_BUTTON2: // Add
		if ( hTI1 )
		{
			hTISelect1 = m_tree1.GetNextSiblingItem( hTI1 );
			if ( !hTISelect1 )
				hTISelect1 = m_tree1.GetNextSiblingItem( m_tree1.GetParentItem( hTI1 ) );
			hTIShow21 = hTI1;
			
			ShowItemAndChildren( hTI1, TRUE );
		}
		else
			return TRUE;
		break;
		
	case IDC_BUTTON3: // Remove
		if ( hTI2 )
		{
			hTISelect2 = m_tree2.GetNextSiblingItem( hTI2 );
			if ( !hTISelect2 )
				hTISelect2 = m_tree2.GetPrevSiblingItem( hTI2 );
			if ( !hTISelect2 )
				hTISelect2 = m_tree2.GetNextSiblingItem( m_tree2.GetParentItem( hTI2 ) );
			
			ShowItemAndChildren( (HTREEITEM)m_tree2.GetItemData(hTI2), FALSE );
		}
		else
			return TRUE;
		break;
		
	case IDC_BUTTON4: // RemoveAll
		hTI1 = m_tree1.GetRootItem();
		if( hTI1 )
		{
			hTISelect1 = m_tree1.GetChildItem( hTI1 );
			ShowItemAndChildren( hTI1, FALSE );
		}; // while hTI1
		break;
		
	default:
		return FALSE;
	}; // switch
	
	SetModified(TRUE);
	m_bUpdated = TRUE;
	
	if ( hTISelect1 )
	{
		m_tree1.SelectItem( hTISelect1 );
		m_tree1.SetFocus();
	};
	
	if ( hTISelect2 )
		hTISelect21 = (HTREEITEM)m_tree2.GetItemData( hTISelect2 );
	
	UpdateTree2();
	
	if ( hTISelect21 )
	{
		HTREEITEM hTI = FindTree2( m_tree2.GetRootItem(), hTISelect21 );
		m_tree2.SelectItem( hTI );
		m_tree2.SetFocus();
	};
	
	if ( hTIShow21 )
	{
		HTREEITEM hTI = FindTree2( m_tree2.GetRootItem(), hTIShow21 );
		if ( hTI )
		{
			m_tree2.EnsureVisible( hTI );
			m_tree2.Expand( hTI, TVE_EXPAND );
		};
	}; // if hTIShow2
	
	m_pParent->m_nFormat = 1;
	m_pParent->m_bUpdatePagesNeeded = TRUE;
	
	return TRUE;
}; // OnChange

/**  Dragging */
void CDataPage::OnBegindragTree2(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
  TVITEM* tvItem = &pNMTreeView->itemNew;
  
  TreeData* tData = (TreeData*)m_tree1.GetItemData( (HTREEITEM)tvItem->lParam );
  if ( tData->position != -1 )
  {
    m_dragItem = tvItem->hItem;
    m_dragImageList = m_tree2.CreateDragImage( tvItem->hItem );
		m_dragImageList->BeginDrag( 0, CPoint( 0,0 ) );
    m_dragImageList->DragEnter( this, pNMTreeView->ptDrag );
		ShowCursor(FALSE);
		SetCapture();
	}; // if position != -1

	*pResult = 0;
}; // OnBeginDragTree2

void CDataPage::OnMouseMove( UINT nFlags, CPoint point ) 
{
	CPoint adjust;
	CIntIRect rectWnd, rectClient;
	
	if ( m_dragItem )
	{
    // DragCursor an die entsprechende Stelle bewegen
    m_dragImageList->DragMove( point );

    // falls ein über einem Item, makrieren
    CPoint pt( point );
		ClientToScreen( &pt );
		m_tree2.ScreenToClient( &pt );

    UINT flag;
		HTREEITEM hTI = m_tree2.HitTest( pt, &flag );
		if ( hTI  && m_tree2.GetParentItem( hTI ) == m_tree2.GetParentItem( m_dragItem ) )
    {
      m_dragImageList->DragLeave( this );
			m_tree2.SelectDropTarget( hTI );
      m_dragImageList->DragEnter( this, point );
    } // if hIT und m_dragItem den gleichen Vater haben
    else
      m_tree2.SelectDropTarget( NULL );
	}; // if m_dragItem

	CPropertyPage::OnMouseMove( nFlags, point );
}; // OnMouseMove

void CDataPage::OnLButtonUp(UINT nFlags, CPoint point) 
{
	if ( m_dragItem )
	{
		m_dragImageList->DragLeave( this );
		m_dragImageList->EndDrag();
    delete m_dragImageList;
    m_dragImageList = NULL;
		ReleaseCapture();
		ShowCursor( TRUE );

		HTREEITEM hTI = m_tree2.GetDropHilightItem();
		if ( hTI )
		{
			HTREEITEM hParent = m_tree2.GetParentItem( hTI );
			if ( hParent == m_tree2.GetParentItem( m_dragItem ) )
        MoveItem( (HTREEITEM)m_tree2.GetItemData( m_dragItem ), (HTREEITEM)m_tree2.GetItemData( hTI ) );
			else
				MessageBeep(0xFFFFFFFF);

      m_dragItem = NULL;
		}; // if hIT
		m_tree2.SelectDropTarget(NULL);
	}; // if m_dragItem

	CPropertyPage::OnLButtonUp(nFlags, point);
}; // OnLButtonUp



////////////////////
// Implementation //
////////////////////

void CDataPage::UpdateTree1()
{
	CDrawObjListArray* table = m_pDoc->GetTable();
	CDrawObjListArray* tableKey1 = m_pDoc->GetTableKey1();
	CDrawObjListArray* tableKey2 = m_pDoc->GetTableKey2();
	CProfil* profil = m_pDoc->GetProfil();

	if ( !GetSafeHwnd() )
		return;

	// reset tree
	DeleteTree1( m_tree1.GetRootItem() );

	// erstmal rausfinden wie viele Sections es geben muss
	int maxSectionIndex = 0;
	for( int i = 0; i < table->GetSize(); i++ )
	{
		CDrawObjList* drawList = table->GetAt( i );
		if( drawList && drawList->GetObjectCount() > 0 )
		{
			CDrawObj* drawObj = drawList->GetHeadObject();
			if( drawObj )
				maxSectionIndex = max( maxSectionIndex, drawObj->GetSectionIndex() );
		}; // if drawList
	}; // for i

	CTypedPtrArray<CPtrArray, HTREEITEM> sections; // Zuordnung sectionIndex -> TreeItem

	// die Sections erzeugen == Root-Items
	for( i = 0; i < maxSectionIndex + 1; i++ )
	{
		State* state = m_pDoc->GetMState( i );
		Section* section = m_pDoc->GetMSection( i );
		
		// zuerst den Texteintrag für dieses Blatt finden
		CString treeStr;
		if ( section )
		{
			if ( section->GetClassType() == CLASS_TYPE_CSECTION && state )
			{
				CString tempStr;
				tempStr.Format( "%.4lf", ((CrossSection*)section)->GetStation() );
				treeStr.FormatMessage( IDS_STATION_DESC1, state->GetWaterName(), state->GetName(), tempStr );
			}
			else if ( section->GetClassType() == CLASS_TYPE_LSECTION )
				treeStr = ((LengthSection*)section)->GetName();
		}; // if section
		
		if ( treeStr.IsEmpty() )
			treeStr.LoadString( IDS_DATA );
		
		// jetzt im Baum erzeugen
		HTREEITEM hTI = m_tree1.InsertItem( treeStr, IMAGE_CSECTION, IMAGE_CSECTION );
		TreeData* tD = new TreeData( section, TRUE, TreeData::Section, i );
		m_tree1.SetItemData( hTI, DWORD(tD) );
		
		sections.SetAtGrow( i, hTI ); // merken, um später die Unterknoten richtig einzutragen
	}; // for i


	// jetzt die Tabelle und das Profil auslesen, für jeden Tabelleneintrag gibts einen
	// Oberbegriff == Datenblock
	for( i = 0; i < table->GetSize(); i++ )
	{
		// als Unterbegriffe können vorkommen:
		CDrawObj* objTabelle = NULL; // Tabelle: es gibt überhaupt einen Tabelleneintrag
		CDrawObj* objXCoord = NULL; // XCoord:  X-koordinaten
		CDrawObj* objYCoord = NULL; // YCoord:  Y-Koordinaten
		CDrawObj* objNormal = NULL; // Normal:  Zusätzlicher Text in der Tabelle
		CDrawObj* objPolyline = NULL; // Polylinie: z.B. die Darstellung des Datenblock im Profil
		CDrawObj* objPolygon = NULL; // Polygon: die Füllung im Profil
		// zu jedem dieser Begriffe gibts ein Referenzobjekt, diese werden erst mal gesucht
		
		// zuerst die Tabelle durchsuchen
		POSITION pos = table->GetAt( i )->GetHeadPosition();
		while( pos && ( !objTabelle || !objXCoord || !objYCoord || !objNormal ) )
		{
			CDrawObj* pObj = table->GetAt( i )->GetNextObject( pos );
			
			if ( pObj->IsText() )
			{
				switch ( ((CDrawRect*)pObj)->GetTextType() )
				{
				case CDrawRect::xcoord:
					if ( !objXCoord )
						objXCoord = pObj;
					break;
					
				case CDrawRect::ycoord:
					if ( !objYCoord )
						objYCoord = pObj;
					break;
					
				case CDrawRect::normal:
					if ( !objNormal )
						objNormal = pObj;
					break;
				}; // switch ((CDrawRect*)pObj)->GetTextType()
			}
			else
				objTabelle = pObj;
			
		}; // while pos
		
		// wenn kein Tabelleneintrag gefunden wurd, jetzt abbrechen
		if ( !objTabelle )
			break;
		
		// ein paar Abkürzungen
		int type = objTabelle->GetType();
		int sectionIndex = objTabelle->GetSectionIndex();
		int dbIndex = objTabelle->GetDataBlockIndex();
		
		// jetzt das Profil durchsuchen
		pos = profil->GetHeadPosition();
		while( pos && ( !objPolyline || !objPolygon ) )
		{
			CDrawObj* pObj = profil->GetNextObject( pos );
			if ( pObj->GetType()==type && pObj->GetSectionIndex() == sectionIndex &&
				pObj->GetDataBlockIndex() == dbIndex )
			{
				if( !objPolyline && ( !pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) || 
					((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polyline ) )
					objPolyline = pObj;
				else if ( !objPolygon && pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && 
					((CDrawPoly*)pObj)->GetShape() == CDrawPoly::polygon )
					objPolygon = pObj;
			}; // if pObj->type == type && ...
		}; // while pos
		
		// jetzt Baumstruktur erzeugen
		
		// zuerst den Knoten für den Datenblock
		
		// den Namen rausfinden
		DataBlock temp( NULL );
		temp.SetType( type );
		CString treeStr = temp.GetDesc( 0 );
		
		int image = GETPLOTTERAPP->GetImageType( type );
		
		HTREEITEM hTIInsertAfter = NULL; // bei wpl Dateien gibts keine Profile mehr, d.h es kann sein, dass sections leer ist
		if( 0 <= sectionIndex && sectionIndex < sections.GetSize() )
			hTIInsertAfter = sections[sectionIndex];
		// hTIInsertAfter müsste eigentlich hTIParent heissen
		HTREEITEM hTI = m_tree1.InsertItem( treeStr, image, image, hTIInsertAfter );
		// die Daten können erst gesetzt werden, wenn bXcoord etc. initialisiert wurden
		
		// jetzt die einzelnen Graphikelemente
		// erstmal die Sichtbarkeiten rausfinden
		BOOL bLegende, bXCoord, bYCoord, bNormal, bPolyline, bPolygon;
		bLegende = bXCoord = bYCoord = bNormal = bPolyline = bPolygon = FALSE;
		TreeData* treeData;
		CDrawObj* objTemp;
		
		// Legende
		if ( m_pTemp )
			objTemp = m_pTemp->GetTemplateObj( TMPL_TABELLE_LINE, type );
		else
			objTemp = objTabelle;
		bLegende = !objTemp->IsInvisible();
		
		treeData = new TreeData( objTabelle, bLegende, TreeData::Tabel, -1 );
		HTREEITEM hLegende = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_LEGENDE ) ), 
			IMAGE_TEXT, IMAGE_TEXT, hTI );
		m_tree1.SetItemData( hLegende, DWORD(treeData) );
		
		// XCoord
		if ( objXCoord )
		{
			if ( m_pTemp )
				objTemp = m_pTemp->GetTemplateObj( TMPL_TABELLE_XCOORD, type );
			else
				objTemp = objXCoord;
			bXCoord = !objTemp->IsInvisible();
			
			treeData = new TreeData( objXCoord, bXCoord, TreeData::Tabel, -1 );
			HTREEITEM hXcoord = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_XVALUE ) ), IMAGE_TEXT, 
				IMAGE_TEXT, hTI );
			
			m_tree1.SetItemData( hXcoord, DWORD(treeData) );
		}; // if objXCoord
		
		// YCoord
		if ( objYCoord )
		{
			if ( m_pTemp )
				objTemp = m_pTemp->GetTemplateObj( TMPL_TABELLE_YCOORD, type );
			else
				objTemp = objYCoord;
			bYCoord = !objTemp->IsInvisible();
			
			treeData = new TreeData( objYCoord, bYCoord, TreeData::Tabel, -1 );
			HTREEITEM hYcoord = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_YVALUE ) ), IMAGE_TEXT, 
				IMAGE_TEXT, hTI );
			
			m_tree1.SetItemData( hYcoord, DWORD(treeData) );
		}; // if objYCoord
		
		// Normal
		if ( objNormal )
		{
			if ( m_pTemp )
				objTemp = m_pTemp->GetTemplateObj( TMPL_TABELLE_NORMTEXT, type );
			else
				objTemp = objNormal;
			bNormal = !objTemp->IsInvisible();
			
			treeData = new TreeData( objNormal, bNormal, TreeData::Tabel, -1 );
			HTREEITEM hNormal = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_EXTRATEXT ) ), IMAGE_TEXT, 
				IMAGE_TEXT, hTI );
			
			m_tree1.SetItemData( hNormal, DWORD(treeData) );
		}; // if objNormal
		
		// Polyline
		if ( objPolyline )
		{
			if ( m_pTemp )
				objTemp = m_pTemp->GetTemplateObj( TMPL_PROFIL_POLYLINE, type );
			else
				objTemp = objPolyline;
			bPolyline = !objTemp->IsInvisible();
			
			treeData = new TreeData( objPolyline, bPolyline, TreeData::Profile, -1 );
			HTREEITEM hPolyline = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_PROFIL ) ), image, 
				image, hTI );
			
			m_tree1.SetItemData( hPolyline, DWORD(treeData) );
		}; // if objXCoord
		
		// Polygon
		if( objPolygon )
		{
			if ( m_pTemp )
				objTemp = m_pTemp->GetTemplateObj( TMPL_PROFIL_POLYGON, type );
			else
				objTemp = objPolygon;
			bPolygon = !objTemp->IsInvisible();
			
			treeData = new TreeData( objPolygon, bPolygon, TreeData::Profile, -1 );
			HTREEITEM hPolygon = m_tree1.InsertItem( CString( MAKEINTRESOURCE( IDS_FILLING ) ), 
				IMAGE_FILLING, IMAGE_FILLING, hTI );
			
			m_tree1.SetItemData( hPolygon, DWORD(treeData) );
		}; // if objPolygon
		
		
		// zuletzt noch die Daten für den Überbegriff setzen
		treeData = new TreeData( objTabelle, bLegende || bXCoord || bYCoord || bNormal || 
			bPolyline || bPolygon, TreeData::Datablock, i );
		m_tree1.SetItemData( hTI, DWORD(treeData) );
  }; // for i
  
  UpdateTree2();
  
  UpdateData(FALSE);
}; // UpdateTree1

void CDataPage::DeleteTree1( const HTREEITEM hTI )
// löscht (rekursiv) den linken Baum, insbesonderre seine Daten
// Parameter:
//        HTREEITEM hTI: dieses Element, alle seine Schwestern und Kinder wird gelöscht
{
  // hTI und alle seine Schwestern löschen
  HTREEITEM hAktuell = hTI;
  while( hAktuell )
  {
    // erstmal evtl. Kinder löschen
    HTREEITEM hChild = m_tree1.GetChildItem( hAktuell );
    if ( hChild )
      DeleteTree1( hChild );

    // jetzt die Daten des Elements löschen
    delete (TreeData*)m_tree1.GetItemData( hAktuell );
    m_tree1.SetItemData( hAktuell, NULL ); // man kann nie wissen

    // die nächste Schwester holen
    HTREEITEM hOld = hAktuell;
    hAktuell = m_tree1.GetNextItem( hAktuell, TVGN_NEXT );

    // das Element selbst löschen
    m_tree1.DeleteItem( hOld );
  }; // while hAktuell


}; // DeleteTree1

void CDataPage::UpdateTree2()
{
  // erstmal alles löschen
  m_tree2.DeleteAllItems();

  // jetzt den Baum neu füllen
  FillTree2( m_tree1.GetRootItem(), TVI_ROOT );

  // zuletzt noch sortieren
  HTREEITEM hChild = m_tree2.GetRootItem();
  while ( hChild )
  {
    TVSORTCB sortCB;
    sortCB.hParent = hChild;
    sortCB.lParam = (LPARAM)&m_tree1;
    sortCB.lpfnCompare = this->Tree2CompareFunc;
    m_tree2.SortChildrenCB( &sortCB );

    

    hChild = m_tree2.GetNextSiblingItem( hChild );
  }; // while hChild
}; // UpdateTree2

void CDataPage::FillTree2( HTREEITEM hTI1, HTREEITEM hTI2 )
// füllt (rekursiv) den Baum 2 mit den sichtbaren Blättern aus Baum 1
// Parameter:
//        HTREEITEM hTI1: ab diesem Item wird alles kopiert ( alle Kinder + alle Schwestern )
//        HTREEITEM hTI2: an dieses Item wird alles angehängt
{
  // dieses Element und alle seine Schwestern kopieren
  HTREEITEM hTIAktuell = hTI1;
  while( hTIAktuell )
  {
    TreeData* tD = (TreeData*)m_tree1.GetItemData( hTIAktuell );
    CString treeStr = m_tree1.GetItemText( hTIAktuell );
    int treeImage, treeSelImage;
    m_tree1.GetItemImage( hTIAktuell, treeImage, treeSelImage );

    if( tD->bVisible )
    {
      HTREEITEM hTINeu2 = m_tree2.InsertItem( treeStr, treeImage, treeSelImage, hTI2 );
      m_tree2.SetItemData( hTINeu2, DWORD(hTIAktuell) ); // der Verweis auf das entsprechende Element im Baum 1

      // jetzt evtl. Kinder hinzufügen
      HTREEITEM hTChild = m_tree1.GetChildItem( hTIAktuell );
      if ( hTChild )
        FillTree2( hTChild, hTINeu2 );
    }; // if bVisible

    // jetzt die nächste Schwester holen
    hTIAktuell = m_tree1.GetNextItem( hTIAktuell, TVGN_NEXT );
  }; // while hTIAktuell
}; // FillTree2

HTREEITEM CDataPage::FindTree2( const HTREEITEM hTI2, const HTREEITEM hTI1 ) const
// findet ( rekturiv ), dasjenige Item aus Tree2, welches als Daten hTI1 hat
// Parameter:
//        HTREEITEM hTI2: dieses Blatt und seine Kinder aus Baum2 werden durchsucht
//        HTREEITEM hTI1: ein Item aus Baum1, welches das zu suchende Element aus Baum 2 haben soll
// Rückgabewert:
//        HTREITEM: NULL, falls kein Blatt aus Baum 2 gefunden wurde, sonst das Element
{
  HTREEITEM hItem = hTI2;
  while ( hItem )
  {
    HTREEITEM hTIData = (HTREEITEM)m_tree2.GetItemData( hItem );
    if (  hTIData == hTI1 )
      return hItem;

    // jetzt die Kinder durchsuchen
    HTREEITEM hChild = m_tree2.GetChildItem( hItem );
    HTREEITEM hReturn = FindTree2( hChild, hTI1 );
    if ( hReturn )
      return hReturn;

    hItem = m_tree2.GetNextSiblingItem( hItem );
  }; // while hItem

  return NULL;
}; // FindTree2

int CALLBACK CDataPage::Tree2CompareFunc( LPARAM param1, LPARAM param2, LPARAM paramSort )
// sortiert Baumelemente
// Parameter:
//        LPARAM param1, param2: zwei Baumelemente, deren Daten vom Typ TreeData sind
//        LPARAM paramSort: der Baum, zu welchem param1 und param2 gehören
{
  CTreeCtrl* tree1 = (CTreeCtrl*)paramSort;
  TreeData* tD1 = (TreeData*)tree1->GetItemData( (HTREEITEM)param1 );
  TreeData* tD2 = (TreeData*)tree1->GetItemData( (HTREEITEM)param2 );

  return (int)tD1->position - (int)tD2->position;
}; // Tree2CompareFunc

void CDataPage::OnDestroy() 
{
	CPropertyPage::OnDestroy();
	
	DeleteTree1( m_tree1.GetRootItem() );
	
}; // OnDestroy

void CDataPage::ApplyTemplate( CTemplate *pTemp )
{
	m_pTemp = pTemp;
	UpdateTree1();
	m_bUpdated = TRUE;
	m_pTemp = NULL;
}; // ApplyTemplate

void CDataPage::ShowItemAndChildren( HTREEITEM hTI, BOOL bShow )
// setzt ein Blatt und seine Kinder auf sichtbar
// Parameter:
//        HTREEITEM hTI: ein Item aus Baum 1
{
  TreeData* tData = (TreeData*)m_tree1.GetItemData( hTI );
  tData->bVisible = bShow;

  // jetzt die Kinder behandeln
  HTREEITEM hChild = m_tree1.GetChildItem( hTI );
  while ( hChild )
  {
    ShowItemAndChildren( hChild, bShow );
    hChild = m_tree1.GetNextSiblingItem( hChild );
  }; // while hChild

  
  // falls das Element sichtbar gemacht wurde, auch seine Eltern sichtbar machen
  // falls es unsichtbar gemacht wurde, Eltern unsichtbar machen, wenns das letzte war
  if ( bShow )
  {
    HTREEITEM hParent = m_tree1.GetParentItem( hTI );
    while( hParent )
    {
      tData = (TreeData*)m_tree1.GetItemData( hParent );
      if ( !tData->bVisible )
      {
        tData->bVisible = TRUE;
        hParent = m_tree1.GetParentItem( hParent );
      }
      else
        hParent = NULL; // sonst abbrechen

    }; // if hParent
  } // if bShow
  else
  {
    HTREEITEM hParent = m_tree1.GetParentItem( hTI );
    if ( hParent )
    {
      HTREEITEM hChild = m_tree1.GetChildItem( hParent );
      BOOL  bLast = TRUE;
      while ( hChild )
      {
        tData = (TreeData*)m_tree1.GetItemData( hChild );
        if ( tData->bVisible )
        {
          bLast = FALSE; // nicht alle sind unsichtbar, abrechen
          break;
        };
        hChild = m_tree1.GetNextSiblingItem( hChild );
      }; // while bChild
      if ( bLast ) // falls es das letzte war, auch das Elternelement unsichtbar machen
      {
        tData = (TreeData*)m_tree1.GetItemData( hParent );
        tData->bVisible = FALSE;
      }; // if bLast
    }; // if hParent
  }; // if bShow
}; // ShowItemAndChildren

void CDataPage::MoveItem( HTREEITEM hFrom, HTREEITEM hBefore )
// ändert die Reihenfolge der sortierung
// Parameter:
//        HTREEITEM hFrom, hBefore: hFrom wird vor hBefore geschoben
//            beides sind Elemente von Baum1 mit gleichem Vater

{
  TreeData* tDBefore = (TreeData*)m_tree1.GetItemData( hBefore );
  int posBefore = tDBefore->position;

  TreeData* tDFrom = (TreeData*)m_tree1.GetItemData( hFrom );
  int posFrom = tDFrom->position;

  if ( posBefore > posFrom )
    posFrom = 1000000; // hoffentlich gibts nie mehr als 1 Million Datenblöcke

  HTREEITEM hParent = m_tree1.GetParentItem( hFrom );
  HTREEITEM hChild = m_tree1.GetChildItem( hParent );
  while( hChild )
  {
    TreeData* tData = (TreeData*)m_tree1.GetItemData( hChild );

    if ( tData->position >= posBefore && tData->position < posFrom )
      tData->position++;

    hChild = m_tree1.GetNextSiblingItem( hChild );
  }; // while hChild

  tDFrom->position = posBefore;

  SetModified( TRUE );
  m_bUpdated = TRUE;
  m_pParent->m_nFormat = 1;
  UpdateTree2();
  HTREEITEM hSelect = FindTree2( m_tree2.GetRootItem(), hFrom );
  m_tree2.SelectItem( hSelect );
}; // MoveItem


/**
  * Sortiert die Tabelle wie die aktuelle Sortierung in Baum 2.
  */
void CDataPage::SortTable()
{
  UpdateTree2(); // im Zweifelsfall nochmal den Baum 2 erzeugen
  HTREEITEM hAktuell = m_tree2.GetRootItem();
  int count = 0;
  CDrawObjListArray* table = m_pDoc->GetTable();
  CDrawObjListArray* tableKey1 = m_pDoc->GetTableKey1();
  CDrawObjListArray* tableKey2 = m_pDoc->GetTableKey2();

  while( hAktuell )
  {
    HTREEITEM hChild = m_tree2.GetChildItem( hAktuell );
    while( hChild )
    {
      // jetzt sind wir in der richtigen Ebene, einfach so, wie die Elemente kommen, in die tabelle einsortieren
      TreeData* tData = (TreeData*)m_tree1.GetItemData( (HTREEITEM)m_tree2.GetItemData( hChild ) );
      CDrawObj* pObj = (CDrawObj*)tData->data;
      int index = table->FindObjectIndex( pObj );
      if ( index != -1 )
      { // jetzt diesen Tabelleneintrag nach vorne ziehen, tableKey1 und tablekey2 haben den gleichen index
        CDrawObjList *pObjList = table->GetAt( index );
        table->RemoveIndex( index );
        table->InsertIndex( count, pObjList, TRUE );
        pObjList = tableKey1->GetAt( index );
        tableKey1->RemoveIndex( index );
        tableKey1->InsertIndex( count, pObjList, TRUE );
        pObjList = tableKey2->GetAt( index );
        tableKey2->RemoveIndex( index );
        tableKey2->InsertIndex( count, pObjList, TRUE );
        count++;
      }; // if index != -1

      hChild = m_tree2.GetNextSiblingItem( hChild );
    }; // while hChild

    hAktuell = m_tree2.GetNextSiblingItem( hAktuell );
  }; // while hAktuell
}; // SortTable

void CDataPage::UpdateDrawing( const HTREEITEM hTI, CDrawObjList* pUndoList )
// erneuert die Zeichenobjekte
// Parameter:
//        const HTREEITEM hTI: ein Element von Baum 1, ab hier wird erneuert
// Rückgabewert:
//        CDrawObjList*: falls ungleich NULL, werden hier anstatt die Objekte zu ändern, 
//                        alle Objekte angehängt, welche geändert werden müssen ( für undo )
{
  HTREEITEM hAktuell = hTI;
  while ( hAktuell )
  {
    TreeData* tData = (TreeData*)m_tree1.GetItemData( hAktuell );
    switch ( tData->type )
    {
    case TreeData::Section:
    case TreeData::Datablock:
      break;

    case TreeData::Tabel:
      m_pDoc->GetTable()->SetVisibility( (CDrawObj*)tData->data, tData->bVisible, pUndoList );
      m_pDoc->GetTableKey1()->SetVisibility( (CDrawObj*)tData->data, tData->bVisible, pUndoList );
      m_pDoc->GetTableKey2()->SetVisibility( (CDrawObj*)tData->data, tData->bVisible, pUndoList );
      break;

    case TreeData::Profile:
      m_pDoc->GetProfil()->SetVisibility( (CDrawObj*)tData->data, tData->bVisible, pUndoList );
      break;
    }; // switch
    

    // jetzt nocht evtl. Kinder 
    HTREEITEM hChild = m_tree1.GetChildItem( hAktuell );
    if ( hChild )
      UpdateDrawing( hChild, pUndoList );

    hAktuell = m_tree1.GetNextSiblingItem( hAktuell );
  }; // while hAktuell
}; // UpdateDrawing

