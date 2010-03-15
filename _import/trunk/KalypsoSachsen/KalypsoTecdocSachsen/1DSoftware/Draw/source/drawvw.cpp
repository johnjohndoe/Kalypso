// rawvw.cpp : implementation of the CDrawView class
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "bce\include\wspfeatures.h"

#include "drawdoc.h"
#include "graphdlg.h"
#include "zoomdlg.h"
#include "draw.h"
#include "cntritem.h"
#include "drawtool.h"
#include "mainfrm.h"
#include "summinfo.h"
#include "debugdlg.h"

#include "drawvw.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CLIPFORMAT CDrawView::m_cfObjectDescriptor = NULL;

/////////////////////////////////////////////////////////////////////////////
// CDrawView

IMPLEMENT_DYNCREATE(CDrawView, CScrollView)

BEGIN_MESSAGE_MAP(CDrawView, CScrollView)
  // Menu 'Edit'
  ON_COMMAND_EX( ID_EDIT_SELECT_ALL, OnEdit )
  ON_COMMAND_EX( ID_EDIT_CLEAR, OnEdit )
  ON_COMMAND_EX( ID_EDIT_COPY, OnEdit )
  ON_COMMAND_EX( ID_EDIT_CUT, OnEdit )
  ON_COMMAND_EX( ID_EDIT_PASTE, OnEdit )
  ON_COMMAND_EX( ID_EDIT_UNDO, OnEdit )
  ON_COMMAND_EX( ID_EDIT_ZOOM, OnEdit )
  ON_COMMAND_EX( ID_EDIT_PASTE_SPECIAL, OnEdit )
  ON_COMMAND_EX( ID_EDIT_PROPS, OnEdit )

  ON_UPDATE_COMMAND_UI(ID_EDIT_SELECT_ALL, OnUpdateEdit )
  ON_UPDATE_COMMAND_UI(ID_EDIT_CLEAR, OnUpdateEdit )
  ON_UPDATE_COMMAND_UI(ID_EDIT_COPY, OnUpdateEdit )
  ON_UPDATE_COMMAND_UI(ID_EDIT_CUT, OnUpdateEdit )
  ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, OnUpdateEdit )
  ON_UPDATE_COMMAND_UI(ID_EDIT_UNDO, OnUpdateEdit )

  ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE_SPECIAL, OnUpdateEdit )

	//{{AFX_MSG_MAP(CDrawView)
	ON_COMMAND(ID_OLE_INSERT_NEW, OnInsertObject)
	ON_COMMAND(ID_CANCEL_EDIT_CNTR, OnCancelEditCntr)
	ON_COMMAND(ID_CANCEL_EDIT_SRVR, OnCancelEditSrvr)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONDBLCLK()
	ON_COMMAND(ID_DRAW_SELECT, OnDrawSelect)
	ON_COMMAND(ID_DRAW_RECT, OnDrawRect)
	ON_COMMAND(ID_DRAW_LINE, OnDrawLine)
	ON_COMMAND(ID_DRAW_TEXT, OnDrawText)
	ON_COMMAND(ID_DRAW_ELLIPSE, OnDrawEllipse)
	ON_UPDATE_COMMAND_UI(ID_DRAW_ELLIPSE, OnUpdateDrawEllipse)
	ON_UPDATE_COMMAND_UI(ID_DRAW_LINE, OnUpdateDrawLine)
	ON_UPDATE_COMMAND_UI(ID_DRAW_RECT, OnUpdateDrawRect)
	ON_UPDATE_COMMAND_UI(ID_DRAW_TEXT, OnUpdateDrawText)
	ON_UPDATE_COMMAND_UI(ID_DRAW_SELECT, OnUpdateDrawSelect)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVEBACK, OnUpdateSingleSelect)
	ON_COMMAND(ID_DRAW_POLYLINE, OnDrawPolyline)
	ON_UPDATE_COMMAND_UI(ID_DRAW_POLYLINE, OnUpdateDrawPolyline)
	ON_WM_SIZE()
	ON_WM_ERASEBKGND()
	ON_COMMAND(ID_OBJECT_MOVEBACK, OnObjectMoveBack)
	ON_COMMAND(ID_OBJECT_MOVEFORWARD, OnObjectMoveForward)
	ON_COMMAND(ID_OBJECT_MOVETOBACK, OnObjectMoveToBack)
	ON_COMMAND(ID_OBJECT_MOVETOFRONT, OnObjectMoveToFront)
	ON_WM_SETFOCUS()
	ON_COMMAND(ID_OBJECT_PROPERTIES, OnObjectProperties)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_PROPERTIES, OnUpdateObjectProperties)
	ON_WM_DESTROY()
	ON_WM_CREATE()
	ON_WM_CONTEXTMENU()
	ON_COMMAND(ID_VIEW_GRID, OnViewGrid)
	ON_UPDATE_COMMAND_UI(ID_VIEW_GRID, OnUpdateViewGrid)
	ON_COMMAND(ID_VIEW_SHOWOBJECTS, OnViewShowobjects)
	ON_UPDATE_COMMAND_UI(ID_VIEW_SHOWOBJECTS, OnUpdateViewShowobjects)
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_WM_KEYDOWN()
	ON_WM_CHAR()
	ON_WM_KILLFOCUS()
	ON_WM_KEYUP()
	ON_COMMAND(ID_DRAW_POLYGON, OnDrawPolygon)
	ON_UPDATE_COMMAND_UI(ID_DRAW_POLYGON, OnUpdateDrawPolygon)
	ON_COMMAND(ID_VIEW_ZOOMIN, OnViewZoomin)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ZOOMIN, OnUpdateViewZoomin)
	ON_COMMAND(ID_VIEW_ZOOMOUT, OnViewZoomout)
	ON_UPDATE_COMMAND_UI(ID_VIEW_ZOOMOUT, OnUpdateViewZoomout)
	ON_COMMAND(ID_DRAW_MEASURE, OnDrawMeasure)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVEFORWARD, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVETOBACK, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVETOFRONT, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_DRAW_MEASURE, OnUpdateDrawMeasure)
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND( ID_FILE_PRINT, OnFilePrint )
	ON_COMMAND( ID_FILE_PRINT_DIRECT, OnFilePrint )
	ON_COMMAND( ID_FILE_PRINT_PREVIEW, OnFilePrintPreview )

	// Debug
#ifdef _DEBUG
	ON_COMMAND(ID_OBJECT_DEBUG, OnObjectDebug)
#endif
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDrawView construction/destruction

CDrawView::CDrawView()
{
	m_nZoomFactor = 100;
	m_bGrid = FALSE;
	m_gridColor = RGB(0, 0, 128);
	m_bActive = FALSE;
	m_bStempelText = FALSE;
	m_bEditing = FALSE;
	m_bSelectedText = FALSE;
	m_bCanSelectText = FALSE;
	m_pDrawRect = NULL;
	m_nCaretPos = 0;
// new
	if( m_cfObjectDescriptor == NULL )
		m_cfObjectDescriptor = (CLIPFORMAT)::RegisterClipboardFormat(_T("Object Descriptor") );
	m_prevDropEffect = DROPEFFECT_NONE;
// end new
	SetScrollSizes(MM_TEXT, CSize(0, 0));
#ifdef _DEBUG
	m_bTraceUndo = GETDRAWAPP->m_bTraceUndo;
	m_bShowPosInfo = FALSE;
#endif

  m_pClientDC = NULL;
}

CDrawView::~CDrawView()
{
	EndTextEdit();
	DeleteContents();
	m_CaretBitmap.DeleteObject();	// just in case
}

void CDrawView::DeleteContents()
{
	ASSERT_VALID( this );

	EmptyUndoBuffer();

	ASSERT_VALID( this );

  delete m_pClientDC;
  m_pClientDC = NULL;
}

BOOL CDrawView::PreCreateWindow(CREATESTRUCT& cs)
{
	ASSERT(cs.style & WS_CHILD);
	if (cs.lpszClass == NULL)
		cs.lpszClass = AfxRegisterWndClass(CS_DBLCLKS);
	return TRUE;
}

void CDrawView::OnActivateView(BOOL bActivate, CView* pActiveView,
	CView* pDeactiveView)
{
  CDrawApp* drawApp = GETDRAWAPP;

	CView::OnActivateView(bActivate, pActiveView, pDeactiveView);

	// invalidate selections when active status changes
	if (m_bActive != bActivate)
	{
		if (bActivate)  // if becoming active update as if active
			m_bActive = bActivate;
		if (!m_selection.IsEmpty())
			OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);
		m_bActive = bActivate;
		if (bActivate)
		{
			if (drawApp->m_hWndZoomBox!=NULL)
			{
				::EnableWindow(drawApp->m_hWndZoomBox, TRUE);
				::SendMessage(drawApp->m_hWndZoomBox, CDrawApp::m_nZoomFactorChangedMsg, 0, 0);
			}
		}
		else
		{
			if (drawApp->m_hWndZoomBox!=NULL)
				::EnableWindow(drawApp->m_hWndZoomBox, FALSE);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CDrawView drawing

void CDrawView::InvalObj( CDrawObj* pObj )
{
	CIntIRect intRect( pObj->GetAdjustedRect() );
  CRect rect( intRect );
	DocToClient( rect );
  rect.NormalizeRect();

	if( m_bActive && IsSelected( pObj ) )
	{
		int width = max( (int)( HANDLE_WIDTH * MM_FACTOR ), (int)( pObj->GetLogPen().lopnWidth.x /  2 ) );
    
    rect.left -= width;
		rect.top -= width;
		rect.right += width;
		rect.bottom += width;
	}
	rect.InflateRect( 1 * MM_FACTOR, 1 * MM_FACTOR ); // handles CDrawOleObj objects

	if( pObj->HasArrow() && ( pObj->LeftArrow() || pObj->RightArrow() ) )
	{
		int x = (int)( ( cos( PI / 4 ) * abs( rect.Width() ) + sin( PI / 4 ) * abs( rect.Height() ) ) / FACTOR_ARROW );
		int y = (int)( ( sin( PI / 4 ) * abs( rect.Width() ) + cos( PI / 4 ) * abs( rect.Height() ) ) / FACTOR_ARROW );
		rect.InflateRect( x, y );		// handles arrows
	}

	InvalidateRect( rect, FALSE );
}

void CDrawView::InvalAll()
{
	CDrawDoc *pDoc = GetDocument();
	CWaitCursor wait;

	pDoc->InvalAll();
	Invalidate();
}

void CDrawView::OnUpdate( CView* , LPARAM lHint, CObject* pHint )
{
	switch( lHint )
	{
	case HINT_UPDATE_WINDOW:    // redraw entire window
    ResyncScrollSizes();
		Invalidate( FALSE );
		break;

	case HINT_UPDATE_DRAWOBJ:   // a single object has changed
		InvalObj((CDrawObj*)pHint);
		break;

	case HINT_UPDATE_SELECTION: // an entire selection has changed
		{
			CDrawObjList* pList = pHint != NULL ?
				(CDrawObjList*)pHint : &m_selection;
			POSITION pos = pList->GetHeadPosition();
			while (pos != NULL)
				InvalObj(pList->GetNextObject( pos ));
		}
		break;

	case HINT_DELETE_SELECTION: // an entire selection has been removed
		if( pHint != &m_selection )
		{
			CDrawObjList* pList = (CDrawObjList*)pHint;
			POSITION pos = pList->GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pList->GetNextObject( pos );
				InvalObj(pObj);
				Remove(pObj);   // remove it from this view's selection
			}
		}
		break;

	case HINT_UPDATE_OLE_ITEMS:
		{
			CDrawDoc* pDoc = GetDocument();
			POSITION pos = pDoc->GetObjects()->GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pDoc->GetObjects()->GetNextObject( pos );
				if (pObj->IsKindOf(RUNTIME_CLASS(CDrawOleObj)))
					InvalObj(pObj);
			}
		}
		break;

	default:
		ASSERT(FALSE);
		break;
	}
}

void CDrawView::OnPrepareDC( CDC* pDC, CPrintInfo* pInfo )
{
	CScrollView::OnPrepareDC( pDC, pInfo );
	
	CDrawDoc* pDoc = GetDocument();
  if( !pDoc )
    return;

  CSize pageSize = pDoc->GetPageSize();
  CSize drawSize = pDoc->GetDrawingSize();
	
	// mapping mode is MM_ANISOTROPIC
	// these extents setup a mode similar to MM_LOMETRIC
	// MM_LOMETRIC is in .1 physical millimetres
	// these extents provide .1 logical millimetres
	CSize sizeNum( 1, 1 );
	CSize sizeDenom( 1, 1 );
  //pDoc->GetZoomFactor(&sizeNum, &sizeDenom);

  pDC->SetMapMode( MM_ANISOTROPIC );
    
  double hppmm = (double)( pDC->GetDeviceCaps( HORZRES ) ) / (double)( pDC->GetDeviceCaps( HORZSIZE ) );
  double vppmm = (double)( pDC->GetDeviceCaps( VERTRES ) ) / (double)( pDC->GetDeviceCaps( VERTSIZE ) );
  CSize sizeDoc = pDoc->GetDrawingSize();
  sizeDoc.cy = -sizeDoc.cy;
  pDC->SetWindowExt( sizeDoc );

	long xExt = (long)(sizeDoc.cx * hppmm * sizeNum.cx);
	xExt /= MM_FACTOR * (long)sizeDenom.cx;
	long yExt = (long)(sizeDoc.cy * vppmm * sizeNum.cy);
	yExt /= MM_FACTOR * (long)sizeDenom.cy;
	pDC->SetViewportExt((int)xExt, (int)-yExt);
	
	if( !pDC->IsPrinting() )
		pDC->ScaleViewportExt( m_nZoomFactor, 100, m_nZoomFactor, 100 );

	// set the origin of the coordinate system to the bottom left corner of the page
	CPoint ptOrg;
	if( !pDC->IsPrinting() )
	{	// when not printing the page is the whole drawing
		ptOrg.x = 0;
		ptOrg.y = drawSize.cy;
	}
	else
	{	// when printing we must set the origin to the top left of the current page
    UINT curPage = pInfo->m_nCurPage - 1;
    UINT curPageX = curPage % pDoc->GetPages().cx;
    UINT curPageY = div( curPage, pDoc->GetPages().cx ).quot;
    ptOrg.x = curPageX * pageSize.cx - pDoc->GetPageMargins().left;
    ptOrg.y = drawSize.cy - curPageY * pageSize.cy + pDoc->GetPageMargins().top;
	}

	// IMPORTANT: We must reset the coordinate origin before we make an offset!
	// This is necessary for printing since the CView printing routines make repeated
	// calls to OnPrepareDC.
	pDC->SetWindowOrg( 0, 0 );
	// ptOrg is in logical coordinates
	pDC->OffsetWindowOrg( ptOrg.x, ptOrg.y );
}; // OnPrepareDC

BOOL CDrawView::OnScrollBy(CSize sizeScroll, BOOL bDoScroll)
{
	// do the scroll
	if (!CScrollView::OnScrollBy(sizeScroll, bDoScroll))
		return FALSE;

	// update the position of any in-place active item
	if (bDoScroll)
	{
		UpdateActiveItem();
		UpdateWindow();
		Invalidate();
	}
	return TRUE;
}

void CDrawView::OnDraw( CDC* pDC )
{
	CDrawDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	CDC dc;
	CDC* pDrawDC = pDC;
	CBitmap bitmap;
	CBitmap* pOldBitmap;

	// only paint the rect that needs repainting
	CRect client;
	pDC->GetClipBox( client );
	CRect rect = client;
	DocToClient( rect );

	if (!pDC->IsPrinting())
	{
		// draw to offscreen bitmap for fast looking repaints
		if (dc.CreateCompatibleDC(pDC))
		{
			if (bitmap.CreateCompatibleBitmap(pDC, rect.Width(), rect.Height()))
			{
				OnPrepareDC(&dc, NULL);
				pDrawDC = &dc;

				// offset origin more because bitmap is just piece of the whole drawing
				dc.OffsetViewportOrg(-rect.left, -rect.top);
				pOldBitmap = dc.SelectObject(&bitmap);
				dc.SetBrushOrg(rect.left % 8, rect.top % 8);

				// might as well clip to the same rectangle
				dc.IntersectClipRect(client);
			}
		}
	}

	// paint background
	CBrush brush;
	if (!brush.CreateSolidBrush(pDoc->GetPaperColor()))
		return;

	brush.UnrealizeObject();
	pDrawDC->FillRect( client, &brush );

	if( !pDC->IsPrinting() )
  {
    // wenn das Grid an ist, es jetzt zeichnen
    if( m_bGrid )
      DrawGrid( pDrawDC, pDoc->GetGridSize() );

    // in jedem Fall die Seiten als Grid zeichnen
    DrawGrid( pDrawDC, pDoc->GetPageSize() );
  };

	pDoc->Draw( pDrawDC, this );

	if( pDrawDC != pDC )
	{
		pDC->SetViewportOrg(0, 0);
		pDC->SetWindowOrg(0,0);
		pDC->SetMapMode(MM_TEXT);
		dc.SetViewportOrg(0, 0);
		dc.SetWindowOrg(0,0);
		dc.SetMapMode(MM_TEXT);
		pDC->BitBlt(rect.left, rect.top, rect.Width(), rect.Height(),
			&dc, 0, 0, SRCCOPY);
		dc.SelectObject(pOldBitmap);
	}
}

void CDrawView::DrawGrid( CDC* pDC, const CSize& gridSize )
{
	CDrawDoc* pDoc = GetDocument();

	COLORREF oldBkColor = pDC->SetBkColor( pDoc->GetPaperColor() );

	CIntIRect rect( 0, 0, pDoc->GetDrawingSize().cx, pDoc->GetDrawingSize().cy );

	// Center lines
	CPen penDash;
	penDash.CreatePen( PS_DASH, 0, m_gridColor );
	CPen* pOldPen = pDC->SelectObject( &penDash );

	pDC->MoveTo( 0, rect.top );
	pDC->LineTo( 0, rect.bottom );
	pDC->MoveTo( rect.left, 0 );
	pDC->LineTo( rect.right, 0 );

	// Major unit lines
	CPen penDot;
	penDot.CreatePen(PS_DOT, 0, m_gridColor);
	pDC->SelectObject(&penDot);

	for( int x = rect.left / MM_FACTOR * MM_FACTOR; x < rect.right; x += gridSize.cx )
	{
		if( x != 0 )
		{
			pDC->MoveTo( x, rect.top );
			pDC->LineTo( x, rect.bottom );
		}
	}

	for( int y = rect.top / MM_FACTOR * MM_FACTOR; y < rect.bottom; y += gridSize.cy )
	{
		if( y != 0 )
		{
			pDC->MoveTo(rect.left, y);
			pDC->LineTo(rect.right, y);
		}
	}

	// Outlines
	CPen penSolid;
	penSolid.CreatePen(PS_SOLID, 1, m_gridColor);
	pDC->SelectObject(&penSolid);
	pDC->MoveTo(rect.left, rect.top);
	pDC->LineTo(rect.right, rect.top);
	pDC->LineTo(rect.right, rect.bottom);
	pDC->LineTo(rect.left, rect.bottom);
	pDC->LineTo(rect.left, rect.top);

	pDC->SelectObject(pOldPen);
	pDC->SetBkColor(oldBkColor);
} // DrawGrid


void CDrawView::Remove(CDrawObj* pObj)
{
	POSITION pos = m_selection.FindObject( pObj );
	if (pos != NULL)
		m_selection.RemoveObjectAt( pos );
}

void CDrawView::PasteNative(COleDataObject& dataObject)
{
	// get file refering to clipboard data
	CFile* pFile = dataObject.GetFileData( GetDocument()->m_cfPrivate );
	if( pFile == NULL )
		return;

	// connect the file to the archive
	CArchive ar( pFile, CArchive::load );
	TRY
	{
		ar.m_pDocument = GetDocument(); // set back-pointer in archive

		// read the selection
		m_selection.Serialize( ar );
	}
	CATCH_ALL(e)
	{
		ar.Close();
		delete pFile;
		THROW_LAST();
	}
	END_CATCH_ALL

	ar.Close();
	delete pFile;
}

void CDrawView::PasteEmbedded( COleDataObject& dataObject, CPoint point )
{
	BeginWaitCursor();

	// paste embedded
	CDrawOleObj* pObj = new CDrawOleObj( GetInitialPosition(), GetDocument() );
	ASSERT_VALID(pObj);
	CDrawItem* pItem = new CDrawItem( GetDocument(), pObj );
	ASSERT_VALID(pItem);
	pObj->m_pClientItem = pItem;

	TRY
	{
		if (!pItem->CreateFromData(&dataObject) &&
			!pItem->CreateStaticFromData(&dataObject))
		{
			AfxThrowMemoryException();      // any exception will do
		}

		// add the object to the document
		GetDocument()->AddObject( pObj, 0, 0 );
		m_selection.AddTailObject( pObj );
		ClientToDoc( point );
		pObj->MoveTo( CIntIRect( point.x, point.y, point.x + pObj->m_extent.cx, point.y + pObj->m_extent.cy ), this );

		// try to get initial presentation data
		pItem->UpdateLink();
		pItem->UpdateExtent();
	}
	CATCH_ALL(e)
	{
		// clean up item
		pItem->Delete();
		pObj->m_pClientItem = NULL;
		GetDocument()->RemoveObject( pObj );
		pObj->Remove();

		AfxMessageBox(IDP_FAILED_TO_CREATE);
	}
	END_CATCH_ALL

	EndWaitCursor();
}

void CDrawView::PasteText(COleDataObject& dataObject)
{
	CString text, str;

	if (m_bEditing)
	{
		// get file refering to clipboard data
		CFile *pFile = dataObject.GetFileData(CF_TEXT);
		if (pFile == NULL)
			return;

		// connect the file to the archive and read the contents
		CArchive ar(pFile, CArchive::load);
		ar.ReadString(str);	// get first line only
		ar.Close();
		delete pFile;
		
		m_pDrawRect->GetText(text);
		text = text.Left(m_nCaretPos) + str + text.Right(text.GetLength()-m_nCaretPos);
		m_pDrawRect->SetText(text);
		m_nCaretPos += str.GetLength();
		HideCaret();
		m_pDrawRect->SetTextSelStartPos(m_nCaretPos);
		m_pDrawRect->SetTextSelEndPos(m_nCaretPos);
		m_pDrawRect->Invalidate();
		MoveCaret();
 	}
}

void CDrawView::OnInitialUpdate()
{
	CWaitCursor wait;
	ResyncScrollSizes();
	InvalAll();

  CFrameWnd* pWnd = GetParentFrame();
  if( pWnd )
    pWnd->ShowWindow( SW_SHOWMAXIMIZED );
}

////////////////////////////////////////////////////////////////////////////
// CDrawView printing

BOOL CDrawView::OnPreparePrinting( CPrintInfo* pInfo )
{
  if (!WSPFeatures::Instance()->isEnabled("PLOTTER","plot_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("PLOTTER", "plot_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return 0;
	}

  CDrawDoc* pDoc = GetDocument();
  if( !pDoc )
    return 0;

  // den Drucker setzen
  pDoc->GetPrinterSettings()->SetThisPrinter();

  // Anzahl der Seiten setzen
	pInfo->SetMaxPage( pDoc->GetPageCount() );

	// default preparation
	return DoPreparePrinting( pInfo );
}

void CDrawView::OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo)
{
	CScrollView::OnBeginPrinting(pDC,pInfo);

	// check page size -- user could have gone into print setup
	// from print dialog and changed paper or orientation
  // TODO!!!
//	GetDocument()->ComputePageSize();
}

/////////////////////////////////////////////////////////////////////////////
// OLE Client support and commands

BOOL CDrawView::IsSelected(const CObject* pDocItem) const
{
	CDrawObj* pDrawObj = (CDrawObj*)pDocItem;
	if (pDocItem->IsKindOf(RUNTIME_CLASS(CDrawItem)))
		pDrawObj = ((CDrawItem*)pDocItem)->m_pDrawObj;
	return m_selection.FindObject( pDrawObj ) != NULL;
}

void CDrawView::OnInsertObject()
{
	GUID BASED_CODE clsid =
		{ 0x7e4d6180, 0x853f, 0x11d2, { 0x9a, 0x46, 0, 0x80, 0xad, 0x71, 0xe7, 0xc2 } };
	// Invoke the standard Insert Object dialog box to obtain information
	//  for new CDrawItem object.
	COleInsertDialog dlg(IOF_SELECTCREATENEW, this);
	dlg.m_io.cClsidExclude = 1;
	dlg.m_io.lpClsidExclude = &clsid;
	if (dlg.DoModal() != IDOK)
		return;

	BeginWaitCursor();

	// First create the C++ object
	CDrawOleObj* pObj = new CDrawOleObj(GetInitialPosition(), GetDocument());
	pObj->SetFlags(CDrawObj::moveable);
	ASSERT_VALID(pObj);
	CDrawItem* pItem = new CDrawItem(GetDocument(), pObj);
	ASSERT_VALID(pItem);
	pObj->m_pClientItem = pItem;

	// Now create the OLE object/item
	TRY
	{
		if (!dlg.CreateItem(pObj->m_pClientItem))
			AfxThrowMemoryException();

		// add the object to the document
		GetDocument()->AddObject( pObj, 0, 0 );

		pItem->UpdateItemType();

		// try to get initial presentation data
		pItem->UpdateLink();
		pItem->UpdateExtent();

		// if insert new object -- initially show the object
		if (dlg.GetSelectionType() == COleInsertDialog::createNewItem)
			pItem->DoVerb(OLEIVERB_SHOW, this);
		// add object to undo buffer
		AddToUndoBuffer(pObj, HINT_OBJ_ADD);
	}
	CATCH_ALL(e)
	{
		// clean up item
		pItem->Delete();
		pObj->m_pClientItem = NULL;
		GetDocument()->RemoveObject( pObj );
		pObj->Remove();

		AfxMessageBox(IDP_FAILED_TO_CREATE);
	}
	END_CATCH_ALL

	EndWaitCursor();
}

// The following command handler provides the standard keyboard
//  user interface to cancel an in-place editing session.  Here,
//  the container (not the server) causes the deactivation.
void CDrawView::OnCancelEditCntr()
{
	// deactivate any in-place active item on this view!
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL)
	{
		// if we found one, deactivate it
		pActiveItem->Close();
	}
	ASSERT(GetDocument()->GetInPlaceActiveItem(this) == NULL);

	// escape also brings us back into select mode
	ReleaseCapture();

	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool != NULL)
		pTool->OnCancel();

	CDrawTool::c_drawShape = selection;
}

void CDrawView::OnSetFocus(CWnd* pOldWnd)
{
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL &&
		pActiveItem->GetItemState() == COleClientItem::activeUIState)
	{
		// need to set focus to this item if it is in the same view
		CWnd* pWnd = pActiveItem->GetInPlaceWindow();
		if (pWnd != NULL)
		{
			pWnd->SetFocus();
			return;
		}
	}

	CScrollView::OnSetFocus(pOldWnd);
}

CIntIRect CDrawView::GetInitialPosition()
{
	CRect rect(10, 10, 200, 200);
	ClientToDoc(rect);
	return rect;
}

/////////////////////////////////////////////////////////////////////////////
// OLE Server support

// The following command handler provides the standard keyboard
//  user interface to cancel an in-place editing session.  Here,
//  the server (not the container) causes the deactivation.
void CDrawView::OnCancelEditSrvr()
{
	if (GetDocument()->IsInPlaceActive())
		GetDocument()->OnDeactivateUI(FALSE);
}

void CDrawView::ClientToDoc( CPoint& point )
{
  GetClientDC()->DPtoLP( &point );
}

void CDrawView::ClientToDoc( CRect& rect )
{
	GetClientDC()->DPtoLP( rect );

	ASSERT( rect.left <= rect.right );
	ASSERT( rect.bottom <= rect.top );
}

void CDrawView::DocToClient( CPoint& point )
{
	GetClientDC()->LPtoDP( &point );
}

void CDrawView::DocToClient( CRect& rect )
{
	GetClientDC()->LPtoDP( rect );
	rect.NormalizeRect();
}

void CDrawView::Select( CDrawObj* pObj, BOOL bAdd )
{
	if( !bAdd )
	{
		OnUpdate( NULL, HINT_UPDATE_SELECTION, NULL );
		m_selection.RemoveAllObjects();
	}

	if( pObj == NULL || IsSelected( pObj ) || pObj->IsInvisible() )
		return;

	m_selection.AddTailObject( pObj );
	InvalObj( pObj );
}

// rect is in device coordinates
void CDrawView::SelectWithinRect( const CRect& selRect, const BOOL bAdd )
{
	if( !bAdd )
		Select( NULL );

  CWaitCursor wait;

  CRect rect( selRect );  // Rechteck kopieren
	ClientToDoc( rect );

  CDrawObjList* pObList = GetDocument()->GetObjects();
  POSITION posObj = pObList->GetHeadPosition();
  while( posObj )
  {
    CDrawObj* pObj = pObList->GetNextObject( posObj );
    if( pObj && !pObj->IsInvisible() )
    {
      BOOL bSelect = FALSE;
      
      switch( GETDRAWAPP->m_nSelectType )
      {
      case CDrawApp::SELTYPE_WHOLE:
        bSelect = pObj->IsContained( rect );
        break;
        
      case CDrawApp::SELTYPE_INTERSECT:
        bSelect = pObj->Intersects( rect );
        break;
        
      default:
        ASSERT( FALSE );
        break;
      }; // switch m_nSelectType
      
      if( bSelect )	
        Select( pObj, TRUE );
    } // if pObj
  } // while posObj
} // SelectWithinRect

void CDrawView::Deselect(CDrawObj* pObj)
{
	POSITION pos = m_selection.FindObject( pObj );
	if( pos != NULL )
	{
		InvalObj( pObj );
		m_selection.RemoveObjectAt( pos );
	}
}

void CDrawView::CloneSelection()
{
	POSITION pos = m_selection.GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pObj = m_selection.GetNextObject( pos );

    CDrawObj* pDrawObj = pObj->Clone( pObj->m_pDocument );
		((CDrawDoc*)GetDocument())->AddObject( pDrawObj, 0, 0 );
    
    pDrawObj->SetFlags( CDrawObj::user, TRUE );
	}
} // CloneSelection

void CDrawView::UpdateActiveItem()
{
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL &&
		pActiveItem->GetItemState() == COleClientItem::activeUIState)
	{
		// this will update the item rectangles by calling
		//  OnGetPosRect & OnGetClipRect.
		pActiveItem->SetItemRects();
	}
}

/////////////////////////////////////////////////////////////////////////////
// CDrawView message handlers

void CDrawView::OnLButtonDown(UINT nFlags, CPoint point)
{
	if (!m_bActive)
		return;
	if (m_bEditing)
	{
		CPoint local = point;
		ClientToDoc(local);
		CDrawObj *pDrawObj = GetDocument()->ObjectAt(local);
		if (pDrawObj!=m_pDrawRect)
		{
			Deselect(m_pDrawRect);
		}
		EndTextEdit();
		return;
	}
	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool != NULL)
		pTool->OnLButtonDown(this, nFlags, point);
}

void CDrawView::OnLButtonUp(UINT nFlags, CPoint point)
{
	if (!m_bActive)
		return;
	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool != NULL)
		pTool->OnLButtonUp(this, nFlags, point);
	GetDocument()->NotifyChanged();
}

void CDrawView::OnMouseMove(UINT nFlags, CPoint point)
{
	if (!m_bActive)
		return;
	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool != NULL)
		pTool->OnMouseMove(this, nFlags, point);
	GetDocument()->NotifyChanged();
#ifdef _DEBUG
	if (m_bShowPosInfo)
	{
		CString str1, str2;

		CPoint local = point;
		ClientToDoc(local);
		if (GetDocument()->ObjectAt(local))
			str1.Format("Mouse (%d,%d) intersects", local.x, local.y);
		else
			str1.Format("Mouse (%d,%d)", local.x, local.y);
		if (m_selection.GetObjectCount()==1)
		{
			CDrawObj *pObj;

			pObj = m_selection.GetHeadObject();
			str2.Format(", Object (%d,%d,%d,%d)", pObj->m_position.top,
				pObj->m_position.left, pObj->m_position.bottom, pObj->m_position.right);
			str1 += str2;
			if (pObj->IsText())
			{
				CSize sizeText = ((CDrawRect*)pObj)->GetOutputTextSize(this);
				str2.Format(", TextSize = (%d,%d)", sizeText.cx, sizeText.cy);
				str1 += str2;
			}
		}
		((CMainFrame*)GetTopLevelFrame())->m_wndStatusBar.SetPaneText(0, str1, TRUE);
		((CMainFrame*)GetTopLevelFrame())->m_wndStatusBar.UpdateWindow();
	}
#endif
}

void CDrawView::OnLButtonDblClk(UINT nFlags, CPoint point)
{
	if (!m_bActive)
		return;
	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool != NULL)
		pTool->OnLButtonDblClk(this, nFlags, point);
	GetDocument()->NotifyChanged();
}

void CDrawView::OnDestroy()
{
	CScrollView::OnDestroy();

	// deactivate the inplace active item on this view
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL && pActiveItem->GetActiveView() == this)
	{
		pActiveItem->Deactivate();
		ASSERT(GetDocument()->GetInPlaceActiveItem(this) == NULL);
	}
}

void CDrawView::OnDrawSelect()
{
	CDrawTool::c_drawShape = selection;
}

void CDrawView::OnDrawRect()
{
	CDrawTool::c_drawShape = rect;
}

void CDrawView::OnDrawLine()
{
	CDrawTool::c_drawShape = line;
}

void CDrawView::OnDrawText() 
{
	CDrawTool::c_drawShape = textbox;
}

void CDrawView::OnDrawEllipse()
{
	CDrawTool::c_drawShape = ellipse;
}

void CDrawView::OnDrawPolyline()
{
	CDrawTool::c_drawShape = polyl;
}

void CDrawView::OnDrawPolygon() 
{
	CDrawTool::c_drawShape = polyg;
}

void CDrawView::OnDrawMeasure() 
{
	CDrawTool::c_drawShape = measure;
}

void CDrawView::OnUpdateDrawLine(CCmdUI* pCmdUI)
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == line);
}

void CDrawView::OnUpdateDrawRect(CCmdUI* pCmdUI)
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == rect);
}

void CDrawView::OnUpdateDrawText(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == textbox);
}

void CDrawView::OnUpdateDrawSelect(CCmdUI* pCmdUI)
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == selection);
}

void CDrawView::OnUpdateDrawEllipse(CCmdUI* pCmdUI)
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == ellipse);
}

void CDrawView::OnUpdateDrawPolygon(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == polyg);
}

void CDrawView::OnUpdateSingleSelect(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_selection.GetObjectCount() == 1);
}

void CDrawView::OnUpdateDrawMeasure(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == measure);
}

void CDrawView::OnViewZoomin() 
{
	CDrawTool::c_drawShape = zoomin;
}

void CDrawView::OnUpdateViewZoomin(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == zoomin);
}

void CDrawView::OnViewZoomout() 
{
  CDrawTool::c_drawShape = zoomout;
}

void CDrawView::OnUpdateViewZoomout(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == zoomout);
}

void CDrawView::Cut()
{
  Copy();
  Clear();
}; // Cut

void CDrawView::Clear()
{
  CDrawDoc* pDoc = GetDocument();
  if( pDoc == NULL )
    return;

  if( !m_bEditing )
	{
		// update all the views before the selection goes away
		pDoc->UpdateAllViews( NULL, HINT_DELETE_SELECTION, &m_selection );
		OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);

    // jetzt Objekte in zu löschende und zu versteckende aufteilen
    CDrawObjList deleteObs; // diese Objekte werden gelöscht
    CDrawObjList hideObs;  // diese nur versteckt
    POSITION pos = m_selection.GetHeadPosition();
    while( pos != NULL )
    {
      POSITION oldPos = pos; // alte Position fürs löschen merken

      CDrawObj* pObj = m_selection.GetNextObject( pos );
      if( pObj->IsUser() )
      {
        deleteObs.AddTailObject( pObj );
        m_selection.RemoveObjectAt( oldPos );
        pDoc->RemoveObject( pObj );
      }
      else if( pObj->IsHideable() )
      {
        hideObs.AddTailObject( pObj );
        m_selection.RemoveObjectAt( oldPos );
      }

      // für Stempeleditor???
      if( pObj->IsText() )
        m_texts.RemoveKey( ((CDrawRect*)pObj)->GetStempelTextType() );
    } // while pos

    // jetzt die beiden listen entsprechend zum UndoBuffer hinzufügen
    if( deleteObs.GetObjectCount() > 0 )
      AddToUndoBuffer( &deleteObs, HINT_OBJ_DELETE );
    if( hideObs.GetObjectCount() > 0 )
      AddToUndoBuffer( &hideObs, HINT_OBJ_HIDE );

    // die HideObjecs können erst unsichtbar gemacht werden, NACHDEM sie zum undo-Buffer hinzugefügt
    // wurden, da sie sonst unsichtbar hinzugefügt werden
    pos = hideObs.GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = hideObs.GetNextObject( pos );
      pObj->SetFlags( CDrawObj::invisible );
    } // while pos
	}
	else
	{
		if( m_bSelectedText )
		{
			CString text;
			
			int nStartPos = m_pDrawRect->GetTextSelStartPos();
			int nEndPos = m_pDrawRect->GetTextSelEndPos();
			m_pDrawRect->GetText(text);
			text = text.Left(nStartPos) + text.Right(text.GetLength()-nEndPos);
			m_pDrawRect->SetText(text);
			HideCaret();
			m_nCaretPos = nStartPos;
			m_pDrawRect->SetTextSelStartPos(nStartPos);
			m_pDrawRect->SetTextSelEndPos(nStartPos);
			m_pDrawRect->Invalidate();
			MoveCaret();
		}
	}
} // Clear

void CDrawView::OnUpdateDrawPolyline(CCmdUI* pCmdUI)
{
	pCmdUI->SetRadio(CDrawTool::c_drawShape == polyl);
}

void CDrawView::OnSize(UINT nType, int cx, int cy)
{
	ResyncScrollSizes();
	CScrollView::OnSize(nType, cx, cy);
	UpdateActiveItem();
}

BOOL CDrawView::OnEraseBkgnd(CDC* pDC)
{
	CBrush br( GetSysColor( COLOR_WINDOW ) ); 
    FillOutsideRect( pDC, &br );
    return TRUE;                   // Erased
}

void CDrawView::OnObjectMoveBack()
{
	CDrawDoc* pDoc = GetDocument();
	CDrawObj* pObj = m_selection.GetHeadObject();
	CDrawObjList* pObjects = pDoc->GetObjects();
	POSITION pos = pObjects->FindObject( pObj );
	ASSERT(pos != NULL);
	if (pos != pObjects->GetHeadPosition())
	{
		POSITION posPrev = pos;
		pObjects->GetPrevObject(posPrev);
		pObjects->RemoveObjectAt( pos );
		pObjects->InsertBeforeObject( posPrev, pObj );
		InvalObj(pObj);
	}
}

void CDrawView::OnObjectMoveForward()
{
	CDrawDoc* pDoc = GetDocument();
	CDrawObj* pObj = m_selection.GetHeadObject();
	CDrawObjList* pObjects = pDoc->GetObjects();
	POSITION pos = pObjects->FindObject(pObj);
	ASSERT(pos != NULL);
	if (pos != pObjects->GetTailPosition())
	{
		POSITION posNext = pos;
		pObjects->GetNextObject(posNext);
		pObjects->RemoveObjectAt( pos );
		pObjects->InsertAfterObject( posNext, pObj );
		InvalObj(pObj);
	}
}

void CDrawView::OnObjectMoveToBack()
{
	CDrawDoc* pDoc = GetDocument();
	CDrawObj* pObj = m_selection.GetHeadObject();
	CDrawObjList* pObjects = pDoc->GetObjects();
	POSITION pos = pObjects->FindObject(pObj);
	ASSERT(pos != NULL);
	pObjects->RemoveObjectAt( pos );
	pObjects->AddHeadObject( pObj );
	InvalObj(pObj);
}

void CDrawView::OnObjectMoveToFront()
{
	CDrawObj* pObj = m_selection.GetHeadObject();
  GetDocument()->MoveObjectToFront( pObj );
	InvalObj( pObj );
}

void CDrawView::Copy()
{
	ASSERT_VALID(this);
	ASSERT(GetDocument()->m_cfPrivate != NULL);

	// Create a shared file and associate a CArchive with it
	CSharedFile file;
	CArchive ar( &file, CArchive::store );

	if( !m_bEditing )
	{
		// Serialize selected objects to the archive
		m_selection.Serialize( ar );
		ar.Close();

		COleDataSource* pDataSource = NULL;
		TRY
		{
			pDataSource = new COleDataSource;
			// put on local format instead of or in addition to
			pDataSource->CacheGlobalData(GetDocument()->m_cfPrivate, file.Detach());

			// if only one item and it is a COleClientItem then also
			// paste in that format
			CDrawObj* pDrawObj = m_selection.GetHeadObject();
			if (m_selection.GetObjectCount() == 1 &&
				pDrawObj->IsKindOf(RUNTIME_CLASS(CDrawOleObj)))
			{
				CDrawOleObj* pDrawOle = (CDrawOleObj*)pDrawObj;
				pDrawOle->m_pClientItem->GetClipboardData(pDataSource, FALSE);
			}
			pDataSource->SetClipboard();
		}
		CATCH_ALL(e)
		{
			delete pDataSource;
			THROW_LAST();
		}
		END_CATCH_ALL
	}
	else
	{
		if( m_bSelectedText )
		{
			// Write selected text to the archive
			CString text;

			int nStartPos = m_pDrawRect->GetTextSelStartPos();
			int nEndPos = m_pDrawRect->GetTextSelEndPos();
			m_pDrawRect->GetText(text);
			text = text.Mid(nStartPos, nEndPos-nStartPos);
			ar.Flush();
			file.Flush();
			ar.WriteString(text);
			ar.Close();

			COleDataSource* pDataSource = NULL;
			TRY
			{
				pDataSource = new COleDataSource;
				pDataSource->CacheGlobalData(CF_TEXT, file.Detach());
				pDataSource->SetClipboard();
			}
			CATCH_ALL(e)
			{
				delete pDataSource;
				THROW_LAST();
			}
			END_CATCH_ALL
		}
	}
}

void CDrawView::Paste()
{
	COleDataObject dataObject;
	dataObject.AttachClipboard();

	if( !m_bEditing )
	{
		// invalidate current selection since it will be deselected
		OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);
		m_selection.RemoveAllObjects();
		if( dataObject.IsDataAvailable( GetDocument()->m_cfPrivate ) )
		{
			PasteNative( dataObject );

			// now add all items in m_selection to document
      GetDocument()->AddObjects( &m_selection, 0, 0 );
				
		}
		else
			PasteEmbedded( dataObject, CPoint( GetInitialPosition().TopLeft() ) );

		// add the selection to the undo buffer
		AddToUndoBuffer(&m_selection, HINT_OBJ_ADD);

		GetDocument()->SetModifiedFlag();
		// invalidate new pasted stuff
		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_SELECTION, &m_selection);
	}
	else
	{
		if( dataObject.IsDataAvailable( CF_TEXT ) )
		{
			PasteText( dataObject );
			GetDocument()->SetModifiedFlag();
		}
	}
}

void CDrawView::OnObjectProperties() 
{
	if (m_selection.GetObjectCount() > 0 && CDrawTool::c_drawShape == selection)
	{
		CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
		ASSERT(pTool != NULL);
		pTool->OnEditProperties(this);
	}
}

void CDrawView::OnUpdateObjectProperties(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_selection.GetObjectCount() > 0 &&
				   CDrawTool::c_drawShape == selection);
}

/////////////////////////////////////////////////////////////////////////////
// CDrawView diagnostics

#ifdef _DEBUG
void CDrawView::AssertValid() const
{
	CScrollView::AssertValid();
}

void CDrawView::Dump(CDumpContext& dc) const
{
	CScrollView::Dump(dc);
}

CDrawDoc* CDrawView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CDrawDoc)));
	return (CDrawDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// new
// support for drag/drop

int CDrawView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CScrollView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// register drop target
	if( m_dropTarget.Register( this ) )
		return 0;
	else
		return -1;
}

BOOL CDrawView::GetObjectInfo(COleDataObject* pDataObject,
	CSize* pSize, CSize* pOffset)
{
	ASSERT(pSize != NULL);

	// get object descriptor data
	HGLOBAL hObjDesc = pDataObject->GetGlobalData(m_cfObjectDescriptor);
	if (hObjDesc == NULL)
	{
		if (pOffset != NULL)
			*pOffset = CSize(0, 0); // fill in defaults instead
		*pSize = CSize(0, 0);
		return FALSE;
	}
	ASSERT(hObjDesc != NULL);

	// otherwise, got CF_OBJECTDESCRIPTOR ok.  Lock it down and extract size.
	LPOBJECTDESCRIPTOR pObjDesc = (LPOBJECTDESCRIPTOR)GlobalLock(hObjDesc);
	ASSERT(pObjDesc != NULL);
	pSize->cx = (int)pObjDesc->sizel.cx;
	pSize->cy = (int)pObjDesc->sizel.cy;
	if (pOffset != NULL)
	{
		pOffset->cx = (int)pObjDesc->pointl.x;
		pOffset->cy = (int)pObjDesc->pointl.y;
	}
	GlobalUnlock(hObjDesc);
	GlobalFree(hObjDesc);

	// successfully retrieved pSize & pOffset info
	return TRUE;
}

DROPEFFECT CDrawView::OnDragEnter(COleDataObject* pDataObject,
	DWORD grfKeyState, CPoint point)
{
	ASSERT(m_prevDropEffect == DROPEFFECT_NONE);
	m_bDragDataAcceptable = FALSE;
	if (!COleClientItem::CanCreateFromData(pDataObject))
		return DROPEFFECT_NONE;

	m_bDragDataAcceptable = TRUE;
	GetObjectInfo(pDataObject, &m_dragSize, &m_dragOffset);
	CClientDC dc(NULL);
	dc.HIMETRICtoDP(&m_dragSize);
	dc.HIMETRICtoDP(&m_dragOffset);

	return OnDragOver(pDataObject, grfKeyState, point);
}

DROPEFFECT CDrawView::OnDragOver(COleDataObject*,
	DWORD grfKeyState, CPoint point)
{
	if(m_bDragDataAcceptable == FALSE)
		return DROPEFFECT_NONE;

	point -= m_dragOffset;  // adjust target rect by original cursor offset

	// check for point outside logical area -- i.e. in hatched region
	// GetTotalSize() returns the size passed to SetScrollSizes
	CRect rectScroll( CPoint( 0, 0 ), GetTotalSize() );

	CRect rectItem( point, m_dragSize );
	rectItem.OffsetRect(GetDeviceScrollPosition());

	DROPEFFECT de = DROPEFFECT_NONE;
	CRect rectTemp;
	if (rectTemp.IntersectRect(rectScroll, rectItem))
	{
		// check for force link
#ifndef _MAC		
		if ((grfKeyState & (MK_CONTROL|MK_SHIFT)) == (MK_CONTROL|MK_SHIFT))
#else
		if ((grfKeyState & (MK_OPTION|MK_SHIFT)) == (MK_OPTION|MK_SHIFT))
#endif			
			de = DROPEFFECT_NONE; // DRAWCLI isn't a linking container
		// check for force copy
#ifndef _MAC				
		else if ((grfKeyState & MK_CONTROL) == MK_CONTROL)
#else
		else if ((grfKeyState & MK_CONTROL) == MK_CONTROL)
#endif		
			de = DROPEFFECT_COPY;
		// check for force move
		else if ((grfKeyState & MK_ALT) == MK_ALT)
			de = DROPEFFECT_MOVE;
		// default -- recommended action is move
		else
			de = DROPEFFECT_MOVE;
	}

	if (point == m_dragPoint)
		return de;

	// otherwise, cursor has moved -- need to update the drag feedback
  CClientDC* pDC = GetClientDC();
	if( m_prevDropEffect != DROPEFFECT_NONE )
		pDC->DrawFocusRect( CRect( m_dragPoint, m_dragSize ) );

	m_prevDropEffect = de;
	if( m_prevDropEffect != DROPEFFECT_NONE )
	{
		m_dragPoint = point;
		pDC->DrawFocusRect( CRect( point, m_dragSize ) );
	}
	return de;
}

BOOL CDrawView::OnDrop(COleDataObject* pDataObject,
	DROPEFFECT /*dropEffect*/, CPoint point)
{
	ASSERT_VALID(this);

	// clean up focus rect
	OnDragLeave();

	// offset point as appropriate for dragging
	GetObjectInfo(pDataObject, &m_dragSize, &m_dragOffset);
	CClientDC dc(NULL);
	dc.HIMETRICtoDP(&m_dragSize);
	dc.HIMETRICtoDP(&m_dragOffset);
	point -= m_dragOffset;

	// invalidate current selection since it will be deselected
	OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);
	m_selection.RemoveAllObjects();
	if (m_bDragDataAcceptable)
		PasteEmbedded(*pDataObject, point);

	// update the document and views
	GetDocument()->SetModifiedFlag();
	GetDocument()->UpdateAllViews(NULL, 0, NULL);      // including this view

	return TRUE;
}

void CDrawView::OnDragLeave()
{
	if( m_prevDropEffect != DROPEFFECT_NONE )
	{
		GetClientDC()->DrawFocusRect( CRect( m_dragPoint,m_dragSize ) ); // erase previous focus rect
		m_prevDropEffect = DROPEFFECT_NONE;
	}
}


void CDrawView::OnContextMenu(CWnd* /*pWnd*/, CPoint point) 
{
	// when not in select mode right button click brings us back into select mode
	// when in select mode right button click displays pop-up menu
	ReleaseCapture();

	CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
	if (pTool==NULL || CDrawTool::c_drawShape == selection)
	{
		CMenu menu;
		VERIFY(menu.LoadMenu(CG_IDR_POPUP_CHILD_FRAME));

		CMenu* pPopup = menu.GetSubMenu(0);
		ASSERT(pPopup != NULL);

		CWnd* pWndPopupOwner = this;
		while (pWndPopupOwner->GetStyle() & WS_CHILD)
			pWndPopupOwner = pWndPopupOwner->GetParent();

		pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
			pWndPopupOwner);
	}
	if (pTool != NULL)
		pTool->OnCancel();

	CDrawTool::c_drawShape = selection;
}

void CDrawView::OnPrint( CDC* pDC, CPrintInfo* pInfo ) 
{
	ASSERT_VALID( this );
	ASSERT_VALID( pDC );
	ASSERT( pInfo );
	ASSERT( pInfo->m_bContinuePrinting );

  CDrawDoc* pDoc = GetDocument();
  if( !pDoc )
    return;

  CSize pageSize = pDoc->GetPageSize();
  CSize drawSize = pDoc->GetDrawingSize();


	if( !pInfo->m_bPreview )
		((CDrawDoc*)GetDocument())->m_pData->m_pSummInfo->RecordPrintDate();
  
  int saveID = pDC->SaveDC();
  pDC->SetBkMode( TRANSPARENT );

  // und clipping Region festlegen!
  UINT curPage = pInfo->m_nCurPage - 1;
  UINT curPageX = curPage % pDoc->GetPages().cx;
  UINT curPageY = div( curPage, pDoc->GetPages().cx ).quot;
  
  CRect clipRect( pageSize.cx * curPageX, drawSize.cy - pageSize.cy * curPageY, pageSize.cx * ( curPageX + 1 ) + 1 * MM_FACTOR, drawSize.cy - pageSize.cy * ( curPageY + 1 ) - 1 * MM_FACTOR );

  pDC->IntersectClipRect( clipRect );

	OnDraw( pDC );

  pDC->RestoreDC( saveID );
}

BOOL CDrawView::PreTranslateMessage(MSG* pMsg) 
{
	// Shift+F10: show pop-up menu.
	if ((((pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN) && // If we hit a key and
		(pMsg->wParam == VK_F10) && (GetKeyState(VK_SHIFT) & ~1)) != 0) ||	// it's Shift+F10 OR
		(pMsg->message == WM_CONTEXTMENU))									// Natural keyboard key
	{
		CRect rect;
		GetClientRect(rect);
		ClientToScreen(rect);

		CPoint point = rect.TopLeft();
		point.Offset(5, 5);
		// first change to select mode
		CDrawTool* pTool = CDrawTool::FindTool(CDrawTool::c_drawShape);
		if (pTool != NULL)
			pTool->OnCancel();
		CDrawTool::c_drawShape = selection;
		OnContextMenu(NULL, point);
		return TRUE;
	}

	return CScrollView::PreTranslateMessage(pMsg);
}

void CDrawView::OnViewGrid() 
{
	m_bGrid = !m_bGrid;
	Invalidate(FALSE);
}

void CDrawView::OnUpdateViewGrid(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_bGrid);
}

void CDrawView::OnViewShowobjects() 
{
	CDrawOleObj::c_bShowItems = !CDrawOleObj::c_bShowItems;
	GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_OLE_ITEMS, NULL);
}

void CDrawView::OnUpdateViewShowobjects(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(CDrawOleObj::c_bShowItems);
}

void CDrawView::ResyncScrollSizes()
{
  CDrawDoc* pDoc = GetDocument();
  if( !pDoc )
    return;

	CSize sizeDoc = pDoc->GetDrawingSize();
  CSize sizePage = pDoc->GetPageSize();

  CSize sizeLine( MulDiv( sizePage.cx, 100, m_nZoomFactor * 10 ), 
    MulDiv( sizePage.cy, 100, m_nZoomFactor * 10 ) );
	
  GetClientDC()->LPtoDP( &sizeDoc );
	GetClientDC()->LPtoDP( &sizePage );
	GetClientDC()->LPtoDP( &sizeLine );
	
  SetScrollSizes( MM_TEXT, sizeDoc, sizePage, sizeLine );
} // ResyncScrollSizes

BOOL CDrawView::CreateCaret()
{
	m_bSelectedText = FALSE;
	if (m_pDrawRect!=NULL && m_pDrawRect->IsText())
	{
		CBitmap *pOldBmp;

		CDC dc;
		CSize sizeText, sizeExt;
		CPoint ptText;

		double angle;
		CString text;
		CRect rcDC;
		CBrush *pBrush;
		CPen *pPen, *pOldPen;

    CClientDC* pCDC = GetClientDC();

		/** initialize DC with font **/
		LOGFONT logfont = m_pDrawRect->GetLogFont();
		logfont.lfHeight = m_pDrawRect->TransformFontHeight( pCDC, logfont.lfHeight );
    
    CFont font;
		if( font.CreateFontIndirect(&logfont) )
		{
      CFont* pOldFont = NULL;
			if( m_pDrawRect->HasFont() )
				pOldFont = pCDC->SelectObject( &font );
			else
				pOldFont = (CFont*)pCDC->SelectStockObject( ANSI_FIXED_FONT );
			
      /** find text extents **/
			text = "gM";
			sizeText = pCDC->GetTextExtent(text);
			pCDC->LPtoDP(&sizeText);
			ptText = CPoint(sizeText.cx, sizeText.cy);
			pCDC->SelectObject(pOldFont);
			angle = m_pDrawRect->GetTextOrientation()*2*PI/360;
			rcDC.SetRect(0, 0, abs((int)(ptText.y*sin(angle)))+1, abs((int)(ptText.y*cos(angle)))+1);
			m_sizeCaret = CPoint(rcDC.Width(), -rcDC.Height());
			if( dc.CreateCompatibleDC( pCDC ) )
			{
				m_CaretBitmap.DeleteObject();	// just in case
				if (m_CaretBitmap.CreateCompatibleBitmap(&dc, rcDC.Width(), rcDC.Height()))
				{
					CDC *pDC = GetDC();
					
					pOldBmp = dc.SelectObject(&m_CaretBitmap);
					// Paint the bitmap with the background colour
					dc.SetBkColor(pDC->GetBkColor());
					dc.SetTextColor(pDC->GetTextColor());
					ReleaseDC(pDC);
					pBrush = new CBrush(dc.GetTextColor());
					dc.FillRect(&rcDC, pBrush);
					delete pBrush;
					pPen = new CPen(PS_SOLID, 0, dc.GetBkColor());
					pOldPen = dc.SelectObject(pPen);
					CPoint ptMid((int)(rcDC.Width()/2), (int)(rcDC.Height())/2);
					CPoint ptOff((int)((ptText.y*sin(angle)+1)/2), -(int)((ptText.y*cos(angle)+1)/2));
					CPoint ptPos = ptMid+ptOff;
					dc.MoveTo(ptPos);
					ptPos -= ptOff;
					ptPos -= ptOff;
					dc.LineTo(ptPos);
					dc.SelectObject(pOldPen);
					CWnd::CreateCaret(&m_CaretBitmap);
					dc.SelectObject(pOldBmp);
					delete pPen;
					m_nCaretPos = 0;
					MoveCaret();
					ShowCaret();
					return TRUE;
				}
			}
		}
	}
	return FALSE;
}

void CDrawView::MoveCaret()
{
	if( m_pDrawRect->GetTextSelStartPos()<m_pDrawRect->GetTextSelEndPos() )
	{
		if( !m_bSelectedText )
			HideCaret();
		m_bSelectedText = TRUE;
	}
	else
	{
		if (m_bSelectedText)
			ShowCaret();
		m_bSelectedText = FALSE;
	}

  CClientDC* pCDC = GetClientDC();

	LOGFONT logfont = m_pDrawRect->GetLogFont();
	logfont.lfHeight = m_pDrawRect->TransformFontHeight( pCDC, logfont.lfHeight );
	
  CFont font;
  if( !font.CreateFontIndirect(&logfont) )
		return;

  CFont* pOldFont = NULL;
	if( m_pDrawRect->HasFont() )
		pOldFont = pCDC->SelectObject(&font);
	else
		pOldFont = (CFont*)pCDC->SelectStockObject(ANSI_FIXED_FONT);
	// get size of text
	int x = m_pDrawRect->m_helpers[1];
	int y = m_pDrawRect->m_helpers[2];

  {
    CString text;
  	m_pDrawRect->GetText(text);

		CString str = ' ' + text.Left( m_nCaretPos );
		CSize sizeText = pCDC->GetTextExtent(str);
		// calculate size after text is rotated
		double angle = logfont.lfEscapement * 2 * PI / 3600;
		x += int( sizeText.cx * cos( angle ) - m_sizeCaret.x * ( 1 + sin( angle ) ) );
		y -= int( sizeText.cx * sin( angle ) + m_sizeCaret.y * ( 1 - cos( angle ) ) );
	}
	
  pCDC->SelectObject( pOldFont );
	
  CPoint pt(x, y);

	pCDC->LPtoDP( &pt );
	SetCaretPos( pt );
}

void CDrawView::BeginTextEdit(CDrawRect* pDrawRect)
{
	m_bEditing = TRUE;
	m_pDrawRect = pDrawRect;
	CFrameWnd *pFrameWnd = GetTopLevelFrame();
	if (pFrameWnd!=NULL)
	{
		if (pFrameWnd->IsKindOf(RUNTIME_CLASS(CMainFrame)))		// could be in-place!
			((CMainFrame*)pFrameWnd)->m_bEditing = TRUE;
	}
	m_undoText.RemoveAll();
	m_undoTextSels.RemoveAll();
	m_undoCaretPos.RemoveAll();
	CreateCaret();
}

void CDrawView::EndTextEdit()
{
	if (m_bEditing)
	{
		m_bEditing = FALSE;
		m_bSelectedText = FALSE;
		m_pDrawRect->SetTextSelStartPos(0);
		m_pDrawRect->SetTextSelEndPos(0);
		m_pDrawRect->Invalidate();
		::DestroyCaret();
		m_CaretBitmap.DeleteObject();
		CFrameWnd *pFrameWnd = GetTopLevelFrame();
		if (pFrameWnd!=NULL)
		{
			if (pFrameWnd->IsKindOf(RUNTIME_CLASS(CMainFrame)))		// could be in-place!
				((CMainFrame*)pFrameWnd)->m_bEditing = FALSE;
		}
		m_pDrawRect = NULL;
		m_undoText.RemoveAll();
		m_undoTextSels.RemoveAll();
		m_undoCaretPos.RemoveAll();
	}
}

void CDrawView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	CString text, oldText;
	int nOldCaretPos;
	
	if (m_bEditing)
	{
		int nStartPos = m_pDrawRect->GetTextSelStartPos();
		int nEndPos = m_pDrawRect->GetTextSelEndPos();
		m_pDrawRect->GetText(text);
		m_pDrawRect->GetText(oldText);
		nOldCaretPos = m_nCaretPos;
		switch (nChar)
		{
 			case VK_SHIFT:
				m_bCanSelectText = TRUE;
				break;
		
			case VK_HOME:       /* Home */ 
				if (m_bCanSelectText)
				{
					if (m_nCaretPos==nStartPos)
						m_pDrawRect->SetTextSelStartPos(0);
					else if (m_nCaretPos==nEndPos)
					{
						m_pDrawRect->SetTextSelStartPos(0);
						m_pDrawRect->SetTextSelEndPos(nStartPos);
					}
				}
				else
				{
					m_pDrawRect->SetTextSelStartPos(0);
					m_pDrawRect->SetTextSelEndPos(0);
				}
                m_nCaretPos = 0; 
				m_pDrawRect->Invalidate();
                break; 
 
            case VK_END:        /* End */ 
				if (m_bCanSelectText)
				{
					if (m_nCaretPos==nEndPos)
						m_pDrawRect->SetTextSelEndPos(text.GetLength());
					else if (m_nCaretPos==nStartPos)
					{
						m_pDrawRect->SetTextSelStartPos(nEndPos);
						m_pDrawRect->SetTextSelEndPos(text.GetLength());
					}
				}
				else
				{
					m_pDrawRect->SetTextSelStartPos(text.GetLength());
					m_pDrawRect->SetTextSelEndPos(text.GetLength());
				}
                m_nCaretPos = text.GetLength(); 
				m_pDrawRect->Invalidate();
                break; 
 
            case VK_LEFT:       /* Left arrow */ 
				if (m_bCanSelectText)
				{
					if (m_nCaretPos==nStartPos)
						m_pDrawRect->SetTextSelStartPos(max(m_nCaretPos-1, 0));
					else if (m_nCaretPos==nEndPos)
						m_pDrawRect->SetTextSelEndPos(max(m_nCaretPos-1, 0));
					m_nCaretPos = max(m_nCaretPos-1, 0);
				}
				else
				{
					if (nStartPos!=nEndPos)
						m_nCaretPos = nStartPos;
					else
						m_nCaretPos = max(m_nCaretPos-1, 0);
					m_pDrawRect->SetTextSelStartPos(m_nCaretPos);
					m_pDrawRect->SetTextSelEndPos(m_nCaretPos);
				}
				m_pDrawRect->Invalidate();
                break; 
 
            case VK_RIGHT:      /* Right arrow */ 
				if (m_bCanSelectText)
				{
					if (m_nCaretPos==nEndPos)
						m_pDrawRect->SetTextSelEndPos(min(m_nCaretPos+1, text.GetLength()));
					else if (m_nCaretPos==nStartPos)
						m_pDrawRect->SetTextSelStartPos(min(m_nCaretPos+1, text.GetLength()));
					m_nCaretPos = min(m_nCaretPos+1, text.GetLength()); 
				}
				else
				{
					if (nStartPos!=nEndPos)
						m_nCaretPos = nEndPos;
					else
						m_nCaretPos = min(m_nCaretPos+1, text.GetLength()); 
					m_pDrawRect->SetTextSelStartPos(m_nCaretPos);
					m_pDrawRect->SetTextSelEndPos(m_nCaretPos);
				}
				m_pDrawRect->Invalidate();
				break; 

			case VK_DELETE:		/* Delete */
				if (m_bSelectedText)
				{
					text = text.Left(nStartPos) + text.Right(text.GetLength()-nEndPos);
					m_pDrawRect->SetText(text);
					HideCaret();
					m_nCaretPos = nStartPos;
					m_pDrawRect->SetTextSelStartPos(nStartPos);
					m_pDrawRect->SetTextSelEndPos(nStartPos);
					m_pDrawRect->Invalidate();
					ShowCaret();
				}
				else if (m_nCaretPos<text.GetLength())
				{
					text = text.Left(m_nCaretPos) + text.Right(text.GetLength()-m_nCaretPos-1);
					m_pDrawRect->SetText(text);
					HideCaret();
					m_pDrawRect->Invalidate();
					ShowCaret();
				}
				break;
		}
		MoveCaret();
		ShowCaret();	// temp
		if (oldText!=text)
		{	// add to undo buffer
			AddToUndoBuffer(oldText, nStartPos, nEndPos, nOldCaretPos);
		}
	}
	else
		CScrollView::OnKeyDown(nChar, nRepCnt, nFlags);
}

void CDrawView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	CString text, oldText;
	int nOldCaretPos;

	if( m_bEditing )
	{
		COleDataObject dataObject;
		int nStartPos = m_pDrawRect->GetTextSelStartPos();
		int nEndPos = m_pDrawRect->GetTextSelEndPos();
		m_pDrawRect->GetText(text);
		m_pDrawRect->GetText(oldText);
		nOldCaretPos = m_nCaretPos;
		switch (nChar)
		{
			case 0x1A:			/* Ctrl-Z */
				Undo();
				break;
		
			case 0x18:			/* Ctrl-X */
				if (m_bSelectedText)
					Cut();
				break;
		
			case 0x03:			/* Ctrl-C */
				if (m_bSelectedText)
					Copy();
				break;

			case 0x16:			/* Ctrl-V */
				if (dataObject.AttachClipboard() &&
					dataObject.IsDataAvailable(CF_TEXT))
					Paste();
				break;
		
			case 0x08:          /* Backspace */ 

				/* 
				* Move the caret back one space, and then 
				* process this like the DEL key. 
				*/ 

				if (m_nCaretPos > 0)
				{ 
					m_nCaretPos--; 
					SendMessage(WM_KEYDOWN, VK_DELETE, 1L);
				}
				break; 

			case 0x09:          /* Tab */ 
			case 0x0A:        /* Linefeed */ 

                /* No processing. */ 
 
				MessageBeep((UINT) -1); 
				break; 

			case 0x0D:          /* Carriage return */ 
			case 0x1B:        /* Escape */ 
				EndTextEdit();
				return;
				break;

			default:            /* all other characters */
				if (m_bSelectedText)
				{
					text = text.Left(nStartPos) + (char)nChar + text.Right(text.GetLength()-nEndPos);
					m_nCaretPos = nStartPos+1;
				}
				else
				{
					text = text.Left(m_nCaretPos) + (char)nChar + text.Right(text.GetLength()-m_nCaretPos);
					m_nCaretPos++;
				}
				m_pDrawRect->SetText(text);
				HideCaret();
				m_pDrawRect->SetTextSelStartPos(m_nCaretPos);
				m_pDrawRect->SetTextSelEndPos(m_nCaretPos);
				m_pDrawRect->Invalidate();
				ShowCaret();
				break;
		}
		MoveCaret();
		ShowCaret();	// temp
		if (nChar!=0x1A && oldText!=text)
		{	// add to undo buffer
			AddToUndoBuffer(oldText, nStartPos, nEndPos, nOldCaretPos);
		}
	}
	else
		CScrollView::OnChar(nChar, nRepCnt, nFlags);
}

void CDrawView::OnKillFocus(CWnd* pNewWnd) 
{
	CScrollView::OnKillFocus(pNewWnd);
	EndTextEdit();
}

void CDrawView::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (m_bEditing)
	{
		switch (nChar)
		{
 			case VK_SHIFT:
				m_bCanSelectText = FALSE;
				break;
		}
	}
	else
		CScrollView::OnKeyUp(nChar, nRepCnt, nFlags);
}

void CDrawView::Undo() 
// Die Undo-Operation
{
	if( m_bEditing )
	{
		if( m_undoText.GetSize() > 0 )
		{
			CPoint pt;

			HideCaret();
			m_pDrawRect->SetText(m_undoText[0]);
			pt = m_undoTextSels[0];
			m_pDrawRect->SetTextSelStartPos(pt.x);
			m_pDrawRect->SetTextSelEndPos(pt.y);
			m_nCaretPos = m_undoCaretPos[0];
			m_pDrawRect->Invalidate();
			ShowCaret();
			MoveCaret();
			m_undoText.RemoveAt(0);
			m_undoTextSels.RemoveAt(0);
			m_undoCaretPos.RemoveAt(0);
		}
		else
			MessageBeep((UINT) -1); 
	}
	else
	{
		if( m_undoBuffer.GetSize() > 0 )
		{
			CDrawDoc *pDoc = GetDocument();

#ifdef _DEBUG
			if (m_bTraceUndo)
				TRACE("Undo operation.\n");
#endif
			// we are about to undo so update all the buffer object maps
			UpdateUndoBufferObjs();

      // now undo
      CDrawObjList* pObjList = m_undoBuffer.GetAt( 0 );
			POSITION pos = pObjList->GetHeadPosition();
      WORD nHint; // wird für Undo selection noch benötigt; es wird davon ausgegangen, daß pro undo nur ein mögl. Hint verwendet wurde
			while( pos != NULL )
			{
        CDrawObj* pObj = pObjList->GetNextObject( pos );

				CDrawObj* pOrgObj;
				if( m_undoMapOrgObjs.Lookup( pObj, pOrgObj ) )
				{
					m_undoMapHints.Lookup( pObj, nHint );
					switch( nHint )
					{
						case HINT_OBJ_DELETE:	// original object was deleted
              // pOrgObj is now invalid and pObj is a copy of what it used to be
#ifdef _DEBUG
              if( m_bTraceUndo )
								TRACE( "Undo delete operation: Copy %x added.\n", pObj );
#endif
							pDoc->AddObject( pObj, 0, 0 );
							pObj->Invalidate();
							break;

						case HINT_OBJ_ADD:		// original object was added
							// pObj is an exact copy of pOrgObj
#ifdef _DEBUG
							if( m_bTraceUndo )
								TRACE( "Undo add operation: Original %x removed.\n", pOrgObj );
#endif
							pDoc->RemoveObject( pOrgObj );
							pOrgObj->Remove();
							pObj->Remove();
							break;
				
						case HINT_OBJ_EDIT:		// original object was edited
							// pOrgObj still exist and pObj is a copy of its original state
#ifdef _DEBUG
							if( m_bTraceUndo )
								TRACE( "Undo edit operation: Original %x replaced with copy %x.\n", pOrgObj, pObj );
#endif
							VERIFY( pDoc->ReplaceObject( pOrgObj, pObj ) );
							pObj->Invalidate();
              break;

            case HINT_OBJ_HIDE:
              // das Original Objekt wurde nur unsichtbar gemacht, dies rückgangig machen und fertig
              pOrgObj->UnsetFlags( CDrawObj::invisible );

              // das Kopierte Objekt brauchen wird gar nicht mehr
              pObj->Remove();
              break;

            default:
              ASSERT( FALSE );
              break;
          } // switch nHint
        } // if m_undoMapOrgObjs.Lookup
      } // while pos

      // Selection undo
			m_selection.RemoveAllObjects();
			if( nHint != HINT_OBJ_ADD )
			{
				POSITION pos = m_undoSelections.GetAt( 0 )->GetHeadPosition();
				while( pos != NULL )
				{
					CDrawObj* pObj = m_undoSelections.GetAt( 0 )->GetNextObject( pos );
#ifdef _DEBUG
					if( m_bTraceUndo )
						TRACE( "Object added to selection: %x.\n", pObj );
#endif
					m_selection.AddTailObject( pObj );
				}
      } // if nHint != HINZ_OBJ_ADD
			delete m_undoSelections.GetAt( 0 );
			m_undoSelections.RemoveIndex( 0 );
			
      // alle swas mit diesem Undo zusammenhängt kann jetzt gelöscht werden
      pos = pObjList->GetHeadPosition();
			while( pos != NULL )
			{
				CDrawObj* pObj = pObjList->GetNextObject( pos );
				m_undoMapOrgObjs.RemoveKey( pObj );
				m_undoMapHints.RemoveKey( pObj );
			}
			delete pObjList;
			m_undoBuffer.RemoveIndex( 0 );
		}
	}
  // und die View neu zeichnen
	Invalidate();
} // Undo

BOOL CDrawView::OnEdit( UINT nID )
{
  switch( nID )
  {
  case ID_EDIT_SELECT_ALL:
    {
      CDrawObjList* pObList = GetDocument()->GetObjects();
      POSITION pos = pObList->GetHeadPosition();
      while (pos != NULL)
        Select( pObList->GetNextObject( pos ), TRUE );
    }
    break;
  
  case ID_EDIT_CLEAR:
    Clear();
    break;
  
  case ID_EDIT_COPY:
    Copy();
    break;
  
  case ID_EDIT_CUT:
    Cut();
    break;
  
  case ID_EDIT_PASTE:
    Paste();
    break;
  
  case ID_EDIT_UNDO:
    Undo();
    break;
  
  case ID_EDIT_ZOOM:
    {
      CZoomDialog dlg( this );
      dlg.DoModal();
    }
    break;
  
  case ID_EDIT_PASTE_SPECIAL:
    PasteSpecial();
    break;

  case ID_EDIT_PROPS:
    Props();
    break;

  default:
    return FALSE; // bin nicht zuständig
  };

  return TRUE; // Kommando wurde ausgeführt
}; // OnEdit

void CDrawView::OnUpdateEdit( CCmdUI* pCmdUI )
// Alles Updates für das Menü 'Objekt editieren' kommen hier an
{
  BOOL bEnable = FALSE;
  switch( pCmdUI->m_nID )
  {
  case ID_EDIT_SELECT_ALL:
    bEnable = GetDocument()->GetObjects()->GetObjectCount() != 0;
    break;

  case ID_EDIT_CLEAR:
    {
      POSITION pos = m_selection.GetHeadPosition();
      while( pos != NULL )
      {
        CDrawObj* pObj = m_selection.GetNextObject( pos );
        if( pObj->IsUser() || pObj->IsHideable() )
        {
          bEnable = TRUE;
          break;
        }
      } // while pos
    }
    break;

  case ID_EDIT_COPY:
    bEnable = !m_selection.IsEmpty() || ( m_bEditing && m_bSelectedText );
    break;

  case ID_EDIT_CUT:
    {
      POSITION pos = m_selection.GetHeadPosition();
      while( pos != NULL )
      {
        if( m_selection.GetNextObject( pos )->IsUser() )
        {
          bEnable = TRUE;
          break;
        }
      } // while pos
      bEnable = bEnable || ( m_bEditing && m_bSelectedText );
    }
    break;

  case ID_EDIT_PASTE:
  case ID_EDIT_PASTE_SPECIAL:
    {
      // determine if private or standard OLE formats are on the clipboard
      COleDataObject dataObject;
      bEnable = dataObject.AttachClipboard() &&
        ( ( m_bEditing ? dataObject.IsDataAvailable(CF_TEXT) : dataObject.IsDataAvailable(GetDocument()->m_cfPrivate)) ||
        COleClientItem::CanCreateFromData(&dataObject) );
    }
    break;

  case ID_EDIT_UNDO:
    bEnable = CanUndo();
    break;
  
  default:
    ASSERT( FALSE ); // darf nicht sein
    break;
  } // switch nID

  pCmdUI->Enable( bEnable );
} // OnUpdateEdit

void CDrawView::AddToUndoBuffer( CDrawObjList* pObjList, int nHint )
{
	CDrawDoc* pDoc = GetDocument();

  CDrawObjList* pNewObjList = new CDrawObjList;
#ifdef _DEBUG
  if( m_bTraceUndo )
    TRACE( "Add to undo buffer: object list %x.\n", pNewObjList );
#endif

  CTypedPtrMap<CMapPtrToPtr, CDrawObj*, CDrawObj*> undoCheckObjs;
	
  // new undo buffer entry
  POSITION pos = pObjList->GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pOrgObj = pObjList->GetNextObject( pos );
    CDrawObj* pNewObj = NULL;

		if( undoCheckObjs.Lookup( pOrgObj, pNewObj ) )
			continue;	// object already added!

    switch( nHint )
    {
    case HINT_OBJ_HIDE:
      pNewObj = pOrgObj->Clone( pDoc, FALSE ); // es wird zwar kein neues gebraucht, aber trotzdem wird mit Kopie gearbeitet, 
      // da sonst möglicherweise mal das Original gelöscht wird
      break;

    case HINT_OBJ_DELETE:
      pNewObj = pOrgObj; // das alte wird ganz gelöscht, deswegen wird das alte gleich dem neuen
      break;

    default:
      pNewObj = pOrgObj->Clone( pDoc, TRUE ); // das alte wurde verändert, deswegen clonen wirs schnell vorher
      break;
    } // switch nHint

		// add to list and maps
		undoCheckObjs.SetAt( pOrgObj, pNewObj );
		pNewObjList->AddTailObject( pNewObj );
		m_undoMapOrgObjs.SetAt( pNewObj, pOrgObj );
#ifdef _DEBUG
		if( m_bTraceUndo )
			TRACE( "Undo buffer addition: Original %x, Copy %x.\n", pOrgObj, pNewObj );
#endif
    m_undoMapHints.SetAt( pNewObj, nHint );
  }
	m_undoBuffer.InsertIndex( 0, pNewObjList, FALSE );
	
  // remember selection
	pNewObjList = new CDrawObjList;
#ifdef _DEBUG
	if( m_bTraceUndo )
		TRACE( "Add to selection buffer: object list %x.\n", pNewObjList );
#endif
	pos = m_selection.GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pOrgObj = m_selection.GetNextObject( pos );
#ifdef _DEBUG
		if( m_bTraceUndo )
			TRACE( "Selection buffer addition: %x.\n", pOrgObj );
#endif
		pNewObjList->AddTailObject( pOrgObj );
	}
	m_undoSelections.InsertIndex( 0, pNewObjList, FALSE );

	int nSize = m_undoBuffer.GetSize();
	if( nSize > UNDO_BUFFER_MAX )
	{
#ifdef _DEBUG
    if( m_bTraceUndo )
      TRACE("Undo buffer too big: removed last undo buffer.\n" );
#endif
		m_undoBuffer.GetAt( nSize - 1 )->DeleteContents( FALSE );
		delete m_undoBuffer.GetAt( nSize - 1 );
		delete m_undoSelections.GetAt( nSize - 1 );
		m_undoBuffer.RemoveIndex( nSize - 1 );
		m_undoSelections.RemoveIndex( nSize - 1 );
  } // if nSize
} // AddToUndoBuffer

void CDrawView::AddToUndoBuffer( CDrawObj *pObj, int nHint )
{
	CDrawObjList objList;
	objList.AddTailObject( pObj );
	AddToUndoBuffer( &objList, nHint );
}

void CDrawView::AddToUndoBuffer(CString& text, int nStartPos, int nEndPos, int nCaretPos)
{
	CPoint pt(nStartPos, nEndPos);

	if (m_bEditing)
	{
		m_undoText.InsertAt(0, text);
		m_undoTextSels.InsertAt(0, pt);
		m_undoCaretPos.InsertAt(0, nCaretPos);
	}
}

BOOL CDrawView::SelectionHasChanged()
{
	if( m_undoBuffer.GetSize() > 0 )
	{
		POSITION pos;
		CDrawObj *pObj, *pOrgObj;

		pos = m_undoBuffer.GetAt( 0 )->GetHeadPosition();
		while( pos != NULL )
		{
			pOrgObj = m_undoBuffer.GetAt( 0 )->GetNextObject( pos );
			if( m_undoMapOrgObjs.Lookup( pOrgObj, pObj ) )
			{
				if( m_selection.FindObject( pObj ) != NULL )
				{
          if( pObj->IsMovedFrom( *pOrgObj ) )
            return TRUE;
				}
			}
		}
	}

	return FALSE;
}

void CDrawView::RemoveLastUndoBuffer()
{
#ifdef _DEBUG
	if (m_bTraceUndo)
		TRACE("Remove last undo buffer added.\n");
#endif
	if (m_undoBuffer.GetSize()>0)
	{
		m_undoBuffer.GetAt( 0 )->DeleteContents( FALSE );
		delete m_undoBuffer.GetAt( 0 );
		m_undoBuffer.RemoveIndex( 0 );
		delete m_undoSelections.GetAt( 0 );
		m_undoSelections.RemoveIndex( 0 );
	}
}

BOOL CDrawView::CanUndo()
{
	if (m_bEditing)
	{
		if (m_undoText.GetSize()>0)
			return TRUE;
	}
	else
	{
		if (m_undoBuffer.GetSize()>0)
			return TRUE;
	}
	return FALSE;
}

void CDrawView::EmptyUndoBuffer()
{
	m_undoBuffer.DeleteContents();
	for (int i=0; i<m_undoSelections.GetSize(); i++)
		delete m_undoSelections.GetAt( i );
	m_undoSelections.RemoveAllIndices();
}

void CDrawView::UpdateUndoBufferObjs()
{
	int i;
	POSITION pos1, pos2, pos3;
	CDrawObj *pObj1, *pOrgObj1;
	CDrawObj *pObj2, *pOrgObj2;

#ifdef _DEBUG
	if (m_bTraceUndo)
		TRACE("Update undo buffer.\n");
#endif
	if (m_undoBuffer.GetSize()>0)
	{
		pos1 = m_undoBuffer.GetAt( 0 )->GetHeadPosition();
		while (pos1!=NULL)
		{
			pObj1 = m_undoBuffer.GetAt( 0 )->GetNextObject( pos1 );
			if (pObj1->IsConnected())
			{
				CDrawObjList *pCons;

				// update connections in undo buffer
				if (m_undoMapOrgObjs.Lookup(pObj1, pOrgObj1))
				{
					for (i=0; i<m_undoBuffer.GetSize(); i++)
					{
						pos2 = m_undoBuffer.GetAt( i )->GetHeadPosition();
						while (pos2!=NULL)
						{
							pObj2 = m_undoBuffer.GetAt( i )->GetNextObject( pos2 );
							if (pObj2->IsConnected())
							{
								pCons = pObj2->GetConnections();
								pos3 = pCons->FindObject( pOrgObj1 );
								if (pos3!=NULL)
								{
#ifdef _DEBUG
									if (m_bTraceUndo)
										TRACE("Connection updated: Original %x replaced with copy %x.\n", pOrgObj1, pObj1);
#endif
									pCons->RemoveObjectAt( pos3 );
									pCons->AddTailObject( pObj1 );
								}
							}
						}
					}
				}
				// update all connections for buffer to be undone
				pCons = pObj1->GetConnections();
				pos2 = pCons->GetHeadPosition();
				while (pos2!=NULL)
				{
					CDrawObjList *pOrgCons;
					
					pObj2 = pCons->GetNextObject( pos2 );
					pOrgCons = pObj2->GetConnections();
					pos3 = pOrgCons->FindObject( pOrgObj1 );
					if (pos3!=NULL)
					{
#ifdef _DEBUG
						if (m_bTraceUndo)
							TRACE("Connection updated: Original %x replaced with copy %x.\n", pOrgObj1, pObj1);
#endif
						pOrgCons->RemoveObjectAt( pos3 );
						pOrgCons->AddTailObject( pObj1 );
					}
				}
			}
			if (m_undoBuffer.GetSize()>1)
			{
				// update original object map
				if (m_undoMapOrgObjs.Lookup(pObj1, pOrgObj1))
				{
					for (i=1; i<m_undoBuffer.GetSize(); i++)
					{
						pos2 = m_undoBuffer.GetAt( i )->GetHeadPosition();
						while (pos2!=NULL)
						{
							pObj2 = m_undoBuffer.GetAt( i )->GetNextObject( pos2 );
							if (m_undoMapOrgObjs.Lookup(pObj2, pOrgObj2))
							{
								if (pOrgObj1==pOrgObj2)
								{
#ifdef _DEBUG
									if (m_bTraceUndo)
										TRACE("Update undo map: Copy %x now maps to original %x.\n", pObj2, pObj1);
#endif
									m_undoMapOrgObjs.SetAt(pObj2, pObj1);
								}
							}
						}
					}
				}
			}
			if (m_undoMapOrgObjs.Lookup(pObj1, pOrgObj1))
			{
				// update original selection map
				for (i=0; i<m_undoSelections.GetSize(); i++)
				{
					pos2 = m_undoSelections.GetAt( i )->FindObject( pOrgObj1 );
					if (pos2!=NULL)
					{
#ifdef _DEBUG
						if (m_bTraceUndo)
							TRACE("Update selection buffer: %x replaced with %x.\n", pOrgObj1, pObj1);
#endif
						m_undoSelections.GetAt( i )->RemoveObjectAt( pos2 );
						m_undoSelections.GetAt( i )->AddTailObject( pObj1 );
					}
				}
			}
		}
	}
}

#ifdef _DEBUG
void CDrawView::OnObjectDebug() 
{
	CDebugDialog dlg(this);

	if (dlg.DoModal()==IDOK)
		GETDRAWAPP->m_bTraceUndo = m_bTraceUndo;
}
#endif

void CDrawView::PasteSpecial()
{
	COlePasteSpecialDialog dlg;
	dlg.AddStandardFormats();

	if (dlg.DoModal() != IDOK)
		return;

	// First create the C++ object
	CDrawOleObj* pObj = new CDrawOleObj(GetInitialPosition(), GetDocument());
	pObj->SetFlags(CDrawObj::moveable);
	ASSERT_VALID(pObj);
	CDrawItem* pItem = new CDrawItem(GetDocument(), pObj);
	ASSERT_VALID(pItem);
	pObj->m_pClientItem = pItem;

	// Now create the OLE object/item
	TRY
	{
		if (!dlg.CreateItem(pObj->m_pClientItem))
			AfxThrowMemoryException();

		// add the object to the document
		GetDocument()->AddObject( pObj, 0, 0 );

		pItem->UpdateItemType();

		// try to get initial presentation data
		pItem->UpdateLink();
		pItem->UpdateExtent();

		// if insert new object -- initially show the object
		if (dlg.GetSelectionType() == COleInsertDialog::createNewItem)
			pItem->DoVerb(OLEIVERB_SHOW, this);
	}
	CATCH_ALL(e)
	{
		// clean up item
		pItem->Delete();
		pObj->m_pClientItem = NULL;
		GetDocument()->RemoveObject( pObj );
		pObj->Remove();

		AfxMessageBox(IDP_FAILED_TO_CREATE);
	}
	END_CATCH_ALL

	// add object to undo buffer
	AddToUndoBuffer(pObj, HINT_OBJ_ADD);
	EndWaitCursor();
}

CClientDC* CDrawView::GetClientDC()
{
  delete m_pClientDC;
  m_pClientDC = NULL;

  if( !m_pClientDC )
  {
    m_pClientDC = new CClientDC( this );
    OnPrepareDC( m_pClientDC, NULL );
  }

  return m_pClientDC;
};
