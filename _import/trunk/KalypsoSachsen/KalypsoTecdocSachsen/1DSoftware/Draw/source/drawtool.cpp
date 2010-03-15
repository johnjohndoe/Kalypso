// drawtool.cpp - implementation for drawing tools

#include "stdafx.h"

#include "drawvw.h"
#include "drawdoc.h"
#include "draw.h"
#include "distdlg.h"

#include "drawtool.h"

/////////////////////////////////////////////////////////////////////////////
// CDrawTool implementation

CPtrList CDrawTool::c_tools;

static CSelectTool selectTool;
static CZoomTool zoominTool(zoomin);
static CZoomTool zoomoutTool(zoomout);
static CRectTool lineTool(line);
static CRectTool rectTool(rect);
static CRectTool ellipseTool(ellipse);
static CRectTool textboxTool(textbox);
static CPolyTool polylTool(polyl);
static CPolyTool polygTool(polyg);
static CRectTool measureTool(measure);

CPoint CDrawTool::c_down;
UINT CDrawTool::c_nDownFlags;
CPoint CDrawTool::c_last;
DrawShape CDrawTool::c_drawShape = selection;

CDrawTool::CDrawTool(DrawShape drawShape)
{
	m_drawShape = drawShape;
	c_tools.AddTail(this);
}

CDrawTool* CDrawTool::FindTool(DrawShape drawShape)
{
	POSITION pos = c_tools.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawTool* pTool = (CDrawTool*)c_tools.GetNext(pos);
		if (pTool->m_drawShape == drawShape)
			return pTool;
	}

	return NULL;
}

void CDrawTool::OnLButtonDown(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	// deactivate any in-place active item on this view!
	COleClientItem* pActiveItem = pView->GetDocument()->GetInPlaceActiveItem(pView);
	if (pActiveItem != NULL)
	{
		pActiveItem->Close();
		ASSERT(pView->GetDocument()->GetInPlaceActiveItem(pView) == NULL);
	}

	pView->SetCapture();
	c_nDownFlags = nFlags;
	c_down = point;
	c_last = point;
}

void CDrawTool::OnLButtonDblClk(CDrawView* /*pView*/, UINT /*nFlags*/, const CPoint& /*point*/)
{
}

void CDrawTool::OnLButtonUp(CDrawView* /*pView*/, UINT /*nFlags*/, const CPoint& point)
{
	ReleaseCapture();

	if( point == c_down )
		c_drawShape = selection;
}

void CDrawTool::OnMouseMove(CDrawView* pView, UINT /*nFlags*/, const CPoint& point)
{
	c_last = point;
	if (pView->GetCapture() != pView)
	{
		if (c_drawShape == selection && pView->m_selection.GetObjectCount() >= 1)
		{
			POSITION pos = pView->m_selection.GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pView->m_selection.GetNextObject(pos);
				CPoint local = point;
				pView->ClientToDoc(local);
				int nHandle = pObj->HitTest(local, pView, TRUE);
				if (nHandle < 0)	// Multiple selections can only be moved!
				{
					SetCursor(pObj->GetHandleCursor(nHandle));
					return;
				}
			}
		}
	}
	if (c_drawShape==zoomin || c_drawShape==zoomout)
		return;
	SetCursor(GETDRAWAPP->LoadStandardCursor(IDC_ARROW));
}

void CDrawTool::OnEditProperties(CDrawView* /*pView*/)
{
}

void CDrawTool::OnFormatSchrift(CDrawView* /*pView*/)
{
}

void CDrawTool::OnCancel()
{
	c_drawShape = selection;
}

////////////////////////////////////////////////////////////////////////////
// CSelectTool

enum SelectMode
{
	none,
	netSelect,
	move,
	size
};

SelectMode selectMode = none;
int nDragHandle;

CPoint lastPoint;

CSelectTool::CSelectTool()
	: CDrawTool(selection)
{
}

void CSelectTool::OnLButtonDown(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	CPoint local = point;
	pView->ClientToDoc( local );

	//CDrawObj* pObj;
	selectMode = none;

	// Check for resizing (only allowed on single selections)
	if( pView->m_selection.GetObjectCount() == 1 )
	{
		CDrawObj* pObj = pView->m_selection.GetHeadObject();
		nDragHandle = pObj->HitTest( local, pView, TRUE );
		if( nDragHandle > 0 )
		{
			// add selection to undo buffer
			pView->AddToUndoBuffer( &(pView->m_selection), HINT_OBJ_EDIT );
			selectMode = size;
		}
	}

	// See if the click was on an object, select and start move if so
	if( selectMode == none )
	{
		CDrawObj* pObj = pView->GetDocument()->ObjectAt( local );

		if( pObj && !pObj->IsInvisible() )
		{
			selectMode = move;

			if( !pView->IsSelected( pObj ) )
				pView->Select( pObj, ( nFlags & MK_SHIFT ) != 0 );

			// Ctrl+Click clones the selection...
      if( ( nFlags & MK_CONTROL ) != 0)
        pView->CloneSelection();

      // add selection to undo buffer
      pView->AddToUndoBuffer( &(pView->m_selection), HINT_OBJ_EDIT );
    }
  }

	// Click on background, start a net-selection
	if( selectMode == none )
	{
		if( ( nFlags & MK_SHIFT ) == 0 )
			pView->Select( NULL );

		selectMode = netSelect;

		CClientDC dc( pView );
		CRect rect( point.x, point.y, point.x, point.y );
    rect.NormalizeRect();
		dc.DrawFocusRect( rect );
	}

	lastPoint = local;
	CDrawTool::OnLButtonDown( pView, nFlags, point );
}

void CSelectTool::OnLButtonDblClk(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	if( ( nFlags & MK_SHIFT ) != 0 )
	{
		// Shift+DblClk deselects object...
		CPoint local = point;
		pView->ClientToDoc(local);
		CDrawObj* pObj = pView->GetDocument()->ObjectAt(local);
		if( pObj != NULL )
			pView->Deselect(pObj);
	}
	else
	{
		// "Normal" DblClk opens properties, or OLE server...
		if( pView->m_selection.GetObjectCount() > 0 )
			pView->m_selection.GetHeadObject()->OnOpen(pView);
	}

	CDrawTool::OnLButtonDblClk(pView, nFlags, point);
}

void CSelectTool::OnEditProperties( CDrawView* pView )
{
	if( pView->m_selection.GetObjectCount() > 0 )
		pView->m_selection.GetHeadObject()->OnEditProperties(pView);
}

void CSelectTool::OnFormatSchrift(CDrawView* pView)
{
	if( pView->m_selection.GetObjectCount() > 0 )
		pView->m_selection.GetHeadObject()->OnFormatSchrift( pView );
}

void CSelectTool::OnLButtonUp( CDrawView* pView, UINT nFlags, const CPoint& point )
{
	if( pView->GetCapture() == pView )
	{
    if( selectMode == netSelect )
		{
			CClientDC dc( pView );
			CRect rect( c_down.x, c_down.y, c_last.x, c_last.y );
      rect.NormalizeRect();

      dc.DrawFocusRect( rect );

      if( rect.Width() != 0 || rect.Height() != 0 )
        pView->SelectWithinRect( rect, TRUE );
		}
		else if( selectMode != none )
		{
			// check to see if objects in selection have changed
      if( !pView->SelectionHasChanged() )
        // falls sich nichts geändert hat, brauchen wir das letzte Undo nicht mehr
				pView->RemoveLastUndoBuffer();

      // auf jeden Fall alles neu zeichnen
			pView->GetDocument()->UpdateAllViews( pView );
		}
		selectMode = none;
	}

	CDrawTool::OnLButtonUp(pView, nFlags, point);
}

void CSelectTool::OnMouseMove( CDrawView* pView, UINT nFlags, const CPoint& point )
{
  // das Document wird immer gebraucht
  CDrawDoc* pDoc = pView->GetDocument();
  if( pDoc == NULL )
    return;

	if( pView->GetCapture() != pView )
	{
		if( c_drawShape == selection && pView->m_selection.GetObjectCount() == 1 )
		{
			CDrawObj* pObj = pView->m_selection.GetHeadObject();
			CPoint local = point;
			pView->ClientToDoc( local );
			int nHandle = pObj->HitTest( local, pView, TRUE );
			if( nHandle != 0 )
			{
				SetCursor( pObj->GetHandleCursor( nHandle ) );
				return; // bypass CDrawTool
			}
		}
		if( c_drawShape == selection )
			CDrawTool::OnMouseMove( pView, nFlags, point );
		return;
	}

	if( selectMode == netSelect )
	{
		CClientDC dc( pView );
		CRect rect( c_down.x, c_down.y, c_last.x, c_last.y );
    rect.NormalizeRect();
		dc.DrawFocusRect( rect );

		rect.SetRect( c_down.x, c_down.y, point.x, point.y );
		rect.NormalizeRect();
		dc.DrawFocusRect( rect );

		CDrawTool::OnMouseMove( pView, nFlags, point );
		return;
	}

	CPoint local = point;
	pView->ClientToDoc( local );

  // verhindern, dass der Cursor ausserhalb des Dokumentes landet
  CSize sizeDrawing = pDoc->GetDrawingSize();
  if( local.x < 0 )
    local.x = 0;
  if( local.y < 0 )
    local.y = 0;
  if( local.x > sizeDrawing.cx )
    local.x = sizeDrawing.cx;
  if( local.y > sizeDrawing.cy )
    local.y = sizeDrawing.cy;

  // SnapToGrid implementieren
	CSize sizeGrid = pDoc->m_pData->m_sizeGrid;
	if( pDoc->m_pData->m_bSnapToGrid )
	{
		local.x -= local.x % sizeGrid.cx;
		local.y -= local.y % sizeGrid.cy;
	}
	CPoint delta = (CPoint)(local - lastPoint);
	if( pDoc->m_pData->m_bSnapToGrid )
	{
		delta.x -= delta.x % sizeGrid.cx;
		delta.y -= delta.y % sizeGrid.cy;
	}

	POSITION pos = pView->m_selection.GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pObj = pView->m_selection.GetNextObject( pos );
		CIntIRect position( pObj->m_position );

		if( selectMode == move && ( pObj->IsMoveable() || pObj->IsOffsetable() ) )
    {
				position += delta;
				SetCursor( GETDRAWAPP->LoadStandardCursor( IDC_SIZEALL ) );
				pObj->MoveTo( position, pView );
    }
    else if( pObj->IsMoveable() && nDragHandle > 0 )
      pObj->MoveHandleTo( nDragHandle, local, pView );
  } // while pos

	lastPoint = local;

	if( selectMode == size && c_drawShape == selection )
	{
		c_last = point;
		SetCursor( pView->m_selection.GetHeadObject()->GetHandleCursor( nDragHandle ) );
		return; // bypass CDrawTool
	}

	c_last = point;

	if( c_drawShape == selection && selectMode != move )
		CDrawTool::OnMouseMove(pView, nFlags, point);
}

////////////////////////////////////////////////////////////////////////////
// CZoomTool

CZoomTool::CZoomTool(DrawShape nDrawShape)
	: CDrawTool(nDrawShape)
{
}

void CZoomTool::OnLButtonDown(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	CPoint local = point;
	pView->ClientToDoc(local);

	selectMode = none;
	// Click on background, start a net-selection
	if (selectMode == none)
	{
		selectMode = netSelect;

		CClientDC dc(pView);
		CRect rect(point.x, point.y, point.x, point.y);
    rect.NormalizeRect();
		dc.DrawFocusRect(rect);
	}

	lastPoint = local;
	CDrawTool::OnLButtonDown(pView, nFlags, point);
}

void CZoomTool::OnLButtonUp(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	if (pView->GetCapture() == pView)
	{
		if (selectMode == netSelect)
		{
			CClientDC dc(pView);
			CRect rect(c_down.x, c_down.y, c_last.x, c_last.y);
      rect.NormalizeRect();
			dc.DrawFocusRect(rect);

			if (!rect.IsRectEmpty())
			{
				int factor;
				CPoint pt;
				CRect client;

				pView->GetClientRect(&client);
				if (m_drawShape==zoomin)
				{
					factor = min(MulDiv(100, abs(client.Width()), abs(rect.Width())),
						MulDiv(100, abs(client.Height()), abs(rect.Height())));
					pt = CPoint(min(rect.left, rect.right), min(rect.top, rect.bottom));
					pView->ClientToDoc(pt);
					pView->m_nZoomFactor = MulDiv(pView->m_nZoomFactor, factor, 100);
					pView->DocToClient(pt);
					pt += pView->GetScrollPosition();
					pView->ResyncScrollSizes();
					pView->Invalidate();
					pView->ScrollToPosition(pt);
				}
				else
				{	// zoomout
					factor = min(MulDiv(100, abs(client.Width()), abs(rect.Width())),
						MulDiv(100, abs(client.Height()), abs(rect.Height())));
					pt = CPoint(abs(MulDiv(1, rect.left+rect.right, 2)), abs(MulDiv(1, rect.top+rect.bottom, 2)));
					pView->ClientToDoc(pt);
					pView->m_nZoomFactor = MulDiv(pView->m_nZoomFactor, 100, factor);
					pView->ClientToDoc(client);
					pt.x -= MulDiv(1, abs(client.Width()), 2);
					pt.y += MulDiv(1, abs(client.Height()), 2);
					CSize sizeDrawing = pView->GetDocument()->GetDrawingSize();
					pt.x = min(pt.x, sizeDrawing.cx-abs(client.Width()));
					pt.y = min(pt.y, sizeDrawing.cy);
					pt.x = max(0, pt.x);
					pt.y = max(abs(client.Height()), pt.y);
					pView->DocToClient(pt);
					pt += pView->GetScrollPosition();
					pView->ResyncScrollSizes();
					pView->Invalidate();
					pView->ScrollToPosition(pt);
				}
				if (GETDRAWAPP->m_hWndZoomBox!=NULL)
					::SendMessage(GETDRAWAPP->m_hWndZoomBox, CDrawApp::m_nZoomFactorChangedMsg, 0, 0);
			}
		}
		selectMode = none;
		c_drawShape = selection;
	}

	CDrawTool::OnLButtonUp(pView, nFlags, point);
}

void CZoomTool::OnMouseMove(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	SetCursor(GETDRAWAPP->LoadCursor(IDC_MAGNIFY));

	if (selectMode == netSelect)
	{
		CClientDC dc(pView);
		CRect rect(c_down.x, c_down.y, c_last.x, c_last.y);
		rect.NormalizeRect();
		dc.DrawFocusRect(rect);
		rect.SetRect(c_down.x, c_down.y, point.x, point.y);
		rect.NormalizeRect();
		dc.DrawFocusRect(rect);
	}

	CDrawTool::OnMouseMove(pView, nFlags, point);
}

////////////////////////////////////////////////////////////////////////////
// CRectTool (does rectangles, round-rectangles, and ellipses)

CRectTool::CRectTool(DrawShape drawShape)
	: CDrawTool(drawShape)
{
}

void CRectTool::OnLButtonDown(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	int i, j;
	CSize sizeGrid;
	
	CPoint local = point;
	pView->ClientToDoc( local );
	if( m_drawShape == textbox && !pView->m_bEditing )
	{
		CDrawObj* pDrawObj = pView->GetDocument()->ObjectAt( local );
		if( EditTextObject( pView, pDrawObj ) )
			return;
	}

	CDrawTool::OnLButtonDown( pView, nFlags, point );

	if (pView->GetDocument()->m_pData->m_bSnapToGrid)
	{
		sizeGrid = pView->GetDocument()->m_pData->m_sizeGrid;
		local.x -= local.x % sizeGrid.cx;
		local.y -= local.y % sizeGrid.cy;
	}

  CIntIRect localRect( local.x, local.y, local.x, local.y );
  localRect.NormalizeRect();

	CDrawRect* pObj = new CDrawRect( localRect, (CDrawDoc*)pView->GetDocument() );
	switch( m_drawShape )
	{
	default:
		ASSERT(FALSE); // unsuported shape!

	case rect:
		pObj->SetShape( CDrawRect::rectangle );
		break;

	case ellipse:
		pObj->SetShape( CDrawRect::ellipse );
		break;

	case measure:
	case line:
		pObj->SetShape( CDrawRect::line );
		break;

	case textbox:
		pObj->SetShape( CDrawRect::text );
		pObj->SetFlags( CDrawObj::editable );
		if( pView->m_bStempelText )
		{
			for ( i = 0; i < N_STPLTEXTS; i++ )
			{
				if( !pView->m_texts.Lookup( i, j ) )
				{
					pObj->SetStempelTextType( i );
					pView->GetDocument()->GetDefaultStempelText( i, pObj->m_text );
					pView->m_texts.SetAt( i, 1 );
					pObj->UnsetFlags( CDrawObj::editable );
					break;
				}
			}
		}
		break;
	}
	pObj->SetFlags( CDrawObj::moveable );
  pView->GetDocument()->AddObject( pObj, 0, 0 );
  pView->Select( pObj );
	// add object to undo buffer
	pView->AddToUndoBuffer(pObj, HINT_OBJ_ADD);
	pObj->Invalidate();
	selectMode = size;
	nDragHandle = 1;
	lastPoint = local;
}

void CRectTool::OnLButtonDblClk(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	CDrawTool::OnLButtonDblClk(pView, nFlags, point);
}

void CRectTool::OnLButtonUp( CDrawView* pView, UINT nFlags, const CPoint& point )
{
	if( point == c_down && !pView->m_bEditing )
	{
		// Don't create empty objects...
		CDrawObj* pObj = pView->m_selection.GetTailObject();
		pView->GetDocument()->RemoveObject( pObj );
		pView->RemoveLastUndoBuffer();
		pObj->Remove();
		selectTool.OnLButtonDown(pView, nFlags, point); // try a select!
	}
	else if( m_drawShape == measure )
	{
		CDrawObj *pObj = pView->m_selection.GetTailObject();
		CDrawDoc *pDoc = pView->GetDocument();
		CDistanceDialog dlg(pView);
		CIntPoint logPoint;
		CDoublePoint metPoint;

		logPoint.x = abs(pObj->m_position.left-pObj->m_position.right);
		logPoint.y = abs(pObj->m_position.top-pObj->m_position.bottom);
		pDoc->LogicalToMeters(logPoint, metPoint, NULL);
		dlg.m_distance.Format("%.3f", sqrt(metPoint.x*metPoint.x+metPoint.y*metPoint.y));
		logPoint.x = abs(pObj->m_position.left-pObj->m_position.right);
		logPoint.y = 0;
		pDoc->LogicalToMeters(logPoint, metPoint, NULL);
		dlg.m_XDistance.Format("%.3f", fabs(metPoint.x));
		logPoint.x = 0;
		logPoint.y = abs(pObj->m_position.top-pObj->m_position.bottom);
		pDoc->LogicalToMeters(logPoint, metPoint, NULL);
		dlg.m_YDistance.Format("%.3f", fabs(metPoint.y));

		pView->GetDocument()->RemoveObject( pObj );
		pView->RemoveLastUndoBuffer();
		pObj->Remove();
		pView->Invalidate();

		selectTool.OnLButtonUp(pView, nFlags, point);

		dlg.DoModal();
		return;
	}

	selectTool.OnLButtonUp( pView, nFlags, point );

	if( m_drawShape == textbox && !pView->m_bEditing && pView->m_selection.GetObjectCount() > 0 )
	{
		CDrawObj *pDrawObj = pView->m_selection.GetTailObject();
		if( EditTextObject( pView, pDrawObj ) )
			return;
	}
}

void CRectTool::OnMouseMove( CDrawView* pView, UINT nFlags, const CPoint& point )
{
	CPoint local = point;
	pView->ClientToDoc( local );
	if( m_drawShape == textbox )
	{
		CDrawObj* pDrawObj = pView->GetDocument()->ObjectAt( local );
		if( pDrawObj != NULL && pDrawObj->IsText() && pDrawObj->IsEditable() )
			SetCursor( GETDRAWAPP->LoadStandardCursor( IDC_IBEAM ) );
		else
			SetCursor( GETDRAWAPP->LoadStandardCursor( IDC_CROSS ) );
	}
	else
		SetCursor( GETDRAWAPP->LoadStandardCursor( IDC_CROSS ) );
	if( !pView->m_bEditing )
		selectTool.OnMouseMove( pView, nFlags, point );
}

BOOL CRectTool::EditTextObject(CDrawView* pView, CDrawObj* pObj)
{
	CFont font;
	CString str;

	if (pObj!=NULL && pObj->IsText() && pObj->IsEditable())
	{
		CRect rect = pObj->m_position;
		pView->DocToClient(rect);
		((CDrawRect*)pObj)->GetText(str);
		pView->BeginTextEdit((CDrawRect*)pObj);
		return TRUE;
	}
	return FALSE;
}

////////////////////////////////////////////////////////////////////////////
// CPolyTool

CPolyTool::CPolyTool(DrawShape drawShape)
	: CDrawTool(drawShape)
{
	m_pDrawObj = NULL;
}

void CPolyTool::OnLButtonDown(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	CSize sizeGrid;
	
	CDrawTool::OnLButtonDown(pView, nFlags, point);

	CPoint local = point;
	pView->ClientToDoc(local);
	if (m_pDrawObj!=NULL && pView->GetDocument()->m_pData->m_bSnapToGrid)
	{
		sizeGrid = pView->GetDocument()->m_pData->m_sizeGrid;
		local.x -= local.x % sizeGrid.cx;
		local.y -= local.y % sizeGrid.cy;
	}

	if (m_pDrawObj == NULL)
	{
		pView->SetCapture();

		m_pDrawObj = new CDrawPoly(CIntIRect(local.x, local.y, local.x, local.y), (CDrawDoc*)pView->GetDocument());
		switch (m_drawShape)
		{
		default:
			ASSERT(FALSE); // unsuported shape!
			
		case polyl:
			m_pDrawObj->SetShape( CDrawPoly::polyline );
			break;
			
		case polyg:
			m_pDrawObj->SetShape( CDrawPoly::polygon );
			break;
		}
		m_pDrawObj->SetFlags( CDrawObj::moveable );
		pView->GetDocument()->AddObject( m_pDrawObj, 0, 0 );
		pView->Select(m_pDrawObj);
		m_pDrawObj->AddPoint(local, pView);
		// add object to undo buffer
		pView->AddToUndoBuffer(m_pDrawObj, HINT_OBJ_ADD);
	}
	else if (local == m_pDrawObj->m_points[0])
	{
		// Stop when the first point is repeated...
		ReleaseCapture();
		m_pDrawObj->m_nPoints -= 1;
		if (m_pDrawObj->m_nPoints < 2)
		{
			m_pDrawObj->Remove();
		}
		else
		{
			pView->InvalObj(m_pDrawObj);
			// add object to undo buffer
			pView->AddToUndoBuffer(m_pDrawObj, HINT_OBJ_EDIT);
		}
		m_pDrawObj = NULL;
		c_drawShape = selection;
		return;
	}

	local.x += 1; // adjacent points can't be the same!
	m_pDrawObj->AddPoint(local, pView);

	selectMode = size;
	nDragHandle = m_pDrawObj->GetHandleCount();
	lastPoint = local;
}

void CPolyTool::OnLButtonUp(CDrawView* /*pView*/, UINT /*nFlags*/, const CPoint& /*point*/)
{
	// Don't release capture yet!
}

void CPolyTool::OnMouseMove(CDrawView* pView, UINT nFlags, const CPoint& point)
{
	if (m_pDrawObj != NULL && (nFlags & MK_LBUTTON) != 0)
	{
		CPoint local = point;
		pView->ClientToDoc(local);
		m_pDrawObj->AddPoint(local);
		nDragHandle = m_pDrawObj->GetHandleCount();
		lastPoint = local;
		c_last = point;
		SetCursor(GETDRAWAPP->LoadCursor(IDC_PENCIL));
	}
	else
	{
		SetCursor(GETDRAWAPP->LoadStandardCursor(IDC_CROSS));
		selectTool.OnMouseMove(pView, nFlags, point);
	}
}

void CPolyTool::OnLButtonDblClk(CDrawView* pView, UINT , const CPoint& )
{
	ReleaseCapture();

	int nPoints = m_pDrawObj->m_nPoints;
	if (nPoints > 2 &&
		(m_pDrawObj->m_points[nPoints - 1] == m_pDrawObj->m_points[nPoints - 2] ||
		m_pDrawObj->m_points[nPoints - 1].x - 1 == m_pDrawObj->m_points[nPoints - 2].x &&
		m_pDrawObj->m_points[nPoints - 1].y == m_pDrawObj->m_points[nPoints - 2].y))

	{
		// Nuke the last point if it's the same as the next to last...
		m_pDrawObj->m_nPoints -= 1;
		pView->InvalObj(m_pDrawObj);
	}

	m_pDrawObj = NULL;
	c_drawShape = selection;
}

void CPolyTool::OnCancel()
{
	CDrawTool::OnCancel();

	m_pDrawObj = NULL;
}

/////////////////////////////////////////////////////////////////////////////
