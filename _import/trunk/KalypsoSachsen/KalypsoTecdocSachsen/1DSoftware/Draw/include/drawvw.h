// drawvw.h : interface of the CDrawView class
//

#ifndef _DRAWVIEW_H_INCLUDED_
#define _DRAWVIEW_H_INCLUDED_

// Hints for UpdateAllViews/OnUpdate
#define HINT_UPDATE_WINDOW      0
#define HINT_UPDATE_DRAWOBJ     1
#define HINT_UPDATE_SELECTION   2
#define HINT_DELETE_SELECTION   3
#define HINT_UPDATE_OLE_ITEMS   4

// Hints for Undo operations
#define HINT_OBJ_DELETE		0
#define HINT_OBJ_ADD			1
#define HINT_OBJ_EDIT			2
#define HINT_OBJ_HIDE     3

#define UNDO_BUFFER_MAX			20

#include "drawobj.h"

class CDrawView : public CScrollView
{
protected: // create from serialization only
	CDrawView();
	DECLARE_DYNCREATE(CDrawView)

	int m_nZoomFactor;

// Attributes
public:
	CDrawDoc* GetDocument();
	CIntIRect GetInitialPosition();

// Operations
public:
	// coordinate system transformations
	void DocToClient( CRect& rect );
	void DocToClient( CPoint& point );
	void ClientToDoc( CPoint& point );
	void ClientToDoc( CRect& rect );

	// object selection
	void Select(CDrawObj* pObj, BOOL bAdd = FALSE);
	void SelectWithinRect( const CRect& selRect, const BOOL bAdd = FALSE );
	void Deselect(CDrawObj* pObj);
	virtual void CloneSelection();

	// updating of objects
	void UpdateActiveItem();
	void InvalObj(CDrawObj* pObj);
	void InvalAll();
	void Remove(CDrawObj* pObj);
	// paste
	void PasteNative(COleDataObject& dataObject);
	void PasteEmbedded(COleDataObject& dataObject, CPoint point );
	void PasteSpecial(COleDataObject& dataObject, CPoint point );
	void PasteText(COleDataObject& dataObject);
	
  // update scroll sizes
	void ResyncScrollSizes();
	
  // text editing
	virtual void BeginTextEdit(CDrawRect* pDrawRect);
	virtual void EndTextEdit();
protected:
	BOOL CreateCaret();
	void MoveCaret();

  virtual void Undo();
  virtual void PasteSpecial();
  virtual void Clear();
  virtual void Copy();
  virtual void Cut();
  virtual void Paste();
  virtual void Props() {};

// Implementation
protected:
	// added for drop-target functionality
	COleDropTarget m_dropTarget; 		// for drop target functionality
	CPoint m_dragPoint;					// current position
	CSize m_dragSize;					// size of dragged object
	CSize m_dragOffset;					// offset between pt and drag object corner
	DROPEFFECT m_prevDropEffect;
	BOOL m_bDragDataAcceptable;
	CDrawRect *m_pDrawRect;	// text to be edited
	// text editing stuff
	BOOL m_bSelectedText, m_bCanSelectText;
	int m_nCaretPos;
	CPoint m_sizeCaret;
	CBitmap m_CaretBitmap;

	BOOL GetObjectInfo(COleDataObject* pDataObject,
		CSize* pSize, CSize* pOffset);
	// end of drop-target additions

	// added for undo functionality
	// text edit undo
	CStringArray m_undoText;
	CArray<CPoint, CPoint> m_undoTextSels;
	CArray<int, int> m_undoCaretPos;
	void AddToUndoBuffer(CString& text, int nStartPos, int nEndPos, int nCaretPos);
	// drawing object undo
	BOOL CanUndo();
	void UpdateUndoBufferObjs();
	CDrawObjListArray m_undoBuffer;
	CDrawObjListArray m_undoSelections;
	CTypedPtrMap<CMapPtrToPtr, CDrawObj*, CDrawObj*> m_undoMapOrgObjs;	// maps objects in undo buffer to originals
	CMapPtrToWord m_undoMapHints;

private:
  CClientDC* GetClientDC();
  CClientDC* m_pClientDC;

public:
	void EmptyUndoBuffer();
	void AddToUndoBuffer(CDrawObjList *pObjList, int nHint);
	void AddToUndoBuffer(CDrawObj *pObj, int nHint);
	BOOL SelectionHasChanged();
	void RemoveLastUndoBuffer();
	// end of undo additions
	 
public:
	virtual ~CDrawView();
	virtual void DeleteContents();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
	BOOL m_bTraceUndo;
	BOOL m_bShowPosInfo;
#endif

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDrawView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual void OnInitialUpdate(); // called first time after construct
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnActivateView(BOOL bActivate, CView* pActiveView, CView* pDeactiveView);
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual BOOL OnScrollBy(CSize sizeScroll, BOOL bDoScroll);
	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);  // overriden to record time/date
	//}}AFX_VIRTUAL

	void DrawGrid( CDC* pDC, const CSize& gridSize );

	// added for drop-target functionality
	virtual BOOL OnDrop(COleDataObject* pDataObject,
		DROPEFFECT dropEffect, CPoint point);
	virtual DROPEFFECT OnDragEnter(COleDataObject* pDataObject,
		DWORD grfKeyState, CPoint point);
	virtual DROPEFFECT OnDragOver(COleDataObject* pDataObject,
		DWORD grfKeyState, CPoint point);
	virtual void OnDragLeave();
	static CLIPFORMAT m_cfObjectDescriptor;
	// end of drop-target additions

public:
	CDrawObjList m_selection;
	BOOL m_bGrid;
	COLORREF m_gridColor;
	BOOL m_bActive; // is the view active?
	// stempel text info
	BOOL m_bStempelText;
	CMap<int, int, int, int> m_texts;
	BOOL m_bEditing;	// in text edit mode

// OLE Container support
public:
	virtual BOOL IsSelected(const CObject* pDocItem) const;

// Generated message map functions
protected:
  afx_msg void OnUpdateEdit( CCmdUI* pCmdUI );
  afx_msg BOOL OnEdit( UINT nID );

	//{{AFX_MSG(CDrawView)
	afx_msg void OnInsertObject();
	afx_msg void OnCancelEditCntr();
	afx_msg void OnCancelEditSrvr();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnDrawSelect();
	afx_msg void OnDrawRect();
	afx_msg void OnDrawLine();
	afx_msg void OnDrawText();
	afx_msg void OnDrawEllipse();
	afx_msg void OnUpdateDrawEllipse(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawLine(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawRect(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawText(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawSelect(CCmdUI* pCmdUI);
	afx_msg void OnUpdateSingleSelect(CCmdUI* pCmdUI);
	afx_msg void OnDrawPolyline();
	afx_msg void OnUpdateDrawPolyline(CCmdUI* pCmdUI);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnObjectMoveBack();
	afx_msg void OnObjectMoveForward();
	afx_msg void OnObjectMoveToBack();
	afx_msg void OnObjectMoveToFront();
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	afx_msg void OnObjectProperties();
	afx_msg void OnUpdateObjectProperties(CCmdUI* pCmdUI);
	afx_msg void OnDestroy();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnViewGrid();
	afx_msg void OnUpdateViewGrid(CCmdUI* pCmdUI);
	afx_msg void OnViewShowobjects();
	afx_msg void OnUpdateViewShowobjects(CCmdUI* pCmdUI);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnDrawPolygon();
	afx_msg void OnUpdateDrawPolygon(CCmdUI* pCmdUI);
	afx_msg void OnViewZoomin();
	afx_msg void OnUpdateViewZoomin(CCmdUI* pCmdUI);
	afx_msg void OnViewZoomout();
	afx_msg void OnUpdateViewZoomout(CCmdUI* pCmdUI);
	afx_msg void OnDrawMeasure();
	afx_msg void OnUpdateDrawMeasure(CCmdUI* pCmdUI);
	//}}AFX_MSG
	// Debug
#ifdef _DEBUG
	afx_msg void OnObjectDebug();
#endif
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in Drawvw.cpp
inline CDrawDoc* CDrawView::GetDocument()
   { return (CDrawDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

#endif _DRAWVIEW_H_INCLUDED_