// drawobj.h - interface for CDrawObj and derivatives
//

#ifndef __DRAWOBJ_H__
#define __DRAWOBJ_H__

#define FACTOR_ARROW  10
#define HANDLE_WIDTH  0.75  // handle width (mm)

class CDrawView;
class CDrawDoc;
class CDrawObj;
class CDrawLayer;
class CDXFZeichnung;
class CDrawObjList;
class CDrawObjListArray;

#include "bce/include/AssocNN.h"
#include "commonMfc/include/rect.h"
#include "log4cpp/category.hh"

typedef BCE::AssocNN<CDrawObj*, CDrawLayer*> ObjLayerAssoc;

extern BOOL operator!=(const LOGFONT& lf1, const LOGFONT& lf2);

/////////////////////////////////////////////////////////////////////////////
// CDrawObj - base class for all 'drawable objects'
class CDrawObj : public CObject
{
protected:
  DECLARE_SERIAL(CDrawObj);
  CDrawObj(); // soll nur von der serialisierung aufgerufen werden
  
  // Constructors
public:
  CDrawObj( const CIntIRect& position, CDrawDoc* pDoc );
  CDrawObj( const CDoubleIRect& position, CDrawDoc* pDoc );

private:
  Init( const CIntIRect& position, const CDoubleIRect& dPosition, CDrawDoc* pDoc );

public:
  // Attributes
  CIntIRect m_position;
  CDoubleIRect m_dPosition;
  CDrawDoc* m_pDocument;
  CDrawObj *m_pThisDrawObj; // pointer to this object (only used when serializing connections)
#ifdef _DEBUG
  BOOL m_bBreakInDraw;
  BOOL m_bBreakWhenIntersected;
#endif

  void SetLogBrush( const COLORREF color, const UINT style, const LONG hatch );
  LOGFONT GetLogFont() const { return m_logfont; }
  LOGPEN GetLogPen() const { return m_logpen; }
  long TransformFontHeight( CDC *pDC, const long height );
  COLORREF GetTextColor() const { return m_colorText; }
  virtual int GetHandleCount();
  virtual CPoint GetHandle(int nHandle);
  CIntIRect GetHandleRect(int nHandleID, CDrawView* pView);
  virtual HCURSOR GetHandleCursor(int nHandle);
  virtual void SetLineColor(COLORREF color);
  virtual COLORREF GetFillColor() { return m_logbrush.lbColor; }
  virtual void SetLineStyle(int style);
  virtual void SetFillColor( COLORREF color );
  virtual void SetType(int type);
  virtual int GetType() { return m_nType; }
  virtual void SetTextOrientation(int angle);
  virtual int GetTextOrientation() { return (int)(m_logfont.lfEscapement/10); }
  void SetFlags(long flags, BOOL bUpdate = FALSE);
  void UnsetFlags(long flags, BOOL bUpdate = FALSE);
  BOOL HasFont() const;
  BOOL HasPen() const;
  BOOL HasBrush() const;
  BOOL HasArrow() const;
  BOOL IsFilled() const;
  BOOL IsUser() const;
  BOOL IsInvisible() const;
  BOOL IsEditable() const;
  BOOL IsMoveable() const;
  BOOL IsOffsetable() const;
  BOOL IsHideable() const;
  BOOL IsConnected() const { return m_pConnections != NULL; }
  BOOL IsClipped() const { return m_bClip; }
  void SetClipped( const BOOL bClip ) { m_bClip = bClip; };
  virtual BOOL IsText() { return FALSE; }
  virtual BOOL IsLine() { return FALSE; }
  int LeftArrow() { return m_nLeftArrow; }
  int RightArrow() { return m_nRightArrow; }
  void AddConnection(CDrawObj* pObj);
  void RemoveConnections( BOOL bDeleteContents );
  virtual void UpdateConnections();
  CDrawObjList* GetConnections() { return m_pConnections; }
  void SetSectionIndex(int n) { m_nSectionIndex = n; }
  int GetSectionIndex() { return m_nSectionIndex; }
  void SetDataBlockIndex(int n) { m_nDataBlockIndex = n; }
  int GetDataBlockIndex() { return m_nDataBlockIndex; }
  void SetOffset(CIntPoint& offset) { m_ptOffset = offset; }
  CIntPoint GetOffset() const { return m_ptOffset; }
  
  void SetClipRange( double from, double to, double bottom );
  double GetFrom() const { return m_dFrom; };
  double GetTo() const { return m_dTo; };
  double GetBottom() const { return m_dBottom; };
  
  void DeletePattern();

  BOOL IsMovedFrom( const CDrawObj& pOther ) const;

  void SetLayer( const int layer ) { m_nLayer = layer; };
  int GetLayer() const { return m_nLayer; };

  void SetIndex( const int index ) { m_nIndex = index; };
  int GetIndex() const { return m_nIndex; };

  CDrawLayer* GetDrawLayer() const { return m_pDrawLayer; };
  void SetDrawLayer( CDrawLayer* pLayer );
  
  // Operations
  virtual void Draw(CDC* pDC) {};
  enum TrackerState { normal, selected, active };
  virtual void DrawTracker(CDC* pDC, TrackerState state);
  virtual void MoveTo(const CIntIRect& positon, CDrawView* pView = NULL);
  virtual int HitTest(CPoint point, CDrawView* pView, BOOL bSelected);
  virtual BOOL Intersects( const CIntIRect& rect ) const;
  virtual BOOL IsContained( const CIntIRect& rect ) const;
  virtual void MoveHandleTo(int nHandle, CIntPoint point, CDrawView* pView = NULL);
  virtual void OnOpen(CDrawView* pView);
  virtual void OnEditProperties(CDrawView* pView);
  virtual void OnFormatSchrift(CDrawView* pView);
  virtual CDrawObj* Clone(CDrawDoc* pDoc = NULL, BOOL bExactCopy = FALSE );
  virtual void Remove();
  virtual void Invalidate(BOOL bConnections = TRUE);
  virtual CIntIRect GetAdjustedRect() const  { return m_position; };
  virtual CIntIRect GetClippedRect() const { return GetAdjustedRect(); };
  virtual void SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList ) {}; // rein virtual

  void ScaleText( const double faktor );
  
  virtual void AddToDXF( CDXFZeichnung* zn, const CString& layerName ) {};
  
  virtual void CalcDrawingHelpers(CDrawView *pView) {};

  virtual CDoubleIRect CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs );
  
  enum
  {
    font = 0x0001,
      pen = 0x0002,
      brush = 0x0004,
      arrow = 0x0008,
      filled = 0x0010,
      user = 0x0020,
      invisible = 0x0040,
      editable = 0x0080,
      moveable = 0x0100,
      offsetable = 0x0200,
      hideable = 0x0800
  };
  
  // Implementation
public:
  virtual ~CDrawObj();
  virtual void Serialize( CArchive& ar );

  void AssertValid();

#ifdef _DEBUG
    void debug( log4cpp::Category& cat ) const;
#endif _DEBUG
  
  // implementation data
protected:
  long x_flags;
  LOGFONT m_logfont;
  LOGPEN m_logpen;
  LOGBRUSH m_logbrush;
  COLORREF m_colorText;
  int m_nType;
  int m_nLayer; // gibt an, zu welchem 'Layer' das Objekt gehört ( Profil, User, Stempel, Tabelle, etc. )
  int m_nIndex; // noch ein zusätzlicher Index, bestimmt, in welchem Tabelleintrag das Objekt ist
  enum ArrowHead { noarrow, open, closed, solid };
  ArrowHead m_nLeftArrow; // for lines
  ArrowHead m_nRightArrow;  // for lines
  CDrawObjList* m_pConnections;
  int m_nSectionIndex;
  int m_nDataBlockIndex;
  CIntPoint m_ptOffset;  // for formatting
  
  BOOL m_bClip;
  double m_dFrom, m_dTo, m_dBottom;
  
  HBITMAP m_hPattern;

private:
  CDrawLayer* m_pDrawLayer;

public:
  // temporary drawing helpers (used in an effort to speed up drawing)
  // m_helpers[0] is always the height of the font in logical units
  // the rest of the helpers are specific to the type of object
  int m_helpers[21];
  
  friend class CRectTool;
  friend class CLinePropPage;
  friend class CSolidPropPage;
  friend class CTextPropPage;
  friend class CLinePage;
  friend class CTextPage;
  friend class CSolidPage;
};

// special 'list' class for this application (requires afxtempl.h)
// Nur wegen CDrawDoc::UpdateAllViews von CObject abgeleitet
class CDrawObjList : public CObject
{
  /////////////////////////////////
  // Konstruktion // Destruktion //
  /////////////////////////////////
public:
  CDrawObjList();

  virtual ~CDrawObjList() {};

  void DeleteContents( BOOL bDestroyConns );
  void RemoveConnections( BOOL bDestroyConns );
  
  ///////////////
  // Attribute //
  ///////////////
public:
  CSize GetRectSize() const { return CSize( m_rect.Width(), m_rect.Height() ); }
  void SetRectSize( const CSize& size );

  CIntPoint GetRectPos() const { return CIntPoint( m_rect.left, m_rect.bottom ); };
  void SetRectPos( const CIntPoint& pos );
  
  CIntIRect GetRect() const { return m_rect; };
  void SetRect( const CIntIRect& rect ) { m_rect = rect; };
  
  void SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList );

  void SetLayer( const int layer );

  void SetIndex( const int index );

  CSize GetExtent() const;
  CIntIRect CalcRect() const;

protected:
  CIntIRect m_rect;
  CTypedPtrList<CObList, CDrawObj*> m_list;

  /////////////////
  // Operationen //
  /////////////////
public:
  void Serialize( CArchive& ar );
  void SerializeNew( CArchive& ar );

  void Intersect( CDrawObjList* pObs, BOOL bDestroyObs );

  void SetFlags( long flags );
  void UnsetFlags( long flags );

  void ScaleText( const double faktor );

  void debug( log4cpp::Category& cat ) const;

  //////////////////////////////////
  // Facade für die CTypedPtrList //
  //////////////////////////////////
public:
  int GetObjectCount() const { return m_list.GetCount(); }
  POSITION GetHeadPosition( ) const { return m_list.GetHeadPosition(); };
  POSITION GetTailPosition( ) const { return m_list.GetTailPosition(); };
  CDrawObj* GetNextObject( POSITION& pos ) const { return m_list.GetNext( pos ); };
  CDrawObj* GetPrevObject( POSITION& pos ) const { return m_list.GetPrev( pos ); };
  CDrawObj*& GetHeadObject() { return  m_list.GetHead(); };
  CDrawObj* GetHeadObject() const { return  m_list.GetHead(); };
  CDrawObj* GetTailObject() const { return  m_list.GetTail(); };
  CDrawObj*& GetObject( POSITION position ) { return m_list.GetAt( position ); };
  CDrawObj* GetAt( POSITION position ) const { return m_list.GetAt( position ); };
  POSITION FindObject( CDrawObj* pObj, POSITION startAfter = NULL ) const { return m_list.Find( pObj, startAfter ); };
  void RemoveAllObjects();
  POSITION AddTailObject( CDrawObj* pObj ) { return m_list.AddTail( pObj ); }
  void AddTailObjects( CDrawObjList* pObjs );
  void AddTailObjects( CDrawObjListArray* pObjsArray );
  POSITION AddHeadObject( CDrawObj* pObj ) { return m_list.AddHead( pObj ); };
  BOOL RemoveObject( CDrawObj* pObj );
  void RemoveObjectAt( POSITION pos ) { m_list.RemoveAt( pos ); };
  POSITION InsertBeforeObject( POSITION position, CDrawObj* pObj ) { return m_list.InsertBefore( position, pObj ); };
  POSITION InsertAfterObject( POSITION position, CDrawObj* pObj ) { return m_list.InsertAfter( position, pObj ); };
  BOOL IsEmpty() const { return m_list.IsEmpty(); };
  void CopyTo( CDrawObjList* pOther );
};

// CDrawObjListArray: eine Facade für CTypedPtrArray<CObArray, CDrawObjList*>
// wurde nur protected vererbt, dass nieman deinfach so Objekte hinzufügen
// oder löschen kann; alles andere wird durchgeschleift
class CDrawObjListArray : public CObject
{
public:
  /////////////////////////////////
  // Konstruktion // Destruktion //
  /////////////////////////////////
  CDrawObjListArray();
  virtual ~CDrawObjListArray() {};

  void DeleteContents();
  void DeleteAllIndices( const BOOL bDestroyLists, const BOOL bDestroyObjs );

  ///////////////
  // Attribute //
  ///////////////
public:
  int FindObjectIndex( CDrawObj* pObj );
  
  CSize GetRectSize() const { return CSize( m_rect.Width(), m_rect.Height() ); }
  void SetRectSize( const CSize& size );
  
  CPoint GetRectPos() const { return CPoint( m_rect.left, m_rect.bottom ); };
  void SetRectPos( const CPoint& pos );
  
  CIntIRect GetRect() const { return m_rect; };
  void SetRect( const CIntIRect& rect ) { m_rect = rect; };

  void SetLayer( const int layer );

  /////////////////
  // Operationen //
  /////////////////
public:
  int CreateNewIndex();

  void AddObject( int index, CDrawObj* pObj );
  BOOL RemoveObject( CDrawObj* pObj, int* index = NULL );

  void SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList );

  void InitIndex();
  
  void Serialize( CArchive& ar );

  void SetFlags( long flags );
  void UnsetFlags( long flags );


#ifdef _DEBUG
  void Dump( CDumpContext& dc ) const;
#endif _DEBUG
  
protected:
  CIntIRect m_rect;
  CTypedPtrArray<CPtrArray, CDrawObjList*> m_array;

  //////////////////////////////////////////////////////////
  // Facade für CTypedPtrArray<CObArray, CDrawObjList*>:: //
  //////////////////////////////////////////////////////////
public:
  void RemoveIndex( const int nIndex, const int nCount = 1 ) { m_array.RemoveAt( nIndex, nCount ); };
  void RemoveAllIndices() { m_array.RemoveAll(); };
  void InsertIndex( int nIndex, CDrawObjList* newElement, BOOL bChangeIndex );
  int GetSize( ) const { return m_array.GetSize(); };
  void SetSize( int nNewSize, int nGrowBy = -1 ) { m_array.SetSize( nNewSize, nGrowBy ); };
  CDrawObjList* GetAt( int nIndex ) const { return m_array.GetAt( nIndex ); };
  void AddList( CDrawObjList* newElement ) { m_array.Add( newElement ); };

  void CopyTo( CDrawObjList* pObjList );
}; // class CDrawObjListArray

////////////////////////////////////////////////////////////////////////
// specialized draw objects

class CDrawRect : public CDrawObj
{
protected:
  DECLARE_SERIAL(CDrawRect);
  CDrawRect();
  virtual ~CDrawRect() {};
  
public:
  CDrawRect( const CIntIRect& position, CDrawDoc* pDoc );
  CDrawRect( const CDoubleIRect& position, CDrawDoc* pDoc );

private:
  void InitializeRect();
  
  // Implementation
public:
  virtual void Serialize(CArchive& ar);
  virtual void Draw(CDC* pDC);
  virtual void DrawTracker(CDC* pDC, TrackerState state);
  virtual int GetHandleCount();
  virtual CPoint GetHandle(int nHandle);
  virtual HCURSOR GetHandleCursor(int nHandle);
  virtual void MoveHandleTo(int nHandle, CIntPoint point, CDrawView* pView = NULL);
  virtual BOOL Intersects( const CIntIRect& rect ) const;
//  virtual BOOL IsContained( const CIntIRect& rect ) const;
  virtual CDrawObj* Clone(CDrawDoc* pDoc, BOOL bExactCopy = FALSE);
  void SetShape(int nShape);
  int GetShape() { return m_nShape; }
  void SetHorzJust(int nJust);
  void SetVertJust(int nJust);
  int GetHorzJust() { return m_nHorzJust; }
  int GetVertJust() { return m_nVertJust; }
  virtual CIntIRect GetAdjustedRect() const;
  virtual CIntIRect GetClippedRect() const;
  void SetTextType(int nType);
  int GetTextType() { return m_nTextType; }
  void SetStempelTextType(int nType);
  int GetStempelTextType() { return m_nStempelTextType; }
  void SetText( const CString& text ) { m_text = text; }
  void GetText(CString& text);
  void SetArcLeft( CDoublePoint& left ) { m_arcLeft = left; }
  void SetArcCenter(CDoublePoint& center) { m_arcCenter = center; }
  void SetArcRight(CDoublePoint& right) { m_arcRight = right; }
  virtual BOOL IsText() { return m_nShape==text; }
  virtual BOOL IsLine() { return m_nShape==line; }
  CSize GetOutputTextSize( CView* pView );
  void SetHorzAdjust(int adjust) { m_nHorzAdjust = adjust; }
  int GetHorzAdjust() { return m_nHorzAdjust; }
  void SetVertAdjust(int adjust) { m_nVertAdjust = adjust; }
  int GetVertAdjust() { return m_nVertAdjust; }
  void SetTextHeight(int height);
  void SetTextSelStartPos(int nPos) { m_nSelStartPos = nPos; }
  void SetTextSelEndPos(int nPos) { m_nSelEndPos = nPos; }
  int GetTextSelStartPos() { return m_nSelStartPos; }
  int GetTextSelEndPos() { return m_nSelEndPos; }
  BOOL ShowNegative() { return m_bShowNegative; }
  void SetShowNegative(BOOL bShow) { m_bShowNegative = bShow; }
  int GetPrecision() { return m_nPrecision; }
  void SetPrecision(int n) { m_nPrecision = n; }
  
  virtual void AddToDXF(CDXFZeichnung* zn, const CString& layerName);
  virtual void SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList );

  enum Shape { rectangle, ellipse, line, text, egg, mouth, arc };
  enum Justification { left, center, right, nojust }; // used for both horizontal and vertical (left==top, right==bottom)
  enum TextType { normal, xcoord, ycoord };
  
public:
  void Dump( CDumpContext& dc ) const;

protected:
  Shape m_nShape;
  Justification m_nHorzJust;  // for text and lines
  Justification m_nVertJust;  // for text and lines
  TextType m_nTextType;   // for text
  int m_nStempelTextType; // for stempel text fields
  CString m_text;   // for text
  CDoublePoint m_arcLeft, m_arcCenter, m_arcRight;     // for arcs
  /**** horz adjust used for formatting ****/
  // ==-1       : no adjustment
  // !=-1 && line   : m_position.right += m_nRectAdjust
  // !=-1 && xcoord : m_position.left = m_position.right - m_nRectAdjust
  // !=-1 && ycoord : m_position.right = m_position.left + m_nRectAdjust
  // !=-1 && rectangle: m_position.right += m_nRectAdjust and m_position.left -= m_nRectAdjust
  int m_nHorzAdjust;
  /**** vert adjust used for formatting ****/
  // ==-1       : no adjustment
  // !=-1 && rectangle: m_position.top += m_nRectAdjust and m_position.bottom -= m_nRectAdjust
  int m_nVertAdjust;
  BOOL m_bShowNegative;
  int m_nPrecision;
  
  // temporary drawing helpers
  virtual void CalcDrawingHelpers(CDrawView *pView);
  CSize m_sizeText;

  virtual CDoubleIRect CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs );

  // text selection positions
  int m_nSelStartPos, m_nSelEndPos;

  friend class CRectTool;
  friend class CLinePropPage;
  friend class CTextPropPage;
  friend class CTextPage;
};

/////////////////////////////////////////////////////////////////////////////

class CDrawPoly : public CDrawObj
{
protected:
  DECLARE_SERIAL(CDrawPoly);
  CDrawPoly();
  virtual ~CDrawPoly();
  
public:
  CDrawPoly(const CIntIRect& position, CDrawDoc* pDoc);
  CDrawPoly(const CDoubleIRect& position, CDrawDoc* pDoc);
  
  // Operations
  void AddPoint(const CPoint& point, CDrawView* pView = NULL);
  void AddPoint(const CDoublePoint& pt, CDrawView* pView = NULL);
  BOOL RecalcBounds(CDrawView* pView = NULL);
  int GetNumPoints() { return m_nPoints; }
  CIntPoint GetCPoint( int n ) const;
  CDoublePoint GetPoint(int n) const;
  void SetPoint(int n, CDoublePoint& pt);
  virtual CIntIRect GetClippedRect() const;

  virtual CDoubleIRect CalcBounds( CMap<double, double, double, double>& mins, CMap<double, double, double, double>& maxs );
  
  // Implementation
public:
  virtual void Serialize(CArchive& ar);
  virtual void Draw(CDC* pDC);
  virtual void MoveTo(const CIntIRect& position, CDrawView* pView = NULL);
  virtual int GetHandleCount();
  virtual CPoint GetHandle(int nHandle);
  virtual HCURSOR GetHandleCursor(int nHandle);
  virtual void MoveHandleTo(int nHandle, CIntPoint point, CDrawView* pView = NULL);
  virtual BOOL Intersects( const CIntIRect& rect ) const;
//  virtual BOOL IsContained( const CIntIRect& rect ) const;
  virtual CDrawObj* Clone(CDrawDoc* pDoc, BOOL bExactCopy = FALSE );
  void SetShape(int nShape);
  int GetShape() { return m_nShape; }
  virtual void Invalidate(BOOL bConnections = TRUE);
  virtual void SetVisibility( CDrawObj* pObj, BOOL bVisible, CDrawObjList* pUndoList );
  
  virtual void AddToDXF(CDXFZeichnung* zn, const CString& layerName);
  
  enum Shape { polyline, polygon };

public:
  void Dump( CDumpContext& dc ) const;
  
protected:
  Shape m_nShape;
  int m_nPoints;
  int m_nAllocPoints;
  CPoint* m_points;
  CDoublePoint* m_coords;
  CDrawPoly* m_pDrawObj;
  
  friend class CPolyTool;

private:
#ifdef _DEBUG
  static log4cpp::Category& m_logCat;
#endif _DEBUG
};


class CDrawItem;    // COleClientItem derived class

class CDrawOleObj : public CDrawObj
{
protected:
  DECLARE_SERIAL(CDrawOleObj);
  CDrawOleObj();
  virtual ~CDrawOleObj();
  
public:
  CDrawOleObj(const CIntIRect& position, CDrawDoc* pDoc);
  CDrawOleObj(const CDoubleIRect& position, CDrawDoc* pDoc);
  
  // Implementation
public:
  virtual void Serialize(CArchive& ar);
  virtual void Draw(CDC* pDC);
  virtual CDrawObj* Clone(CDrawDoc* pDoc, BOOL bExactCopy = FALSE );
  virtual void OnOpen(CDrawView* pView);
  virtual void MoveTo(const CIntIRect& positon, CDrawView* pView = NULL);
  virtual void OnEditProperties(CDrawView* pView);
  virtual void Remove();

  virtual void Dump( CDumpContext& dc ) const;
  
  CDrawItem* m_pClientItem;
  CSize m_extent; // current extent is tracked separate from scaled position
  
  static BOOL c_bShowItems;

private:
  static log4cpp::Category& m_logCat;
};

#endif // __DRAWOBJ_H__
