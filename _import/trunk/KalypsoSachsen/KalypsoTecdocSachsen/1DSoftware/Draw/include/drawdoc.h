// drawdoc.h : interface of the CDrawDoc class
//

#ifndef _DRAWDOC_H_INCLUDED_
#define _DRAWDOC_H_INCLUDED_

#define STPL_TEXT_NONE		-1
#define STPL_TEXT_AG1		0
#define STPL_TEXT_AG2		1
#define STPL_TEXT_PB1		2
#define STPL_TEXT_PB2		3
#define STPL_TEXT_PB3		4
#define STPL_TEXT_BB1		5
#define STPL_TEXT_BB2		6
#define STPL_TEXT_BB3		7
#define STPL_TEXT_PN		8
#define STPL_TEXT_DAT		9
#define STPL_TEXT_BN		10
#define STPL_TEXT_MS		11
#define STPL_TEXT_UD		12
#define STPL_TEXT_ZU		13

#define N_STPLTEXTS			14

#include "commonMfc\include\printerSettings.h"
#include "bce\include\assocNN.h"

#include "drawobj.h"

class CSummInfo;
class CEmbeddedItem;

class CDrawLayer;
class CDrawObj;
class CDrawRect;

typedef BCE::AssocNN<CDrawObj*, CDrawLayer*> ObjLayerAssoc;
typedef CArray<CDrawLayer*, CDrawLayer*> CDrawLayerArray;

// CDrawDoc serializable data
class CDrawDocData : public CObject
{
protected:
	DECLARE_SERIAL(CDrawDocData);
	CDrawDocData();
	void DeleteContents();

	enum ValueFormat { topLeft, bottomLeft, topRight, bottomRight };

	void FormatValue(CDrawRect* pObj, BOOL bTwoValuesInRow);

// Implementation
public:
	virtual ~CDrawDocData();
	virtual void Serialize(CArchive& ar);
#ifdef _DEBUG
	void AssertValid();
#endif

	// implementation data
protected:
	CDrawObjList m_objects;		// all visible objects
	CSize m_sizeDrawing;
	int m_nMapMode;
	COLORREF m_paperColor;
	ValueFormat m_nXValueFormat;
	ValueFormat m_nYValueFormat;

  CPrinterSettings m_printerSettings;

public:
	CSize m_sizeGrid;
	BOOL m_bSnapToGrid;

private:
  CSize m_pages;

  CMap<CString, LPCTSTR, CDrawLayer*, CDrawLayer*> m_drawLayers;
  
public:
#if !defined(_MAC)
#if _MFC_VER>=0x0421
	CSummInfo* m_pSummInfo;
#endif
#endif

	friend class CDrawDoc;
};

class CDrawDoc : public COleServerDoc
{
protected: // create from serialization only
	CDrawDoc();
	DECLARE_DYNCREATE(CDrawDoc)

// Attributes
public:
	CDrawObjList* GetObjects() { return &(m_pData->m_objects); }

	const CSize& GetDrawingSize() const { return m_pData->m_sizeDrawing; }
	void SetDrawingSize( const CSize& size );
protected:
	/** Called by SetDrawingSize to give implementors a chance to change the drawing size. */
	virtual CSize AdaptDrawingSize( const CSize& size );

public:
  CSize GetGridSize() const { return m_pData->m_sizeGrid; };

	int GetMapMode() const { return m_nMapMode; }
	COLORREF GetPaperColor() const { return m_pData->m_paperColor; }
	virtual void MetersToLogical( const CDoubleIRect& input, CIntIRect& output, const CDrawObj* pObj ) const;
	virtual void LogicalToMeters( const CIntIRect& input, CDoubleIRect& output, const CDrawObj* pObj) const;
	virtual void MetersToLogical( const CDoublePoint& input, CIntPoint& output, const CDrawObj* pObj ) const;
	virtual void LogicalToMeters( const CIntPoint& input, CDoublePoint& output, const CDrawObj* pObj ) const;
  virtual void UpdateDrawing() {};
	void GetDefaultStempelText(int n, CString& str);
	CDrawView* GetView() const;
	CEmbeddedItem* GetEmbeddedItem() { return (CEmbeddedItem*)COleServerDoc::GetEmbeddedItem(); }

  int GetXValueFormat() const { return m_pData->m_nXValueFormat; }
  void SetXValueFormat( const int n ) {	m_pData->m_nXValueFormat = (CDrawDocData::ValueFormat)n; }
	int GetYValueFormat() const { return m_pData->m_nYValueFormat; }
  void SetYValueFormat( const int n ) {	m_pData->m_nYValueFormat = (CDrawDocData::ValueFormat)n; }

  CPrinterSettings* GetPrinterSettings() const { return &(m_pData->m_printerSettings); };
  CRect GetPageMargins() { return m_pData->m_printerSettings.GetMargins(); };

  UINT GetPageCount() const { return GetPages().cx * GetPages().cy; };
  CSize GetPages() const { return m_pData->m_pages; };
  void SetPages( const CSize& pages );
  CSize GetPageSize() const;

  virtual BOOL HasType( const CDrawObj* pObj, const int type, int* index = NULL ) const;
  virtual BOOL HasType( const CDrawObjList* pObs, const int type, int* index = NULL ) const;

	// private clipboard format
	static CLIPFORMAT NEAR m_cfPrivate;

// Operations
public:
	virtual CDrawObj* ObjectAt(const CPoint& point);
	virtual void Draw(CDC* pDC, CDrawView* pView);

  virtual void AddObject( CDrawObj* pObj, const int type, const int index );
  virtual void AddObjects( CDrawObjList* pObjList, const int type, const int index );
  virtual int RemoveObject( CDrawObj* pObj, int* index = NULL );
  BOOL ReplaceObject( CDrawObj* pOrgObj, CDrawObj *pNewObj );
  virtual void MoveObjectToFront( CDrawObj* pObj );
	
  virtual void InvalAll();
	
  virtual void GetDrawText( CDrawRect* pObj, CString& drawText ) { pObj->GetText( drawText ); };
  virtual void SetDrawText( CDrawRect* pObj, CString& drawText ) { pObj->SetText( drawText ); };

  void SetCompound( BOOL bCompound ) { m_bCompoundFile = bCompound; };
  BOOL GetCompound() const { return m_bCompoundFile; };

  CDrawLayer* CreateDrawLayer( const CString& string );
  CDrawLayer* GetDrawLayer( const CString& string ) const;

  void GetDrawLayers( CDrawLayerArray* pLayers, const CString& namePrefix );

// Implementation
public:
	virtual ~CDrawDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	virtual double GetLogicalXScale( const CDrawObj* pDrawObj ) const;
	virtual double GetLogicalYScale( const CDrawObj* pDrawObj ) const;

	int m_nMapMode;

public:
	CDrawDocData* m_pData;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDrawDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual void OnSetItemRects(LPCRECT lpPosCDoubleIRect, LPCRECT lpClipCDoubleIRect);
	virtual void DeleteContents();
	virtual COleServerItem* OnGetEmbeddedItem();
	//}}AFX_VIRTUAL

// Generated message map functions
protected:
	//{{AFX_MSG(CDrawDoc)
	afx_msg void OnEditCopy();
	//}}AFX_MSG
#if _MFC_VER>=0x0421
	afx_msg void OnFileSummaryinfo();
  afx_msg void OnFileDxfExport();
  afx_msg void OnUpdateFileDxfExport( CCmdUI* pCmdUI );
#endif
  virtual afx_msg BOOL OnFilePageSetup();

	DECLARE_MESSAGE_MAP()

	friend class CDrawDocData;
};

/////////////////////////////////////////////////////////////////////////////

#endif _DRAWDOC_H_INCLUDED_