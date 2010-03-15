// PlotterDoc.h : interface of the CPlotterDoc class
//
/////////////////////////////////////////////////////////////////////////////

#ifndef _PLOTTERDOC_H
#define _PLOTTERDOC_H

#include "drawdoc.h"
#include "table.h"
#include "tableKey1.h"
#include "tableKey2.h"

class CDrawObjListArray;
class CPlotterView;
class CTemplate;
class State;
class Section;
class Profil;
class CPlotterDocData;
class CProfil;
class CTitle;
class CStempel;

class CPlotterDoc : public CDrawDoc
{
  //////////////
  // TypeDefs //
  //////////////
public:
  
  // Die LayerTypen: repräsentieren die versch. Zeichnungselemente
  enum LayerType
  {
    user = 0,
      profil = 1,
      table = 2,
      tableKey1 = 3,
      tableKey2 = 4,
      title = 5,
      height = 6,
      comment = 7,
      stamp = 8,
      border = 9
  }; // LayerType
  
  
protected: // create from serialization only
  CPlotterDoc();
  DECLARE_DYNCREATE(CPlotterDoc)
    virtual void DeleteContents();
  
  // Attributes
public:
  CStempel* GetStempel();
  CStempel* GetStempel() const;
  CDrawObjList* GetUser();
  CDrawObjList* GetUser() const;
  CProfil* GetProfil();
  CProfil* GetProfil() const;
  CTable* GetTable();
  CTable* GetTable() const;
  CTableKey1* GetTableKey1();
  CTableKey1* GetTableKey1() const;
  CTableKey2* GetTableKey2();
  CTableKey2* GetTableKey2() const;
  CDrawObjList* GetRahmen();
  CDrawObjList* GetRahmen() const;
  CTitle* GetTitle();
  CTitle* GetTitle() const;
  CDrawRect* GetComment();
  CDrawRect* GetComment() const;
  CDrawRect* GetHeight();
  CDrawRect* GetHeight() const;
  
  CSize ComputeDrawingSize( CSize& tableSize, CSize& profilSize, const CSize& stampSize, const CDoubleIRect& profRect );
  
  void SetScale(CDoublePoint& scale);
  void GetScale(CDoublePoint& scale);
  void GetRealScale(CDoublePoint& scale);
  virtual void MetersToLogical( const CDoubleIRect& input, CIntIRect& output, const CDrawObj* pObj ) const;
  virtual void LogicalToMeters( const CIntIRect& input, CDoubleIRect& output, const CDrawObj* pObj ) const;
  virtual void MetersToLogical( const CDoublePoint& input, CIntPoint& output, const CDrawObj* pObj ) const;
  virtual void LogicalToMeters( const CIntPoint& input, CDoublePoint& output, const CDrawObj* pObj ) const;
  CIntPoint GetLogicalOffset( const CDrawObj* pObj ) const;
  CDoublePoint GetMetersOffset( const CDrawObj* pObj ) const;
  virtual void UpdateDrawing();
  BOOL InsertData( State* state, Section* section, BOOL bLoaded, BOOL bInserted, BOOL bAlign = FALSE );
  void AlignProfil(Profil* prof, BOOL bAlign);
  void SetRangeFrom( double x );
  double GetRangeFrom();
  void SetRangeTo(double x);
  double GetRangeTo();

  const CIntIRect& GetBorderGaps() const;
  void SetBorderGaps( const CIntIRect& borderGaps );
 
  CTable::TableFormat GetTableFormat();
  void SetTableFormat( CTable::TableFormat n );
  int GetProfilFormat();
  void SetProfilFormat( int n );
  
  /* virtual */ BOOL HasType( const CDrawObj* pObj, const int type, int* index = NULL ) const;
  /* virtual */ BOOL HasType( const CDrawObjList* pObs, const int type, int* index = NULL ) const;

  void ChangeType( CDrawObj* pObj, const int type );
  void ChangeType( const CDrawObjList& pObs, const int type );
  
  CTypedPtrArray<CObArray, State*>* GetStates() const;
  CTypedPtrArray<CObArray, Section*>* GetSections() const;
  State* GetMState( const int index ) const;
  Section* GetMSection( const int index ) const;

  virtual UINT GetPageCount() const { return m_pages.GetSize(); };

  CIntIRect GetProfilAndTableExtent();
  void GetProfilAndTableObjects( CDrawObjList* pObjList );


  Profil* m_pProfil;
  BOOL m_bAlignLowPoint;  // used by insert dialog
  double m_dAlignValue; // used by insert dialog
  
  CUIntArray m_tableHeights;

protected:
	virtual CSize AdaptDrawingSize( const CSize& size );

  // Operations
public:
  virtual void AddObject( CDrawObj* pObj, const int type, const int index );
  virtual int RemoveObject( CDrawObj* pObj, int* index = NULL );
  
  BOOL FormatText( CString& str );

 
protected:
  // comment routines
  void SetComment( CDrawRect* pObj );
  // Height routines
  void SetHeight( CDrawRect* pObj );
  // File routines
private:
  // Tabellebereich routines
  UINT CalcTableHeightsAndWidths( CView* pView, UINT& wmax1, UINT& wmax2 );
  
  void ShiftRightOfTable(CDrawObjList* pObjList);
  
  // Rahmen routines
  void UpdateRahmen(CPlotterView* pView);
  
  // Titel routines
  void UpdateComment( const CIntPoint& basePoint, CView *pView );
  
  // Height routines
  void UpdateHeight(CPlotterView *pView);
  
  // Sontiges
  void GetDrawText( CDrawRect* pObj, CString& drawText );
  void SetDrawText( CDrawRect* pObj, CString& drawText );
  
  // LCommentRoutines
  void CalculateLCommentHeight();
  void UpdateLComment();
  
protected:
  // Drawing routines
  virtual void CreateDrawing(BOOL bLoaded = FALSE, BOOL bInserted = FALSE);
  void CreateStempelFromTemplate(CTemplate *pTemp, Profil* pProf);

  void ResetLogicalCoords( CDrawObj* pObj );
private:
  double GetRealXScale() const;
  double GetRealYScale() const;
  virtual double GetLogicalXScale( const CDrawObj* pDrawObj ) const;
  virtual double GetLogicalYScale( const CDrawObj* pDrawObj ) const;
  
public:
  CPlotterDocData* m_pPData;
  CArray<CIntIRect, CIntIRect&> m_pages;
  
  // Overrides
  // ClassWizard generated virtual function overrides
  //{{AFX_VIRTUAL(CPlotterDoc)
public:
  virtual void Serialize(CArchive& ar);
  virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
  virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
  virtual void SetTitle(LPCTSTR lpszTitle);
  virtual BOOL SaveModified();
  //}}AFX_VIRTUAL
  
  // Implementation
public:
  virtual ~CPlotterDoc();
  
  // Implementation helpers
  virtual BOOL DoFileSave();
  
  // Generated message map functions
protected:
  //{{AFX_MSG(CPlotterDoc)
  afx_msg void OnUpdateFileSave(CCmdUI* pCmdUI);
  afx_msg void OnUpdateFileSaveAs(CCmdUI* pCmdUI);
  afx_msg void OnFileInsert();
  afx_msg void OnUpdateFileInsert(CCmdUI* pCmdUI);
  afx_msg void OnUpdateFilePrintSetup(CCmdUI* pCmdUI);
  afx_msg void OnFileDxfExport();
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP();
  
  friend class CLinePage;
  friend class CTextPage;
  friend class CDataPage;
  friend class CGeneralPage;
  friend class CTemplateData;
};

#endif  // _PLOTTERDOC_H

/////////////////////////////////////////////////////////////////////////////
