//{{AFX_INCLUDES()
//}}AFX_INCLUDES
#ifndef AFX_MAPVIEW_H__0421B741_3E84_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_MAPVIEW_H__0421B741_3E84_11D3_A4B9_0080ADAC5D6B__INCLUDED_

// mapview.h : Header-Datei
//

class CLayer;
class CMapLayer;
class CMapDoc;
class CLegend;
class CCoolDialogBar;
class CScaleBar;
class CProfilEditor;
struct NMPROJECTMNG;

class CGroupController;
class CMultiMapController;

/////////////////////////////////////////////////////////////////////////////
// Formularansicht CMapView 

#include "profilModel.h"
#include "mapDocListener.h"
#include "profilauswahl.h"

class CMapView : public CFormView, public IMapDocListener
{
protected:
	CMapView();           // Dynamische Erstellung verwendet geschützten Konstruktor
  virtual ~CMapView();
	DECLARE_DYNCREATE(CMapView)

// Formulardaten
public:
	//{{AFX_DATA(CMapView)
	enum { IDD = IDD_MAP_FORM };
	CMoMap	m_map;
	//}}AFX_DATA

// Attribute
public:
  inline CMapDoc* GetDocument() { return (CMapDoc*)CFormView::GetDocument(); }

protected:
  int m_restoreState; // siehe OnFilePrintPreview wofür das gut ist

// Operationen
public:
  void RestoreAfterPreview(); // siehe OnFilePrintPreview
	//
	// Alignment of map on printer page.  Use DrawText constants.
	//
public:
	void SetAlignment(UINT alignment) { m_alignment = alignment; }
	UINT GetAlignment() { return m_alignment; }

  double GetSearchDistance();

  void SetCursorPos( const CDoublePoint& point ) { m_cursorPos = point; };

  void Zoom( const double faktor, CMoPoint& cursor );

public:
  virtual void MapDocChanged( const long type, CLayer* layer );

protected:
	UINT	m_alignment;	// Alignment of map on printer page.
  CLegend* m_legend;
  CProfilEditor* m_profilEditor;

private:
  CDoublePoint m_cursorPos;

  CProfilModel m_profilModel;

  std::auto_ptr<CMultiMapController> m_mapController;
  std::auto_ptr<CGroupController> m_toolController;

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMapView)
	public:
	virtual void OnInitialUpdate();
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint = NULL);
	virtual void OnEndPrintPreview(CDC* pDC, CPrintInfo* pInfo, POINT point, CPreviewView* pView);
	//}}AFX_VIRTUAL

// Implementierung
protected:
  void RedrawLayerBorder();
	int GetActiveLayerIndex();
  void SetActiveProfileTrackingLayer( long profilID, BOOL bShow ); // erzeugt ein GeoEvent für das aktive Profil

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Nachrichtenzuordnungsfunktionen

  afx_msg void OnUpdateIndicator( CCmdUI* pCmdUI );

  // Kommandos aus dem Scalebar Popup-Menu
  afx_msg BOOL OnViewScalebarUnits( UINT nID );
  afx_msg void OnUpdateViewUnits( CCmdUI* pCmdUI );
  
  // Kommandos aus dem Thema Menü
  afx_msg void OnUpdateLayer( CCmdUI* pCmdUI );
  afx_msg BOOL OnLayerCommand( UINT nID );
  afx_msg void OnLayerHmoImport();
  afx_msg void OnLayerInsert();
  afx_msg void OnLayerRemove();
  afx_msg void OnLayerNew();
  afx_msg void OnLayerVerschneid();

  // Menu Karte
  afx_msg void OnMapClipboard();
  afx_msg void OnMapSaveAs();
  afx_msg void OnUpdateMap( CCmdUI* pCmdUI );

  // Kommandos aus den Menues 'GeoObjekte' und 'Tools'
  afx_msg BOOL OnToolsCommand( UINT nID );
  afx_msg void OnUpdateTools( CCmdUI* pCmdUI );

  // sonstige einzelne Kommandos
  afx_msg BOOL OnSingleCommand( UINT nID );
  afx_msg void OnUpdateSingle( CCmdUI* pCmdUI );

  afx_msg void OnFilePrintSetup();

  afx_msg BOOL OnMouseWheel( UINT nFlags, short zDelta, CPoint pt );

  // Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMapView)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnFilePrintPreview();
	afx_msg void OnMapMouseDown(short Button, short Shift, long x, long y);
	afx_msg void OnMapMouseMove(short Button, short Shift, long x, long y);
	afx_msg void OnMapDblClick();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnMapMouseUp(short Button, short Shift, long x, long y);
	afx_msg void OnAfterTrackingLayerDrawMap1(long hDC);
	DECLARE_EVENTSINK_MAP()
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_MAPVIEW_H__0421B741_3E84_11D3_A4B9_0080ADAC5D6B__INCLUDED_
