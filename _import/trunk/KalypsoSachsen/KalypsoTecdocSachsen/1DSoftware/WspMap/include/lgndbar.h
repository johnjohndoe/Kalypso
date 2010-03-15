#ifndef AFX_LGNDBAR_H__16066CB5_807C_11D3_BDAA_00104BB3E537__INCLUDED_
#define AFX_LGNDBAR_H__16066CB5_807C_11D3_BDAA_00104BB3E537__INCLUDED_

// lgndbar.h : Header-Datei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)


#include "resource.h"


class CMapDoc;

#include "mapDocListener.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CLegend 

class CLegend : public CDialog, public IMapDocListener
{
// Konstruktion
public:
	short GetActiveLayerIndex();
	CLegend();

// Dialogfelddaten
	//{{AFX_DATA(CLegend)
	enum { IDD = IDD_LEGENDBAR };
	//}}AFX_DATA

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CLegend)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CLegend)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnAfterReorderLegende();
	DECLARE_EVENTSINK_MAP()
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
  afx_msg void OnBeforeSetLayerVisibleLegende(short FAR* Index, BOOL FAR* cancel);
	afx_msg void OnAfterSetLayerVisibleLegend(short FAR* Index, BOOL FAR* isVisible);
	afx_msg void OnLayerDblClickLegend(short FAR* Index);
	afx_msg void OnRenderClickLegend(short FAR* LayerIndex, short FAR* BreakIndex, VARIANT FAR* val1, VARIANT FAR* val2);
	afx_msg void OnMouseDownLegend(short FAR* Index, short FAR* Button, short FAR* Shift, float FAR* X, float FAR* Y);
  afx_msg void OnAfterTrackingLayerDrawMap1OverView(long hDC);
  afx_msg void OnMapMouseDownOverView(short Button, short Shift, long x, long y);
  afx_msg void OnMapMouseMoveOverView(short Button, short Shift, long x, long y);
  afx_msg void OnMapMouseUpOverView(short Button, short Shift, long x, long y);


//
// Implementierung
//

public:
  void SetMapDoc( CMapDoc* doc );

  void SetBackStyle( int style ) { m_legend.SetBackStyle( style ); };

private:
	void UpdateLegend();
  void DrawOverviewLayer();
	void ClearOverview();
  void UpdateOverView(BOOL bRedraw);
  CWnd* GetDocParent();

public:
  virtual void MapDocChanged( const long type, CLayer* layer );

protected:
	void UpdateActiveLayer();

  CArray<CMoLine,CMoLine> m_pProfilLinien;      // Linien für Profilvorschau
  CArray<CMoSymbol,CMoSymbol> m_pProfilSymbole;  // Symbole für Profilvorschau
	CMoLegend m_legend;
  CMoMap m_ovMap;
  CMapDoc* m_pDoc;
  double mdownX_OV;
  double mdownY_OV;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_LGNDBAR_H__16066CB5_807C_11D3_BDAA_00104BB3E537__INCLUDED_
