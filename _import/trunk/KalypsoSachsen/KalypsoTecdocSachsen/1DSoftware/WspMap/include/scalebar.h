#ifndef AFX_SCALEBAR_H__96CFCAA1_9682_11D3_BDB8_00104BB3E537__INCLUDED_
#define AFX_SCALEBAR_H__96CFCAA1_9682_11D3_BDB8_00104BB3E537__INCLUDED_

// scalebar.h : Header-Datei
//

class CMapDoc;

#include "mapDocListener.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CScaleBar 

class CScaleBar : public CDialogBar, public IMapDocListener
{
// Konstruktion
public:
	CScaleBar();   // Standardkonstruktor
  void SetMapDoc( CMapDoc* doc );

	void RefreshScale();

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CScaleBar)
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

public:
  virtual void MapDocChanged( const long type, CLayer* layer );

// Implementierung
protected:
	CMoScaleBar m_scaleBar;
  CMapDoc* m_pDoc;

	virtual void DoPaint( CDC* pDC );

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CScaleBar)
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	afx_msg LRESULT HandleInitDialog( WPARAM wParam, LPARAM lParam );
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_SCALEBAR_H__96CFCAA1_9682_11D3_BDB8_00104BB3E537__INCLUDED_
