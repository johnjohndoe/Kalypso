//{{AFX_INCLUDES()
//}}AFX_INCLUDES
#ifndef AFX_PROFILEDITOR_H__FDA28430_0CB6_11D5_BDE8_00104BB3E525__INCLUDED_
#define AFX_PROFILEDITOR_H__FDA28430_0CB6_11D5_BDE8_00104BB3E525__INCLUDED_

// profileditor.h : Header-Datei
//

#include "profilmodel.h"
#include "mapDocListener.h"


/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CProfilEditor 



class CProfilEditor : public CSizingControlBar, public IMapDocListener
{
// Konstruktion
public:
	CProfilEditor();   // Standardkonstruktor
  BOOL Create( LPCTSTR lpszWindowName, CWnd* pParentWnd, CSize sizeDefault, 
    BOOL bHasGripper, UINT nID, DWORD dwStyle = WS_CHILD | WS_VISIBLE | CBRS_TOP );

// Dialogfelddaten
	//{{AFX_DATA(CProfilEditor)
	CStatic	m_profile;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CProfilEditor)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

protected:
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CProfilEditor)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnPaint();
	//}}AFX_MSG
  afx_msg BOOL OnEraseBkgnd( CDC* pDC );

	DECLARE_MESSAGE_MAP()

// Implementierung
public:
  void SetMapDoc( CMapDoc* pDoc );
  virtual void MapDocChanged( const long type, CLayer* );

protected:
  std::auto_ptr<CProfilModel> m_profilModel;

private:
  void Update();
  void PaintDC( CDC& dc, CRect& clientRect );

  CMapDoc* m_pMapDoc;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PROFILEDITOR_H__FDA28430_0CB6_11D5_BDE8_00104BB3E525__INCLUDED_
