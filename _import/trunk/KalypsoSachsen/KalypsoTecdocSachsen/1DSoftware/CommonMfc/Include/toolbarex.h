#ifndef AFX_TOOLBAREX_H__074A2102_1EF5_11D5_BDFC_00104BB3E525__INCLUDED_
#define AFX_TOOLBAREX_H__074A2102_1EF5_11D5_BDFC_00104BB3E525__INCLUDED_

// toolbarex.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Fenster CToolBarEx 

class CToolBarEx : public CToolBar
{
// Konstruktion
public:

// Attribute
public:

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CToolBarEx)
	//}}AFX_VIRTUAL

// Implementierung
public:

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	//{{AFX_MSG(CToolBarEx)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
  afx_msg LRESULT OnIdleUpdateCmdUI( WPARAM wParam, LPARAM lParam );
  	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_TOOLBAREX_H__074A2102_1EF5_11D5_BDFC_00104BB3E525__INCLUDED_
