#ifndef AFX_IMPROPS_H__CF628A61_95B3_11D3_BDB7_00104BB3E537__INCLUDED_
#define AFX_IMPROPS_H__CF628A61_95B3_11D3_BDB7_00104BB3E537__INCLUDED_

#include "colorbtnex.h"
#include "resource.h"

class CImageLayer;
class CMapDoc;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CImageProperties 

class CImageProperties : public CDialog
{
// Konstruktion
public:
	CImageProperties( CImageLayer* pLayer, CWnd* pParent = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CImageProperties)
	enum { IDD = IDD_IMAGE_PROPS };
	CColorButtonEx m_color;
	BOOL	m_transparent;
	CString	m_name;
	CString	m_file;
	CString	m_staticColor;
	CString	m_staticFile;
	CString	m_staticName;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CImageProperties)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CImageLayer* m_pLayer;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CImageProperties)
	virtual BOOL OnInitDialog();
	afx_msg void OnTransparentColor();
	afx_msg void OnTransparent();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_IMPROPS_H__CF628A61_95B3_11D3_BDB7_00104BB3E537__INCLUDED_
