#ifndef AFX_NMAPDLG_H__2D6A03B1_7005_11D3_BD96_00104BB3E537__INCLUDED_
#define AFX_NMAPDLG_H__2D6A03B1_7005_11D3_BD96_00104BB3E537__INCLUDED_

// nmapdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNewMapDlg 

#include "resource.h"

class CMapProject;

class CNewMapDlg : public CDialog
{
// Konstruktion
public:
	CNewMapDlg(CWnd* pParent = NULL, CMapProject* pMapProject = NULL);   // Standardkonstruktor

	CString m_name;

// Dialogfelddaten
	//{{AFX_DATA(CNewMapDlg)
	enum { IDD = IDD_NEW_MAP };
	CComboBox	m_names;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CNewMapDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CMapProject *m_pMapProject;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CNewMapDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_NMAPDLG_H__2D6A03B1_7005_11D3_BD96_00104BB3E537__INCLUDED_
