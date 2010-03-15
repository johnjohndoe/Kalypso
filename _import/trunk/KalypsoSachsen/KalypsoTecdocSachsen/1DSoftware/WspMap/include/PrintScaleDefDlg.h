#ifndef AFX_PRINTSCALEDEFDLG_H__B1EE77F3_2FB7_11D4_9DE2_0090270D4773__INCLUDED_
#define AFX_PRINTSCALEDEFDLG_H__B1EE77F3_2FB7_11D4_9DE2_0090270D4773__INCLUDED_

// PrintScaleDefDlg.h : Header-Datei
//

#include "resource.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintScaleDefDlg 

class CPrintScaleDefDlg : public CDialog
{
// Konstruktion
public:
	CPrintScaleDefDlg(CWnd* pParent = NULL,CMapDoc *pDoc = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CPrintScaleDefDlg)
	enum { IDD = IDD_FILE_PRINT_SCALE };
	int		m_MapUnit;
	CString	m_scale;
	//}}AFX_DATA


// �berschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktions�berschreibungen
	//{{AFX_VIRTUAL(CPrintScaleDefDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterst�tzung
	//}}AFX_VIRTUAL

// Implementierung
protected:
    CMapDoc *m_pDoc;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintScaleDefDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio f�gt zus�tzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PRINTSCALEDEFDLG_H__B1EE77F3_2FB7_11D4_9DE2_0090270D4773__INCLUDED_
