#ifndef AFX_PRJPRDLG_H__33E8CD41_5FC4_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_PRJPRDLG_H__33E8CD41_5FC4_11D3_A4B9_0080ADAC5D6B__INCLUDED_

// prjprdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectPrintDialog 

class ProjectPrintDialog : public CDialog
{
// Konstruktion
public:
	ProjectPrintDialog(CWnd* pParent = NULL, BOOL bNoSelection = FALSE);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(ProjectPrintDialog)
	enum { IDD = IDD_PROJECT_PRINT };
	int		m_type;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ProjectPrintDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	BOOL m_bNoSelection;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(ProjectPrintDialog)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PRJPRDLG_H__33E8CD41_5FC4_11D3_A4B9_0080ADAC5D6B__INCLUDED_
