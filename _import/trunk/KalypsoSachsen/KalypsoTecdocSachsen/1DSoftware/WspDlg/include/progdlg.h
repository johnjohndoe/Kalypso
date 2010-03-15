#ifndef AFX_PROGDLG_H__6AA00AE1_1D7C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_PROGDLG_H__6AA00AE1_1D7C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// progdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProgressDialog 

class ProgressDialog : public CDialog
{
// Konstruktion
public:
	ProgressDialog(CWnd* pParent = NULL);   // Standardkonstruktor
	void IncProgress();
	void SetText(CString& text);

// Dialogfelddaten
	//{{AFX_DATA(ProgressDialog)
	enum { IDD = IDD_PROGRESS };
	CProgressCtrl	m_progress;
	CString	m_text;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ProgressDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CWnd* m_pParent;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(ProgressDialog)
	virtual void OnCancel();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PROGDLG_H__6AA00AE1_1D7C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
