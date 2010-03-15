#ifndef AFX_KOPFTXTDLG_H__C81334C2_EAA6_11D3_9D90_0090270D4773__INCLUDED_
#define AFX_KOPFTXTDLG_H__C81334C2_EAA6_11D3_9D90_0090270D4773__INCLUDED_

// KopfTxtDlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld KopfTxtDlg 

class KopfTxtDlg : public CDialog
{
// Konstruktion
public:
  CString pFileNameKopf,pFileNameHydra;
	KopfTxtDlg( BOOL bChangeKunde, CWnd* pParent = NULL,CString start_path = "\0");   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(KopfTxtDlg)
	enum { IDD = IDD_KOPF_TXT };
	CEdit	m_kundenControl;
	CString	m_Strasse;
	CString	m_PLZStadt;
	CString	m_Telefon;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(KopfTxtDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
    CWnd* m_pParent;
    BOOL m_bChangeKunde;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(KopfTxtDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_KOPFTXTDLG_H__C81334C2_EAA6_11D3_9D90_0090270D4773__INCLUDED_
