#ifndef AFX_DIRPAGE_H__79ED66E3_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_DIRPAGE_H__79ED66E3_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// dirpage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld DirPage 

class DirPage : public CPropertyPage
{
	DECLARE_DYNCREATE(DirPage)

// Konstruktion
public:
	DirPage();
	~DirPage();

// Dialogfelddaten
	//{{AFX_DATA(DirPage)
	enum { IDD = IDD_DIR_PAGE };
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(DirPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CString m_maindir;
	CString m_editor;
	CString m_CAD;
	CString m_special;

	void MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen);

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(DirPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnMainDir();
	afx_msg void OnEditor();
	afx_msg void OnCAD();
	afx_msg void OnSpecial();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_DIRPAGE_H__79ED66E3_3835_11D3_A4B8_0080ADAC5D6B__INCLUDED_
