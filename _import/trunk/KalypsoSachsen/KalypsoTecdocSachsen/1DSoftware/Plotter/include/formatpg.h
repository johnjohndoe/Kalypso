#ifndef AFX_FORMATPG_H__DA2DC0A0_613B_11D3_BD79_00104BB3E537__INCLUDED_
#define AFX_FORMATPG_H__DA2DC0A0_613B_11D3_BD79_00104BB3E537__INCLUDED_

// formatpg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CFormatPage 

class CFormatPage : public CPropertyPage
{
// Konstruktion
public:
	CFormatPage( CPropertyDialog* pParent, CPlotterDoc* pDoc = NULL );
	~CFormatPage();

// Dialogfelddaten
	//{{AFX_DATA(CFormatPage)
	enum { IDD = IDD_FORMAT };
	int		m_profilFormat;
	int		m_tableFormat;
	int		m_XValueFormat;
	int		m_YValueFormat;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CFormatPage)
	public:
	virtual BOOL OnApply();
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementation
public:
	void Update();
	void ApplyTemplate(CTemplate *pTemp);

protected:
	CPropertyDialog *m_pParent;
	CPlotterDoc *m_pDoc;
	CTemplate *m_pTemp;		// used for applying templates

	void DisableYButton();
	void DisableXButton();

// Implementierung
protected:
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CFormatPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeXFormat();
	afx_msg void OnChangeYFormat();
	afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_FORMATPG_H__DA2DC0A0_613B_11D3_BD79_00104BB3E537__INCLUDED_
