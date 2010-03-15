#ifndef AFX_MARGPAGE_H__89EB2FA1_3B5F_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_MARGPAGE_H__89EB2FA1_3B5F_11D3_A4B9_0080ADAC5D6B__INCLUDED_

// margpage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMarginsPage 

class CMarginsPage : public CPropertyPage
{
// Konstruktion
public:
	CMarginsPage( CPropertyDialog *pParent, CPlotterDoc* pDoc = NULL );

// Dialogfelddaten
	//{{AFX_DATA(CMarginsPage)
	enum { IDD = IDD_MARGINS };
	CButton	m_checkHor;
	CButton	m_stampCheckbox;
	CComboBox	m_stampCombo;
	CComboBox	m_profilCombo;
	double	m_editLeft;
	double	m_editRight;
	double	m_editTop;
	double	m_editBottom;
	CString	m_staticBottom;
	CString	m_staticDist;
	CString	m_staticHint;
	CString	m_staticLeft;
	CString	m_staticProfil;
	CString	m_staticRight;
	CString	m_staticStempel;
	CString	m_staticTop;
	UINT	m_stampScale;
	CString	m_staticStampScale;
	CString	m_staticHor;
	CString	m_staticStamp;
	CString	m_staticProfile;
	double	m_editHor;
	double	m_editVert;
	CString	m_staticVert;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMarginsPage)
	public:
	virtual BOOL OnApply();
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
public:
	void Update();
	void ApplyTemplate(CTemplate *pTemp);

protected:
  static UINT alignMap[][2];
  static UINT alignCount;

	CPropertyDialog* m_pParent;
	CPlotterDoc* m_pDoc;
	CTemplate* m_pTemp;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMarginsPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_MARGPAGE_H__89EB2FA1_3B5F_11D3_A4B9_0080ADAC5D6B__INCLUDED_
