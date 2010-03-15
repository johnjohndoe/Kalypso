#ifndef AFX_CALCPG1_H__42E38582_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_CALCPG1_H__42E38582_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// calcpg1.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LWACalcPage1 

class LWACalcPage1 : public CPropertyPage
{
	DECLARE_DYNCREATE(LWACalcPage1)

// Konstruktion
public:
	LWACalcPage1(CalcData* pCD = NULL);
	~LWACalcPage1();

// Dialogfelddaten
	//{{AFX_DATA(LWACalcPage1)
	enum { IDD = IDD_LWACALC_PAGE1 };
	CComboBox	m_anzahl;
	CComboBox	m_endprofil;
	CComboBox	m_anfangsprofil;
	CComboBox	m_abflussereignis;
	CComboBox	m_fliessgesetz;
	CString	m_name;
	int		m_ausdruck;
	BOOL	m_wehr;
	BOOL	m_schiess;
	int		m_bedingungen;
	double	m_hoehe;
	double	m_gefaelle;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LWACalcPage1)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CalcData *m_pCD;
	State *m_pState;
	BOOL m_bOK;
	BOOL m_bIncreasingStations;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(LWACalcPage1)
	virtual BOOL OnInitDialog();
	afx_msg void OnAnfangsbedingungen();
	afx_msg void OnAusdruck();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

class BCECalcSheet;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld BCECalcPage1 

class BCECalcPage1 : public CPropertyPage
{
	DECLARE_DYNCREATE(BCECalcPage1)

// Konstruktion
public:
	BCECalcPage1(CalcData* pCD = NULL, BCECalcSheet* pParent = NULL);
	~BCECalcPage1();

// Dialogfelddaten
	//{{AFX_DATA(BCECalcPage1)
	enum { IDD = IDD_BCECALC_PAGE1 };
	CComboBox	m_endprofil;
	CComboBox	m_anfangsprofil;
	int		m_calctype;
	int		m_rauheit;
	int		m_ausgabe;
	BOOL	m_profilnum;
	BOOL	m_ausgabewerte;
	CString	m_name;
	BOOL	m_kalmin;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(BCECalcPage1)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CalcData* m_pCD;
	State* m_pState;
	BCECalcSheet* m_pParent;
	BOOL m_bOK;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(BCECalcPage1)
	afx_msg void OnCalcType();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_CALCPG1_H__42E38582_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
