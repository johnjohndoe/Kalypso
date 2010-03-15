#ifndef AFX_CALCPG3_H__42E38584_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_CALCPG3_H__42E38584_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// calcpg3.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LWACalcPage3 

class LWACalcPage3 : public CPropertyPage
{
	DECLARE_DYNCREATE(LWACalcPage3)

// Konstruktion
public:
	LWACalcPage3(CalcData *pCD = NULL);
	~LWACalcPage3();

// Dialogfelddaten
	//{{AFX_DATA(LWACalcPage3)
	enum { IDD = IDD_LWACALC_PAGE3 };
	int		m_ncar;
	BOOL	m_nfrou;
	BOOL	m_nForm;
	int		m_nposey;
	BOOL	m_nbeta;
	double	m_epsh;
	double	m_epsv;
	double	m_rny;
	double	m_cwr;
	CString	m_sm;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LWACalcPage3)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CalcData *m_pCD;
	BOOL m_bOK;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(LWACalcPage3)
	virtual BOOL OnInitDialog();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld BCECalcPage3 

class BCECalcPage3 : public CPropertyPage
{
	DECLARE_DYNCREATE(BCECalcPage3)

// Konstruktion
public:
	BCECalcPage3(CalcData* pCD = NULL);
	~BCECalcPage3();

// Dialogfelddaten
	//{{AFX_DATA(BCECalcPage3)
	enum { IDD = IDD_BCECALC_PAGE3 };
	int		m_wasserspiegel;
	int		m_VZVerlust;
	int		m_RVerlust;
	int		m_anfangsWSP;
	double	m_qmin;
	double	m_qstep;
	double	m_qmax;
	BOOL	m_brucke;
	BOOL	m_wehre;
	double	m_gefaelle;
	BOOL	m_WQDateien;
	BOOL	m_ergebnislisten;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(BCECalcPage3)
	public:
	virtual void OnOK();
	virtual BOOL OnApply();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CalcData* m_pCD;
	BOOL m_bOK;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(BCECalcPage3)
	virtual BOOL OnInitDialog();
	afx_msg void OnAnfangsWSP();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
  static const double Q_FAKTOR;
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_CALCPG3_H__42E38584_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
