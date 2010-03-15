#ifndef AFX_CALCPG4_H__42E38585_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_CALCPG4_H__42E38585_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// calcpg4.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LWACalcPage4 

class LWACalcPage4 : public CPropertyPage
{
	DECLARE_DYNCREATE(LWACalcPage4)

// Konstruktion
public:
	LWACalcPage4(CalcData *pCD = NULL);
	~LWACalcPage4();

// Dialogfelddaten
	//{{AFX_DATA(LWACalcPage4)
	enum { IDD = IDD_LWACALC_PAGE4 };
	int		m_zmax;
	BOOL	m_nNN;
	BOOL	m_nDat;
	BOOL	m_nDr;
	int		m_nFP;
	BOOL	m_hmo;
	BOOL	m_wsfq;
	BOOL	m_wsfl;
	double	m_dhwmax;
	double	m_vfmax;
	double	m_hzvmax;
	double	m_ffmax;
	double	m_faklhg;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LWACalcPage4)
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
	//{{AFX_MSG(LWACalcPage4)
	virtual BOOL OnInitDialog();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	afx_msg void OnCheck9();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_CALCPG4_H__42E38585_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
