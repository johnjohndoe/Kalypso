#ifndef AFX_CALCPG2_H__42E38583_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_CALCPG2_H__42E38583_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// calcpg2.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LWACalcPage2 

#include "..\..\commonMfc\commonMfc.h"

class LWACalcPage2 : public CPropertyPage
{
	DECLARE_DYNCREATE(LWACalcPage2)

// Konstruktion
public:
	LWACalcPage2(CalcData *pCD = NULL);
	~LWACalcPage2();

// Dialogfelddaten
	//{{AFX_DATA(LWACalcPage2)
	enum { IDD = IDD_LWACALC_PAGE2 };
	CListCtrlEx	m_list;
	int		m_eichung;
	BOOL	m_wqbez;
	double	m_qmin;
	double	m_qstep;
	double	m_qmax;
	BOOL	m_nasall;
	BOOL	m_qwv;
	BOOL	m_kalmin;
	BOOL	m_wtau;
	int		m_auto;
	double	m_he;
	BOOL	m_nasabs;
	BOOL	m_hgralle;
	BOOL	m_normalle;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LWACalcPage2)
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
	CWnd *pFocus;
	BOOL m_bOK;
	CString m_stations;

	void FillNasim(int nIndex);

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(LWACalcPage2)
	virtual BOOL OnInitDialog();
	afx_msg void OnWQBez();
	afx_msg void OnNasimKalmin();
	afx_msg void OnEichung();
	afx_msg void OnInsertNasim();
	afx_msg void OnDeleteNasim();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	afx_msg void OnCheck6();
	afx_msg void OnCheck7();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld BCECalcPage2 

class BCECalcPage2 : public CPropertyPage
{
	DECLARE_DYNCREATE(BCECalcPage2)

// Konstruktion
public:
	BCECalcPage2(CalcData* pCD = NULL);
	~BCECalcPage2();

// Dialogfelddaten
	//{{AFX_DATA(BCECalcPage2)
	enum { IDD = IDD_BCECALC_PAGE2 };
	CComboBox	m_abflussereignis;
	int		m_wasserspiegel;
	int		m_VZVerlust;
	int		m_anfangsWSP;
	int		m_RVerlust;
	BOOL	m_brucke;
	BOOL	m_wehre;
	double	m_gefaelle;
	double	m_hoehe;
	BOOL	m_wsfq;
	BOOL	m_wsfl;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(BCECalcPage2)
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
	BOOL m_bOK;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(BCECalcPage2)
	virtual BOOL OnInitDialog();
	afx_msg void OnAnfangsWSP();
	afx_msg void OnCheck8();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_CALCPG2_H__42E38583_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
