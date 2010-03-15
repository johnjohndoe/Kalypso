#ifndef AFX_STRDPAGE_H__03A2CB22_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_STRDPAGE_H__03A2CB22_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// strdpage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld StandardPage 

class StandardPage : public CPropertyPage
{
	DECLARE_DYNCREATE(StandardPage)

// Konstruktion
public:
	StandardPage( const BOOL bFeatureSort = TRUE, const BOOL bBCE = FALSE );
	~StandardPage();

// Dialogfelddaten
	//{{AFX_DATA(StandardPage)
	enum { IDD = IDD_STANDARD_PAGE };
	BOOL	m_auto;
	int		m_type;
	int		m_sort_type;
	int		m_laengs_sort_type;
	int		m_sort_Direction;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(StandardPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	BOOL m_bBCE;
  BOOL m_bFeatureSort;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(StandardPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnAuto();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_STRDPAGE_H__03A2CB22_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
