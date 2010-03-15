#ifndef AFX_INTRPPG3_H__6691F901_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_
#define AFX_INTRPPG3_H__6691F901_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_

// intrppg3.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CInterpPage3 

class CInterpPage3 : public CPropertyPage
{
	DECLARE_DYNCREATE(CInterpPage3)

// Konstruktion
public:
	CInterpPage3(CInterpSheet *pOwner = NULL);
	~CInterpPage3();

// Dialogfelddaten
	//{{AFX_DATA(CInterpPage3)
	enum { IDD = IDD_INTERP_PAGE3 };
	int		m_wholeProfile;
	double	m_distance;
	BOOL	m_existing;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CInterpPage3)
	public:
	virtual BOOL OnSetActive();
	virtual LRESULT OnWizardNext();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	CInterpSheet *m_pOwner;
	CString m_file;

	void MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen);
	
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CInterpPage3)
	virtual BOOL OnInitDialog();
	afx_msg void OnButton1();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_INTRPPG3_H__6691F901_9BFC_11D3_BDBE_00104BB3E537__INCLUDED_
