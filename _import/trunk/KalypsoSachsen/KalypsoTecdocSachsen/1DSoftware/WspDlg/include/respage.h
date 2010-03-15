#ifndef AFX_RESPAGE_H__03A2CB23_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_RESPAGE_H__03A2CB23_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// respage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ResPage 

class ResPage : public CPropertyPage
{
	DECLARE_DYNCREATE(ResPage)

// Konstruktion
public:
	ResPage();
	~ResPage();

// Dialogfelddaten
	//{{AFX_DATA(ResPage)
	enum { IDD = IDD_RESOLUTION_PAGE };
	BOOL	m_auto;
	int		m_size;
	CString	m_res;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(ResPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(ResPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnAuto();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_RESPAGE_H__03A2CB23_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
