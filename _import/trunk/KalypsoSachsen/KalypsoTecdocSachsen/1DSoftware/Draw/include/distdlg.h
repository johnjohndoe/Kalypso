#ifndef AFX_DISTDLG_H__91B88EB1_8D4A_11D3_BDB1_00104BB3E537__INCLUDED_
#define AFX_DISTDLG_H__91B88EB1_8D4A_11D3_BDB1_00104BB3E537__INCLUDED_

// distdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDistanceDialog 

class CDistanceDialog : public CDialog
{
// Konstruktion
public:
	CDistanceDialog(CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CDistanceDialog)
	enum { IDD = IDD_MEASURE };
	CString	m_distance;
	CString	m_XDistance;
	CString	m_YDistance;
	//}}AFX_DATA


// �berschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktions�berschreibungen
	//{{AFX_VIRTUAL(CDistanceDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterst�tzung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CDistanceDialog)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio f�gt zus�tzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_DISTDLG_H__91B88EB1_8D4A_11D3_BDB1_00104BB3E537__INCLUDED_
