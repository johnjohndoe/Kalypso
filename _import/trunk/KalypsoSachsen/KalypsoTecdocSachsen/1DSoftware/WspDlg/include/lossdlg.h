#ifndef AFX_LOSSDLG_H__B6F28BA1_2B16_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_LOSSDLG_H__B6F28BA1_2B16_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// lossdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld LossDialog 

#include "..\..\commonMfc\commonMfc.h"

class LossDialog : public CDialog
{
// Konstruktion
public:
  LossDialog( CWnd* pParent = NULL, State* pState = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(LossDialog)
	enum { IDD = IDD_LOSS };
	CListCtrlEx	m_list;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LossDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	State *m_pState;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(LossDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_LOSSDLG_H__B6F28BA1_2B16_11D3_A4B8_0080ADAC5D6B__INCLUDED_
