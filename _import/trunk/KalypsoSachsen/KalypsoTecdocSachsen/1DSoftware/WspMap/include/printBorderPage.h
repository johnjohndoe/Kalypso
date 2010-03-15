#if !defined(AFX_PRINTBORDERPAGE_H__01052AB3_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_)
#define AFX_PRINTBORDERPAGE_H__01052AB3_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// printborderpage.h : Header-Datei
//

#include "..\..\commonMfc\commonMfc.h"
#include "colorbtnex.h"

class CPrintRect;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintBorderPage 
// Die Page für die Border-Einstellungen eines CPrintRects
/////////////////////////////////////////////////////////////////////////////
class CPrintBorderPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CPrintBorderPage)

// Konstruktion
public:
	CPrintBorderPage( CPrintRect* printRect = NULL );
	~CPrintBorderPage();

// Dialogfelddaten
	//{{AFX_DATA(CPrintBorderPage)
	enum { IDD = IDD_PRINT_BORDER_PAGE };
	CEdit	m_widthCtrl;
	CLineComboBox	m_styleCombo;
	CSpinButtonCtrl	m_widthSpin;
	CColorButtonEx	m_colorButton;
	int		m_width;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintBorderPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CPrintRect* m_printRect;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintBorderPage)
	afx_msg void OnPrintBorderColor();
	afx_msg void OnDeltaposPrintBorderSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnChange();
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangePrintBorderStyle();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTBORDERPAGE_H__01052AB3_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_
