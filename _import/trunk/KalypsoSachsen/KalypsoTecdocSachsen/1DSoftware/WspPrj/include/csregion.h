#if !defined(AFX_CSREGION_H__C8860113_79BE_11D5_BE5D_00104BB3E525__INCLUDED_)
#define AFX_CSREGION_H__C8860113_79BE_11D5_BE5D_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// csregion.h : Header-Datei
//

class State;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CCsRegion 

class CCsRegion : public CDialog
{
// Konstruktion
public:
	CCsRegion( State* state, CWnd* pParent = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CCsRegion)
	enum { IDD = IDD_CS_REGION };
	CButton	m_okbutton;
	CButton	m_cancelButton;
	CComboBox	m_end;
	CComboBox	m_begin;
	//}}AFX_DATA


// �berschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktions�berschreibungen
	//{{AFX_VIRTUAL(CCsRegion)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterst�tzung
	//}}AFX_VIRTUAL

// Implementierung
  public:
    int startIndex;
    int endIndex;

protected:
  State* state;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CCsRegion)
	afx_msg void OnSelchangeBegin();
	afx_msg void OnSelchangeEnd();
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_CSREGION_H__C8860113_79BE_11D5_BE5D_00104BB3E525__INCLUDED_
