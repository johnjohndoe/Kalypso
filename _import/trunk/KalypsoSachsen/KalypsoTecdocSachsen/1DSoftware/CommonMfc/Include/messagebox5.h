#if !defined(AFX_MESSAGEBOX5_H__365E2463_35A9_11D6_B2B3_00104BB3E525__INCLUDED_)
#define AFX_MESSAGEBOX5_H__365E2463_35A9_11D6_B2B3_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// messagebox5.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBox5 
/////////////////////////////////////////////////////////////////////////////
// Dieses Dialogfeld ist ähnlich aufgebaut wie die "normalen" Windows-Message
// boxen, hat aber 5 Schaltfkächen:
// Ja, Immer, Nein, Niemals, Abbrechen
// Als Rückgabewerte des Modalen Dialogs gibts entsprechend
// IDYES, IDALWAYS, IDNO, IDNEVER, IDCANCEL
/////////////////////////////////////////////////////////////////////////////

class CMessageBox5 : public CDialog
{
// Konstruktion
public:
	CMessageBox5( const CString& title, const CString& text, CWnd* pParent = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CMessageBox5)
	enum { IDD = IDD_MESSAGE_BOX_5 };
	CStatic	m_textCtrl;
	CButton	m_yesButton;
	CButton	m_noButton;
	CButton	m_neverButton;
	CButton	m_cancelButton;
	CButton	m_alwaysButton;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMessageBox5)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CString m_title;
  CString m_text;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMessageBox5)
	afx_msg void OnAlways();
	virtual void OnCancel();
	afx_msg void OnNever();
	afx_msg void OnNo();
	afx_msg void OnYes();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MESSAGEBOX5_H__365E2463_35A9_11D6_B2B3_00104BB3E525__INCLUDED_
