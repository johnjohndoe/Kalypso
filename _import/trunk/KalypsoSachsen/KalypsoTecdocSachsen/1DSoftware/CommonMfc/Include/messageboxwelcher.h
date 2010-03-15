#if !defined(AFX_MESSAGEBOXWELCHER_H__4704FFB3_3656_11D6_B2B4_00104BB3E525__INCLUDED_)
#define AFX_MESSAGEBOXWELCHER_H__4704FFB3_3656_11D6_B2B4_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// messageboxwelcher.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBoxWelcher 

class CMessageBoxWelcher : public CDialog
{
// Konstruktion
public:
  CMessageBoxWelcher( const CString& title, const CString& text, const CString& firstName, const CString& secondName, CWnd* pParent = NULL );

// Dialogfelddaten
	//{{AFX_DATA(CMessageBoxWelcher)
	enum { IDD = IDD_WELCHER };
	CStatic	m_textCtrl;
	CButton	m_cancelButton;
	CButton	m_secondYesButton;
	CButton	m_secondAlwaysButton;
	CButton	m_noButton;
	CButton	m_neverButton;
	CButton	m_firstYesButton;
	CButton	m_firstAlwaysButton;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMessageBoxWelcher)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CString m_title;
  CString m_text;
  CStringArray m_names;
  CString m_result;

public:
  CString GetWelcher() { return m_result; };


	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMessageBoxWelcher)
	virtual BOOL OnInitDialog();
	afx_msg void OnFirstAlywas();
	afx_msg void OnFirstYes();
	afx_msg void OnNever();
	afx_msg void OnNo();
	afx_msg void OnSecondAlways();
	afx_msg void OnSecondYes();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MESSAGEBOXWELCHER_H__4704FFB3_3656_11D6_B2B4_00104BB3E525__INCLUDED_
