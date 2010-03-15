#if !defined(AFX_MESSAGEBOX2_H__C8ED9311_3799_11D7_B382_00104BB3E525__INCLUDED_)
#define AFX_MESSAGEBOX2_H__C8ED9311_3799_11D7_B382_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// messagebox2.h : Header-Datei
//

#include "commResource.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMessageBox2 

class CMessageBox2 : public CDialog
{
// Konstruktion
public:
  CMessageBox2( const CString& title, const CString& message, const CString& okText, const CString& cancelText, const BOOL bFocusCancel, CWnd* pParent = NULL );

// Dialogfelddaten
	//{{AFX_DATA(CMessageBox2)
	enum { IDD = IDD_MESSAGE_BOX_2 };
		// HINWEIS: Der Klassen-Assistent fügt hier Datenelemente ein
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMessageBox2)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMessageBox2)
	virtual BOOL OnInitDialog();
//	virtual void OnOK();
//	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
  CString m_title;
  CString m_message;
  CString m_okText;
  CString m_cancelText;
  BOOL m_bFocusCancel;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MESSAGEBOX2_H__C8ED9311_3799_11D7_B382_00104BB3E525__INCLUDED_
