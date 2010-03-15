#ifndef AFX_PROGRESSDLG_H__88B7F0F1_EAE7_11D4_BDBC_00104BB3E525__INCLUDED_
#define AFX_PROGRESSDLG_H__88B7F0F1_EAE7_11D4_BDBC_00104BB3E525__INCLUDED_

// progressdlg.h : Header-Datei
//

#include "resource.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CProgressDlg 

class CProgressDlg : public CDialog
{
// Konstruktion
public:
	void SetText(CString text);
	CProgressDlg(CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CProgressDlg)
	enum { IDD = IDD_PROGRESSDLG };
	CStatic	m_static;
	CProgressCtrl	m_progress;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CProgressDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CProgressDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_PROGRESSDLG_H__88B7F0F1_EAE7_11D4_BDBC_00104BB3E525__INCLUDED_
