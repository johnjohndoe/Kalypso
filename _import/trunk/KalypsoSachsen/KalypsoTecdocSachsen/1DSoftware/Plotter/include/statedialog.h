#if !defined(AFX_STATEDIALOG_H__9E6B2604_985C_11D5_BE70_00104BB3E525__INCLUDED_)
#define AFX_STATEDIALOG_H__9E6B2604_985C_11D5_BE70_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// StateDialog.h : Header-Datei
//
class Project;
class State;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStateDialog 

class CStateDialog : public CDialog
{
// Konstruktion
public:
	CStateDialog( Project* project, CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CStateDialog)
	enum { IDD = IDD_STATE_DIALOG };
	CButton	m_okButton;
	CButton	m_cancelButton;
	CComboBox	m_stateCombo;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CStateDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
public:
  State* GetCurrentState();

protected:
  Project* m_project;
  State* m_state;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CStateDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_STATEDIALOG_H__9E6B2604_985C_11D5_BE70_00104BB3E525__INCLUDED_
