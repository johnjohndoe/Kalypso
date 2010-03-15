#if !defined(AFX_OBSERVERDIALOG_H__16EB02C1_0FD5_11D6_BED9_00104BB3E525__INCLUDED_)
#define AFX_OBSERVERDIALOG_H__16EB02C1_0FD5_11D6_BED9_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// observerdialog.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CObserverDialog 

// Diese Klasse startet und Überwacht ein Objekt vom Typ BCE::Pbservable
// Dabei stellt es eine Fortschrittsanzeige dar
// Ausserdem ist es möglich den Vorgang abzubrechen

class CObserverDialog : public CDialog
{
// Konstruktion
public:
  CObserverDialog( BCE::Observable* observedObj, LPVOID observedParam, const CString& titel, CWnd* pParent = NULL );   // Standardkonstruktor
  CObserverDialog::~CObserverDialog();

// Dialogfelddaten
	//{{AFX_DATA(CObserverDialog)
	enum { IDD = IDD_OBSERVER };
	CProgressCtrl	m_progress;
	CString	m_statusText;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CObserverDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  BCE::Observable* m_observedObj;
  LPVOID m_observedParam;
  CWinThread* m_obsThread;
  CString m_titel;
  int m_returnCode;
  UINT m_timerID;

  void stopThread();

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CObserverDialog)
	virtual BOOL OnInitDialog();
	virtual void OnCancel();
	afx_msg void OnClose();
	afx_msg void OnTimer(UINT nIDEvent);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_OBSERVERDIALOG_H__16EB02C1_0FD5_11D6_BED9_00104BB3E525__INCLUDED_
