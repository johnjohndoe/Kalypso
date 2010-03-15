#if !defined(AFX_EXECUTEEXTERN_H__F7F930C1_00B9_11D8_B468_00104BB3E525__INCLUDED_)
#define AFX_EXECUTEEXTERN_H__F7F930C1_00B9_11D8_B468_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/**
 *  Dient dazu, ein externes Programm in einem neuen Thread zu starten und zu warten, bis es fertig ist
 *  Währenddessen wird ein modaler Dialog mit einer Meldung gezeigt
 *
 */


class CExecuteExtern : public CDialog
{
// Konstruktion
public:
	CExecuteExtern( const CString& text, const CString& cmdLine, CWnd* pParent = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CExecuteExtern)
	enum { IDD = IDD_EXECUTE_EXTERN };
	CString	m_exeName;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CExecuteExtern)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CExecuteExtern)
	virtual BOOL OnInitDialog();
	afx_msg void OnTimer(UINT nIDEvent);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
   const CString& m_cmdLine;
   PROCESS_INFORMATION m_pi;
   STARTUPINFO m_sui;
   UINT m_timerID;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_EXECUTEEXTERN_H__F7F930C1_00B9_11D8_B468_00104BB3E525__INCLUDED_
