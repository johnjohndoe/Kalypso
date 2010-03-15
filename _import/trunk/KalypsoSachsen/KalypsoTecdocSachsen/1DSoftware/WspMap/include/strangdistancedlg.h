#if !defined(AFX_STRANGDISTANCEDLG_H__E10560D7_D360_11D9_9697_000C29C56F8A__INCLUDED_)
#define AFX_STRANGDISTANCEDLG_H__E10560D7_D360_11D9_9697_000C29C56F8A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// strangdistancedlg.h : Header-Datei
//

#include "resource.h"

#include "commonMfc/include/gridCtrl.h"

#include "profileDistancer.h"

class State;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStrangDistanceDlg 

class CStrangDistanceDlg : public CDialog
{
public:
	private:
		State* m_state;
		const CProfileDistancer::ConnMap& m_connMap;
		CGridCtrl m_gridCtrl;
		CString m_logFile;

// Konstruktion
public:
	CStrangDistanceDlg( State* state, const CProfileDistancer::ConnMap& connMap, CString logFile, CWnd* pParent = NULL );   // Standardkonstruktor

	/** Schreibt den Inhalt des Grid in den State */
	void ApplyChanges();

// Dialogfelddaten
	//{{AFX_DATA(CStrangDistanceDlg)
	enum { IDD = IDD_STRANG_DISTANCES };
		// HINWEIS: Der Klassen-Assistent fügt hier Datenelemente ein
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CStrangDistanceDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

private:
	void SetDistance( int nRow, int nCol, double istValue, Connection* conn, CProfilModel::Zone zone );
	void SetItem( int nRow, int nCol, UINT nFormat, const CString& formatStr, double value, COLORREF color, double oldValue );
	double GetValue( int row, int col );

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CStrangDistanceDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnButtonLog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_STRANGDISTANCEDLG_H__E10560D7_D360_11D9_9697_000C29C56F8A__INCLUDED_
