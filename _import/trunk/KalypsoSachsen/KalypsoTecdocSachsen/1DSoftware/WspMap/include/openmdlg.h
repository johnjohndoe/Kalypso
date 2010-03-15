#ifndef AFX_OPENMDLG_H__1CAE3201_4598_11D3_A4B9_0080ADAC5D6B__INCLUDED_
#define AFX_OPENMDLG_H__1CAE3201_4598_11D3_A4B9_0080ADAC5D6B__INCLUDED_

// openmdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld COpenMapDialog 

#include "..\..\commonMfc\commonMfc.h"

class COpenMapDialog : public CDialog
{
// Konstruktion
public:
	COpenMapDialog(CStringArray* m_names, CStringArray* m_files, CWnd* pParent = NULL, BOOL bDelete = FALSE);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(COpenMapDialog)
	enum { IDD = IDD_MAP_OPEN };
	CListCtrlEx	m_list;
	//}}AFX_DATA

  CArray<int, int> m_numbers;


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(COpenMapDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	BOOL m_bDelete;
  CStringArray* m_names;
  CStringArray* m_files;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(COpenMapDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_OPENMDLG_H__1CAE3201_4598_11D3_A4B9_0080ADAC5D6B__INCLUDED_
