#ifndef AFX_NEWLDLG_H__8172E241_7A4D_11D3_BDA3_00104BB3E537__INCLUDED_
#define AFX_NEWLDLG_H__8172E241_7A4D_11D3_BDA3_00104BB3E537__INCLUDED_

// newldlg.h : Header-Datei
//

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNewLayerDialog 

class CNewLayerDialog : public CDialog
{
// Konstruktion
public:
	CNewLayerDialog(CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CNewLayerDialog)
	enum { IDD = IDD_NEW_LAYER };
	CSpinButtonCtrl	m_spin;
	CListCtrlEx	m_list;
	int		m_type;
	CString	m_name;
	int		m_fieldCount;
	//}}AFX_DATA

	CString m_file;
	CString GetFieldName(int i);
	short GetFieldType(int i);

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CNewLayerDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	void MakeShortPath(CDC* pDC, CString& strLong, int nStaticLen);
	CStringArray m_fieldNames;
	CArray<short, short> m_fieldTypes;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CNewLayerDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnChangeFile();
	afx_msg void OnChangeFieldCount();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_NEWLDLG_H__8172E241_7A4D_11D3_BDA3_00104BB3E537__INCLUDED_
