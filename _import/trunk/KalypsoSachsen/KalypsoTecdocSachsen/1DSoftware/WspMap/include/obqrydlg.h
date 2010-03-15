#ifndef AFX_OBQRYDLG_H__880A4BD1_771C_11D3_BD9E_00104BB3E537__INCLUDED_
#define AFX_OBQRYDLG_H__880A4BD1_771C_11D3_BD9E_00104BB3E537__INCLUDED_

// obqrydlg.h : Header-Datei
//

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CObjectQueryDialog 

struct ObjectQuery
{
	CStringArray m_fieldNames;
	CArray<long, long> m_fieldTypes;
	CArray<LONG, LONG> m_longValues;
	CArray<double, double> m_doubleValues;
	CArray<COleDateTime, COleDateTime> m_dateValues;
	CStringArray m_stringValues;
	CArray<BOOL, BOOL> m_boolValues;
};

class CObjectQueryDialog : public CDialog
{
// Konstruktion
public:
	CObjectQueryDialog(CWnd* pParent = NULL, BOOL bEdit = FALSE);   // Standardkonstruktor
	~CObjectQueryDialog();

// Dialogfelddaten
	//{{AFX_DATA(CObjectQueryDialog)
	enum { IDD = IDD_OBJECT_QUERY };
	CComboBox	m_features;
	CListCtrlEx	m_list;
	CString	m_position;
	CString	m_numfound;
	CString	m_layername;
	CString	m_objecttype;
	//}}AFX_DATA

	CTypedPtrArray<CPtrArray, ObjectQuery*> m_queries;
	long m_nShapeType;

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CObjectQueryDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	BOOL m_bEdit;
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CObjectQueryDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnSelchangeCombo1();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_OBQRYDLG_H__880A4BD1_771C_11D3_BD9E_00104BB3E537__INCLUDED_
