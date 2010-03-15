#if !defined(AFX_NUTZKLASS_H__5C3063A3_ABFF_11D5_BE85_00104BB3E525__INCLUDED_)
#define AFX_NUTZKLASS_H__5C3063A3_ABFF_11D5_BE85_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// nutzklass.h : Header-Datei
//

#include "..\..\commonMfc\commonMfc.h"

class CZList;
class CZTable;
/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNutzKlass 

class CNutzKlass : public CDialog
{
// Konstruktion
public:
	CNutzKlass( const CString& directory,  CWnd* pParent = NULL );   // Standardkonstruktor
  ~CNutzKlass();

// Dialogfelddaten
	//{{AFX_DATA(CNutzKlass)
	enum { IDD = IDD_NUTZKLASS };
	CButton	m_delRowButton;
	CButton	m_addColButton;
	CComboBox	m_colCombo;
	CButton	m_deleteColButton;
	CEdit	m_editCtrl;
	CComboBox	m_nameCombo;
	CButton	m_newButton;
	CButton	m_cancelButton;
	CButton	m_okButton;
	CString	m_staticZoTable;
	//}}AFX_DATA
  CGridCtrl m_gridCtrl;


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CNutzKlass)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
  public:

protected:
  void ShowCurZTable();
  void ReadCurZTable();

  CString m_directory;
  CZList* m_zList;
  CZTable* m_zTable;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CNutzKlass)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangeNutzklassNameCombo();
	afx_msg void OnNutzklassNewButton();
	virtual void OnOK();
  virtual void OnCancel();
	afx_msg void OnKillfocusNutzklassEdit();
	afx_msg void OnNutzklassDeleteCol();
	afx_msg void OnNutzklassDeleteRow();
	afx_msg void OnNutzklassAddCol();
	//}}AFX_MSG
  afx_msg void OnNutzklassOKButton();
  afx_msg void OnNutzklassCancelButton();
  afx_msg void OnEndlabeleditGridCtrl(NMHDR* pNMHDR, LRESULT* pResult);
	DECLARE_MESSAGE_MAP()
private:
	void FillColCombo();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_NUTZKLASS_H__5C3063A3_ABFF_11D5_BE85_00104BB3E525__INCLUDED_
