#if !defined(AFX_MULTIPROPS_H__527B2231_0860_11D7_B361_00104BB3E525__INCLUDED_)
#define AFX_MULTIPROPS_H__527B2231_0860_11D7_B361_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// multiprops.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CMultiProps 

class CMultiDoc;

class CMultiProps : public CDialog
{
// Konstruktion
public:
	CMultiProps( CMultiDoc* pDoc, CWnd* pParent = NULL );   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CMultiProps)
	enum { IDD = IDD_MULTI_PROPS };
	CButton	m_applyButton;
	CButton	m_distPanel;
	UINT	m_xDist;
	CString	m_distXStatic;
	UINT	m_yDist;
	CString	m_distYStatic;
	CString	m_title;
	CString	m_titleStatic;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CMultiProps)
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
  //}}AFX_VIRTUAL
  
private:
  CMultiDoc* m_pDoc;
  BOOL m_bModified;

  void SetModified( const BOOL bMod );

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CMultiProps)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnApply();
	afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MULTIPROPS_H__527B2231_0860_11D7_B361_00104BB3E525__INCLUDED_
