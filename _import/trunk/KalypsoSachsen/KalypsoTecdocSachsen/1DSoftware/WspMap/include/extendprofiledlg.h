#if !defined(AFX_EXTENDPROFILEDLG_H__0F984403_E56B_11D5_BEB8_00104BB3E525__INCLUDED_)
#define AFX_EXTENDPROFILEDLG_H__0F984403_E56B_11D5_BEB8_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// extendprofiledlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CExtendProfileDlg 

class Profil;
class ProfilCtrl;
struct Triple;

class CExtendProfileDlg : public CDialog
{
// Konstruktion
public:
	CExtendProfileDlg( const CString& titel, const CString& okText, 
                     Profil* profil, TripleArray* tripleArray = NULL, CWnd* pParent = NULL );   // Standardkonstruktor
  ~CExtendProfileDlg();

// Dialogfelddaten
	//{{AFX_DATA(CExtendProfileDlg)
	enum { IDD = IDD_VIEW_PROFILE };
	CStatic	m_profileDummyCtrl;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CExtendProfileDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CString m_titel;
  CString m_okText;
  Profil* m_profil;
  ProfilCtrl* m_profilCtrl;
  TripleArray m_tripleArray;
  TripleArray* m_oldTripleArray;
  COLORREF m_editColor;

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CExtendProfileDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	virtual void OnOK();
	afx_msg void OnUpdateEditAusduenn();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
  static double s_ausduennLimit;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_EXTENDPROFILEDLG_H__0F984403_E56B_11D5_BEB8_00104BB3E525__INCLUDED_
