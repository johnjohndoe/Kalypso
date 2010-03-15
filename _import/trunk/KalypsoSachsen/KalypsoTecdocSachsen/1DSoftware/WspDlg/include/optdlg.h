#ifndef AFX_OPTDLG_H__03A2CB21_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_OPTDLG_H__03A2CB21_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// optdlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// OptionsDialog

class StandardPage;
class ResPage;
class DirPage;

class OptionsDialog : public CPropertySheet
{
	DECLARE_DYNAMIC(OptionsDialog)

// Konstruktion
public:
	OptionsDialog( const BOOL bFeatureSort, const BOOL bBCE, UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	OptionsDialog( const BOOL bFeatureSort, const BOOL bBCE, LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);

// Attribute
public:

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(OptionsDialog)
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~OptionsDialog();

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	StandardPage *page1;
	ResPage *page2;
	DirPage *page3;
	//{{AFX_MSG(OptionsDialog)
	afx_msg BOOL OnNcCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_OPTDLG_H__03A2CB21_35F0_11D3_A4B8_0080ADAC5D6B__INCLUDED_
