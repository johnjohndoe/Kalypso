#if !defined(AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_)
#define AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CPrintRectImage;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintImagePage 
// Die Page für die Einstellungen einen CPrintRectImage
/////////////////////////////////////////////////////////////////////////////
class CPrintImagePage : public CPropertyPage
{
// Konstruktion
public:
	CPrintImagePage( UINT captionID, CPrintRectImage* imageRect );
	~CPrintImagePage();

// Dialogfelddaten
	//{{AFX_DATA(CPrintImagePage)
	enum { IDD = IDD_PRINT_IMAGE_PAGE };
	CButton	m_sizeCheckbox;
	CString	m_fileName;
	BOOL	m_fixSize;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintImagePage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintImagePage)
	virtual BOOL OnInitDialog();
	afx_msg void OnPrintImageSearch();
	afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

  CPrintRectImage* m_imageRect;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_
