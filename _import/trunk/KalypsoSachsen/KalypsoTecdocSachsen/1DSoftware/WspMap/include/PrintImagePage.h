#if !defined(AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_)
#define AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CPrintRectImage;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintImagePage 
// Die Page f�r die Einstellungen einen CPrintRectImage
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


// �berschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktions�berschreibungen
	//{{AFX_VIRTUAL(CPrintImagePage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterst�tzung
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
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_PRINTIMAGEPAGE_H__FB955C96_575D_11D6_B2CD_00104BB3E525__INCLUDED_
