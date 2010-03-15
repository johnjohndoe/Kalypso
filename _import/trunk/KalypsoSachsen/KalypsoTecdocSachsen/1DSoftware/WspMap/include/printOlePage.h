#if !defined(AFX_PRINTOLEPAGE_H__A846C491_4E0D_11D6_B2C5_00104BB3E525__INCLUDED_)
#define AFX_PRINTOLEPAGE_H__A846C491_4E0D_11D6_B2C5_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// printolepage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintOlePage 

class CPrintRectOle;

class CPrintOlePage : public CPropertyPage
{
// Konstruktion
public:
	CPrintOlePage( UINT captionID, CPrintRectOle* printRect );
	~CPrintOlePage();

// Dialogfelddaten
	//{{AFX_DATA(CPrintOlePage)
	enum { IDD = IDD_PRINT_OLE_PAGE };
		// HINWEIS - Der Klassen-Assistent fügt hier Datenelemente ein.
		//    Innerhalb dieser generierten Quellcodeabschnitte NICHTS BEARBEITEN!
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintOlePage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintOlePage)
	afx_msg void OnPrintOleButton();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()


  CPrintRectOle* m_printRect;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTOLEPAGE_H__A846C491_4E0D_11D6_B2C5_00104BB3E525__INCLUDED_
