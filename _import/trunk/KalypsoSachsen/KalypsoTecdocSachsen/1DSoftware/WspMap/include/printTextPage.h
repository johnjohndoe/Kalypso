#if !defined(AFX_PRINTTEXTPAGE_H__01052AB4_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_)
#define AFX_PRINTTEXTPAGE_H__01052AB4_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// printtextpage.h : Header-Datei
//

class CPrintRectText;

#include "resource.h"

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintTextPage 
// Die Page für die Einstellungen des CPrintRectText
class CPrintTextPage : public CPropertyPage
{
// Konstruktion
public:
	CPrintTextPage( UINT captionID, CPrintRectText* textRect );
	virtual ~CPrintTextPage();

// Dialogfelddaten
	//{{AFX_DATA(CPrintTextPage)
	enum { IDD = IDD_PRINT_TEXT_PAGE };
	CStatic	m_staticAlign;
	CStatic	m_staticText;
	int		m_radio;
	CString	m_text;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintTextPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CPrintRectText* m_textRect;

  LOGFONT m_logFont;
  COLORREF m_color;
  UINT m_ptSize; // Grösse der Schrift in Punkt

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintTextPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnSchrift();
	afx_msg void OnChangeEdit();
	afx_msg void OnRadio();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTTEXTPAGE_H__01052AB4_4AF0_11D6_B2C1_00104BB3E525__INCLUDED_
