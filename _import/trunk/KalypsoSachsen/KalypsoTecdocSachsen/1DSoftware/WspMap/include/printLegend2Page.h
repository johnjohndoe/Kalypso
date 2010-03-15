#if !defined(AFX_PRINTLEGEND2PAGE_H__D08E38E3_06BF_11D7_B360_00104BB3E525__INCLUDED_)
#define AFX_PRINTLEGEND2PAGE_H__D08E38E3_06BF_11D7_B360_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// printlegend2page.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintLegend2Page 

class CPrintRectLegend2;

class CPrintLegend2Page : public CPropertyPage
{
// Konstruktion
public:
	CPrintLegend2Page( UINT captionID, CPrintRectLegend2* pPrintRectLegend2 );   // Standardkonstruktor
  virtual ~CPrintLegend2Page() {};

// Dialogfelddaten
	//{{AFX_DATA(CPrintLegend2Page)
	enum { IDD = IDD_PRINT_LEGEND2_PAGE };
		// HINWEIS: Der Klassen-Assistent fügt hier Datenelemente ein
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintLegend2Page)
protected:
  virtual void OnOK();
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CPrintRectLegend2* m_pPrintRectLegend2;

  LOGFONT m_logFont;
  COLORREF m_color;
  UINT m_ptSize; // Grösse der Schrift in Punkt


	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintLegend2Page)
	afx_msg void OnSchrift();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTLEGEND2PAGE_H__D08E38E3_06BF_11D7_B360_00104BB3E525__INCLUDED_
