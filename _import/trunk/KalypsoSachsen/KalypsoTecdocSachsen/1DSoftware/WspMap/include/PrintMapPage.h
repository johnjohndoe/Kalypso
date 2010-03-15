#if !defined(AFX_PRINTMAPPAGE_H__7ED2F944_4C50_11D6_B2C3_00104BB3E525__INCLUDED_)
#define AFX_PRINTMAPPAGE_H__7ED2F944_4C50_11D6_B2C3_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PrintMapPage.h : Header-Datei
//

class CPrintRectMap;

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintMapPage 
// Die Page für die Einstellungen eines CPrintRectMap
/////////////////////////////////////////////////////////////////////////////
class CPrintMapPage : public CPropertyPage
{
// Konstruktion
public:
	CPrintMapPage( UINT captionID, CPrintRectMap* mapRect );
	~CPrintMapPage();

// Dialogfelddaten
	//{{AFX_DATA(CPrintMapPage)
	enum { IDD = IDD_PRINT_MAP_PAGE };
	CButton	m_fontButton;
	CComboBox	m_alignCombo;
	CStatic	m_scaleStatic;
	CString	m_scale;
  int	m_adjust;
	CString	m_alignText;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPrintMapPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:
  CPrintRectMap* m_mapRect;

  LOGFONT m_logFont;
  COLORREF m_color;
  UINT m_ptSize; // Grösse der Schrift in Punkt

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPrintMapPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnChangeData();
	afx_msg void OnPrintMapFontButton();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PRINTMAPPAGE_H__7ED2F944_4C50_11D6_B2C3_00104BB3E525__INCLUDED_
