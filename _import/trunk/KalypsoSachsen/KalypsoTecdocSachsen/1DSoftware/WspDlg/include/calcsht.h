#ifndef AFX_CALCSHT_H__42E38581_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_CALCSHT_H__42E38581_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_

// calcsht.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// LWACalcSheet

class LWACalcPage1;
class LWACalcPage2;
class LWACalcPage3;
class LWACalcPage4;
class BCECalcPage1;
class BCECalcPage1;
class BCECalcPage2;
class BCECalcPage3;


class LWACalcSheet : public CPropertySheet
{
	DECLARE_DYNAMIC(LWACalcSheet)

public:
  static const char REPLACE_CHAR;

// Konstruktion
public:
	LWACalcSheet(CalcData *pCD, UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	LWACalcSheet(CalcData *pCD, LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);

// Attribute
public:

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(LWACalcSheet)
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~LWACalcSheet();

public:
  static void const replaceChar( const CWnd* parentWnd, const int ctrlID );

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	CalcData* m_pCD;
	LWACalcPage1 *page1;
	LWACalcPage2 *page2;
	LWACalcPage3 *page3;
	LWACalcPage4 *page4;
	//{{AFX_MSG(LWACalcSheet)
	afx_msg BOOL OnNcCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// BCECalcSheet

class BCECalcSheet : public CPropertySheet
{
	DECLARE_DYNAMIC(BCECalcSheet)

// Konstruktion
public:
	BCECalcSheet(CalcData* pCD, UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0, int nMode = 0);
	BCECalcSheet(CalcData* pCD, LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0, int nMode = 0);

// Attribute
public:

// Operationen
public:

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(BCECalcSheet)
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~BCECalcSheet();

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	CalcData* m_pCD;
	BCECalcPage1 *page1;
	BCECalcPage2 *page2;
	BCECalcPage3 *page3;
	//{{AFX_MSG(BCECalcSheet)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	friend class BCECalcPage1;
};

/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_CALCSHT_H__42E38581_142C_11D3_A4B8_0080ADAC5D6B__INCLUDED_
