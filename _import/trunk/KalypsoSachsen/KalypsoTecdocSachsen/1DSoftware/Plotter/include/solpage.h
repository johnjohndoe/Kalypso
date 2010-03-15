// solpage.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CSolidPage 

#include "..\..\commonMfc\commonMfc.h"

class CSolidPage : public CPropertyPage
{
// Konstruktion
public:
	CSolidPage( CPropertyDialog* pParent, CPlotterDoc* pDoc = NULL );
	~CSolidPage();

// Dialogfelddaten
	//{{AFX_DATA(CSolidPage)
	enum { IDD = IDD_SOLID };
	CBrushComboBox	m_fpattern;
	CTreeCtrl	m_tree;
	BOOL	m_filled;
	CColorComboBox	m_fcolor;
	//}}AFX_DATA


// Überschreibungen
	// Der Klassen-Assistent generiert virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CSolidPage)
	public:
	virtual BOOL OnApply();
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
public:
	void UpdateTree();
	void ApplyTemplate(CTemplate *pTemp);

protected:
	CPropertyDialog *m_pParent;
	CPlotterDoc *m_pDoc;
	CPlotterView *m_pView;
	CTemplate *m_pTemp;		// used for applying templates
	CTypedPtrMap<CMapPtrToPtr, HANDLE, LPLOGBRUSH> m_logbrushes;
	CTypedPtrMap<CMapPtrToWord, HANDLE, WORD> m_fills;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, CDrawObj*> m_objects;
	CTypedPtrMap<CMapPtrToWord, HANDLE, BOOL> m_updated;
	CTypedPtrList<CPtrList, HANDLE> m_items;

    enum
	{
		fill = 0x0001,
		color = 0x0002,
		pattern = 0x0004
	};

	void UpdateItemAndChildren(HTREEITEM hTI, int format);
	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CSolidPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangeColor();
	afx_msg void OnCheck1();
	afx_msg void OnSelchangePattern();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};
