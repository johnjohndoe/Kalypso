// LinePage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CLinePage dialog

#include "..\..\commonMfc\commonMfc.h"

class CLinePage : public CPropertyPage
{
// Construction
public:
	CLinePage( CPropertyDialog *pParent, CPlotterDoc* pDoc = NULL );
	~CLinePage();

// Dialog Data
	//{{AFX_DATA(CLinePage)
	enum { IDD = IDD_LINE };
	CSpinButtonCtrl	m_spin;
	CColorComboBox	m_lcolor;
	CTreeCtrl	m_tree;
	CLineComboBox	m_ltype;
	double	m_lbreite;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CLinePage)
	public:
	virtual BOOL OnApply();
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
public:
	void UpdateTree();
	void ApplyTemplate(CTemplate *pTemp);

protected:
	CPropertyDialog *m_pParent;
	CPlotterDoc *m_pDoc;
	CPlotterView *m_pView;
	CTemplate *m_pTemp;		// used for applying templates
	CTypedPtrMap<CMapPtrToPtr, HANDLE, LPLOGPEN> m_logpens;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, CDrawObj*> m_objects;
	CTypedPtrMap<CMapPtrToWord, HANDLE, BOOL> m_updated;
	CTypedPtrList<CPtrList, HANDLE> m_items;

  enum
	{
		TYPE = 0x0001,
		WIDTH = 0x0002,
		COLOR = 0x0004
	};

	void UpdateItemAndChildren(HTREEITEM hTI, int format);
	// Generated message map functions
	//{{AFX_MSG(CLinePage)
	virtual BOOL OnInitDialog();
	afx_msg void OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangeType();
	afx_msg void OnSelchangeColor();
	afx_msg void OnChangeWidth();
	afx_msg void OnKillfocusWidth();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	friend class CPropertyDialog;
};
