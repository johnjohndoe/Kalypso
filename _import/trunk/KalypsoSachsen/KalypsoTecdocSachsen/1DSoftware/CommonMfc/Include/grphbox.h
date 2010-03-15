// GrphBox.h : header file
//

#ifndef GRPHBOX_H
#define GRPHBOX_H

//#define COMBO_ITEMHEIGHT	18

/////////////////////////////////////////////////////////////////////////////
// CGraphicComboBox window

class CGraphicComboBox : public CComboBox
{
// Construction
public:
	CGraphicComboBox();

// Attributes
public:

// Operations
public:
	virtual void Init( const BOOL bOneMore = FALSE );
	void SetHeight(UINT nHeight) { m_nItemHeight = nHeight; }

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGraphicComboBox)
	public:
	virtual int CompareItem(LPCOMPAREITEMSTRUCT lpCompareItemStruct);
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CGraphicComboBox();

protected:
	CDC* pDC;
	CBitmap bmp, *pOldBmp;
	CRect rcItem;
	int m_nItems;
	int m_nItemHeight;
	void PrepareDC(CDC& dc, LPDRAWITEMSTRUCT lpDIS);
	void OutputDC(CDC& dc);
	// Generated message map functions
protected:
	//{{AFX_MSG(CGraphicComboBox)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif	/* GRPHBOX_H */
