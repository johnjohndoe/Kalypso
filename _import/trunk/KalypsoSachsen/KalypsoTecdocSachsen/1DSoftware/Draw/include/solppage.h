// solppage.h : header file
//
#ifndef _SOLPPAGE_H_INCLUDED_
#define _SOLPPAGE_H_INCLUDED_
/////////////////////////////////////////////////////////////////////////////
// CSolidPropPage dialog

class CDrawObjList;

#include "..\..\commonMfc\commonMfc.h"

class CSolidPropPage : public CPropertyPage
{
// Construction
public:
	CSolidPropPage(CDrawObjList* pSelection, CDrawView* pView );

// Dialog Data
	//{{AFX_DATA(CSolidPropPage)
	enum { IDD = IDD_PROP_SOLID };
	CBrushComboBox	m_pattern;
	CColorComboBox	m_color;
	BOOL	m_filled;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CSolidPropPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDrawObjList* m_pSelection;
	CDrawView *m_pView;
	// Generated message map functions
	//{{AFX_MSG(CSolidPropPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnChange();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif // _SOLPPAGE_H_INCLUDED_