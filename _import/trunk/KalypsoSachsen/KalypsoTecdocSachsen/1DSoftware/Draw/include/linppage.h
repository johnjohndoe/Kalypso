// linppage.h : header file
//
#ifndef _LINEPAGE_H_INCLUDED_
#define _LINEPAGE_H_INCLUDED_
/////////////////////////////////////////////////////////////////////////////
// CLinePropPage dialog

#include "..\..\commonMfc\commonMfc.h"

class CLinePropPage : public CPropertyPage
{
// Construction
public:
  CLinePropPage( CDrawObjList* pSelection, CDrawView *pView );

// Dialog Data
	//{{AFX_DATA(CLinePropPage)
	enum { IDD = IDD_PROP_LINE };
	CSpinButtonCtrl	m_spin;
	CColorComboBox	m_color;
	CLineComboBox	m_type;
	CComboBox	m_rightArrow;
	CComboBox	m_leftArrow;
	double	m_breite;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CLinePropPage)
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
	//{{AFX_MSG(CLinePropPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnKillfocusEdit1();
	afx_msg void OnChange();
	afx_msg void OnSelchangeCombo1();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif // _LINEPAGE_H_INCLUDED_