// txtppage.h : header file
//

#ifndef _TXT_PPAGE__H_INCLUDED_
#define _TXT_PPAGE__H_INCLUDED_
/////////////////////////////////////////////////////////////////////////////
// CTextPropPage dialog

class CDrawView;

class CTextPropPage : public CPropertyPage
{
// Construction
public:
  CTextPropPage::CTextPropPage( CDrawObjList* pSelection, CDrawView* pView );

// Dialog Data
	//{{AFX_DATA(CTextPropPage)
	enum { IDD = IDD_PROP_TEXT };
	CEdit	m_text;
	CSpinButtonCtrl	m_spin2;
	CComboBox	m_type;
	CSpinButtonCtrl	m_spin;
	int		m_orientation;
	int		m_horz;
	int		m_vert;
	int		m_precision;
	//}}AFX_DATA

protected:
	BOOL FontIsTrueType(LPLOGFONT lpLogFont);

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CTextPropPage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDrawObjList* m_pSelection;
	LOGFONT m_logfont;
	COLORREF m_color;
	CDrawView *m_pView;
  BOOL bTextHasChanged;
	// Generated message map functions
	//{{AFX_MSG(CTextPropPage)
	afx_msg void OnSchrift();
	virtual BOOL OnInitDialog();
	afx_msg void OnChange();
	afx_msg void OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposSpin2(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnChangeEditText();
	afx_msg void OnSelchangeCombo3();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif // _TXT_PPAGE__H_INCLUDED_