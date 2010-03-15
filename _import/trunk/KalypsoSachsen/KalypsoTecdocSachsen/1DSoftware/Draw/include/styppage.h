// styppage.h : header file
//

#ifndef _STYPPAGE_H_INCLUDED_
#define _STYPPAGE_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CSelectTypePage dialog

class CSelectTypePage : public CPropertyPage
{
	DECLARE_DYNCREATE(CSelectTypePage)

// Construction
public:
	CSelectTypePage();
	~CSelectTypePage();

// Dialog Data
	//{{AFX_DATA(CSelectTypePage)
	enum { IDD = IDD_OPTION_SELTYPE };
	int		m_select;
	double	m_tolerance;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CSelectTypePage)
	public:
	virtual void OnOK();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CSelectTypePage)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif // _STYPPAGE_H_INCLUDED_