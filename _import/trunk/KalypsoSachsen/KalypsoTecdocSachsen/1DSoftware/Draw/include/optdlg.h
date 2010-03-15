// optdlg.h : header file
//

#ifndef _OPTDLG_H_INCLUDED_
#define _OPTDLG_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// COptionDialog

class CSelectTypePage;

class COptionDialog : public CPropertySheet
{
	DECLARE_DYNAMIC(COptionDialog)

// Construction
public:
	COptionDialog(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	COptionDialog(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COptionDialog)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~COptionDialog();

	// Generated message map functions
protected:
	CSelectTypePage *page1;
	//{{AFX_MSG(COptionDialog)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // _OPTDLG_H_INCLUDED_