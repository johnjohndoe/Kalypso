// OpenPDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// COpenProjectDialog dialog

#include "..\..\commonMfc\commonMfc.h"

class COpenProjectDialog : public CDialog
{
// Construction
public:
	COpenProjectDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(COpenProjectDialog)
	enum { IDD = IDD_OPENPROJEKT };
	CListCtrlEx	m_list;
	//}}AFX_DATA
	CString m_dir;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COpenProjectDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(COpenProjectDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnDblclkList1(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
