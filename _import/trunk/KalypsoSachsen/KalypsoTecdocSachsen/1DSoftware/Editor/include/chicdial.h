// chicdial.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CCSDialog dialog

class CCSDialog : public CDialog
{
// Construction
public:
	CCSDialog();
	CCSDialog(LPCTSTR lpszTemplateName, CWnd* pParentWnd = NULL);
	CCSDialog(UINT nIDTemplate, CWnd* pParentWnd = NULL);

// Implementation
protected:
	virtual const DWORD* GetHelpIDs() = 0;

	// Generated message map functions
	//{{AFX_MSG(CCSDialog)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	afx_msg LONG OnHelp(UINT wParam, LONG lParam);
	afx_msg LONG OnHelpContextMenu(UINT wParam, LONG lParam);
	DECLARE_MESSAGE_MAP()
};