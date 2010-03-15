// pageset.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPageSetupDlg dialog

class CPageSetupDlg : public CCSDialog
{
// Construction
public:
	CPageSetupDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CPageSetupDlg)
	enum { IDD = IDD_PAGE_SETUP_DIALOG };
	int		m_nTopMargin;
	int		m_nRightMargin;
	int		m_nLeftMargin;
	int		m_nBottomMargin;
	//}}AFX_DATA

// Implementation
	static const DWORD m_nHelpIDs[];
	virtual const DWORD* GetHelpIDs() {return m_nHelpIDs;}
protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

	// Generated message map functions
	//{{AFX_MSG(CPageSetupDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
