// onvertDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// ConvertDialog dialog

class ConvertDialog : public CDialog
{
// Construction
public:
	ConvertDialog(CString& filename, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(ConvertDialog)
	enum { IDD = IDD_SAVE_FORMAT };
	int		m_nType;
	CString	m_text;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(ConvertDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString m_filename;

	// Generated message map functions
	//{{AFX_MSG(ConvertDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
