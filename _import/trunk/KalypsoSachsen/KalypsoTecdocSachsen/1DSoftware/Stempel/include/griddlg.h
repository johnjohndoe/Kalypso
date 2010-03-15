// griddlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CGridDialog dialog

class CDrawView;

class CGridDialog : public CDialog
{
// Construction
public:
	CGridDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CGridDialog)
	enum { IDD = IDD_GRID };
	BOOL	m_snap;
	int		m_width;
	int		m_height;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGridDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDrawView *m_pView;
	// Generated message map functions
	//{{AFX_MSG(CGridDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
