// debugdlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CDebugDialog dialog

class CDebugDialog : public CDialog
{
// Construction
public:
	CDebugDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CDebugDialog)
	enum { IDD = IDD_DEBUG };
	BOOL m_bBreakInDraw;
	BOOL m_bTraceUndo;
	BOOL m_bShowPosInfo;
	BOOL m_bBreakWhenIntersected;
	CListBox m_list;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDebugDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDrawView *m_pView;

	// Generated message map functions
	//{{AFX_MSG(CDebugDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
