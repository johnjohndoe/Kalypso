// OpenDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// OpenDialog dialog

class OpenDialog : public CDialog
{
// Construction
public:
	OpenDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(OpenDialog)
	enum { IDD = IDD_OFFNEN };
	CListCtrlEx	m_list;
	CTreeCtrl	m_tree;
	CString	m_project;
	//}}AFX_DATA

	CStringArray m_filenames;
	CStringArray m_directories;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(OpenDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_smap_tree;
	CStringArray m_otherfiles;
	CStringArray m_otherdirs;
	CMapPtrToWord m_rmap_tree;

	// Generated message map functions
	//{{AFX_MSG(OpenDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnDblclkList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
